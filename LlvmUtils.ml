(*
    Caraml compiler
    Copyright (C) 2012 Brian Hurt (bhurt@spnz.org)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Function;;

let get_member ptr ty i =
    let bind = Block.bind in
    match ty with
    | Type.Base(Type.Unit) ->
        (* We can elide the load altogether *)
        Block.unit_const ()
    | Type.Base(Type.Boolean) ->
        begin
            perform
                p <-- Block.offset ptr i;
                v <-- Block.load p;
                Block.int_to_bool v
        end
    | Type.Base(Type.Int) ->
        begin
            perform
                p <-- Block.offset ptr i;
                Block.load p
        end
    | Type.Arrow(_, _)
    | Type.Tuple(_) ->
        begin
            perform
                p <-- Block.offset ptr i;
                ty <-- Block.intptr_type;
                ty <-- Block.ptr_type ty;
                p' <-- Block.bitcast p ty;
                Block.load p
        end
;;

let set_member ptr ty i v =
    let bind = Block.bind in
    match ty with
    | Type.Base(Type.Boolean) ->
        begin
            perform
                p <-- Block.offset ptr i;
                v <-- Block.bool_to_int v;
                Block.store ~ptr:p ~value:v
        end
    | Type.Base(Type.Unit)
    | Type.Base(Type.Int) ->
        begin
            perform
                p <-- Block.offset ptr i;
                Block.store ~ptr:p ~value:v
        end
    | Type.Arrow(_, _)
    | Type.Tuple(_) ->
        begin
            perform
                p <-- Block.offset ptr i;
                ty <-- Block.intptr_type;
                ty <-- Block.ptr_type ty;
                p' <-- Block.bitcast p ty;
                Block.store ~ptr:p' ~value:v;
        end
;;


let make_tag_word tag tys =
    let rec mask_bits b s = function
        | Type.Arrow(_, _) :: xs
        | Type.Tuple(_) :: xs
            -> mask_bits (b lsl 1) (b lor s) xs
        | Type.Base(_) :: xs
            -> mask_bits (b lsl 1) s xs
        | [] -> s
    in
    let high = mask_bits 1 0 tys in
    let low = ((tag land 0xFFFFFF) lsl 8)
                lor (((List.length tys) - 1) lsl 3)
                lor 1
    in
    let high = Int64.of_int high in
    let high = Int64.shift_left high 32 in
    let low = Int64.of_int low in
    Int64.logor high low
;;

let heap_alloc start_block num_words =
    perform
        alloc_block <-- Function.new_block;
        _ <-- Block.in_block start_block
                (Block.br alloc_block);
        base <-- Function.lookup_global "caraml_base";
        base_r <-- Block.in_block alloc_block
                    (Block.load base);
        limit <-- Function.lookup_global "caraml_limit";
        limit_r <-- Block.in_block alloc_block
                        (Block.load limit);
        new_base_r <-- Block.in_block alloc_block
                        (Block.offset base_r (~- (num_words + 1)));
        test <-- Block.in_block alloc_block
                        (Block.ptr_cmp_lt new_base_r limit_r);
        gc_block <-- Function.new_block;
        res_block <-- Function.new_block;
        _ <-- Block.in_block alloc_block
                    (Block.cond_br ~test ~on_true:gc_block
                                                ~on_false:res_block);

        f <-- Function.lookup_global "caraml_gc";
        _ <-- Block.in_block gc_block (Block.call f []);
        _ <-- Block.in_block gc_block (Block.br alloc_block);
        _ <-- Block.in_block res_block (Block.store base new_base_r);
        r <-- Block.in_block res_block (Block.offset new_base_r 1);
        return (r, res_block)
;;


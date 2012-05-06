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

let load block ?lltype ptr off =
    let ptr =   if (off != 0) then
                    LlvmIntf.offset block ptr off
                else
                    ptr
    in
    let ptr = match lltype with
                | Some ty ->
                    LlvmIntf.bitcast block ptr (LlvmIntf.ptr_type ty)
                | None -> ptr
    in
    LlvmIntf.load block ptr
;;

let store block ?lltype ~ptr off ~value =
    let ptr =   if (off != 0) then
                    LlvmIntf.offset block ptr off
                else
                    ptr
    in
    let ptr = match lltype with
                | Some ty ->
                    LlvmIntf.bitcast block ptr (LlvmIntf.ptr_type ty)
                | None -> ptr
    in
    LlvmIntf.store block ~ptr ~value
;;

let get_member b ptr ty i =
    match ty with
    | Type.Base(Type.Unit) ->
        (* We can elide the load altogether *)
        LlvmIntf.unit_const ()
    | Type.Base(Type.Boolean) ->
        LlvmIntf.int_to_bool b (load b ptr i)
    | Type.Base(Type.Int) ->
        load b ptr i
    | Type.Base(Type.Float) ->
        load b ~lltype:LlvmIntf.float_type ptr i
    | Type.Arrow(_, _)
    | Type.Named(_)
    | Type.Tuple(_) ->
        load b ~lltype:LlvmIntf.intptr_type ptr i
;;

let set_member b ~ptr ty i ~value =
    match ty with
    | Type.Base(Type.Boolean) ->
        store b ~ptr i ~value:(LlvmIntf.bool_to_int b value)
    | Type.Base(Type.Unit)
    | Type.Base(Type.Int) ->
        store b ~ptr i ~value
    | Type.Base(Type.Float) ->
        store b ~lltype:LlvmIntf.float_type ~ptr i ~value
    | Type.Arrow(_, _)
    | Type.Named(_)
    | Type.Tuple(_) ->
        store b ~lltype:LlvmIntf.intptr_type ~ptr i ~value
;;


let make_tag_word ~tag ~len tys =
    assert ((List.length tys) <= 32);
    assert (len <= 32);
    assert (len > 0);
    assert ((List.length tys) > 0);
    let rec mask_bits b s = function
        | Type.Arrow(_, _) :: xs
        | Type.Named(_) :: xs
        | Type.Tuple(_) :: xs
            -> mask_bits (b lsl 1) (b lor s) xs
        | Type.Base(_) :: xs
            -> mask_bits (b lsl 1) s xs
        | [] -> s
    in
    let high = mask_bits 1 0 tys in
    let low = ((tag land 0xFFFFFF) lsl 8)
                lor ((len - 1) lsl 3)
                lor 1
    in
    let low = Int64.of_int low in
    if (high != 0) then
        let high = Int64.of_int high in
        let high = Int64.shift_left high 32 in
        Int64.logor high low
    else
        low
;;

let set_tag_word_length b ~len v =
    assert (len > 0);
    assert (len <= 32);
    let mask = Int64.lognot (Int64.of_int 0xF8) in
    let mask = LlvmIntf.int64_const mask in
    let len = (len - 1) lsl 3 in
    let len = LlvmIntf.int_const len in
    let v = LlvmIntf.bool_and b v mask in
    LlvmIntf.bool_or b v len
;;

let heap_alloc start_block num_words =
    assert ((num_words > 0) && (num_words <= 32));

    let alloc_block = LlvmIntf.new_block () in
    let gc_block = LlvmIntf.new_block () in
    let res_block = LlvmIntf.new_block () in

    (* start_block: *)
    let _ = LlvmIntf.br start_block alloc_block in

    (* alloc_block: *)
    let base = LlvmIntf.load_global alloc_block "caraml_base" in
    let limit = LlvmIntf.load_global alloc_block "caraml_limit" in
    let new_base = LlvmIntf.offset alloc_block base (~- (num_words + 1)) in
    let test = LlvmIntf.ptr_cmp_lt alloc_block new_base limit in
    let _ = LlvmIntf.cond_br alloc_block ~test ~on_true:gc_block
                                ~on_false:res_block in

    (* gc_block: *)
    let f = LlvmIntf.lookup_function "caraml_gc" in
    let nwords = LlvmIntf.int_const (num_words + 1) in
    let _ = LlvmIntf.void_call gc_block f [ nwords ] in
    let _ = LlvmIntf.br gc_block alloc_block in

    (* res_block: *)
    let _ = LlvmIntf.store res_block
                ~ptr:(LlvmIntf.lookup_global "caraml_base")
                ~value:new_base
    in
    let r = LlvmIntf.offset res_block new_base 1 in
    (r, res_block)
;;


type get_val_t = LlvmIntf.block_t -> (Llvm.llvalue * LlvmIntf.block_t);;

let alloc_closure start_block nargs ~tag_word ~fn_ptr applied_vals =
    (* Allocate the closure *)
    let (p, b) = heap_alloc start_block
                        (2 + (List.length applied_vals))
    in

    (* Store the tag word *)
    let (t_word, b) = tag_word b in
    let _ = store b ~ptr:p (-1) ~value:t_word in

    (* Set the apply function table pointer *)
    let table_name =
        Config.apply_table_name nargs (List.length applied_vals)
    in
    let table = LlvmIntf.lookup_global table_name in
    let _ = store b
                ~lltype:(LlvmIntf.ptr_type
                            (LlvmIntf.app_table_type ()))
                ~ptr:p 0 ~value:table
    in

    (* Set the function pointer *)
    let (fn_ptr, b) = fn_ptr b in
    let _ = store b ~ptr:p ~value:fn_ptr in

    (* Save the applied values *)
    let b =
        Utils.fold_lefti ~start:2
            (fun i b f ->
                let (v, b) = f b in
                let _ = store b ~ptr:p i ~value:v in
                b)
            b
            applied_vals
    in

    (p, b)
;;

let apply block closure args =
    let lltype = LlvmIntf.ptr_type (LlvmIntf.app_table_type ()) in
    let table_p = load block ~lltype closure 0 in
    let fn_p = LlvmIntf.struct_gep block table_p ((List.length args) - 1) in
    let fn_p = LlvmIntf.load block fn_p in
    LlvmIntf.call block fn_p (closure :: args)
;;



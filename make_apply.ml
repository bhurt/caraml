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


(* Terminology:
 *
 * nparams: the number of parameters the underlying function has.
 * nvals:   the number of values that have already been applied to
 *          the closure (this will always be less than nparams).
 * nargs:   The number of arguments to the apply function (the number
 *          of new values to apply to the closure).  Note that nargs
 *          can be larger than nparams, as can nvals+nargs.
 *)

let apply_fn_name nparams nvals nargs =
    Printf.sprintf "caraml_apply_%d_%d_%d" nparams nvals nargs
;;

let vals nvals ptr =
    Block.seq
        (Utils.unfoldi
            (fun i ->
                let bind = Block.bind in
                perform
                    p <-- Block.offset ptr (i + 2);
                    Block.load p)
            nvals)
;;

let fn_ptr nparams ptr =
    let bind = Block.bind in
    perform
        p <-- Block.offset ptr 1;
        ty <-- Block.make_app_fn_type nparams;
        ptr_ty <-- Block.ptr_type ty;
        ptr_ty <-- Block.ptr_type ty;
        p <-- Block.bitcast p ptr_ty;
        Block.load p
;;

let make_apply_fn nparams nvals nargs =
    if (nparams < (nvals + nargs)) then
        begin
            (* We can call the function, and apply the remaining vals/args
             * to the result.
             *)
            let bind = Function.bind in
            perform
                ps <-- Function.params;
                b <-- Function.entry_block;
                Block.in_block b
                    begin
                        let bind = Block.bind in
                        perform
                            vs <-- vals nvals (List.hd ps);
                            f_ptr <-- fn_ptr nparams (List.hd ps);
                            r <-- Block.call f_ptr
                                    (Utils.take nparams
                                        (List.append vs (List.tl ps)));
                            app_table_t <-- Block.app_table_type;
                            app_table_ptr_t <-- Block.ptr_type app_table_t;
                            ptr <-- Block.bitcast r app_table_ptr_t;
                            table_p <-- Block.load ptr;
                            fn_p <-- Block.offset table_p
                                            (nvals + nargs - nparams);
                            fn_p <-- Block.load fn_p;
                            res <-- Block.call fn_p
                                    (Utils.drop nparams
                                        (List.append vs (List.tl ps)));
                            _ <-- Block.set_tail_call res;
                            _ <-- Block.ret res;
                            Block.return ()
                    end

        end
    else if (nparams == (nvals + nargs)) then
        begin
            (* We just (tail-)call the function. *)
            let bind = Function.bind in
            perform
                ps <-- Function.params;
                b <-- Function.entry_block;
                Block.in_block b
                    begin
                        let bind = Block.bind in
                        perform
                            vs <-- vals nvals (List.hd ps);
                            f_ptr <-- fn_ptr nparams (List.hd ps);
                            res <-- Block.call f_ptr
                                    (List.append vs (List.tl ps));
                            _ <-- Block.set_tail_call res;
                            _ <-- Block.ret res;
                            Block.return ()
                    end
        end
    else
        begin
            (* We can't call the function at all, just allocate a new
             * closure.
             *)
            let bind = Function.bind in
            perform
                start_block <-- Function.entry_block;
                (p, b) <-- LlvmUtils.heap_alloc start_block
                                                (2 + nvals + nargs);
                ps <-- Function.params;
                Block.in_block b
                    begin
                        let c_ptr = List.hd ps in
                        let params = List.tl ps in
                        let bind = Block.bind in
                        perform
                            tag_ptr <-- Block.offset c_ptr (-1);
                            tword <-- Block.load tag_ptr;
                            tword <-- LlvmUtils.set_tag_word_length
                                        ~len:(2 + nvals + nargs)
                                        tword;
                            p0 <-- Block.offset p (-1);
                            _ <-- Block.store ~ptr:p0 ~value:tword;
                            app_table_t <-- Block.app_table_type;
                            app_table_ptr_t <-- Block.ptr_type app_table_t;
                            ptr <-- Block.bitcast p app_table_ptr_t;
                            table <-- Block.load_global
                                            (Config.apply_table_name nparams
                                                (nvals + nargs));
                            _ <-- Block.store ~ptr ~value:table;
                            p0 <-- Block.offset c_ptr 1;
                            t <-- Block.load p0;
                            p0 <-- Block.offset p 1;
                            _ <-- Block.store ~ptr:p0 ~value:t;
                            _ <-- Block.seq
                                    (Utils.unfoldi
                                        (fun i ->
                                            perform
                                                src <-- Block.offset c_ptr
                                                                (i + 2);
                                                v <-- Block.load src;
                                                dst <-- Block.offset p (i + 2);
                                                Block.store ~ptr:dst ~value:v)
                                        nvals);
                            _ <-- Block.seq
                                    (Utils.mapi ~start:(2 + nvals)
                                        (fun i v ->
                                            perform
                                                dst <-- Block.offset p i;
                                                Block.store ~ptr:dst
                                                            ~value:v)
                                        params);
                            rval <-- Block.box_ptr p;
                            _ <-- Block.ret rval;
                            Block.return ()
                    end
        end
;;

(*
let _ =
    let name = apply_fn_name nparams nvals nargs in name
;;
*)

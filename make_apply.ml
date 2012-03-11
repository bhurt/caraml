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

let _ = LlvmIntf.with_module ~no_tables:true "caraml_apply";;

let fn_ptr block nparams ptr =
    let fn_ty = LlvmIntf.func_type
                    (Utils.repeat nparams LlvmIntf.int_type)
                    LlvmIntf.int_type
    in
    let ty = LlvmIntf.ptr_type fn_ty in
    LlvmUtils.load block ~lltype:ty ptr 1
;;

(* Not tail recursive, but since lists passed to this function will never
 * be more than Config.max_args = 8 long, I don't care.
 *)
let rec in_block block = function
    | [] -> [], block
    | f :: fs ->
        let x, block = f block in
        let xs, block = in_block block fs in
        (x :: xs), block
;;

let make_apply_fn nparams nvals nargs =
    let fn_type = LlvmIntf.make_app_fn_type nargs in
    let fn_name =
        Printf.sprintf "caraml_apply_%d_%d_%d" nparams nvals nargs
    in
    let _ = LlvmIntf.with_function fn_name fn_type in
    let ps = LlvmIntf.params () in
    let ptr = List.hd ps in
    let vals = 
        List.append
            (Utils.unfoldi
                (fun i b -> (LlvmUtils.load b ptr (i + 2)), b)
                nvals)
            (List.map (fun x b -> x,b) (List.tl ps))
    in

    let block = LlvmIntf.entry_block () in
    let (r, block) = 

        if (nparams < (nvals + nargs)) then

            (* We can call the function, and apply the remaining vals/args
             * to the result.
             *)
            let (xs, ys) = Utils.take_drop nparams vals in
            let (xs, block) = in_block block xs in
            let fn_ptr = fn_ptr block nparams ptr in
            let r = LlvmIntf.call block fn_ptr xs in
            let r = LlvmIntf.unbox_ptr block r in
            let (ys, block) = in_block block ys in
            let r = LlvmUtils.apply block r ys in
            let _ = LlvmIntf.set_tail_call r in
            r, block

        else if (nparams == (nvals + nargs)) then

            (* We just (tail-)call the function. *)
            let (xs, block) = in_block block vals in
            let fn_ptr = fn_ptr block nparams ptr in
            let r = LlvmIntf.call block fn_ptr xs in
            let _ = LlvmIntf.set_tail_call r in
            r, block

        else

            (* We can't call the function at all, just allocate a new
             * closure.
             *)
        
            let (r, block) = LlvmUtils.alloc_closure block nparams
                ~tag_word:(fun b ->
                    let t_word = LlvmUtils.load b ptr (-1) in
                    (LlvmUtils.set_tag_word_length b
                        ~len:(2 + nvals + nargs)
                        t_word), b)
                ~fn_ptr:(fun b ->
                    (LlvmUtils.load b ~lltype:LlvmIntf.intptr_type ptr 1), b)
                vals
            in
            (LlvmIntf.box_ptr block r), block

    in
    let _ = LlvmIntf.ret block r in
    let _ = LlvmIntf.end_function () in
    fn_name
;;

let make_table nparams nvals =
    let fn_names =
        Utils.unfoldi (fun i -> make_apply_fn nparams nvals (i + 1))
            Config.max_args
    in
    let fns = List.map LlvmIntf.lookup_function fn_names in
    let c = LlvmIntf.const_struct fns in
    let table_name = Config.apply_table_name nparams nvals in
    let _ = LlvmIntf.define_global table_name c in
    ()
;;

for nparams = 1 to Config.max_args do
    for nvals = nparams-1 downto 0 do
        make_table nparams nvals
    done
done;;

(*
let _ = LlvmIntf.dump_module ();;
*)

match Llvm_analysis.verify_module (LlvmIntf.mdl ()) with
    | None -> ()
    | Some reason ->
        Printf.fprintf stderr "Compile failed: %s\n%!" reason
;;

let _ = LlvmIntf.write_bitcode_file "caraml_apply.bc";;



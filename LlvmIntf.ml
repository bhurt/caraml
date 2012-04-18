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

let ctx = Llvm.create_context ();;

let void_type = Llvm.void_type ctx;;

let unit_type = Llvm.i64_type ctx;;

let bool_type = Llvm.i1_type ctx;;

let int_type = Llvm.i64_type ctx;;

let float_type = Llvm.double_type ctx;;

let intptr_type = Llvm.pointer_type int_type;;

let ptr_type base = Llvm.pointer_type base;;

let func_type arg_types ret_type =
    Llvm.function_type ret_type (Array.of_list arg_types)
;;

let struct_type tys =
    Llvm.struct_type ctx (Array.of_list tys)
;;

let llvm_of_type = function
    | Type.Base(Type.Unit) -> unit_type
    | Type.Base(Type.Int) -> int_type
    | Type.Base(Type.Boolean) -> bool_type
    | Type.Base(Type.Float) -> float_type
    | Type.Arrow(_, _)
    | Type.Tuple(_) -> intptr_type
;;

let int_const x = Llvm.const_int int_type x;;

let int64_const x = Llvm.const_of_int64 int_type x true;;

let float_const x = Llvm.const_float float_type x;;

let bool_const x = Llvm.const_int bool_type (if x then 1 else 0);;

let unit_const () = Llvm.const_int unit_type 0;;

let const_struct xs = Llvm.const_struct ctx (Array.of_list xs);;

let int_init = Llvm.const_null int_type;;

let float_init = Llvm.const_null float_type;;

let bool_init = Llvm.const_null bool_type;;

let unit_init = Llvm.const_null unit_type;;

let ptr_init = Llvm.const_pointer_null intptr_type;;

let init_of_type = function
    | Type.Base(Type.Int) -> int_init
    | Type.Base(Type.Boolean) -> bool_init
    | Type.Base(Type.Unit) -> unit_init
    | Type.Base(Type.Float) -> float_init
    | Type.Arrow(_, _)
    | Type.Tuple(_) -> ptr_init
;;

let mdl_ref : Llvm.llmodule option ref = ref None;;

let mdl () =
    match !mdl_ref with
    | None -> assert false
    | Some m -> m
;;

let dump_module () = Llvm.dump_module (mdl ());;

let lookup_global name =
    match (Llvm.lookup_global name (mdl ())) with
    | None ->
        failwith
            (Printf.sprintf "Unknown global named \"%s\""
                                    name)
    | Some v -> v
;;

let lookup_function name =
    match (Llvm.lookup_function name (mdl ())) with
    | None ->
        failwith
            (Printf.sprintf "Unknown function named \"%s\"" name)
    | Some v -> v
;;

let define_global name value = Llvm.define_global name value (mdl ());;

let declare_global name typ = Llvm.declare_global typ name (mdl ());;

let declare_function name typ =
    Llvm.declare_function name typ (mdl ())
;;

let app_table_type () =
    match Llvm.type_by_name (mdl ()) "caraml_app_table_t" with
    | Some t -> t
    | None -> assert false
;;

let write_bitcode_file fname =
    Llvm_bitwriter.write_bitcode_file (mdl ()) fname
;;

let make_app_fn_type num_args =
    func_type (intptr_type :: (Utils.repeat num_args int_type)) int_type
;;

let make_app_table_type () =
    struct_type
        (Utils.unfoldi
            (fun i ->
                ptr_type (make_app_fn_type (i + 1)))
            Config.max_args)
;;

let init_module no_tables =
    begin
        let m = mdl () in
        let table_type = make_app_table_type () in
        let _ = Llvm.define_type_name "caraml_app_table_t" table_type m in
        let _ = Llvm.declare_global intptr_type "caraml_base" m in
        let _ = Llvm.declare_global intptr_type "caraml_limit" m in
        let _ = Llvm.declare_function "caraml_gc"
                        (func_type [ int_type ] void_type)
                        m
        in
        if not no_tables then
            begin
                for i = 1 to Config.max_args do
                    for j = 0 to i-1 do
                        let _ = Llvm.declare_global table_type
                                    (Config.apply_table_name i j)
                                    m
                        in ()
                    done;
                done;
            end
        else
        ();
        ()
    end
;;

let with_module ?(no_tables=false) name =
    let m = Llvm.create_module ctx name in
    mdl_ref := Some m;
    init_module no_tables;
    ()
;;

let fn_ref : Llvm.llvalue option ref = ref None;;

let fn () =
    match !fn_ref with
    | None -> assert false
    | Some f -> f
;;

let reg_count = ref 0;;

let block_count = ref 0;;

let with_function name fn_type =
    let v = Llvm.define_function name fn_type (mdl ()) in
    fn_ref := Some v;
    reg_count := 0;
    block_count := 0;
    ()
;;

let end_function () =
    fn_ref := None;
(*
    begin
        match Llvm_analysis.verify_module (mdl ()) with
        | None -> ()
        | Some reason ->
            let _ = dump_module () in
            let _ = Printf.fprintf stderr "Error: %s\n%!" reason in
            exit (-1)
    end;
*)
    ()
;;

let alloc_reg_name () =
    incr reg_count;
    Printf.sprintf "r%d" !reg_count
;;

let alloc_block_name () =
    incr block_count;
    Printf.sprintf "block%d" !block_count
;;

let param n = Llvm.param (fn ()) n;;

let params () = Array.to_list (Llvm.params (fn ()));;

type block_t = {
    blk : Llvm.llbasicblock;
    builder : Llvm.llbuilder;
};;

let make_block basic_block =
    let bldr = Llvm.builder_at_end ctx basic_block in
    {
        blk = basic_block;
        builder = bldr;
    }
;;

let entry_block () = make_block (Llvm.entry_block (fn ()));;

let new_block () =
    make_block
        (Llvm.append_block ctx (alloc_block_name ()) (fn ()));;

let ret b v = Llvm.build_ret v b.builder;;

let ret_void b = Llvm.build_ret_void b.builder;;

let br b dest = Llvm.build_br dest.blk b.builder;;

let cond_br b ~test ~on_true ~on_false =
    Llvm.build_cond_br test on_true.blk on_false.blk b.builder
;;

let binop op b x y =
    let name = alloc_reg_name () in
    op x y name b.builder
;;

let add b x y = binop Llvm.build_add b x y;;
let sub b x y = binop Llvm.build_sub b x y;;
let mul b x y = binop Llvm.build_mul b x y;;
let div b x y = binop Llvm.build_sdiv b x y;;
let bool_and b x y = binop Llvm.build_and b x y;;
let bool_or b x y = binop Llvm.build_or b x y;;

let compare t b x y = binop (Llvm.build_icmp t) b x y;;
let lt b x y = compare Llvm.Icmp.Slt b x y;;
let le b x y = compare Llvm.Icmp.Sle b x y;;
let gt b x y = compare Llvm.Icmp.Sgt b x y;;
let ge b x y = compare Llvm.Icmp.Sge b x y;;
let eq b x y = compare Llvm.Icmp.Eq b x y;;
let ne b x y = compare Llvm.Icmp.Ne b x y;;

let unop op b x =
    let name = alloc_reg_name () in
    op x name b.builder
;;

let neg b x = unop Llvm.build_neg b x;;
let bool_not b x = unop Llvm.build_not b x;;

let fadd b x y = binop Llvm.build_fadd b x y;;
let fsub b x y = binop Llvm.build_fsub b x y;;
let fmul b x y = binop Llvm.build_fmul b x y;;
let fdiv b x y = binop Llvm.build_fdiv b x y;;

let fcompare t b x y = binop (Llvm.build_fcmp t) b x y;;
let flt b x y = fcompare Llvm.Fcmp.Ult b x y;;
let fle b x y = fcompare Llvm.Fcmp.Ule b x y;;
let fgt b x y = fcompare Llvm.Fcmp.Ugt b x y;;
let fge b x y = fcompare Llvm.Fcmp.Uge b x y;;
let feq b x y = fcompare Llvm.Fcmp.Ueq b x y;;
let fne b x y = fcompare Llvm.Fcmp.Une b x y;;

let fneg b x = unop Llvm.build_fneg b x;;

let phi b vs =
    let name = alloc_reg_name () in
    let vs =
        List.map
            (fun (v, b) -> v, b.blk)
            vs
    in
    Llvm.build_phi vs name b.builder
;;

let load b v =
    let name = alloc_reg_name () in
    Llvm.build_load v name b.builder
;;

let store b ~ptr ~value = Llvm.build_store value ptr b.builder;;

let offset b ptr idx =
    let name = alloc_reg_name () in
    let i = int_const idx in
    Llvm.build_in_bounds_gep ptr [| i |] name b.builder
;;

let struct_gep b ptr idx =
    let name = alloc_reg_name () in
    Llvm.build_struct_gep ptr idx name b.builder
;;
    

let call b f xs =
    let name = alloc_reg_name () in
    Llvm.build_call f (Array.of_list xs) name b.builder
;;

let void_call b f xs =
    Llvm.build_call f (Array.of_list xs) "" b.builder
;;

let int_to_bool b x =
    let name = alloc_reg_name () in
    Llvm.build_trunc x unit_type name b.builder
;;

let bool_to_int b x =
    let name = alloc_reg_name () in
    Llvm.build_zext x int_type name b.builder
;;
    
let box_unit _ x = x;;

let box_bool b x = bool_to_int b x;;

let box_int _ x = x;;

let box_float b x =
    let name = alloc_reg_name () in
    Llvm.build_bitcast x int_type name b.builder
;;

let box_ptr b x =
    let name = alloc_reg_name () in
    Llvm.build_ptrtoint x int_type name b.builder
;;

let box = function
    | Type.Base(Type.Unit) -> box_unit
    | Type.Base(Type.Int) -> box_int
    | Type.Base(Type.Boolean) -> box_bool
    | Type.Base(Type.Float) -> box_float
    | Type.Arrow(_, _)
    | Type.Tuple(_) -> box_ptr
;;

let unbox_unit _ x = x;;

let unbox_bool b x = int_to_bool b x;;

let unbox_int _ x = x;;

let unbox_float b x =
    let name = alloc_reg_name () in
    Llvm.build_bitcast x float_type name b.builder
;;

let unbox_ptr b x =
    let name = alloc_reg_name () in
    Llvm.build_inttoptr x intptr_type name b.builder
;;

let unbox = function
    | Type.Base(Type.Unit) -> unbox_unit
    | Type.Base(Type.Int) -> unbox_int
    | Type.Base(Type.Boolean) -> unbox_bool
    | Type.Base(Type.Float) -> unbox_float
    | Type.Arrow(_, _)
    | Type.Tuple(_) -> unbox_ptr
;;

let bitcast b x ty =
    let name = alloc_reg_name () in
    Llvm.build_bitcast x ty name b.builder
;;

let ptr_cmp_lt b x y =
    let xname = alloc_reg_name () in
    let yname = alloc_reg_name () in
    let name = alloc_reg_name () in
    let r1 = Llvm.build_ptrtoint x int_type xname b.builder in
    let r2 = Llvm.build_ptrtoint y int_type yname b.builder in
    Llvm.build_icmp Llvm.Icmp.Ult r1 r2 name b.builder
;;

let load_global b name =
    let g = lookup_global name in
    let reg = alloc_reg_name () in
    Llvm.build_load g reg b.builder
;;

let set_tail_call ?(is_tail=true) call_inst =
    Llvm.set_tail_call is_tail call_inst
;;



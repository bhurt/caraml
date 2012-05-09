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

module InnerExpr = struct

    let oversized_tuple _ =
        Format.print_string
            "Tuples of more than 32 elements not supported yet."
    ;;

    let load_members block names ptr args =
        let f i m (ty, v) =
            match v with
            | None -> m
            | Some t ->
                let x = LlvmUtils.get_member block ptr ty i in
                Common.Var.Map.add t x m
        in
        Utils.fold_lefti f names args
    ;;

    let get_name block names name : Llvm.llvalue =
        try
            Common.Var.Map.find name names
        with
        | Not_found ->
            LlvmIntf.load_global block (Common.Var.to_string name)
    ;;

    let write_tuple tag block names ptr xs =
        let tag_word = LlvmUtils.make_tag_word ~tag ~len:(List.length xs)
                            (List.map fst xs)
        in
        let f i (ty, v) =
            let value = get_name block names v in
            let _ = LlvmUtils.set_member block ~ptr ty i ~value in
            i + 1
        in
        let tagptr = LlvmIntf.offset block ptr (-1) in
        let tag_word = LlvmIntf.int64_const tag_word in
        let _ = LlvmIntf.store ~ptr:tagptr ~value:tag_word in
        let _ = List.fold_left f 0 xs in
        ()
    ;;

    let binop_ins = function
        | Common.BinOp.Times -> LlvmIntf.mul
        | Common.BinOp.Divide -> LlvmIntf.div
        | Common.BinOp.Add -> LlvmIntf.add
        | Common.BinOp.Subtract -> LlvmIntf.sub
        | Common.BinOp.Le -> LlvmIntf.le
        | Common.BinOp.Ge -> LlvmIntf.ge
        | Common.BinOp.Lt -> LlvmIntf.lt
        | Common.BinOp.Gt -> LlvmIntf.gt
        | Common.BinOp.Eq -> LlvmIntf.eq
        | Common.BinOp.Ne -> LlvmIntf.ne
        | Common.BinOp.And -> LlvmIntf.bool_and
        | Common.BinOp.Or -> LlvmIntf.bool_or

        | Common.BinOp.FTimes -> LlvmIntf.fmul
        | Common.BinOp.FDivide -> LlvmIntf.fdiv
        | Common.BinOp.FAdd -> LlvmIntf.fadd
        | Common.BinOp.FSubtract -> LlvmIntf.fsub
        | Common.BinOp.FLe -> LlvmIntf.fle
        | Common.BinOp.FGe -> LlvmIntf.fge
        | Common.BinOp.FLt -> LlvmIntf.flt
        | Common.BinOp.FGt -> LlvmIntf.fgt
        | Common.BinOp.FEq -> LlvmIntf.feq
        | Common.BinOp.FNe -> LlvmIntf.fne

    ;;

    let unop_ins = function
        | Common.UnOp.Neg -> LlvmIntf.neg
        | Common.UnOp.Not -> LlvmIntf.bool_not
        | Common.UnOp.FNeg -> LlvmIntf.fneg
    ;;

    let box block names (ty, n) =
        let v = get_name block names n in
        LlvmIntf.box ty block v
    ;;


    let rec assemble names start_block = function
        | CallOpt.InnerExpr.Let(_, _, (_, n), x, y) ->
            let (r, b) = assemble names start_block x in
            let names =
                match n with
                | None -> names
                | Some v -> Common.Var.Map.add v r names
            in
            assemble names b y

        | CallOpt.InnerExpr.If(_, _, x, y, z) ->
            let (r, b) = assemble names start_block x in

            let true_start = LlvmIntf.new_block () in
            let (true_r, true_b) = assemble names true_start y in

            let false_start = LlvmIntf.new_block() in
            let (false_r, false_b) = assemble names false_start z in

            let _ = LlvmIntf.cond_br b ~test:r ~on_true:true_start
                                            ~on_false:false_start
            in
            let end_block = LlvmIntf.new_block () in
            let end_r = LlvmIntf.phi end_block
                            [ (true_r, true_b); (false_r, false_b) ]
            in
            let _ = LlvmIntf.br true_b end_block in
            let _ = LlvmIntf.br false_b end_block in
            (end_r, end_block)

        | CallOpt.InnerExpr.AllocTuple(info, ty, tag, xs) ->
            if (List.length xs) > 32 then
                raise (Error.Compiler_error(oversized_tuple,info))
            else
                let (ptr, b) = LlvmUtils.heap_alloc start_block
                                            (List.length xs)
                in
                let _ = write_tuple (Common.Tag.to_int tag) b names ptr xs in
                (ptr, b)

        | CallOpt.InnerExpr.GetField(info, ty, num, (ty', v)) ->
            let res = get_name start_block names v in
            (LlvmUtils.get_member start_block res ty num), start_block

        | CallOpt.InnerExpr.Case(info, ty, (ty', v), opts) ->
            let default = LlvmIntf.new_block () in
            let _ = LlvmIntf.unreachable default in
            let test = get_name start_block names v in
            let switch = LlvmIntf.switch start_block test ~default
                            (List.length opts)
            in
            let res =
                List.map
                    (fun (tag, x) ->
                        let tag =
                            LlvmIntf.int_const (Common.Tag.to_int tag)
                        in
                        let start_block = LlvmIntf.new_block () in
                        let _ = LlvmIntf.add_case ~switch ~tag 
                                    ~dest:start_block
                        in
                        assemble names start_block x)
                    opts
            in
            let end_block = LlvmIntf.new_block () in
            let end_r = LlvmIntf.phi end_block res in
            let _ = List.iter
                        (fun (_, b) ->
                            let _ = LlvmIntf.br b end_block in
                            ())
            in
            end_r, end_block

        | CallOpt.InnerExpr.BinOp(_, _, x, op, y) ->
            let (x, b) = assemble names start_block x in
            let (y, b) = assemble names b y in
            let res = binop_ins op b x y in
            (res, b)

        | CallOpt.InnerExpr.UnOp(_, _, op, x) ->
            let (x, b) = assemble names start_block x in
            let res = unop_ins op b x in
            (res, b)

        | CallOpt.InnerExpr.InnerApply(_, ty, f, xs) ->
            begin
                assert ((List.length xs) <= Config.max_args);
                let xs = List.map (box start_block names) xs in
                let f = get_name start_block names (snd f) in
                let res = LlvmUtils.apply start_block f xs in
                let res = LlvmIntf.unbox ty start_block res in
                (res, start_block)
            end

        | CallOpt.InnerExpr.InnerSafeApply(_, _, f, nargs, xs) ->
            begin
                assert (nargs <= Config.max_args);
                assert ((List.length xs) <= Config.max_args);
                assert ((List.length xs) < nargs);
                LlvmUtils.alloc_closure start_block nargs
                    ~tag_word:(fun b ->
                            let f = get_name b names (snd f) in
                            let t_word = LlvmUtils.load b f (-1) in
                            (LlvmUtils.set_tag_word_length b
                                            ~len:(2 + List.length xs)
                                            t_word), b)
                    ~fn_ptr:(fun b ->
                            let f = get_name b names (snd f) in
                            (LlvmUtils.load b
                                    ~lltype:LlvmIntf.intptr_type
                                    f 1), b)
                    (List.map
                        (fun x b -> (box b names x), b)
                        xs)
            end
        | CallOpt.InnerExpr.InnerCall(_, _, f, xs) ->
            let xs = List.map
                        (fun x -> get_name start_block names (snd x))
                        xs
            in
            let fn_name = Config.direct_name (snd f) in
            let f = LlvmIntf.lookup_function fn_name in
            let res = LlvmIntf.call start_block f xs in
            (res, start_block)

        | CallOpt.InnerExpr.Var(_, _, v) ->
            let res = get_name start_block names v in
            (res, start_block)
        | CallOpt.InnerExpr.Const(_, _, c) ->
            let res =
                match c with
                    | Common.Const.Boolean b -> LlvmIntf.bool_const b
                    | Common.Const.Int i -> LlvmIntf.int_const i
                    | Common.Const.Unit -> LlvmIntf.unit_const ()
                    | Common.Const.Float x -> LlvmIntf.float_const x
            in
            (res, start_block)
        | CallOpt.InnerExpr.CallExtern(_, _, xtern, xs) ->
            let xs = List.map
                        (fun x -> get_name start_block names (snd x))
                        xs
            in
            let fn_name = xtern.Common.External.real_name in
            let fn_type = LlvmIntf.func_type
                            (List.map LlvmIntf.llvm_of_type
                                        xtern.Common.External.arg_types)
                            (LlvmIntf.llvm_of_type
                                    xtern.Common.External.return_type)
            in
            let f = LlvmIntf.declare_function fn_name fn_type in
            let res = LlvmIntf.call start_block f xs in
            (res, start_block)

    ;;

end;;

module TailExpr = struct

    let rec assemble names start_block = function
        | CallOpt.TailExpr.Return(x) ->
            let (x, b) = InnerExpr.assemble names start_block x in
            let _ = LlvmIntf.ret b x in
            ()

        | CallOpt.TailExpr.Let(_, _, (_, n), x, y) ->
            let (r, b) = InnerExpr.assemble names start_block x in
            let names =
                match n with
                | None -> names
                | Some v -> Common.Var.Map.add v r names
            in
            assemble names b y

        | CallOpt.TailExpr.If(_, _, x, y, z) ->
            let (r, b) = InnerExpr.assemble names start_block x in
            let true_start = LlvmIntf.new_block () in
            let _ = assemble names true_start y in
            let false_start = LlvmIntf.new_block () in
            let _ = assemble names false_start z in
            let _ = LlvmIntf.cond_br start_block ~test:r
                        ~on_true:true_start ~on_false:false_start
            in
            ()

        | CallOpt.TailExpr.Case(info, ty, (ty', v), opts) ->
            let default = LlvmIntf.new_block () in
            let _ = LlvmIntf.unreachable default in
            let test = InnerExpr.get_name start_block names v in
            let switch = LlvmIntf.switch start_block test ~default
                            (List.length opts)
            in
            List.iter
                (fun (tag, x) ->
                    let tag = LlvmIntf.int_const (Common.Tag.to_int tag) in
                    let start_block = LlvmIntf.new_block () in
                    let _ = LlvmIntf.add_case ~switch ~tag 
                                ~dest:start_block
                    in
                    assemble names start_block x)
                opts

        | CallOpt.TailExpr.TailCall(_, _, f, xs) ->
            let xs = List.map (fun x -> InnerExpr.get_name start_block
                                            names (snd x)) xs
            in
            let f = LlvmIntf.lookup_function (Config.direct_name (snd f)) in
            let res = LlvmIntf.call start_block f xs in
            let _ = LlvmIntf.set_tail_call res in
            let _ = LlvmIntf.ret start_block res in
            ()
        | CallOpt.TailExpr.TailCallExtern(_, _, xtern, xs) ->
            let xs = List.map
                        (fun x -> InnerExpr.get_name
                                        start_block names (snd x))
                        xs
            in
            let fn_name = xtern.Common.External.real_name in
            let fn_type = LlvmIntf.func_type
                            (List.map LlvmIntf.llvm_of_type
                                xtern.Common.External.arg_types)
                            (LlvmIntf.llvm_of_type
                                xtern.Common.External.return_type)
            in
            let f = LlvmIntf.declare_function fn_name fn_type in
            let res = LlvmIntf.call start_block f xs in
            let _ = LlvmIntf.set_tail_call res in
            let _ = LlvmIntf.ret start_block res in
            ()
    ;;

end;;

let make_direct_fn n args x =
    let name = Config.direct_name n in
    let arg_tys = List.map (fun x -> LlvmIntf.llvm_of_type (fst x)) args in
    let r_ty = LlvmIntf.llvm_of_type (CallOpt.TailExpr.get_type x) in
    let fn_ty = LlvmIntf.func_type arg_tys r_ty in
    let _ = LlvmIntf.with_function name fn_ty in
    let names = List.fold_left2
                    (fun names (_, t) v ->
                        match t with
                        | None -> names
                        | Some n -> Common.Var.Map.add n v names)
                    Common.Var.Map.empty
                    args
                    (LlvmIntf.params ())
    in
    let _ = TailExpr.assemble names (LlvmIntf.entry_block ()) x in
    let _ = LlvmIntf.end_function () in
    ()
;;

let make_apply_fn n args x =
    let fn_ty = LlvmIntf.func_type
                    (Utils.repeat (List.length args) LlvmIntf.int_type)
                    LlvmIntf.int_type
    in
    let _ = LlvmIntf.with_function (Config.apply_name n) fn_ty in
    let start_block = LlvmIntf.entry_block () in
    let xs = List.map2
                (fun (ty, _) v -> LlvmIntf.unbox ty start_block v)
                args (LlvmIntf.params ())
    in
    let f = LlvmIntf.lookup_function (Config.direct_name n) in
    let r = LlvmIntf.call start_block f xs in
    let ret_type = CallOpt.TailExpr.get_type x in
    let r = LlvmIntf.box ret_type start_block r in
    let _ = LlvmIntf.ret start_block r in
    let _ = LlvmIntf.end_function () in
    ()
;;

let make_init n =
    let name = Config.init_name n in
    let fn_t = LlvmIntf.func_type [] LlvmIntf.void_type in
    let _ = LlvmIntf.with_function name fn_t in
    name
;;

let make_init_fn n args x =
    let nil = LlvmIntf.ptr_init in
    let _ = LlvmIntf.define_global (Common.Var.to_string n) nil in
    let init_name = make_init n in
    let start_block = LlvmIntf.entry_block () in
    let (p, b) = LlvmUtils.alloc_closure start_block
                    (List.length args)
                    ~tag_word:(fun b ->
                            let tag_word = LlvmUtils.make_tag_word
                                                ~tag:0 ~len:2
                                                (Type.Base(Type.Int)
                                                    :: Type.Base(Type.Int)
                                                    :: (List.map fst args))
                            in
                            let tag_word = LlvmIntf.int64_const tag_word in
                            (tag_word, b))
                    ~fn_ptr:(fun b ->
                            let fn = LlvmIntf.lookup_function
                                            (Config.apply_name n)
                            in
                            let r = LlvmIntf.bitcast b fn LlvmIntf.int_type in
                            (r, b))
                    []
    in
    let g = LlvmIntf.lookup_global (Common.Var.to_string n) in
    let _ = LlvmIntf.store b ~ptr:g ~value:p in
    let _ = LlvmIntf.ret_void b in
    let _ = LlvmIntf.end_function () in
    Some(init_name)
;;

let make_init_var ty n x =
    let name = Common.Var.to_string n in
    let nil = LlvmIntf.init_of_type ty in
    let _ = LlvmIntf.define_global name nil in
    let init_name = make_init n in
    let start_block = LlvmIntf.entry_block () in
    let (v, b) = InnerExpr.assemble
                    Common.Var.Map.empty
                    start_block
                    x
    in
    let ptr = LlvmIntf.lookup_global name in
    let _ = LlvmIntf.store b ~ptr ~value:v in
    let _ = LlvmIntf.ret_void b in
    let _ = LlvmIntf.end_function () in
    Some(init_name)
;;

let make_static x =
    let init_name = make_init (Common.Var.generate ()) in
    let start_block = LlvmIntf.entry_block () in
    let (v, b) = InnerExpr.assemble
                    Common.Var.Map.empty
                    start_block
                    x
    in
    let _ = LlvmIntf.ret_void b in
    let _ = LlvmIntf.end_function () in
    Some(init_name)
;;

let split_args ty nargs =
    let rec loop t n ty =
        if (n == 0) then
            (List.rev t), ty
        else
            match ty with
            | Type.Arrow(x, y) -> loop (x :: t) (n - 1) y
            | _ -> assert false
    in
    loop [] nargs ty
;;

let make_forward ty n nargs =
    (*
    Is this not needed?
    let name = Config.direct_name n in
    let arg_tys, rty = split_args ty nargs in
    let llty = LlvmIntf.func_type (List.map LlvmIntf.llvm_of_type arg_tys)
                    (LlvmIntf.llvm_of_type rty)
    in
    let x = LlvmIntf.declare_function name llty in
    *)
    None
;;

let assemble = function
    | CallOpt.TopFun(_, _, n, args, x) ->
            let _ = make_direct_fn n args x in
            let _ = make_apply_fn n args x in
            make_init_fn n args x
    | CallOpt.TopVar(_, ty, n, x) -> make_init_var ty n x
    | CallOpt.TopForward(_, ty, n, nargs) -> make_forward ty n nargs
    | CallOpt.TopExpr(_, _, x) -> make_static x
;;


let create_main init_fns =
    let fn_t = LlvmIntf.func_type [] LlvmIntf.void_type in
    let gc_init = LlvmIntf.declare_function "caraml_gc_init" fn_t in
    let _ = LlvmIntf.with_function "main" fn_t in
    let block = LlvmIntf.entry_block () in
    let _ = LlvmIntf.void_call block gc_init [] in
    let rec loop = function
        | [] -> ()
        | Some(f) :: fns ->
            let g = LlvmIntf.lookup_function f in
            let _ = LlvmIntf.void_call block g [] in
            loop fns
        | None :: fns -> loop fns
    in
    let () = loop init_fns in
    let _ = LlvmIntf.ret_void block in
    let _ = LlvmIntf.end_function () in
    ()
;;


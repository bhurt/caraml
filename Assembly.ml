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

    let merge_blocks t u =
        match t, u with
        | None, None -> None
        | Some(_), None -> t
        | None, Some(_) ->  u
        | Some(t_r, t_b), Some(u_r, u_b) ->
            let end_block = LlvmIntf.new_block () in
            let end_r = LlvmIntf.phi end_block 
                            [ (t_r, t_b); (u_r, u_b) ]
            in
            let _ = LlvmIntf.br t_b end_block in
            let _ = LlvmIntf.br u_b end_block in
            Some(end_r, end_block)
    ;;

    let handle_label gotos names label bindings =
        match
            try
                Some(Common.Var.Map.find label gotos)
            with
            | Not_found -> None
        with
        | None -> None
        | Some(sources) ->
            let gotos = Common.Var.Map.remove label gotos in
            let target = LlvmIntf.new_block () in
            let _ =
                List.iter
                    (fun (block, _) ->
                        let _ = LlvmIntf.br block target in
                        ())
                    sources
            in
            let names =
                Common.Var.Map.fold
                    (fun v _ names ->
                        let name = 
                            LlvmIntf.phi target
                                (List.map
                                    (fun (block, args) ->
                                        try
                                            (Common.Var.Map.find
                                                            v args),
                                            block
                                        with
                                        | Not_found -> assert false)
                                    sources)
                        in
                        Common.Var.Map.add v name names)
                    bindings
                    names
            in
            Some(gotos, names, target)
    ;;

    let handle_goto gotos names start_block label bindings =
        let bindings =
            Common.Var.Map.map 
                (fun (_, n) -> get_name start_block names n)
                bindings
        in
        try
            let xs = Common.Var.Map.find label gotos in
            Common.Var.Map.add label ((start_block, bindings) :: xs)
                                gotos
        with
        | Not_found ->
            Common.Var.Map.add label [ start_block, bindings ] gotos
    ;;

    let rec assemble gotos names start_block expr = 
        let info = expr.CallOpt.InnerExpr.info in
        let ty = expr.CallOpt.InnerExpr.typ in
        match expr.CallOpt.InnerExpr.body with
        | CallOpt.InnerExpr.Let((_, n), x, y) ->
            begin
                let (t, gotos) = assemble gotos names start_block x in
                match t with
                | None -> (None, gotos)
                | Some(r, b) ->
                    let names =
                        match n with
                        | None -> names
                        | Some v -> Common.Var.Map.add v r names
                    in
                    assemble gotos names b y
            end

        | CallOpt.InnerExpr.If(x, y, z) ->
            begin
                let (t, gotos) = assemble gotos names start_block x in
                match t with
                | None -> None, gotos
                | Some(r, b) ->
                    let true_start = LlvmIntf.new_block () in
                    let (true_t, gotos) =
                        assemble gotos names true_start y
                    in

                    let false_start = LlvmIntf.new_block() in
                    let (false_t, gotos) =
                        assemble gotos names false_start z
                    in

                    let _ = LlvmIntf.cond_br b ~test:r ~on_true:true_start
                                                ~on_false:false_start
                    in
                    (merge_blocks true_t false_t), gotos
            end

        | CallOpt.InnerExpr.AllocTuple(tag, xs) ->
            if (List.length xs) > 32 then
                raise (Error.Compiler_error(oversized_tuple,info))
            else
                let (ptr, b) = LlvmUtils.heap_alloc start_block
                                            (List.length xs)
                in
                let _ = write_tuple (Common.Tag.to_int tag) b names ptr xs in
                Some(ptr, b), gotos

        | CallOpt.InnerExpr.GetField(num, (ty', v)) ->
            let res = get_name start_block names v in
            Some((LlvmUtils.get_member start_block res ty num), start_block),
            gotos

        | CallOpt.InnerExpr.Case((ty', v), opts) ->
            let default = LlvmIntf.new_block () in
            let _ = LlvmIntf.unreachable default in
            let x = get_name start_block names v in
            let (tag, start_block) = LlvmUtils.get_tag start_block x in
            let switch = LlvmIntf.switch start_block tag ~default
                            (List.length opts)
            in
            let (res, gotos) =
                List.fold_left
                    (fun (acc, gotos) (tag, x) ->
                        let tag =
                            LlvmIntf.int_const (Common.Tag.to_int tag)
                        in
                        let start_block = LlvmIntf.new_block () in
                        let _ = LlvmIntf.add_case ~switch ~tag 
                                    ~dest:start_block
                        in
                        let (t, gotos) = assemble gotos names start_block x in
                        match t with
                        | None -> (acc, gotos)
                        | Some(x) ->
                            (x :: acc, gotos))
                    ([], gotos)
                    opts
            in
            if (res != []) then
                let end_block = LlvmIntf.new_block () in
                let end_r = LlvmIntf.phi end_block res in
                let _ = List.iter
                            (fun (_, b) ->
                                let _ = LlvmIntf.br b end_block in
                                ())
                in
                Some(end_r, end_block), gotos
            else
                None, gotos

        | CallOpt.InnerExpr.Label(x, label, bindings, y) ->
            begin
                let (t, gotos) = assemble gotos names start_block x in
                match handle_label gotos names label bindings with
                | None -> t, gotos
                | Some(gotos, names, target) ->
                    let u, gotos = assemble gotos names target y in
                    (merge_blocks t u), gotos
            end

        | CallOpt.InnerExpr.Goto(label, bindings) ->
            None, (handle_goto gotos names start_block label bindings)

        | CallOpt.InnerExpr.BinOp(x, op, y) ->
            (* There is some opportunity for better dead-code elimination
             * here.
             *)
            begin
                let (t, gotos) = assemble gotos names start_block x in
                match t with
                | None -> None, gotos
                | Some(x, b) ->
                    let (t, gotos) = assemble gotos names b y in
                    match t with
                    | None -> None, gotos
                    | Some(y, b) ->
                        let res = binop_ins op b x y in
                        Some(res, b), gotos
            end

        | CallOpt.InnerExpr.UnOp(op, x) ->
            begin
                let (t, gotos) = assemble gotos names start_block x in
                match t with
                | None -> None, gotos
                | Some(x, b) ->
                    let res = unop_ins op b x in
                    Some(res, b), gotos
            end

        | CallOpt.InnerExpr.InnerApply(f, xs) ->
            begin
                assert ((List.length xs) <= Config.max_args);
                let xs = List.map (box start_block names) xs in
                let f = get_name start_block names (snd f) in
                let res = LlvmUtils.apply start_block f xs in
                let res = LlvmIntf.unbox ty start_block res in
                Some(res, start_block), gotos
            end

        | CallOpt.InnerExpr.InnerSafeApply(f, nargs, xs) ->
            begin
                assert (nargs <= Config.max_args);
                assert ((List.length xs) <= Config.max_args);
                assert ((List.length xs) < nargs);
                let c = LlvmUtils.alloc_closure start_block nargs
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
                in
                Some(c), gotos
            end
        | CallOpt.InnerExpr.InnerCall(f, xs) ->
            let xs = List.map
                        (fun x -> get_name start_block names (snd x))
                        xs
            in
            let fn_name = Config.direct_name (snd f) in
            let f = LlvmIntf.lookup_function fn_name in
            let res = LlvmIntf.call start_block f xs in
            Some(res, start_block), gotos

        | CallOpt.InnerExpr.Var(v) ->
            let res = get_name start_block names v in
            Some(res, start_block), gotos
        | CallOpt.InnerExpr.Const(c) ->
            let res =
                match c with
                    | Common.Const.Boolean b -> LlvmIntf.bool_const b
                    | Common.Const.Int i -> LlvmIntf.int_const i
                    | Common.Const.Unit -> LlvmIntf.unit_const ()
                    | Common.Const.Float x -> LlvmIntf.float_const x
            in
            Some(res, start_block), gotos
        | CallOpt.InnerExpr.CallExtern(xtern, xs) ->
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
            Some(res, start_block), gotos

    ;;

end;;

module TailExpr = struct

    let rec assemble gotos names start_block expr =
        match expr.CallOpt.TailExpr.body with
        | CallOpt.TailExpr.Return(x) ->
            begin
                let t, gotos = InnerExpr.assemble gotos names start_block x in
                match t with
                | None -> gotos
                | Some(x, b) ->
                    let _ = LlvmIntf.ret b x in
                    gotos
            end

        | CallOpt.TailExpr.Let((_, n), x, y) ->
            begin
                let t, gotos = InnerExpr.assemble gotos names start_block x in
                match t with
                | None -> gotos
                | Some(r, b) ->
                    let names =
                        match n with
                        | None -> names
                        | Some v -> Common.Var.Map.add v r names
                    in
                    assemble gotos names b y
            end

        | CallOpt.TailExpr.If(x, y, z) ->
            begin
                let t, gotos = InnerExpr.assemble gotos names start_block x in
                match t with
                | None -> gotos
                | Some(r, b) ->
                    let true_start = LlvmIntf.new_block () in
                    let gotos = assemble gotos names true_start y in
                    let false_start = LlvmIntf.new_block () in
                    let gotos = assemble gotos names false_start z in
                    let _ = LlvmIntf.cond_br start_block ~test:r
                                ~on_true:true_start ~on_false:false_start
                    in
                    gotos
            end

        | CallOpt.TailExpr.Case((ty', v), opts) ->
            let default = LlvmIntf.new_block () in
            let _ = LlvmIntf.unreachable default in
            let test = InnerExpr.get_name start_block names v in
            let switch = LlvmIntf.switch start_block test ~default
                            (List.length opts)
            in
            List.fold_left
                (fun gotos (tag, x) ->
                    let tag = LlvmIntf.int_const (Common.Tag.to_int tag) in
                    let start_block = LlvmIntf.new_block () in
                    let _ = LlvmIntf.add_case ~switch ~tag 
                                ~dest:start_block
                    in
                    assemble gotos names start_block x)
                gotos
                opts

        | CallOpt.TailExpr.Label(x, label, bindings, y) ->
            begin
                let gotos = assemble gotos names start_block x in
                match InnerExpr.handle_label gotos names label bindings with
                | None -> gotos
                | Some(gotos, names, target) ->
                    assemble gotos names target y
            end

        | CallOpt.TailExpr.Goto(label, bindings) ->
            InnerExpr.handle_goto gotos names start_block label bindings

        | CallOpt.TailExpr.TailCall(f, xs) ->
            let xs = List.map (fun x -> InnerExpr.get_name start_block
                                            names (snd x)) xs
            in
            let f = LlvmIntf.lookup_function (Config.direct_name (snd f)) in
            let res = LlvmIntf.call start_block f xs in
            let _ = LlvmIntf.set_tail_call res in
            let _ = LlvmIntf.ret start_block res in
            gotos

        | CallOpt.TailExpr.TailCallExtern(xtern, xs) ->
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
            gotos
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
    let _ = TailExpr.assemble Common.Var.Map.empty
                                names (LlvmIntf.entry_block ()) x
    in
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
    let t, _ = InnerExpr.assemble
                    Common.Var.Map.empty
                    Common.Var.Map.empty
                    start_block
                    x
    in
    match t with
    | None -> assert false
    | Some(v, b) ->
        let ptr = LlvmIntf.lookup_global name in
        let _ = LlvmIntf.store b ~ptr ~value:v in
        let _ = LlvmIntf.ret_void b in
        let _ = LlvmIntf.end_function () in
        Some(init_name)
;;

let make_static x =
    let init_name = make_init (Common.Var.generate ()) in
    let start_block = LlvmIntf.entry_block () in
    let t, _ = InnerExpr.assemble
                    Common.Var.Map.empty
                    Common.Var.Map.empty
                    start_block
                    x
    in
    match t with
    | None -> assert false
    | Some(v, b) ->
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

let assemble top =
    let ty = top.CallOpt.typ in
    match top.CallOpt.body with
    | CallOpt.TopFun(n, args, x) ->
            let _ = make_direct_fn n args x in
            let _ = make_apply_fn n args x in
            make_init_fn n args x
    | CallOpt.TopVar(n, x) -> make_init_var ty n x
    | CallOpt.TopForward(n, nargs) -> make_forward ty n nargs
    | CallOpt.TopExpr(x) -> make_static x
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


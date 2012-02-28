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

    open Function;;

    let oversized_tuple _ =
        Format.print_string
            "Tuples of more than 32 elements not supported yet."
    ;;

    let load_members block names ptr args =
        let f i (ty, v) =
            let bind = Block.bind in
            match v with
            | None -> Block.return Utils.id
            | Some t ->
                (perform
                    x <-- LlvmUtils.get_member ptr ty i;
                    Block.return (Common.Var.Map.add t x))
        in
        perform
            fs <-- Block.in_block block (Block.seq (Utils.mapi f args));
            return (Utils.freduce fs names)
    ;;

    let get_name names name =
        try
            Block.return (Common.Var.Map.find name names)
        with
        | Not_found -> 
            Block.load_global (Common.Var.to_string name)
    ;;

    let write_tuple names ptr xs =
        let tag_word = LlvmUtils.make_tag_word ~tag:0 ~len:(List.length xs)
                            (List.map fst xs)
        in
        let bind = Block.bind in
        let f i (ty, v) =
            perform
                w <-- get_name names v;
                LlvmUtils.set_member ~ptr ty i ~value:w
        in
        perform
            tagptr <-- Block.offset ptr (-1);
            tag_word <-- Block.int64_const tag_word;
            _ <-- Block.store ~ptr:tagptr ~value:tag_word;
            _ <-- Block.seq (Utils.mapi f xs);
            Block.return ()
    ;;

    let binop_ins = function
        | Common.BinOp.Times -> Block.mul
        | Common.BinOp.Divide -> Block.div
        | Common.BinOp.Add -> Block.add
        | Common.BinOp.Subtract -> Block.sub
        | Common.BinOp.Le -> Block.le
        | Common.BinOp.Ge -> Block.ge
        | Common.BinOp.Lt -> Block.lt
        | Common.BinOp.Gt -> Block.gt
        | Common.BinOp.Eq -> Block.eq
        | Common.BinOp.Ne -> Block.ne
        | Common.BinOp.And -> Block.bool_and
        | Common.BinOp.Or -> Block.bool_or
    ;;

    let unop_ins = function
        | Common.UnOp.Neg -> Block.neg
        | Common.UnOp.Not -> Block.bool_not
    ;;

    let box names (ty, n) =
        let bind = Block.bind in
        perform
            v <-- get_name names n;
            Block.box ty v
    ;;

    let rec assemble names start_block = function
        | CallOpt.InnerExpr.Let(_, _, (_, n), x, y) ->
            perform
                (r, b) <-- assemble names start_block x;
                let names =
                    match n with
                    | None -> names
                    | Some v -> Common.Var.Map.add v r names
                in
                assemble names b y

        | CallOpt.InnerExpr.LetTuple(_, _, args, x, y)->
            perform
                (r, b) <-- assemble names start_block x;
                names <-- load_members b names r args;
                assemble names b y
        | CallOpt.InnerExpr.If(_, _, x, y, z) ->
            perform
                (r, b) <-- assemble names start_block x;
                true_start <-- Function.new_block;
                (true_r, true_b) <-- assemble names true_start y;
                false_start <-- Function.new_block;
                (false_r, false_b) <-- assemble names false_start z;
                _ <-- Block.in_block b
                        (Block.cond_br ~test:r ~on_true:true_start
                                    ~on_false:false_start);
                end_block <-- Function.new_block;
                end_r <-- Block.in_block end_block
                            (Block.phi [ (true_r, true_b);
                                            (false_r, false_b) ]);
                _ <-- Block.in_block true_b
                            (Block.br end_block);
                _ <-- Block.in_block false_b
                            (Block.br end_block);
                return (end_r, end_block)
        | CallOpt.InnerExpr.Tuple(info, ty, xs) ->
            if (List.length xs) > 32 then
                raise (Error.Compiler_error(oversized_tuple,info))
            else
                perform
                    (ptr, b) <-- LlvmUtils.heap_alloc start_block
                                    (List.length xs);
                    _ <-- Block.in_block b (write_tuple names ptr xs);
                    return (ptr, b)
        | CallOpt.InnerExpr.BinOp(_, _, x, op, y) ->
            begin
                perform
                    (x, b) <-- assemble names start_block x;
                    (y, b) <-- assemble names b y;
                    res <-- Block.in_block b ((binop_ins op) x y);
                    return (res, b)
            end
        | CallOpt.InnerExpr.UnOp(_, _, op, x) ->
            perform
                (x, b) <-- assemble names start_block x;
                res <-- Block.in_block b ((unop_ins op) x);
                return (res, b)
        | CallOpt.InnerExpr.InnerApply(_, ty, f, xs) ->
            begin
                assert ((List.length xs) <= Config.max_args);
                Block.in_block start_block
                    begin
                        let bind = Block.bind in
                        perform
                            xs <-- Block.seq (List.map (box names) xs);
                            f <-- get_name names (snd f);
                            app_table_t <-- Block.app_table_type;
                            app_table_ptr_t <-- Block.ptr_type app_table_t;
                            ptr <-- Block.bitcast f app_table_ptr_t;
                            table_p <-- Block.load ptr;
                            fn_p <-- Block.offset table_p
                                                ((List.length xs) - 1);
                            fn_p <-- Block.load fn_p;
                            res <-- Block.call fn_p xs;
                            res <-- Block.unbox ty res;
                            Block.return (res, start_block)
                    end
            end
        | CallOpt.InnerExpr.InnerSafeApply(_, _, f, nargs, xs) ->
            begin
                assert (nargs <= Config.max_args);
                assert ((List.length xs) <= Config.max_args);
                assert ((List.length xs) < nargs);
                perform
                    (p, b) <-- LlvmUtils.heap_alloc start_block
                                                    (2 + List.length xs);
                    Block.in_block b
                        begin
                            let bind = Block.bind in
                            perform
                                f <-- get_name names (snd f);
                                p0 <-- Block.offset p (-1);
                                tword <-- Block.load p0;
                                tword <-- LlvmUtils.set_tag_word_length
                                            ~len:(2 + List.length xs)
                                            tword;
                                p0 <-- Block.offset p (-1);
                                _ <-- Block.store ~ptr:p0 ~value:tword;
                                app_table_t <-- Block.app_table_type;
                                app_table_ptr_t <-- Block.ptr_type app_table_t;
                                ptr <-- Block.bitcast p app_table_ptr_t;
                                table <-- Block.load_global
                                                (Config.apply_table_name nargs
                                                    (List.length xs));
                                _ <-- Block.store ~ptr ~value:table;
                                p0 <-- Block.offset f 1;
                                t <-- Block.load p0;
                                p0 <-- Block.offset p 1;
                                _ <-- Block.store ~ptr:p0 ~value:t;
                                _ <-- Block.seq
                                        (Utils.mapi ~start:2
                                            (fun i v ->
                                                perform
                                                    p0 <-- Block.offset
                                                                    p i;
                                                    v <-- box names v;
                                                    Block.store ~ptr:p0
                                                        ~value:v)
                                            xs);
                                Block.return (p, b)
                        end
            end
        | CallOpt.InnerExpr.InnerCall(_, _, f, xs) ->
            begin
                Block.in_block start_block
                    begin
                        let fn_name = Config.direct_name (snd f) in
                        let bind = Block.bind in
                        perform
                            xs <-- Block.seq
                                    (List.map
                                        (fun x -> get_name names (snd x)) xs);
                            f <-- Block.lookup_function fn_name;
                            res <-- Block.call f xs;
                            Block.return (res, start_block)
                    end
            end
        | CallOpt.InnerExpr.Var(_, _, v) ->
            begin
                perform
                    res <-- Block.in_block start_block
                                (get_name names v);
                    return (res, start_block)
            end
        | CallOpt.InnerExpr.Const(_, _, c) ->
            begin
                perform
                    res <--
                        begin
                            match c with
                            | Common.Const.Boolean b -> bool_const b
                            | Common.Const.Int i -> int_const i
                            | Common.Const.Unit -> unit_const ()
                        end;
                    return (res, start_block)
            end
    ;;

end;;

module TailExpr = struct

    open Function;;

    let rec assemble names start_block = function
        | CallOpt.TailExpr.Return(x) ->
            begin
                perform
                    (x, b) <-- InnerExpr.assemble names start_block x;
                    _ <-- Block.in_block b (Block.ret x);
                    return ()
            end
        | CallOpt.TailExpr.Let(_, _, (_, n), x, y) ->
            perform
                (r, b) <-- InnerExpr.assemble names start_block x;
                let names =
                    match n with
                    | None -> names
                    | Some v -> Common.Var.Map.add v r names
                in
                assemble names b y

        | CallOpt.TailExpr.LetTuple(_, _, args, x, y)->
            perform
                (r, b) <-- InnerExpr.assemble names start_block x;
                names <-- InnerExpr.load_members b names r args;
                assemble names b y

        | CallOpt.TailExpr.If(_, _, x, y, z) ->
            perform
                (r, b) <-- InnerExpr.assemble names start_block x;
                true_start <-- Function.new_block;
                _ <-- assemble names true_start y;
                false_start <-- Function.new_block;
                _ <-- assemble names false_start z;
                return ()

        | CallOpt.TailExpr.TailCall(_, _, f, xs) ->
            begin
                Block.in_block start_block
                    begin
                        let fn_name = Config.direct_name (snd f) in
                        let bind = Block.bind in
                        perform
                            xs <-- Block.seq
                                    (List.map
                                        (fun x -> InnerExpr.get_name
                                                    names (snd x))
                                        xs);
                            f <-- Block.lookup_function fn_name;
                            res <-- Block.call f xs;
                            _ <-- Block.set_tail_call res;
                            _ <-- Block.ret res;
                            Block.return ()
                    end
            end
    ;;

end;;

let make_direct_fn n args x =
    let bind = Module.bind in
    let ret_type = CallOpt.TailExpr.get_type x in
    let name = Config.direct_name n in
    perform
        arg_tys <-- Module.seq
                        (List.map
                            (fun x -> Module.llvm_of_type (fst x))
                            args);
        r_ty <-- Module.llvm_of_type ret_type;
        fn_ty <-- Module.func_type arg_tys r_ty;
        Function.with_function name fn_ty
            begin
                let bind = Function.bind in
                perform
                    start_block <-- Function.entry_block;
                    ps <-- Function.params;
                    TailExpr.assemble
                        (List.fold_left2
                            (fun names (_, t) v ->
                                match t with
                                | None -> names
                                | Some n -> Common.Var.Map.add n v names)
                            Common.Var.Map.empty
                            args
                            ps)
                        start_block
                        x
            end
;;

let apply_fn_type nargs =
    let bind = Module.bind in
    perform
        word_ty <-- Module.word_type;
        Module.func_type (Utils.repeat nargs word_ty) word_ty
;;
    
let make_apply_fn n args x =
    let bind = Module.bind in
    let ret_type = CallOpt.TailExpr.get_type x in
    perform
        fn_ty <-- apply_fn_type (List.length args);
        Function.with_function (Config.apply_name n) fn_ty
            begin
                let bind = Function.bind in
                perform
                    start_block <-- Function.entry_block;
                    ps <-- Function.params;
                    Block.in_block start_block
                        begin
                            let bind = Block.bind in
                            perform
                                xs <-- Block.seq
                                            (List.map2
                                                (fun (ty, _) v ->
                                                    Block.unbox ty v)
                                                args ps);
                                f <-- Block.lookup_function
                                        (Config.direct_name n);
                                r <-- Block.call f xs;
                                r <-- Block.box ret_type r;
                                Block.ret r
                        end
            end
;;

let make_init n x =
    let bind = Module.bind in
    let name = Config.init_name n in
    perform
        void <-- Module.void_type;
        fn_t <-- Module.func_type [] void;
        _ <-- Function.with_function name fn_t x;
        Module.return name
;;

let make_init_fn n args x =
    let bind = Module.bind in
    let tag_word = LlvmUtils.make_tag_word ~tag:0 ~len:2
                        (Type.Base(Type.Int) :: Type.Base(Type.Int) ::
                            (List.map fst args))
    in
    perform
        nil <-- Module.ptr_init;
        _ <-- Module.define_global (Common.Var.to_string n) nil;
        fn_type <-- apply_fn_type (List.length args);
        make_init n
            begin
                let bind = Function.bind in
                perform
                    start_block <-- Function.entry_block;
                    (p, b) <-- LlvmUtils.heap_alloc start_block 2;
                    Block.in_block b
                        begin
                            let bind = Block.bind in
                            perform
                                tag_ptr <-- Block.offset p (-1);
                                tag <-- Block.int64_const tag_word;
                                _ <-- Block.store ~ptr:tag_ptr ~value:tag;
                                app_table_t <-- Block.app_table_type;
                                app_table_ptr_t <-- Block.ptr_type app_table_t;
                                ptr <-- Block.bitcast p app_table_ptr_t;
                                table <-- Block.load_global
                                                (Config.apply_table_name
                                                    (List.length args) 0);
                                _ <-- Block.store ~ptr ~value:table;
                                fn_ptr <-- Block.offset p 1;
                                fn_ptr_type <-- Block.ptr_type fn_type;
                                fn_ptr <-- Block.bitcast fn_ptr fn_ptr_type;
                                fn <-- Block.lookup_function
                                            (Config.apply_name n);
                                _ <-- Block.store ~ptr:fn_ptr ~value:fn;
                                g <-- Block.lookup_global
                                            (Common.Var.to_string n);
                                _ <-- Block.store ~ptr:g ~value:p;
                                Block.ret_void
                        end
            end
;;

let make_init_var ty n x =
    let name = Common.Var.to_string n in
    let bind = Module.bind in
    perform
        nil <-- Module.init_of_type ty;
        _ <-- Module.define_global name nil;
        make_init n
            begin
                let bind = Function.bind in
                perform
                    start_block <-- Function.entry_block;
                    (v, b) <-- InnerExpr.assemble
                                    Common.Var.Map.empty
                                    start_block
                                    x;
                    Block.in_block b
                        begin
                            let bind = Block.bind in
                            perform
                                ptr <-- Block.lookup_global name;
                                _ <-- Block.store ~ptr ~value:v;
                                Block.ret_void
                        end
            end
;;

let make_static x =
    make_init (Common.Var.generate ())
        begin
            let bind = Function.bind in
            perform
                start_block <-- Function.entry_block;
                (v, b) <-- InnerExpr.assemble
                                Common.Var.Map.empty
                                start_block
                                x;
                Block.in_block b Block.ret_void
        end
;;

let assemble = function
    | CallOpt.TopFun(_, _, n, args, x) ->
        let bind = Module.bind in
        perform
            _ <-- make_direct_fn n args x;
            _ <-- make_apply_fn n args x;
            make_init_fn n args x
    | CallOpt.TopVar(_, ty, n, x) -> make_init_var ty n x
    | CallOpt.TopExpr(_, _, x) -> make_static x
;;


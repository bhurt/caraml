open LambdaConv;;

module FreeVars : sig

    val bind_lambda : Common.Var.Set.t -> Lambda.t -> Common.Var.Set.t;;

    val free_vars : Common.Var.Set.t -> Expr.t
                                -> Common.VarType.t Common.Var.Map.t;;

    val free_vars_lambdas : Common.Var.Set.t -> Lambda.t list
                                -> Common.VarType.t Common.Var.Map.t;;

end = struct

    let bind_arg bound = function
        | (_, Some n) -> Common.Var.Set.add n bound
        | (_, None) -> bound
    ;;

    let bind_lambda bound lambda =
        Common.Var.Set.add lambda.Lambda.name bound
    ;;

    let add_var bound s (ty, v) =
        if (Common.Var.Set.mem v bound) then
            s
        else
            (Common.Var.Map.add v ty s)
    ;;

    let rec loop bound s expr =
        match expr.Expr.body with
        | Expr.Let(arg, x, y) ->
            let s = loop bound s x in
            let bound = bind_arg bound arg in
            loop bound s y
        | Expr.LetFn(f, x) ->
            let s = loop_lambda bound s f in
            let bound = bind_lambda bound f in
            loop bound s x
        | Expr.LetRec(fns, x) ->
            let bound = List.fold_left bind_lambda bound fns in
            let s = List.fold_left (loop_lambda bound) s fns in
            loop bound s x
        | Expr.If(x, y, z) ->
            let s = loop bound s x in
            let s = loop bound s y in
            loop bound s z
        | Expr.AllocTuple(_, xs) ->
            List.fold_left (loop bound) s xs
        | Expr.ConstantConstructor(_) -> s
        | Expr.GetField(_, x) -> loop bound s x
        | Expr.IsConstantConstructor(x) -> loop bound s x
        | Expr.ConstantConstructorCase(n, opts)
        | Expr.TupleConstructorCase(n, opts) ->
            let s = loop bound s n in
            List.fold_left (fun s x -> loop bound s (snd x)) s opts
        | Expr.Label(x, _, bindings, y) ->
            let s = loop bound s x in
            let bound = Common.Var.Map.fold
                            (fun k _ bound -> Common.Var.Set.add k bound)
                            bindings
                            bound
            in
            loop bound s y
        | Expr.Goto(_, bindings) ->
            Common.Var.Map.fold (fun _ x s -> loop bound s x) bindings s
        | Expr.BinOp(x, _, y) ->
            let s = loop bound s x in
            loop bound s y
        | Expr.UnOp(_, x) ->
            loop bound s x
        | Expr.Apply(x, y) ->
            let s = loop bound s x in
            loop bound s y
        | Expr.Var(v) -> add_var bound s (expr.Expr.typ, v)
        | Expr.Const(_) -> s

    and loop_lambda bound s lambda =
        let bound = List.fold_left bind_arg bound lambda.Lambda.args in
        loop bound s lambda.Lambda.body
    ;;

    let free_vars bound x =
        loop bound Common.Var.Map.empty x
    ;;


    let free_vars_lambdas bound fns =
        List.fold_left (loop_lambda bound) Common.Var.Map.empty fns
    ;;

end;;

module ReplaceVars : sig

    val replace_vars : (Info.t -> Common.VarType.t -> Expr.s) Common.Var.Map.t
                                        -> Expr.t -> Expr.t;;

    val replace_vars_lambda :
        (Info.t -> Common.VarType.t -> Expr.s) Common.Var.Map.t
                                    -> Lambda.t -> Lambda.t;;

end = struct

    (* Only free vars can be replaced- we don't check this, but we depend
     * upon it.
     *)

    let rec replace_vars repl expr =
        let info = expr.Expr.info in
        let ty = expr.Expr.typ in
        let body = match expr.Expr.body with
                    | Expr.Let(arg, x, y) ->
                        let x = replace_vars repl x in
                        let y = replace_vars repl y in
                        Expr.Let(arg, x, y)
            
                    | Expr.LetFn(fn, x) ->
                        let x = replace_vars repl x in
                        let fn = replace_vars_lambda repl fn in
                        Expr.LetFn(fn, x)
            
                    | Expr.LetRec(fns, x) ->
                        let x = replace_vars repl x in
                        let fns = List.map (replace_vars_lambda repl) fns in
                        Expr.LetRec(fns, x)
            
                    | Expr.If(x, y, z) ->
                        let x = replace_vars repl x in
                        let y = replace_vars repl y in
                        let z = replace_vars repl z in
                        Expr.If(x, y, z)
            
                    | Expr.AllocTuple(tag, xs) ->
                        let xs = List.map (replace_vars repl) xs in
                        Expr.AllocTuple(tag, xs)
            
                    | Expr.ConstantConstructor(tag) -> expr.Expr.body

                    | Expr.GetField(num, x) ->
                        let x = replace_vars repl x in
                        Expr.GetField(num, x)
            
                    | Expr.IsConstantConstructor(x) ->
                        let x = replace_vars repl x in
                        Expr.IsConstantConstructor(x)

                    | Expr.ConstantConstructorCase(n, opts) ->
                        (* Note: because of the way we generate it, the
                         * variable we are switching on can never be free.
                         * This simplifies things here a lot.
                         *)
                        let opts =
                            List.map
                                (fun (tag, x) -> tag, replace_vars repl x)
                                opts
                        in
                        Expr.ConstantConstructorCase(n, opts)
            
                    | Expr.TupleConstructorCase(n, opts) ->
                        (* Note: because of the way we generate it, the
                         * variable we are switching on can never be free.
                         * This simplifies things here a lot.
                         *)
                        let opts =
                            List.map
                                (fun (tag, x) -> tag, replace_vars repl x)
                                opts
                        in
                        Expr.TupleConstructorCase(n, opts)
            
                    | Expr.Label(x, label, bindings, y) ->
                        (* Note: we don't even try replacing vars bound
                         * in a label.
                         *)
                        let x = replace_vars repl x in
                        let y = replace_vars repl y in
                        Expr.Label(x, label, bindings, y)
            
                    | Expr.Goto(label, bindings) ->
                        let bindings =
                            Common.Var.Map.map (replace_vars repl) bindings
                        in
                        Expr.Goto(label, bindings)
                    
                    | Expr.BinOp(x, op, y) ->
                        let x = replace_vars repl x in
                        let y = replace_vars repl y in
                        Expr.BinOp(x, op, y)
            
                    | Expr.UnOp(op, x) ->
                        let x = replace_vars repl x in
                        Expr.UnOp(op, x)
            
                    | Expr.Apply(x, y) ->
                        let x = replace_vars repl x in
                        let y = replace_vars repl y in
                        Expr.Apply(x, y)
            
                    | Expr.Var(v) as x ->
                        begin
                            try
                                (Common.Var.Map.find v repl) info ty
                            with
                            | Not_found -> x
                        end
                    | Expr.Const(_) as c -> c
        in
        Expr.make info ty body
    and replace_vars_lambda repl lambda =
        let body = replace_vars repl lambda.Lambda.body in
        { lambda with Lambda.body = body }
    ;;

end;;

let map_vars old_names new_names x =
    let repl =
        List.fold_left2
            (fun m old_name new_name ->
                Common.Var.Map.add old_name
                    (fun _ _ -> Expr.Var(new_name))
                    m)
            Common.Var.Map.empty
            old_names
            new_names
    in
    ReplaceVars.replace_vars repl x
;;

let detuple tuple_name old_vars var_types tuple_type x =
    let new_vars = List.map (fun _ -> Common.Var.generate ()) old_vars in
    let x = map_vars old_vars new_vars x in
    let info = x.Expr.info in
    let ty = x.Expr.typ in
    let tuple_var = Expr.make info tuple_type (Expr.Var(tuple_name)) in
    Utils.fold_right2i
        (fun i nm ty' body ->
            Expr.make info ty
                (Expr.Let((ty', Some nm),
                        Expr.make info ty' (Expr.GetField(i, tuple_var)),
                        body)))
        new_vars
        var_types
        x
;;

let make_apply info f x =
    match f.Expr.typ with
    | Type.Arrow(_, t2) ->
        Expr.make info t2 (Expr.Apply(f, x))
    | _ -> assert false
;;

let preapply_vars rec_fns var_names var_types x =
    let repl =
        List.fold_left
            (fun m n ->
                Common.Var.Map.add n
                    (fun info ty ->
                        let vars =
                            List.map2
                                (fun t v -> Expr.make info t (Expr.Var(v)))
                                var_types
                                var_names
                        in
                        let fty = Type.fn_type var_types ty in
                        let res = List.fold_left (make_apply info) 
                                    (Expr.make info fty (Expr.Var(n)))
                                    vars
                        in
                        res.Expr.body)
                    m)
            Common.Var.Map.empty
            rec_fns
    in
    ReplaceVars.replace_vars repl x
;;

let fix_lambda_tuple rec_fns fvs fvs_types lambda =
    let tuple_name = Common.Var.generate () in
    let tuple_type = Type.Tuple(fvs_types) in
    let body =
        if (rec_fns == []) then
            lambda.Lambda.body
        else
            preapply_vars rec_fns [ tuple_name ] [ tuple_type ]
                                lambda.Lambda.body
    in
    let body = detuple tuple_name fvs fvs_types tuple_type body in
    let fty = Type.Arrow(tuple_type, lambda.Lambda.typ) in
    { lambda with
        Lambda.typ = fty;
        Lambda.args = (tuple_type, Some(tuple_name)) :: lambda.Lambda.args;
        Lambda.body = body }
;;

let fix_lambda_flat rec_fns fvs fvs_types lambda  =
    let new_names = List.map (fun _ -> Common.Var.generate ()) fvs in
    let body =
        if (rec_fns == []) then
            lambda.Lambda.body
        else
            preapply_vars rec_fns new_names fvs_types lambda.Lambda.body
    in
    let body = map_vars fvs new_names body in
    let new_args =
        List.map2
            (fun t v -> t, Some(v))
            fvs_types
            new_names
    in
    { lambda with
        Lambda.typ = Type.fn_type fvs_types lambda.Lambda.typ;
        Lambda.args = List.append new_args lambda.Lambda.args;
        Lambda.body = body }
;;

let tuplize_args n lambda =
    (n + (List.length lambda.Lambda.args)) > Config.max_args
;;

let rec convert_expr publics expr =
    let info = expr.Expr.info in
    let ty = expr.Expr.typ in
    let body =
        match expr.Expr.body with
        | Expr.Let(arg, x, y) ->
            let x = convert_expr publics x in
            let y = convert_expr publics y in
            Expr.Let(arg, x, y)
    
        | Expr.LetFn(fn, x) ->
            let fvmap = FreeVars.free_vars_lambdas publics [ fn ] in
            let nvs = Common.Var.Map.cardinal fvmap in
            let fvs = Common.Var.Map.bindings fvmap in
            let fv_names = List.map fst fvs in
            let fv_types = List.map snd fvs in
            let fn =
                if (tuplize_args nvs fn) then
                    fix_lambda_tuple [] fv_names fv_types fn
                else
                    fix_lambda_flat [] fv_names fv_types fn
            in
            let name = fn.Lambda.name in
            let x = preapply_vars [ name ] fv_names fv_types x  in
            let publics = Common.Var.Set.add name publics in
            let x = convert_expr publics x in
            Expr.LetFn(fn, x)
    
        | Expr.LetRec(fns, x) ->
            let publics, fns, g = convert_lambdas publics fns in
            let x = g x in
            let x = convert_expr publics x in
            Expr.LetRec(fns, x)
    
        | Expr.If(x, y, z) ->
            let x = convert_expr publics x in
            let y = convert_expr publics y in
            let z = convert_expr publics z in
            Expr.If(x, y, z)
    
        | Expr.AllocTuple(tag, xs) ->
            let xs = List.map (convert_expr publics) xs in
            Expr.AllocTuple(tag, xs)
    
        | Expr.ConstantConstructor(tag) ->
            Expr.ConstantConstructor(tag)

        | Expr.GetField(num, x) ->
            let x = convert_expr publics x in
            Expr.GetField(num, x)
    
        | Expr.IsConstantConstructor(x) ->
            let x = convert_expr publics x in
            Expr.IsConstantConstructor(x)

        | Expr.ConstantConstructorCase(n, opts) ->
            let opts =
                List.map (fun (tag, x) -> tag, convert_expr publics x) opts
            in
            Expr.ConstantConstructorCase(n, opts)
    
        | Expr.TupleConstructorCase(n, opts) ->
            let opts =
                List.map (fun (tag, x) -> tag, convert_expr publics x) opts
            in
            Expr.TupleConstructorCase(n, opts)
    
        | Expr.Label(x, label, bindings, y) ->
            let x = convert_expr publics x in
            let y = convert_expr publics y in
            Expr.Label(x, label, bindings, y)
    
        | Expr.Goto(label, bindings) ->
            let bindings = Common.Var.Map.map (convert_expr publics) bindings in
            Expr.Goto(label, bindings)
    
        | Expr.BinOp(x, op, y) ->
            let x = convert_expr publics x in
            let y = convert_expr publics y in
            Expr.BinOp(x, op, y)
    
        | Expr.UnOp(op, x) ->
            let x = convert_expr publics x in
            Expr.UnOp(op, x)
    
        | Expr.Apply(x, y) ->
            let x = convert_expr publics x in
            let y = convert_expr publics y in
            Expr.Apply(x, y)
    
        | Expr.Var(_) as x -> x
        | Expr.Const(_) as x -> x
    in
    Expr.make info ty body
    
and convert_lambdas publics fns =
    let publics =
        List.fold_left
            (fun publics lambda ->
                Common.Var.Set.add lambda.Lambda.name publics)
            publics
            fns
    in
    let fvmap = FreeVars.free_vars_lambdas publics fns in
    let nvs = Common.Var.Map.cardinal fvmap in
    let fvs = Common.Var.Map.bindings fvmap in
    let fv_names = List.map fst fvs in
    let fv_types = List.map snd fvs in
    let rec_fns =
        List.map (fun lambda -> lambda.Lambda.name) fns
    in
    let fns =
        if (List.exists (tuplize_args nvs) fns) then
            List.map (fix_lambda_tuple rec_fns fv_names fv_types) fns
        else
            List.map (fix_lambda_flat rec_fns  fv_names fv_types) fns
    in
    publics, fns, (preapply_vars rec_fns fv_names fv_types)
;;

let convert publics top =
    let info = top.info in
    let publics, body =
        match top.body with
        | Top(v, ty, x) ->
            let x = convert_expr publics x in
            publics, Top(v, ty, x)
        | TopFn(ty, lambda) ->
            let expr = convert_expr publics lambda.Lambda.body in
            (Common.Var.Set.add lambda.Lambda.name publics),
                (TopFn(ty, { lambda with Lambda.body = expr }))
        | TopRec(fns) ->
            let publics, fns, _ = convert_lambdas publics fns in
            publics, (TopRec(fns))
        | Extern(_, _) as x -> publics, x
    in
    publics, { info = info; body = body }
;;

module C : IL.Conversion with type input = LambdaConv.t
                                and type output = LambdaConv.t =
struct
    type input = LambdaConv.t;;
    type output = LambdaConv.t;;
    type state = Common.Var.Set.t;;
    type check_state = unit;;

    let name = "freebind";;
    let sexp_of_output x = LambdaConv.sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = Common.Var.Set.empty;;
    let convert state input =
        let state, output = convert state input in
        state, [ output ]
    ;;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;
end;;

module Convert : IL.Converter with type output = t =
    IL.Make(LambdaConv.Convert)(C);;


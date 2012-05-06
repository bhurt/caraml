open LambdaConv;;

module FreeVars : sig

    val bind_lambda : Common.Var.Set.t -> Expr.lambda
                        -> Common.Var.Set.t;;

    val free_vars : Common.Var.Set.t -> Expr.t
                                -> type_t Common.Var.Map.t;;

    val free_vars_lambdas : Common.Var.Set.t -> Expr.lambda list
                                -> type_t Common.Var.Map.t;;

end = struct

    let bind_arg bound = function
        | (_, Some n) -> Common.Var.Set.add n bound
        | (_, None) -> bound
    ;;

    let bind_lambda bound (_, _, name, _, _) =
        Common.Var.Set.add name bound
    ;;

    let add_var bound s (ty, v) =
        if (Common.Var.Set.mem v bound) then
            s
        else
            (Common.Var.Map.add v ty s)
    ;;

    let rec loop bound s = function
        | Expr.Let(_, _, arg, x, y) ->
            let s = loop bound s x in
            let bound = bind_arg bound arg in
            loop bound s y
        | Expr.LetFn(_, _, f, x) ->
            let s = loop_lambda bound s f in
            let bound = bind_lambda bound f in
            loop bound s x
        | Expr.LetRec(_, _, fns, x) ->
            let bound = List.fold_left bind_lambda bound fns in
            let s = List.fold_left (loop_lambda bound) s fns in
            loop bound s x
        | Expr.If(_, _, x, y, z) ->
            let s = loop bound s x in
            let s = loop bound s y in
            loop bound s z
        | Expr.AllocTuple(_, _, _, xs) ->
            List.fold_left (loop bound) s xs
        | Expr.GetField(_, _, _, x) -> loop bound s x
        | Expr.Case(_, _, n, opts) ->
            let s = add_var bound s n in
            List.fold_left (fun s x -> loop bound s (snd x)) s opts
        | Expr.BinOp(_, _, x, _, y) ->
            let s = loop bound s x in
            loop bound s y
        | Expr.UnOp(_, _, _, x) ->
            loop bound s x
        | Expr.Apply(_, _, x, y) ->
            let s = loop bound s x in
            loop bound s y
        | Expr.Var(_, ty, v) -> add_var bound s (ty, v)
        | Expr.Const(_, _, _) -> s
        
    and loop_lambda bound s (_, _, _, args, x) =
        let bound = List.fold_left bind_arg bound args in
        loop bound s x
    ;;

    let free_vars bound x =
        loop bound Common.Var.Map.empty x
    ;;


    let free_vars_lambdas bound fns =
        List.fold_left (loop_lambda bound) Common.Var.Map.empty fns
    ;;

end;;

module ReplaceVars : sig

    val replace_vars : (Info.t -> type_t -> Expr.t) Common.Var.Map.t
                                        -> Expr.t -> Expr.t;;

    val replace_vars_lambda : (Info.t -> type_t -> Expr.t) Common.Var.Map.t
                                    -> Expr.lambda -> Expr.lambda;;

end = struct

    (* Only free vars can be replaced- we don't check this, but we depend
     * upon it.
     *)

    let rec replace_vars repl = function

        | Expr.Let(info, ty, arg, x, y) ->
            let x = replace_vars repl x in
            let y = replace_vars repl y in
            Expr.Let(info, ty, arg, x, y)

        | Expr.LetFn(info, ty, fn, x) ->
            let x = replace_vars repl x in
            let fn = replace_vars_lambda repl fn in
            Expr.LetFn(info, ty, fn, x)

        | Expr.LetRec(info, ty, fns, x) ->
            let x = replace_vars repl x in
            let fns = List.map (replace_vars_lambda repl) fns in
            Expr.LetRec(info, ty, fns, x)

        | Expr.If(info, ty, x, y, z) ->
            let x = replace_vars repl x in
            let y = replace_vars repl y in
            let z = replace_vars repl z in
            Expr.If(info, ty, x, y, z)

        | Expr.AllocTuple(info, ty, tag, xs) ->
            let xs = List.map (replace_vars repl) xs in
            Expr.AllocTuple(info, ty, tag, xs)

        | Expr.GetField(info, ty, num, x) ->
            let x = replace_vars repl x in
            Expr.GetField(info, ty, num, x)

        | Expr.Case(info, ty, n, opts) ->
            (* Note: because of the way we generate it, the variable we
             * are switching on can never be free.  This simplifies things
             * here a lot.
             *)
            let opts =
                List.map
                    (fun (tag, x) -> tag, replace_vars repl x)
                    opts
            in
            Expr.Case(info, ty, n, opts)

        | Expr.BinOp(info, ty, x, op, y) ->
            let x = replace_vars repl x in
            let y = replace_vars repl y in
            Expr.BinOp(info, ty, x, op, y)

        | Expr.UnOp(info, ty, op, x) ->
            let x = replace_vars repl x in
            Expr.UnOp(info, ty, op, x)

        | Expr.Apply(info, ty, x, y) ->
            let x = replace_vars repl x in
            let y = replace_vars repl y in
            Expr.Apply(info, ty, x, y)

        | Expr.Var(info, ty, v) as x ->
            begin
                try
                    (Common.Var.Map.find v repl) info ty
                with
                | Not_found -> x
            end
        | Expr.Const(_, _, _) as c -> c
    and replace_vars_lambda repl (info, ty, name, args, x) =
        let x = replace_vars repl x in
        (info, ty, name, args, x)
    ;;

end;;

let get_info = function
    | Expr.Let(info, _, _, _, _)
    | Expr.LetFn(info, _, _, _)
    | Expr.LetRec(info, _, _, _)
    | Expr.If(info, _, _, _, _)
    | Expr.AllocTuple(info, _, _, _)
    | Expr.GetField(info, _, _, _)
    | Expr.Case(info, _, _, _)
    | Expr.BinOp(info, _, _, _, _)
    | Expr.UnOp(info, _, _, _)
    | Expr.Apply(info, _, _, _)
    | Expr.Var(info, _, _)
    | Expr.Const(info, _, _)
    -> info
;;

let get_type = function
    | Expr.Let(_, ty, _, _, _)
    | Expr.LetFn(_, ty, _, _)
    | Expr.LetRec(_, ty, _, _)
    | Expr.If(_, ty, _, _, _)
    | Expr.AllocTuple(_, ty, _, _)
    | Expr.GetField(_, ty, _, _)
    | Expr.Case(_, ty, _, _)
    | Expr.BinOp(_, ty, _, _, _)
    | Expr.UnOp(_, ty, _, _)
    | Expr.Apply(_, ty, _, _)
    | Expr.Var(_, ty, _)
    | Expr.Const(_, ty, _)
    -> ty
;;

let map_vars old_names new_names x =
    let repl =
        List.fold_left2
            (fun m old_name new_name ->
                Common.Var.Map.add old_name
                    (fun info ty -> Expr.Var(info, ty, new_name))
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
    let info = get_info x in
    let ty = get_type x in
    let tuple_var = Expr.Var(info, tuple_type, tuple_name) in
    Utils.fold_right2i
        (fun i nm ty' body ->
            Expr.Let(info, ty, (ty', Some nm),
                        Expr.GetField(info, ty', i, tuple_var),
                        body))
        new_vars
        var_types
        x
;;

let make_apply info f x =
    match get_type f with
    | Type.Arrow(_, t2) ->
        Expr.Apply(info, t2, f, x)
    | _ -> assert false
;;

let preapply_vars rec_fns var_names var_types x =
    let repl =
        List.fold_left
            (fun m n ->
                Common.Var.Map.add n
                    (fun info ty ->
                        let vars =
                            List.map2 (fun t v -> Expr.Var(info, t, v))
                                        var_types var_names
                        in
                        let fty = Type.fn_type var_types ty in
                        List.fold_left (make_apply info) 
                            (Expr.Var(info, fty, n))
                            vars)
                    m)
            Common.Var.Map.empty
            rec_fns
    in
    ReplaceVars.replace_vars repl x
;;

let fix_lambda_tuple rec_fns fvs fvs_types (info, ty, name, args, x) =
    let tuple_name = Common.Var.generate () in
    let tuple_type = Type.Tuple(fvs_types) in
    let x =
        if (rec_fns == []) then
            x
        else
            preapply_vars rec_fns [ tuple_name ] [ tuple_type ] x
    in
    let x = detuple tuple_name fvs fvs_types tuple_type x in
    let fty = Type.Arrow(tuple_type, ty) in
    (info, fty, name, (tuple_type, Some(tuple_name)) :: args, x)
;;

let fix_lambda_flat rec_fns fvs fvs_types (info, ty, name, args, x) =
    let new_names = List.map (fun _ -> Common.Var.generate ()) fvs in
    let x =
        if (rec_fns == []) then
            x
        else
            preapply_vars rec_fns new_names fvs_types x
    in
    let x = map_vars fvs new_names x in
    let new_args =
        List.map2
            (fun t v -> t, Some(v))
            fvs_types
            new_names
    in
    (info, (Type.fn_type fvs_types ty), name, (List.append new_args args), x)
;;

let tuplize_args n (_, _, _, args, _) =
    (n + (List.length args)) > Config.max_args
;;

let rec convert_expr publics = function
    | Expr.Let(info, ty, arg, x, y) ->
        let x = convert_expr publics x in
        let y = convert_expr publics y in
        Expr.Let(info, ty, arg, x, y)

    | Expr.LetFn(info, ty, fn, x) ->
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
        let (_, _, name, _, _) = fn in
        let x = preapply_vars [ name ] fv_names fv_types x  in
        let publics = Common.Var.Set.add name publics in
        let x = convert_expr publics x in
        Expr.LetFn(info, ty, fn, x)

    | Expr.LetRec(info, ty, fns, x) ->
        let publics, fns, g = convert_lambdas publics fns in
        let x = g x in
        let x = convert_expr publics x in
        Expr.LetRec(info, ty, fns, x)

    | Expr.If(info, ty, x, y, z) ->
        let x = convert_expr publics x in
        let y = convert_expr publics y in
        let z = convert_expr publics z in
        Expr.If(info, ty, x, y, z)

    | Expr.AllocTuple(info, ty, tag, xs) ->
        let xs = List.map (convert_expr publics) xs in
        Expr.AllocTuple(info, ty, tag, xs)

    | Expr.GetField(info, ty, num, x) ->
        let x = convert_expr publics x in
        Expr.GetField(info, ty, num, x)

    | Expr.Case(info, ty, n, opts) ->
        let opts =
            List.map (fun (tag, x) -> tag, convert_expr publics x) opts
        in
        Expr.Case(info, ty, n, opts)

    | Expr.BinOp(info, ty, x, op, y) ->
        let x = convert_expr publics x in
        let y = convert_expr publics y in
        Expr.BinOp(info, ty, x, op, y)

    | Expr.UnOp(info, ty, op, x) ->
        let x = convert_expr publics x in
        Expr.UnOp(info, ty, op, x)

    | Expr.Apply(info, ty, x, y) ->
        let x = convert_expr publics x in
        let y = convert_expr publics y in
        Expr.Apply(info, ty, x, y)

    | Expr.Var(_, _, _) as x -> x
    | Expr.Const(_, _, _) as x -> x

and convert_lambdas publics fns =
    let publics =
        List.fold_left
            (fun publics (_, _, name, _, _) ->
                Common.Var.Set.add name publics)
            publics
            fns
    in
    let fvmap = FreeVars.free_vars_lambdas publics fns in
    let nvs = Common.Var.Map.cardinal fvmap in
    let fvs = Common.Var.Map.bindings fvmap in
    let fv_names = List.map fst fvs in
    let fv_types = List.map snd fvs in
    let rec_fns =
        List.map (fun (_, _, name, _, _) -> name) fns
    in
    let fns =
        if (List.exists (tuplize_args nvs) fns) then
            List.map (fix_lambda_tuple rec_fns fv_names fv_types) fns
        else
            List.map (fix_lambda_flat rec_fns  fv_names fv_types) fns
    in
    publics, fns, (preapply_vars rec_fns fv_names fv_types)
;;

let convert publics = function
    | Top(info, ty, v, x) ->
        let x = convert_expr publics x in
        publics, Top(info, ty, v, x)
    | TopFn(info, ty, (info', ty', name, args, x)) ->
        let x = convert_expr publics x in
        (Common.Var.Set.add name publics),
            (TopFn(info, ty, (info', ty', name, args, x)))
    | TopRec(info, fns) ->
        let publics, fns, _ = convert_lambdas publics fns in
        publics, (TopRec(info, fns))
    | Extern(_, _, _) as x -> publics, x
;;

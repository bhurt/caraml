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

open Sexplib.Conv;;

module rec Lambda : sig

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;

    val do_convert : Expr.tagmap_t -> Common.Var.Set.t
                        -> MatchReduce.Lambda.t -> t;;

    val convert : Expr.tagmap_t -> MatchReduce.Lambda.t -> t;;

end = struct

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;

    let do_convert tagmap live_vars x = {
        info = x.MatchReduce.Lambda.info;
        typ = x.MatchReduce.Lambda.typ;
        name = x.MatchReduce.Lambda.name;
        args = x.MatchReduce.Lambda.args;
        body = Expr.do_convert tagmap live_vars x.MatchReduce.Lambda.body;
    };;

    let convert tagmap x = {
        info = x.MatchReduce.Lambda.info;
        typ = x.MatchReduce.Lambda.typ;
        name = x.MatchReduce.Lambda.name;
        args = x.MatchReduce.Lambda.args;
        body = Expr.convert tagmap x.MatchReduce.Lambda.body;
    };;

end and Expr : sig

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | ConstantConstructor of Common.Tag.t
        | GetField of int * t
        | IsConstantConstructor of t
        | ConstantConstructorCase of t * ((Common.Tag.t * t) list)
        | TupleConstructorCase of t * ((Common.Tag.t * t) list)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    type tagmap_t = (Common.Tag.t * (Info.t -> Common.VarType.t -> Expr.t))
                                                    Common.Var.Map.t
    ;;

    val do_convert : tagmap_t -> Common.Var.Set.t -> MatchReduce.Expr.t -> t;;

    val convert : tagmap_t -> MatchReduce.Expr.t -> t;;

end = struct

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | ConstantConstructor of Common.Tag.t
        | GetField of int * t
        | IsConstantConstructor of t
        | ConstantConstructorCase of t * ((Common.Tag.t * t) list)
        | TupleConstructorCase of t * ((Common.Tag.t * t) list)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;


    type tagmap_t = (Common.Tag.t * (Info.t -> Common.VarType.t -> Expr.t))
                                                    Common.Var.Map.t
    ;;

    let rec get_live_vars s x =
        match x.MatchReduce.Expr.body with

        | MatchReduce.Expr.Lambda(_, x)
        | MatchReduce.Expr.UnOp(_, x)
        -> get_live_vars s x

        | MatchReduce.Expr.Let(_, x, y)
        | MatchReduce.Expr.LetTuple(_, x, y)
        | MatchReduce.Expr.BinOp(x, _, y)
        | MatchReduce.Expr.Apply(x, y)
        ->
            let s = get_live_vars s x in
            let s = get_live_vars s y in
            s
        | MatchReduce.Expr.LetRec(lambdas, x) ->
            List.fold_left
                (fun s t -> get_live_vars s t.MatchReduce.Lambda.body)
                (get_live_vars s x)
                lambdas

        | MatchReduce.Expr.If(x, y, z) ->
            let s = get_live_vars s x in
            let s = get_live_vars s y in
            let s = get_live_vars s z in
            s

        | MatchReduce.Expr.Case(x, ts) ->
            List.fold_left
                (fun s (_, _, t) -> get_live_vars s t)
                (get_live_vars s x)
                ts
        | MatchReduce.Expr.Tuple(ts) ->
            List.fold_left get_live_vars s ts
        | MatchReduce.Expr.Var(v) -> Common.Var.Set.add v s
        | MatchReduce.Expr.Const(_) -> s
    ;;
 

    let rec replace_var name replacement expr =
        match expr.body with
        | Lambda(args, x) ->
            let x = replace_var name replacement x in
            { expr with body = Lambda(args, x) }
        | Let(arg, x, y) ->
            let x = replace_var name replacement x in
            let y = replace_var name replacement y in
            { expr with body = Let(arg, x, y) }
        | LetRec(lambdas, x) ->
            let x = replace_var name replacement x in
            let lambdas = List.map
                            (fun l ->
                                let body = replace_var name replacement
                                                l.Lambda.body
                                in
                                { l with Lambda.body = body })
                            lambdas
            in
            { expr with body = LetRec(lambdas, x) }
        | If(x, y, z) ->
            let x = replace_var name replacement x in
            let y = replace_var name replacement y in
            let z = replace_var name replacement z in
            { expr with body = If(x, y, z) }
        | AllocTuple(tag, xs) ->
            let xs = List.map (replace_var name replacement) xs in
            { expr with body = AllocTuple(tag, xs) }
        | ConstantConstructor(_) -> expr
        | GetField(i, x) ->
            let x = replace_var name replacement x in
            { expr with body = GetField(i, x) }
        | IsConstantConstructor(x) ->
            let x = replace_var name replacement x in
            { expr with body = IsConstantConstructor(x) }
        | ConstantConstructorCase(x, ds) ->
            let x = replace_var name replacement x in
            let ds = List.map
                        (fun (tag, y) ->
                            let y = replace_var name replacement y in
                            tag, y)
                        ds
            in
            { expr with body = ConstantConstructorCase(x, ds) }
        | TupleConstructorCase(x, ds) ->
            let x = replace_var name replacement x in
            let ds = List.map
                        (fun (tag, y) ->
                            let y = replace_var name replacement y in
                            tag, y)
                        ds
            in
            { expr with body = TupleConstructorCase(x, ds) }
        | BinOp(x, op, y) ->
            let x = replace_var name replacement x in
            let y = replace_var name replacement y in
            { expr with body = BinOp(x, op, y) }
        | UnOp(op, x) ->
            let x = replace_var name replacement x in
            { expr with body = UnOp(op, x) }
        | Apply(x, y) ->
            let x = replace_var name replacement x in
            let y = replace_var name replacement y in
            { expr with body = Apply(x, y) }
        | Var(v) ->
            if (Common.Var.compare v name) == 0 then
                { expr with body = replacement.body }
            else
                expr
        | Const(_) -> expr
    ;;

    let bind_args live_vars args src expr =
        Utils.fold_righti
            (fun i (ty, n) y ->
                match n with
                | None -> y
                | Some name ->
                    if (Common.Var.Set.mem name live_vars) then
                        { expr with body =
                            Let((ty, n), 
                                { src with typ = ty; body =
                                    GetField(i, src) },
                                expr) }
                    else
                        expr)
            args
            expr
    ;;

    let hoist_var ?base x f =
        match x.body with
        | Var(_) -> f x
        | Const(_) -> f x
        | _ ->
            let name = match base with
                            | None -> Common.Var.generate ()
                            | Some n -> Common.Var.derived n
            in
            let y = f { x with body = Var(name) }
            in
            { y with body = Let((x.typ, Some name), x, y) }
    ;;
            
    let rec do_convert tagmap live_vars expr =
        let info = expr.MatchReduce.Expr.info in
        let typ = expr.MatchReduce.Expr.typ in

        match expr.MatchReduce.Expr.body with
        | MatchReduce.Expr.Lambda(args, x) ->
            { info; typ; body = Lambda(args, do_convert tagmap live_vars x) }

        | MatchReduce.Expr.Let(n, x, y) ->
            let x = do_convert tagmap live_vars x in
            let y = do_convert tagmap live_vars y in
            { info; typ; body = Let(n, x, y) }

        | MatchReduce.Expr.LetRec(lambdas, x) ->
            let lambdas = List.map (Lambda.do_convert tagmap live_vars)
                                        lambdas
            in
            let x = do_convert tagmap live_vars x in
            { info; typ; body = LetRec(lambdas, x) }

        | MatchReduce.Expr.If(x, y, z) ->
            let x = do_convert tagmap live_vars x in
            let y = do_convert tagmap live_vars y in
            let z = do_convert tagmap live_vars z in
            { info; typ; body = If(x, y, z) }

        | MatchReduce.Expr.LetTuple(ns, x, y) ->
            let x = do_convert tagmap live_vars x in
            let y = do_convert tagmap live_vars y in
            hoist_var x (fun x -> bind_args live_vars ns x y)

        | MatchReduce.Expr.Case(x, defns) ->
            let x = do_convert tagmap live_vars x in
            hoist_var x
                (fun x -> reduce_case tagmap live_vars info typ x defns)

        | MatchReduce.Expr.Tuple(xs) ->
            { info; typ;
                body = AllocTuple(Common.Tag.of_int 0,
                                List.map (do_convert tagmap live_vars) xs) }
        | MatchReduce.Expr.BinOp(x, op, y) ->
            let x = do_convert tagmap live_vars x in
            let y = do_convert tagmap live_vars y in
            { info; typ; body = BinOp(x, op, y) }
        | MatchReduce.Expr.UnOp(op, x) ->
            let x = do_convert tagmap live_vars x in
            { info; typ; body = UnOp(op, x) }
        | MatchReduce.Expr.Apply(x, y) ->
            begin
                let x = do_convert tagmap live_vars x in
                let y = do_convert tagmap live_vars y in
                (* As we've introduced a bunch of inlined lambda expressions,
                 * most of which will be fully applied anyways, we do a
                 * quick beta-reduction step here.
                 *)
                match x.body with
                | Lambda([ (_, p) ], t) ->
                    hoist_var ?base:p y
                        (fun y ->
                            match p with
                            | Some name -> replace_var name y t
                            | None -> t)
                | Lambda((_, p) :: ps, t) ->
                    hoist_var ?base:p y
                        (fun y ->
                            let t =
                                match p with
                                | Some name -> replace_var name y t
                                | None -> t
                            in
                            let typ =
                                match typ with
                                | Type.Arrow(_, r) -> r
                                | _ -> assert false
                            in
                            { info; typ; body = Lambda(ps, t) })
                | _ -> { info; typ; body = Apply(x, y) }
            end
        | MatchReduce.Expr.Var(v) ->
            begin
                try
                    (snd (Common.Var.Map.find v tagmap)) info typ
                with
                | Not_found -> { info; typ; body = Var(v) }
            end
        | MatchReduce.Expr.Const(c) ->
            { info; typ; body = Const(c) }

    and reduce_case tagmap live_vars info typ x defns =

        let const_defns, tuple_defns =
            let rec loop const_defns tuple_defns = function
                | [] -> (List.rev const_defns), (List.rev tuple_defns)
                | x :: xs ->
                    match x with
                    | (_, [], _) -> loop (x :: const_defns) tuple_defns xs
                    | _ -> loop const_defns (x :: tuple_defns) xs
            in
            loop [] [] defns
        in
        let const_case =
            match const_defns with
            | [] -> None
            | [ (_, _, y) ] ->
                Some(do_convert tagmap live_vars y)
            | _ ->
                let defns =
                    List.map
                        (fun (name, _, y) ->
                            let tag = fst (Common.Var.Map.find name tagmap) in
                            tag, (do_convert tagmap live_vars y))
                        const_defns
                in
                let defns = List.stable_sort
                                (fun (x, _) (y, _) -> Common.Tag.compare x y)
                                defns
                in
                Some({ info; typ; body = ConstantConstructorCase(x, defns) })
        in
        let tuple_case =
            match tuple_defns with
            | [] -> None
            | [ (_, args, y) ] ->
                let y = do_convert tagmap live_vars y in
                Some(bind_args live_vars args x y)
            | _ ->
                    let defns =
                        List.map
                            (fun (name, args, y) ->
                                let tag =
                                    fst (Common.Var.Map.find name tagmap)
                                in
                                tag, bind_args live_vars args x
                                        (do_convert tagmap live_vars y))
                            defns
                    in
                    let defns = List.stable_sort
                                    (fun (x, _) (y, _)
                                                -> Common.Tag.compare x y)
                                    defns
                    in
                    Some({ info; typ; body = TupleConstructorCase(x, defns) })
        in
        match const_case, tuple_case with
        | None, None -> assert false
        | Some a, None
        | None, Some a -> a
        | Some a, Some b -> 
            { info; typ; body = If(
                { info; typ =Type.Base(Type.Boolean);
                    body = IsConstantConstructor(x) },
                a, b) }
    ;;

    let rec convert tagmap x =
        do_convert tagmap (get_live_vars Common.Var.Set.empty x) x
    ;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
and t = {
    info: Info.t;
    body: s;
} with sexp;;

let convert tagmap x =
    let info = x.MatchReduce.info in
    match x.MatchReduce.body with
    | MatchReduce.Top(ty, v, x) ->
        let x = Expr.convert tagmap x in
        tagmap, [ { info; body = Top(ty, v, x) } ]
    | MatchReduce.TopRec(fns) ->
        let fns = List.map (Lambda.convert tagmap) fns in
        tagmap, [ { info; body = TopRec(fns) } ]
    | MatchReduce.Extern(name, xt) ->
        tagmap, [ { info; body = Extern(name, xt) } ]
    | MatchReduce.VariantDef(type_name, opts) ->
        let tagmap =
            let rec loop c t tagmap = function
                | [] -> tagmap
                | (_, n, []) :: xs ->
                    let tag = Common.Tag.of_int c in
                    let temp = Expr.ConstantConstructor tag in
                    let f info ty = { Expr.info = info; Expr.typ = ty;
                                        Expr.body = temp }
                    in
                    let tagmap = Common.Var.Map.add n (tag, f) tagmap in
                    loop (c + 1) t tagmap xs
                | (_, n, tys) :: xs ->
                    let tag = Common.Tag.of_int t in
                    let f info typ =
                        let names =
                            List.map (fun _ -> Common.Var.generate ()) tys
                        in
                        let args = List.map2
                            (fun name ty -> 
                                { Expr.info = info; Expr.typ = ty;
                                    Expr.body = Expr.Var(name) })
                                names
                                tys
                        in
                        let params = List.map2 (fun ty name -> ty, Some name)
                                        tys names
                        in
                        { Expr.info = info; Expr.typ = typ;
                            Expr.body = Expr.Lambda(params,
                                { Expr.info = info;
                                    Expr.typ = Type.Named(type_name);
                                    Expr.body = Expr.AllocTuple(tag, args) })}
                    in
                    let tagmap = Common.Var.Map.add n (tag, f) tagmap in
                    loop c (t + 1) tagmap xs
            in
            loop 0 0 tagmap opts
        in
        tagmap, []
    ;;

module C : IL.Conversion with type input = MatchReduce.t and type output = t =
struct
    type input = MatchReduce.t;;
    type output = t;;
    type state = Expr.tagmap_t;;
    type check_state = unit;;

    let name = "case-reduce";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = Common.Var.Map.empty;;
    let convert state input = convert state input;;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert: IL.Converter with type output = t
    = IL.Make(MatchReduce.Convert)(C);;


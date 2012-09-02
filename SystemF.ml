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
        info : Info.t;
        typ : Common.VarType.t;
        name: Common.Var.t;
        args : Common.Arg.t list;
        body : Expr.t
    } with sexp;;

end = struct

    type t = {
        info : Info.t;
        typ : Common.VarType.t;
        name: Common.Var.t;
        args : Common.Arg.t list;
        body : Expr.t
    } with sexp;;

end and Expr : sig

    type s =
        | Lambda of Lambda.t
        | Let of Common.Arg.t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Case of t * ((Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of t list
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
        | Loop of Common.Var.t * ((Common.VarType.t * Common.Var.t) list) * t
        | Recur of Common.Var.t * ((Common.VarType.t * t) list)
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val make : Info.t -> Common.VarType.t -> s -> t;;
    val map : ?pre:(t -> t) -> ?post:(t -> t) -> t -> t;;
    val fold : ?pre:('a -> t -> 'a) -> ?post:(t -> 'a -> 'a) -> t -> 'a -> 'a;;
    val any: (t -> bool) -> t -> bool;;

end = struct

    type s =
        | Lambda of Lambda.t
        | Let of Common.Arg.t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Case of t * ((Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of t list
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
        | Loop of Common.Var.t * ((Common.VarType.t * Common.Var.t) list) * t
        | Recur of Common.Var.t * ((Common.VarType.t * t) list)
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    let make info typ body = { info; typ; body };;

    let rec map ?pre ?post expr =
        let expr =
            match pre with
                | Some f -> f expr
                | None -> expr
        in
        let expr =
            match expr.body with
            | Lambda(lambda) ->
                { expr with body =
                    Lambda { lambda with Lambda.body =
                                        map ?pre ?post lambda.Lambda.body } }
            | Let(v, x, y) ->
                let x = map ?pre ?post x in
                let y = map ?pre ?post y in
                { expr with body = Let(v, x, y) }
            | LetTuple(args, x, y) ->
                let x = map ?pre ?post x in
                let y = map ?pre ?post y in
                { expr with body = LetTuple(args, x, y) }
            | LetRec(lambdas, x) ->
                let lambdas =
                    List.map
                        (fun lambda ->
                            { lambda with Lambda.body =
                                        map ?pre ?post lambda.Lambda.body })
                        lambdas
                in
                let x = map ?pre ?post x in
                { expr with body = LetRec(lambdas, x) }
            | If(x, y, z) ->
                let x = map ?pre ?post x in
                let y = map ?pre ?post y in
                let z = map ?pre ?post z in
                { expr with body = If(x, y, z) }
            | Case(x, defns) ->
                let x = map ?pre ?post x in
                let defns =
                    List.map
                        (fun (n, args, y) ->
                            let y = map ?pre ?post y in
                            (n, args, y))
                        defns
                in
                { expr with body = Case(x, defns) }
            | Tuple(xs) ->
                let xs = List.map (map ?pre ?post) xs in
                { expr with body = Tuple(xs) }
            | Apply(x, y) ->
                let x = map ?pre ?post x in
                let y = map ?pre ?post y in
                { expr with body = Apply(x, y) }
            | Loop(n, args, x) ->
                let x = map ?pre ?post x in
                { expr with body = Loop(n, args, x) }
            | Var(_)
            | Const(_)
            | Recur(_, _) -> expr
        in
        match post with
        | Some f -> f expr
        | None -> expr
    ;;

    let rec fold ?pre ?post expr acc =
        let acc =
            match pre with
                | Some f -> f acc expr
                | None -> acc
        in
        let acc =
            match expr.body with
            | Lambda(lambda) ->
                fold ?pre ?post lambda.Lambda.body acc
            | Let(v, x, y) ->
                let acc = fold ?pre ?post x acc in
                fold ?pre ?post y acc
            | LetTuple(args, x, y) ->
                let acc = fold ?pre ?post x acc in
                fold ?pre ?post y acc
            | LetRec(lambdas, x) ->
                let acc =
                    List.fold_left
                        (fun acc lambda ->
                            fold ?pre ?post lambda.Lambda.body acc)
                        acc
                        lambdas
                in
                fold ?pre ?post x acc
            | If(x, y, z) ->
                let acc = fold ?pre ?post x acc in
                let acc = fold ?pre ?post y acc in
                fold ?pre ?post z acc
            | Case(x, defns) ->
                let acc = fold ?pre ?post x acc in
                List.fold_left
                    (fun acc (_, _, y) -> fold ?pre ?post y acc)
                    acc
                    defns
            | Tuple(xs) ->
                List.fold_left (fun acc x -> fold ?pre ?post x acc) acc xs
            | Apply(x, y) ->
                let acc = fold ?pre ?post x acc in
                fold ?pre ?post y acc
            | Loop(_, _, x) ->
                fold ?pre ?post x acc
            | Var(_)
            | Const(_)
            | Recur(_, _) -> acc
        in
        match post with
        | Some f -> f expr acc
        | None -> acc
    ;;

    let rec any f expr =
        if (f expr) then
            true
        else
            match expr.body with
            | Lambda(lambda) -> any f lambda.Lambda.body
            | Let(_, x, y)
            | LetTuple(_, x, y)
                -> (any f x) || (any f y)
            | LetRec(lambdas, x) ->
                (List.exists (fun lambda -> any f lambda.Lambda.body) lambdas)
                    || (any f x)
            | If(x, y, z) ->
                (any f x) || (any f y) || (any f z)
            | Case(x, defns) ->
                (any f x) || (List.exists (fun (_, _, y) -> any f y) defns)
            | Tuple(xs) -> List.exists (any f) xs
            | Apply(x, y) -> (any f x) || (any f y)
            | Loop(_, _, x) -> any f x
            | Var(_)
            | Const(_)
            | Recur(_, _) -> false
    ;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
    | VariantDef of Common.Var.t
                * ((Info.t * Common.Var.t * (Common.VarType.t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;



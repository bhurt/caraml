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

module Expr = struct

    type t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * t * t
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        | CallExtern of Info.t * Type.t * Common.External.t
                                        * ((Type.t * Common.Var.t) list)
        with sexp
    ;;

end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * (Common.Arg.t list) * Expr.t
    | TopVar of Info.t * Type.t * Common.Var.t * Expr.t
    | TopForward of Info.t * Type.t * Common.Var.t * int
    | TopExpr of Info.t * Type.t * Expr.t
    with sexp
;;


let make_forward (info, ty, name, args, _) =
    TopForward(info, ty, name, List.length args)
;;

let rec make_top (info, ty, name, args, x) =
    TopFun(info, ty, name, args, x)
;;

let rec convert_expr acc = function
    | LambdaConv.Expr.Let(info, ty, arg, x, y) ->
        let acc, x = convert_expr acc x in
        let acc, y = convert_expr acc y in
        acc, Expr.Let(info, ty, arg, x, y)

    | LambdaConv.Expr.LetTuple(info, ty, args, x, y) ->
        let acc, x = convert_expr acc x in
        let acc, y = convert_expr acc y in
        acc, Expr.LetTuple(info, ty, args, x, y)

    | LambdaConv.Expr.LetFn(info, ty, fn, x) ->
        let acc, fn = convert_lambda acc fn in
        let acc, x = convert_expr acc x in
        ((make_top fn) :: acc), x

    | LambdaConv.Expr.LetRec(info, ty, fns, x) ->
        let acc = List.rev_append (List.rev_map make_forward fns) acc in
        let acc, fns = Utils.map_accum convert_lambda acc fns in
        let acc, x = convert_expr acc x in
        (List.rev_append (List.rev_map make_top fns) acc), x

    | LambdaConv.Expr.If(info, ty, x, y, z) ->
        let acc, x = convert_expr acc x in
        let acc, y = convert_expr acc y in
        let acc, z = convert_expr acc z in
        acc, Expr.If(info, ty, x, y, z)

    | LambdaConv.Expr.Tuple(info, ty, xs) ->
        let acc, xs = Utils.map_accum convert_expr acc xs in
        let xs = List.rev xs in
        acc, Expr.Tuple(info, ty, xs)

    | LambdaConv.Expr.BinOp(info, ty, x, op, y) ->
        let acc, x = convert_expr acc x in
        let acc, y = convert_expr acc y in
        acc, Expr.BinOp(info, ty, x, op, y)

    | LambdaConv.Expr.UnOp(info, ty, op, x) ->
        let acc, x = convert_expr acc x in
        acc, Expr.UnOp(info, ty, op, x)

    | LambdaConv.Expr.Apply(info, ty, x, y) ->
        let acc, x = convert_expr acc x in
        let acc, y = convert_expr acc y in
        acc, Expr.Apply(info, ty, x, y)

    | LambdaConv.Expr.Var(info, ty, v) ->
        acc, Expr.Var(info, ty, v)

    | LambdaConv.Expr.Const(info, ty, c) ->
        acc, Expr.Const(info, ty, c)

and convert_lambda acc (info, ty, name, args, x) =
    let acc, x = convert_expr acc x in
    acc, (info, ty, name, args, x)
;;

let convert = function
    | LambdaConv.Top(info, ty, Some v, x) ->
        let acc, x = convert_expr [] x in
        List.rev_append acc [ TopVar(info, ty, v, x) ]

    | LambdaConv.Top(info, ty, None, x) ->
        let acc, x = convert_expr [] x in
        List.rev_append acc [ TopExpr(info, ty, x) ]

    | LambdaConv.TopFn(info, ty, fn) ->
        let acc, fn = convert_lambda [] fn in
        List.rev_append acc [ make_top fn ]

    | LambdaConv.TopRec(info, fns) ->
        let acc = List.rev_map make_forward fns in
        let acc, fns = Utils.map_accum convert_lambda acc fns in
        List.rev_append acc (List.map make_top fns)

    | LambdaConv.Extern(info, v, x) ->
        let arg_names = List.map (fun _ -> Common.Var.generate ())
                                    x.Common.External.arg_types
        in
        let vars =
            List.map2 (fun n t -> t, n) arg_names x.Common.External.arg_types
        in
        let y = Expr.CallExtern(info, x.Common.External.return_type,
                                    x, vars)
        in
        let fty = Type.fn_type x.Common.External.arg_types
                                    x.Common.External.return_type
        in
        let args =
            List.map2 (fun n t -> t, Some n) arg_names
                                                x.Common.External.arg_types
        in
        [ TopFun(info, fty, v, args, y) ]
;;


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
        | Tuple of Info.t * Type.t * (Type.t * Common.Var.t) list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * (Type.t * Common.Var.t)
                                        * ((Type.t * Common.Var.t) list)
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let get_info = function
        | Let(info, _, _, _, _)
        | LetTuple(info, _, _, _, _)
        | If(info, _, _, _, _)
        | Tuple(info, _, _)
        | BinOp(info, _, _, _, _)
        | UnOp(info, _, _, _)
        | Apply(info, _, _, _)
        | Var(info, _, _)
        | Const(info, _, _)
        -> info
    ;;

    let get_type = function
        | Let(_, ty, _, _, _)
        | LetTuple(_, ty, _, _, _)
        | If(_, ty, _, _, _)
        | Tuple(_, ty, _)
        | BinOp(_, ty, _, _, _)
        | UnOp(_, ty, _, _)
        | Apply(_, ty, _, _)
        | Var(_, ty, _)
        | Const(_, ty, _)
        -> ty
    ;;


    let lift_var v f =
        match v with
        | Var(_, ty, v) -> f (ty, v)
        | _ ->
            let name = Common.Var.generate () in
            let ty' = get_type v in
            let r = f (ty', name) in
            let info = get_info v in
            let ty = get_type r in
            Let(info, ty, (ty', Some name), v, r)
    ;;

    let rec convert = function
        | LambdaLift.Expr.Let(info, ty, arg, x, y) ->
            Let(info, ty, arg, (convert x), (convert y))
        | LambdaLift.Expr.LetTuple(info, ty, args, x, y) ->
            LetTuple(info, ty, args, (convert x), (convert y))
        | LambdaLift.Expr.If(info, ty, x, y, z) ->
            If(info, ty, (convert x), (convert y), (convert z))
        | LambdaLift.Expr.Tuple(info, ty, xs) ->
            let rec loop acc = function
                | [] -> Tuple(info, ty, List.rev acc)
                | x :: xs -> lift_var (convert x)
                                        (fun v -> loop (v :: acc) xs)
            in
            loop [] xs
        | LambdaLift.Expr.BinOp(info, ty, x, op, y) ->
            BinOp(info, ty, (convert x), op, (convert y))
        | LambdaLift.Expr.UnOp(info, ty, op, x) ->
            UnOp(info, ty, op, (convert x))
        | LambdaLift.Expr.Apply(info, ty, f, x) ->
            lift_var (convert x)
                (fun x ->
                    let f = convert f in
                    match f with
                    | Apply(_, _, f', xs) ->
                        Apply(info, ty, f', (List.append xs [ x ]))
                    | _ ->
                        lift_var f (fun f' -> Apply(info, ty, f', [x])))
        | LambdaLift.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | LambdaLift.Expr.Const(info, ty, c) ->
            Const(info, ty, c)
    ;;


end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * Common.Arg.t list * Expr.t
    | TopVar of Info.t * Type.t * Common.Var.t * Expr.t
    | TopExpr of Info.t * Type.t * Expr.t
    with sexp
;;

let convert = function
    | LambdaLift.TopFun(info, ty, v, args, x) ->
        TopFun(info, ty, v, args, Expr.convert x)
    | LambdaLift.TopVar(info, ty, v, x) ->
        TopVar(info, ty, v, Expr.convert x)
    | LambdaLift.TopExpr(info, ty, x) ->
        TopExpr(info, ty, Expr.convert x)
;;


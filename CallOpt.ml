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

module InnerExpr = struct

    (* An inner expression is any expression not in the tail position *)

    type t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * (Type.t * Common.Var.t) list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | InnerApply of Info.t * Type.t * (Type.t * Common.Var.t)
                            * ((Type.t * Common.Var.t) list)
        | InnerSafeApply of Info.t * Type.t * (Type.t * Common.Var.t) * int
                                * ((Type.t * Common.Var.t) list)
        | InnerCall of Info.t * Type.t * (Type.t * Common.Var.t)
                                    * ((Type.t * Common.Var.t) list)
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let rec convert globals = function
        | Simplify.Expr.Let(info, ty, arg, x, y) ->
            let x = convert globals x in
            let y = convert globals y in
            Let(info, ty, arg, x, y)
        | Simplify.Expr.LetTuple(info, ty, arg, x, y) ->
            let x = convert globals x in
            let y = convert globals y in
            LetTuple(info, ty, arg, x, y)
        | Simplify.Expr.If(info, ty, x, y, z) ->
            let x = convert globals x in
            let y = convert globals y in
            let z = convert globals z in
            If(info, ty, x, y, z)
        | Simplify.Expr.Tuple(info, ty, xs) -> Tuple(info, ty, xs)
        | Simplify.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert globals x in
            let y = convert globals y in
            BinOp(info, ty, x, op,y)
        | Simplify.Expr.UnOp(info, ty, op, x) ->
            let x = convert globals x in
            UnOp(info, ty, op, x)
        | Simplify.Expr.Apply(info, ty, f, xs) ->
            begin
                try
                    let nargs = Common.Var.Map.find (snd f) globals in
                    let nparams = List.length xs in
                    if (nparams == nargs) then
                        InnerCall(info, ty, f, xs)
                    else if (nparams > nargs) then
                        let call_args = Utils.take nargs xs in
                        let apply_args = Utils.drop nargs xs in
                        let call_ty =
                            List.fold_right
                                (fun t1 t2 -> Type.Arrow(t1, t2))
                                (List.map fst apply_args)
                                ty
                        in
                        let name = Common.Var.generate () in
                        Let(info, ty, (call_ty, Some name),
                            InnerCall(info, call_ty, f, call_args),
                            InnerApply(info, ty, (call_ty, name), apply_args))
                    else
                        InnerSafeApply(info, ty, f, nargs, xs)
                with
                | Not_found ->
                    InnerApply(info, ty, f,  xs)
            end
        | Simplify.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | Simplify.Expr.Const(info, ty, c) ->
            Const(info, ty, c)
    ;;

    let get_type = function
        | Let(_, ty, _, _, _)
        | LetTuple(_, ty, _, _, _)
        | If(_, ty, _, _, _)
        | Tuple(_, ty, _)
        | BinOp(_, ty, _, _, _)
        | UnOp(_, ty, _, _)
        | InnerApply(_, ty, _, _)
        | InnerSafeApply(_, ty, _, _, _)
        | InnerCall(_, ty, _, _)
        | Var(_, ty, _)
        | Const(_, ty, _)
        -> ty
    ;;

end;;


module TailExpr = struct

    type t =
        | Return of InnerExpr.t
        | Let of Info.t * Type.t * Common.Arg.t * InnerExpr.t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * InnerExpr.t * t
        | If of Info.t * Type.t * InnerExpr.t * t * t
        | TailCall of Info.t * Type.t * (Type.t * Common.Var.t)
                                        * ((Type.t * Common.Var.t) list)
        with sexp
    ;;

    let rec return = function
        | InnerExpr.Let(info, ty, v, x, y) ->
            Let(info, ty, v, x, return y)
        | InnerExpr.LetTuple(info, ty, v, x, y) ->
            LetTuple(info, ty, v, x, return y)
        | InnerExpr.If(info, ty, x, y, z) ->
            If(info, ty, x, return y, return z)
        | InnerExpr.InnerCall(info, ty, f, xs) ->
            TailCall(info, ty, f, xs)
        | x -> Return x
    ;;

    let convert globals x = return (InnerExpr.convert globals x);;

    let get_type = function
        | Return(x) -> InnerExpr.get_type x
        | Let(_, ty, _, _, _)
        | LetTuple(_, ty, _, _, _)
        | If(_, ty, _, _, _)
        | TailCall(_, ty, _, _)
        -> ty
    ;;

end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * Common.Arg.t list * TailExpr.t
    | TopVar of Info.t * Type.t * Common.Var.t * InnerExpr.t
    | TopExpr of Info.t * Type.t * InnerExpr.t
    with sexp
;;

let convert globals = function
    | Simplify.TopFun(info, ty, v, args, x) ->
        let x = TailExpr.convert globals x in
        (Common.Var.Map.add v (List.length args) globals),
        TopFun(info, ty, v, args, x)

    | Simplify.TopVar(info, ty, v, x) ->
        let x = InnerExpr.convert globals x in
        globals, TopVar(info, ty, v, x)

    | Simplify.TopExpr(info, ty, x) ->
        let x = InnerExpr.convert globals x in
        globals, TopExpr(info, ty, x)
;;



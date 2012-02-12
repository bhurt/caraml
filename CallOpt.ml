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

    (* An inner expression is any expression not in the tail position *)

    type t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | InnerApply of Info.t * Type.t * t * (t list)
        | InnerSafeApply of Info.t * Type.t * Common.Var.t * int * (t list)
        | InnerCall of Info.t * Type.t * Common.Var.t * (t list)
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let rec convert globals = function
        | LambdaLift.Expr.Let(info, ty, arg, x, y) ->
            let x = convert globals x in
            let y = convert globals y in
            Let(info, ty, arg, x, y)
        | LambdaLift.Expr.LetTuple(info, ty, arg, x, y) ->
            let x = convert globals x in
            let y = convert globals y in
            LetTuple(info, ty, arg, x, y)
        | LambdaLift.Expr.If(info, ty, x, y, z) ->
            let x = convert globals x in
            let y = convert globals y in
            let z = convert globals z in
            If(info, ty, x, y, z)
        | LambdaLift.Expr.Tuple(info, ty, xs) ->
            let xs = List.map (convert globals) xs in
            Tuple(info, ty, xs)
        | LambdaLift.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert globals x in
            let y = convert globals y in
            BinOp(info, ty, x, op,y)
        | LambdaLift.Expr.UnOp(info, ty, op, x) ->
            let x = convert globals x in
            UnOp(info, ty, op, x)
        | LambdaLift.Expr.Apply(info, ty, f, x) ->
            begin
                let f = convert globals f in
                let x = convert globals x in
                match f with
                | Var(_, _, v) ->
                    begin
                        try
                            let nargs = Common.Var.Map.find v globals in
                            if (nargs == 1) then
                                InnerCall(info, ty, v, [ x ])
                            else
                                InnerSafeApply(info, ty, v, nargs, [ x ])
                        with
                        | Not_found -> InnerApply(info, ty, f,  [ x ])
                    end
                | InnerSafeApply(_, _, v, nargs, xs) ->
                    let xs = List.append xs [ x ] in
                    if (nargs == (List.length xs)) then
                        InnerCall(info, ty, v, xs)
                    else
                        InnerSafeApply(info, ty, v, nargs, xs)
                | InnerApply(_, _, f, xs) ->
                    let xs = List.append xs [ x ] in
                    InnerApply(info, ty, f, xs)
                | _ -> InnerApply(info, ty, f, [ x ])
            end
        | LambdaLift.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | LambdaLift.Expr.Const(info, ty, c) ->
            Const(info, ty, c)
    ;;


end;;


module TailExpr = struct

    type t =
        | Return of InnerExpr.t
        | Let of Info.t * Type.t * Common.Arg.t * InnerExpr.t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * InnerExpr.t * t
        | If of Info.t * Type.t * InnerExpr.t * t * t
        | TailCall of Info.t * Type.t * Common.Var.t * (InnerExpr.t list)
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

end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * Common.Arg.t list * TailExpr.t
    | TopVar of Info.t * Type.t * Common.Var.t * InnerExpr.t
    | TopExpr of Info.t * Type.t * InnerExpr.t
    with sexp
;;

let convert globals = function
    | LambdaLift.TopFun(info, ty, v, args, x) ->
        let x = TailExpr.convert globals x in
        (Common.Var.Map.add v (List.length args) globals),
        TopFun(info, ty, v, args, x)

    | LambdaLift.TopVar(info, ty, v, x) ->
        let x = InnerExpr.convert globals x in
        globals, TopVar(info, ty, v, x)

    | LambdaLift.TopExpr(info, ty, x) ->
        let x = InnerExpr.convert globals x in
        globals, TopExpr(info, ty, x)
;;



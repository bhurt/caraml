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

    type lambda = Info.t * Type.t * Common.Var.t * (Common.Arg.t list) * t
    and t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | LetFn of Info.t * Type.t * lambda * t
        | LetRec of Info.t * Type.t * (lambda list) * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * t * t
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let rec flatten_lambdas args = function
        | Alpha.Expr.Lambda(_, _, args2, y) ->
            flatten_lambdas (List.append args args2) y
        | y -> args, y
    ;;

    let rec convert = function
        | Alpha.Expr.Lambda(info, ty, args, x) ->
            let args, y = flatten_lambdas args x in
            let name = Common.Var.generate () in
            let y = convert y in
            LetFn(info, ty,
                    (info, ty, name, args, y),
                    Var(info, ty, name))

        | Alpha.Expr.Let(info, ty, arg,
                            Alpha.Expr.Lambda(info', ty', args, x),
                            y)
        ->
            begin
                match arg with
                | _, Some name ->
                    let args, x = flatten_lambdas args x in
                    let x = convert x in
                    let y = convert y in
                    LetFn(info, ty, (info', ty', name, args, x), y)
                | _, None ->
                    (* Elide dead code *)
                    convert y
            end

        | Alpha.Expr.Let(info, ty, arg, x, y) ->
            let x = convert x in
            let y = convert y in
            Let(info, ty, arg, x, y)

        | Alpha.Expr.LetTuple(info, ty, args, x, y) ->
            let x = convert x in
            let y = convert y in
            LetTuple(info, ty, args, x, y)

        | Alpha.Expr.LetRec(info, ty, fns, x) ->
            let fns = List.map convert_lambda fns in
            let x = convert x in
            LetRec(info, ty, fns, x)

        | Alpha.Expr.If(info, ty, x, y, z) ->
            let x = convert x in
            let y = convert y in
            let z = convert z in
            If(info, ty, x, y, z)

        | Alpha.Expr.Tuple(info, ty, xs) ->
            let xs = List.map convert xs in
            Tuple(info, ty, xs)

        | Alpha.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert x in
            let y = convert y in
            BinOp(info, ty, x, op, y)
        | Alpha.Expr.UnOp(info, ty, op, x) ->
            let x = convert x in
            UnOp(info, ty, op, x)
        | Alpha.Expr.Apply(info, ty, x, y) ->
            let x = convert x in
            let y = convert y in
            Apply(info, ty, x, y)
        | Alpha.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | Alpha.Expr.Const(info, ty, c) ->
            Const(info, ty, c)

    and convert_lambda (info, ty, name, args, x) =
        let args, x = flatten_lambdas args x in
        let x = convert x in
        (info, ty, name, args, x)
    ;;

end;;

type t =
    | Top of Info.t * Type.t * Common.Var.t option * Expr.t
    | TopFn of Info.t * Type.t * Expr.lambda
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.External.t
    with sexp
;;

let convert = function
    | Alpha.Top(info, ty, arg, x) ->
        begin
            match x with
            | Alpha.Expr.Lambda(info', ty', args, x) ->
                let args, x = Expr.flatten_lambdas args x in
                let x = Expr.convert x in
                let name = match arg with
                                | Some n -> n
                                | None -> Common.Var.generate ()
                in
                TopFn(info, ty, (info', ty', name, args, x))
            | x ->
                let x = Expr.convert x in
                Top(info, ty, arg, x)
        end
    | Alpha.TopRec(info, fns) ->
        let fns = List.map Expr.convert_lambda fns in
        TopRec(info, fns)
    | Alpha.Extern(info, name, xtrn) -> Extern(info, name, xtrn)
;;



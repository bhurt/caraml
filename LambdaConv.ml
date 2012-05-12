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

    type lambda = Info.t * Common.VarType.t * Common.Var.t
                    * (Common.Arg.t list) * t
    and t =
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | LetFn of Info.t * Common.VarType.t * lambda * t
        | LetRec of Info.t * Common.VarType.t * (lambda list) * t
        | If of Info.t * Common.VarType.t * t * t * t
        | AllocTuple of Info.t * Common.VarType.t * Common.Tag.t * (t list)
        | GetField of Info.t * Common.VarType.t * int * t
        | Case of Info.t * Common.VarType.t * (Common.VarType.t * Common.Var.t)
                        * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t * t Common.Var.Map.t
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | Apply of Info.t * Common.VarType.t * t * t
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        with sexp
    ;;

    let rec flatten_lambdas args = function
        | MatchReduce.Expr.Lambda(_, _, args2, y) ->
            flatten_lambdas (List.append args args2) y
        | y -> args, y
    ;;

    let rec convert = function
        | MatchReduce.Expr.Lambda(info, ty, args, x) ->
            let args, y = flatten_lambdas args x in
            let name = Common.Var.generate () in
            let y = convert y in
            LetFn(info, ty,
                    (info, ty, name, args, y),
                    Var(info, ty, name))

        | MatchReduce.Expr.Let(info, ty, arg,
                            MatchReduce.Expr.Lambda(info', ty', args, x),
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

        | MatchReduce.Expr.Let(info, ty, arg, x, y) ->
            let x = convert x in
            let y = convert y in
            Let(info, ty, arg, x, y)

        | MatchReduce.Expr.LetRec(info, ty, fns, x) ->
            let fns = List.map convert_lambda fns in
            let x = convert x in
            LetRec(info, ty, fns, x)

        | MatchReduce.Expr.If(info, ty, x, y, z) ->
            let x = convert x in
            let y = convert y in
            let z = convert z in
            If(info, ty, x, y, z)

        | MatchReduce.Expr.AllocTuple(info, ty, tag, xs) ->
            let xs = List.map convert xs in
            AllocTuple(info, ty, tag, xs)

        | MatchReduce.Expr.GetField(info, ty, num, x) ->
            let x = convert x in
            GetField(info, ty, num, x)

        | MatchReduce.Expr.Case(info, ty, n, opts) ->
            let opts = List.map (fun (tag, x) -> tag, convert x) opts in
            Case(info, ty, n, opts)

        | MatchReduce.Expr.Label(info, ty, x, label, bindings, y) ->
            let x = convert x in
            let y = convert y in
            Label(info, ty, x, label, bindings, y)

        | MatchReduce.Expr.Goto(info, label, bindings) ->
            let bindings = Common.Var.Map.map convert bindings in
            Goto(info, label, bindings)

        | MatchReduce.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert x in
            let y = convert y in
            BinOp(info, ty, x, op, y)
        | MatchReduce.Expr.UnOp(info, ty, op, x) ->
            let x = convert x in
            UnOp(info, ty, op, x)
        | MatchReduce.Expr.Apply(info, ty, x, y) ->
            let x = convert x in
            let y = convert y in
            Apply(info, ty, x, y)
        | MatchReduce.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | MatchReduce.Expr.Const(info, ty, c) ->
            Const(info, ty, c)

    and convert_lambda (info, ty, name, args, x) =
        let args, x = flatten_lambdas args x in
        let x = convert x in
        (info, ty, name, args, x)
    ;;

end;;

type t =
    | Top of Info.t * Common.VarType.t * Common.Var.t option * Expr.t
    | TopFn of Info.t * Common.VarType.t * Expr.lambda
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.Var.t Common.External.t
    with sexp
;;

let convert = function
    | MatchReduce.Top(info, ty, arg, x) ->
        begin
            match x with
            | MatchReduce.Expr.Lambda(info', ty', args, x) ->
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
    | MatchReduce.TopRec(info, fns) ->
        let fns = List.map Expr.convert_lambda fns in
        TopRec(info, fns)
    | MatchReduce.Extern(info, name, xtrn) -> Extern(info, name, xtrn)
;;



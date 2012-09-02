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

    type s =
        | Let of Common.Arg.t * t * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | ConstantConstructor of Common.Tag.t
        | GetField of int * t
        | IsConstantConstructor of t
        | ConstantConstructorCase of t * ((Common.Tag.t * t) list)
        | TupleConstructorCase of t * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t * (t Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
        | CallExtern of Common.Var.t Common.External.t
                    * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info : Info.t;
        typ : Common.VarType.t;
        body : s;
    } with sexp;;

end;;

type s =
    | TopFun of Common.Var.t * Common.Arg.t list * Expr.t
    | TopVar of Common.Var.t * Expr.t
    | TopForward of Common.Var.t * int
    | TopExpr of Expr.t
and t = {
    info : Info.t;
    typ: Common.VarType.t;
    body: s;
} with sexp;;

let make info typ body = { info = info; typ = typ; body = body };;

let make_forward lambda =
    make lambda.LambdaConv.Lambda.info lambda.LambdaConv.Lambda.typ
        (TopForward(lambda.LambdaConv.Lambda.name,
                        List.length lambda.LambdaConv.Lambda.args))
;;

let rec make_top (lambda, body) =
    make lambda.LambdaConv.Lambda.info lambda.LambdaConv.Lambda.typ
        (TopFun(lambda.LambdaConv.Lambda.name, lambda.LambdaConv.Lambda.args,
                    body))
;;

let rec convert_expr acc expr =
    let info = expr.LambdaConv.Expr.info in
    let ty = expr.LambdaConv.Expr.typ in
    let acc, body =
        match expr.LambdaConv.Expr.body with
        | LambdaConv.Expr.Let(arg, x, y) ->
            let acc, x = convert_expr acc x in
            let acc, y = convert_expr acc y in
            acc, Expr.Let(arg, x, y)

        | LambdaConv.Expr.LetFn(fn, x) ->
            let acc, fn = convert_lambda acc fn in
            let acc, x = convert_expr acc x in
            ((make_top fn) :: acc), (x.Expr.body)

        | LambdaConv.Expr.LetRec(fns, x) ->
            let acc = List.rev_append (List.rev_map make_forward fns) acc in
            let acc, fns = Utils.map_accum convert_lambda acc fns in
            let acc, x = convert_expr acc x in
            (List.rev_append (List.rev_map make_top fns) acc), x.Expr.body

        | LambdaConv.Expr.If(x, y, z) ->
            let acc, x = convert_expr acc x in
            let acc, y = convert_expr acc y in
            let acc, z = convert_expr acc z in
            acc, Expr.If(x, y, z)

        | LambdaConv.Expr.AllocTuple(tag, xs) ->
            let acc, xs = Utils.map_accum convert_expr acc xs in
            let xs = List.rev xs in
            acc, Expr.AllocTuple(tag, xs)

        | LambdaConv.Expr.ConstantConstructor(tag) ->
            acc, Expr.ConstantConstructor(tag)

        | LambdaConv.Expr.GetField(num, x) ->
            let acc, x = convert_expr acc x in
            acc, Expr.GetField(num, x)

        | LambdaConv.Expr.IsConstantConstructor(x) ->
            let acc, x = convert_expr acc x in
            acc, Expr.IsConstantConstructor(x)

        | LambdaConv.Expr.ConstantConstructorCase(n, opts) ->
            let acc, opts =
                Utils.map_accum
                    (fun acc (tag, x) ->
                        let acc, x = convert_expr acc x in
                        acc, (tag, x))
                    acc
                    opts
            in
            let acc, n = convert_expr acc n in
            acc, Expr.ConstantConstructorCase(n, opts)

        | LambdaConv.Expr.TupleConstructorCase(n, opts) ->
            let acc, opts =
                Utils.map_accum
                    (fun acc (tag, x) ->
                        let acc, x = convert_expr acc x in
                        acc, (tag, x))
                    acc
                    opts
            in
            let acc, n = convert_expr acc n in
            acc, Expr.TupleConstructorCase(n, opts)

        | LambdaConv.Expr.Label(x, label, bindings, y) ->
            let acc, x = convert_expr acc x in
            let acc, y = convert_expr acc y in
            acc, Expr.Label(x, label, bindings, y)
        | LambdaConv.Expr.Goto(label, bindings) ->
            let acc, (bindings: Expr.t Common.Var.Map.t) = Common.Var.Map.fold
                                    (fun k x (acc, bindings) ->
                                        let acc, x = convert_expr acc x in
                                        let bindings = Common.Var.Map.add
                                                            k x bindings
                                        in
                                        acc, bindings)
                                    bindings
                                    (acc, Common.Var.Map.empty)
            in
            acc, Expr.Goto(label, bindings)
        | LambdaConv.Expr.BinOp(x, op, y) ->
            let acc, x = convert_expr acc x in
            let acc, y = convert_expr acc y in
            acc, Expr.BinOp(x, op, y)

        | LambdaConv.Expr.UnOp(op, x) ->
            let acc, x = convert_expr acc x in
            acc, Expr.UnOp(op, x)

        | LambdaConv.Expr.Apply(x, y) ->
            let acc, x = convert_expr acc x in
            let acc, y = convert_expr acc y in
            acc, Expr.Apply(x, y)

        | LambdaConv.Expr.Var(v) ->
            acc, Expr.Var(v)

        | LambdaConv.Expr.Const(c) ->
            acc, Expr.Const(c)
    in
    acc, { Expr.info = info; Expr.typ = ty; Expr.body = body }

and convert_lambda acc lambda =
    let acc, body = convert_expr acc lambda.LambdaConv.Lambda.body in
    acc, (lambda, body)
;;

let convert top =
    let info = top.LambdaConv.info in
    match top.LambdaConv.body with
    | LambdaConv.Top(ty, Some v, x) ->
        let acc, x = convert_expr [] x in
        List.rev_append acc [ make info ty (TopVar(v, x)) ]

    | LambdaConv.Top(ty, None, x) ->
        let acc, x = convert_expr [] x in
        List.rev_append acc [ make info ty (TopExpr(x)) ]

    | LambdaConv.TopFn(ty, fn) ->
        let acc, fn = convert_lambda [] fn in
        List.rev_append acc [ make_top fn ]

    | LambdaConv.TopRec(fns) ->
        let acc = List.rev_map make_forward fns in
        let acc, fns = Utils.map_accum convert_lambda acc fns in
        List.rev_append acc (List.map make_top fns)

    | LambdaConv.Extern(v, x) ->
        let arg_names = List.map (fun _ -> Common.Var.generate ())
                                    x.Common.External.arg_types
        in
        let vars =
            List.map2 (fun n t -> t, n) arg_names x.Common.External.arg_types
        in
        let y =  {  Expr.info = info;
                    Expr.typ = x.Common.External.return_type;
                    Expr.body = Expr.CallExtern(x, vars) }
        in
        let fty = Type.fn_type x.Common.External.arg_types
                                    x.Common.External.return_type
        in
        let args =
            List.map2 (fun n t -> t, Some n) arg_names
                                                x.Common.External.arg_types
        in
        [ make info fty (TopFun(v, args, y)) ]
;;

module C : IL.Conversion with type input = LambdaConv.t
                                and type output = t =
struct
    type input = LambdaConv.t;;
    type output = t;;
    type state = unit;;
    type check_state = unit;;

    let name = "lambda-lift";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = ();;
    let convert _ input = (), (convert input);;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert : IL.Converter with type output = t =
    IL.Make(FreeBind.Convert)(C);;



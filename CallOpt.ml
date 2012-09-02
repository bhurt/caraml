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

    type s =
        | Let of Common.Arg.t * t * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (Common.VarType.t * Common.Var.t) list
        | ConstantConstructor of Common.Tag.t
        | GetField of int * (Common.VarType.t * Common.Var.t)
        | IsConstantConstructor of t
        | ConstantConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | TupleConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | InnerApply of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | InnerSafeApply of (Common.VarType.t * Common.Var.t) * int
                                * ((Common.VarType.t * Common.Var.t) list)
        | InnerCall of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | Var of Common.Var.t
        | Const of Common.Const.t
        | CallExtern of Common.Var.t Common.External.t
                            * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info: Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    let rec convert globals expr =
        let info = expr.Simplify.Expr.info in
        let ty = expr.Simplify.Expr.typ in
        let body =
            match expr.Simplify.Expr.body with
            | Simplify.Expr.Let(arg, x, y) ->
                let x = convert globals x in
                let y = convert globals y in
                Let(arg, x, y)
            | Simplify.Expr.If(x, y, z) ->
                let x = convert globals x in
                let y = convert globals y in
                let z = convert globals z in
                If(x, y, z)
            | Simplify.Expr.AllocTuple(tag, xs)
                -> AllocTuple(tag, xs)
            | Simplify.Expr.ConstantConstructor(tag)
                -> ConstantConstructor(tag)
            | Simplify.Expr.GetField(n, v)
                -> GetField(n, v)
            | Simplify.Expr.IsConstantConstructor(x) ->
                let x = convert globals x in
                IsConstantConstructor(x)
            | Simplify.Expr.ConstantConstructorCase(n, opts)
                -> let opts =
                        List.map
                            (fun (tag, x) -> tag, (convert globals x)) opts
                    in
                    ConstantConstructorCase(n, opts)
            | Simplify.Expr.TupleConstructorCase(n, opts)
                -> let opts =
                        List.map
                            (fun (tag, x) -> tag, (convert globals x)) opts
                    in
                    TupleConstructorCase(n, opts)
            | Simplify.Expr.Label(x, label, bindings, y) ->
                let x = convert globals x in
                let y = convert globals y in
                Label(x, label, bindings, y)
            | Simplify.Expr.Goto(label, bindings) ->
                Goto(label, bindings)
            | Simplify.Expr.BinOp(x, op, y) ->
                let x = convert globals x in
                let y = convert globals y in
                BinOp(x, op,y)
            | Simplify.Expr.UnOp(op, x) ->
                let x = convert globals x in
                UnOp(op, x)
            | Simplify.Expr.Apply(f, xs) ->
                begin
                    try
                        let nargs = Common.Var.Map.find (snd f) globals in
                        let nparams = List.length xs in
                        if (nparams == nargs) then
                            InnerCall(f, xs)
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
                            Let((call_ty, Some name),
                                { info; typ = call_ty;
                                    body = InnerCall(f, call_args) },
                                { info; typ = ty;
                                    body = InnerApply((call_ty, name),
                                                            apply_args) })
                        else
                            InnerSafeApply(f, nargs, xs)
                    with
                    | Not_found ->
                        InnerApply(f,  xs)
                end
            | Simplify.Expr.Var(v) ->
                Var(v)
            | Simplify.Expr.Const(c) ->
                Const(c)
            | Simplify.Expr.CallExtern(xtern, xs) ->
                CallExtern(xtern, xs)
        in
        { info; typ = ty; body }
    ;;

    let get_type x = x.typ;;

end;;


module TailExpr = struct

    type s =
        | Return of InnerExpr.t
        | Let of Common.Arg.t * InnerExpr.t * t
        | If of InnerExpr.t * t * t
        | ConstantConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | TupleConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | TailCall of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | TailCallExtern of Common.Var.t Common.External.t
                                * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    let rec return expr =
        let info = expr.InnerExpr.info in
        let typ = expr.InnerExpr.typ in
        let body =
            match expr.InnerExpr.body with
            | InnerExpr.Let(v, x, y) ->
                Let(v, x, return y)
            | InnerExpr.If(x, y, z) ->
                If(x, return y, return z)
            | InnerExpr.ConstantConstructorCase(n, opts) ->
                let opts = List.map (fun (tag, x) -> tag, return x) opts in
                ConstantConstructorCase(n, opts)
            | InnerExpr.TupleConstructorCase(n, opts) ->
                let opts = List.map (fun (tag, x) -> tag, return x) opts in
                TupleConstructorCase(n, opts)
            | InnerExpr.Label(x, label, bindings, y) ->
                Label((return x), label, bindings, (return y))
            | InnerExpr.Goto(label, bindings) ->
                Goto(label, bindings)
            | InnerExpr.InnerCall(f, xs) ->
                TailCall(f, xs)
            | InnerExpr.CallExtern(xtern, xs) ->
                TailCallExtern(xtern, xs)
            | x -> Return expr
        in
        { info; typ; body }
    ;;

    let convert globals x = return (InnerExpr.convert globals x);;

    let get_type x = x.typ;;

end;;

type s =
    | TopFun of Common.Var.t * Common.Arg.t list * TailExpr.t
    | TopVar of Common.Var.t * InnerExpr.t
    | TopForward of Common.Var.t * int
    | TopExpr of InnerExpr.t
and t = {
    info: Info.t;
    typ: Common.VarType.t;
    body: s;
} with sexp;;

let convert globals top =
    let info = top.Simplify.info in
    let typ = top.Simplify.typ in
    let globals, body =
        match top.Simplify.body with
        | Simplify.TopFun(v, args, x) ->
            let x = TailExpr.convert globals x in
            (Common.Var.Map.add v (List.length args) globals),
            TopFun(v, args, x)

        | Simplify.TopVar(v, x) ->
            let x = InnerExpr.convert globals x in
            globals, TopVar(v, x)

        | Simplify.TopForward(n, nargs) ->
            (Common.Var.Map.add n nargs globals),
            TopForward(n, nargs)

        | Simplify.TopExpr(x) ->
            let x = InnerExpr.convert globals x in
            globals, TopExpr(x)
    in
    globals, { info; typ; body }
;;

module C : IL.Conversion with type input = Simplify.t
                                and type output = t =
struct
    type input = Simplify.t;;
    type output = t;;
    type state = int Common.Var.Map.t;;
    type check_state = unit;;

    let name = "callopt";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = Common.Var.Map.empty;;
    let convert state input =
        let state, output = convert state input in
        state, [ output ]
    ;;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert : IL.Converter with type output = t =
    IL.Make(Simplify.Convert)(C);;


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
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | If of Info.t * Common.VarType.t * t * t * t
        | AllocTuple of Info.t * Common.VarType.t * Common.Tag.t
                            * (Common.VarType.t * Common.Var.t) list
        | GetField of Info.t * Common.VarType.t * int
                            * (Common.VarType.t * Common.Var.t)
        | Case of Info.t * Common.VarType.t
                        * (Common.VarType.t * Common.Var.t)
                        * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | InnerApply of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | InnerSafeApply of Info.t * Common.VarType.t
                                * (Common.VarType.t * Common.Var.t) * int
                                * ((Common.VarType.t * Common.Var.t) list)
        | InnerCall of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        | CallExtern of Info.t * Common.VarType.t
                            * Common.Var.t Common.External.t
                            * ((Common.VarType.t * Common.Var.t) list)
        with sexp
    ;;

    let rec convert globals = function
        | Simplify.Expr.Let(info, ty, arg, x, y) ->
            let x = convert globals x in
            let y = convert globals y in
            Let(info, ty, arg, x, y)
        | Simplify.Expr.If(info, ty, x, y, z) ->
            let x = convert globals x in
            let y = convert globals y in
            let z = convert globals z in
            If(info, ty, x, y, z)
        | Simplify.Expr.AllocTuple(info, ty, tag, xs)
            -> AllocTuple(info, ty, tag, xs)
        | Simplify.Expr.GetField(info, ty, n, v) -> GetField(info, ty, n, v)
        | Simplify.Expr.Case(info, ty, n, opts) ->
            let opts =
                List.map (fun (tag, x) -> tag, (convert globals x)) opts
            in
            Case(info, ty, n, opts)
        | Simplify.Expr.Label(info, ty, x, label, bindings, y) ->
            let x = convert globals x in
            let y = convert globals y in
            Label(info, ty, x, label, bindings, y)
        | Simplify.Expr.Goto(info, label, bindings) ->
            Goto(info, label, bindings)
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
                            InnerApply(info, ty, (call_ty, name),
                                                        apply_args))
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
        | Simplify.Expr.CallExtern(info, ty, xtern, xs) ->
            CallExtern(info, ty, xtern, xs)
    ;;

    let get_type = function
        | Let(_, ty, _, _, _)
        | If(_, ty, _, _, _)
        | AllocTuple(_, ty, _, _)
        | GetField(_, ty, _, _)
        | Case(_, ty, _, _)
        | Label(_, ty, _, _, _, _)
        | BinOp(_, ty, _, _, _)
        | UnOp(_, ty, _, _)
        | InnerApply(_, ty, _, _)
        | InnerSafeApply(_, ty, _, _, _)
        | InnerCall(_, ty, _, _)
        | Var(_, ty, _)
        | Const(_, ty, _)
        | CallExtern(_, ty, _, _)
        -> ty

        | Goto(_, _, _) -> Type.Base(Type.Unit)
    ;;

end;;


module TailExpr = struct

    type t =
        | Return of InnerExpr.t
        | Let of Info.t * Common.VarType.t * Common.Arg.t * InnerExpr.t * t
        | If of Info.t * Common.VarType.t * InnerExpr.t * t * t
        | Case of Info.t * Common.VarType.t
                    * (Common.VarType.t * Common.Var.t)
                    * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | TailCall of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | TailCallExtern of Info.t * Common.VarType.t
                                * Common.Var.t Common.External.t
                                * ((Common.VarType.t * Common.Var.t) list)
        with sexp
    ;;

    let rec return = function
        | InnerExpr.Let(info, ty, v, x, y) ->
            Let(info, ty, v, x, return y)
        | InnerExpr.If(info, ty, x, y, z) ->
            If(info, ty, x, return y, return z)
        | InnerExpr.Case(info, ty, n, opts) ->
            let opts = List.map (fun (tag, x) -> tag, return x) opts in
            Case(info, ty, n, opts)
        | InnerExpr.Label(info, ty, x, label, bindings, y) ->
            Label(info, ty, (return x), label, bindings, (return y))
        | InnerExpr.Goto(info, label, bindings) ->
            Goto(info, label, bindings)
        | InnerExpr.InnerCall(info, ty, f, xs) ->
            TailCall(info, ty, f, xs)
        | InnerExpr.CallExtern(info, ty, xtern, xs) ->
            TailCallExtern(info, ty, xtern, xs)
        | x -> Return x
    ;;

    let convert globals x = return (InnerExpr.convert globals x);;

    let get_type = function
        | Return(x) -> InnerExpr.get_type x
        | Let(_, ty, _, _, _)
        | If(_, ty, _, _, _)
        | Case(_, ty, _, _)
        | Label(_, ty, _, _, _, _)
        | TailCall(_, ty, _, _)
        | TailCallExtern(_, ty, _, _)
        -> ty

        | Goto(_, _, _) -> Type.Base(Type.Unit)
    ;;

end;;

type t =
    | TopFun of Info.t * Common.VarType.t * Common.Var.t
                    * Common.Arg.t list * TailExpr.t
    | TopVar of Info.t * Common.VarType.t * Common.Var.t * InnerExpr.t
    | TopForward of Info.t * Common.VarType.t * Common.Var.t * int
    | TopExpr of Info.t * Common.VarType.t * InnerExpr.t
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

    | Simplify.TopForward(info, ty, n, nargs) ->
        (Common.Var.Map.add n nargs globals),
        TopForward(info, ty, n, nargs)

    | Simplify.TopExpr(info, ty, x) ->
        let x = InnerExpr.convert globals x in
        globals, TopExpr(info, ty, x)
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


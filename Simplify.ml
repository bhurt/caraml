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
        | Apply of Info.t * Common.VarType.t
                        * (Common.VarType.t * Common.Var.t)
                        * ((Common.VarType.t * Common.Var.t) list)
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        | CallExtern of Info.t * Common.VarType.t
                            * Common.Var.t Common.External.t
                            * ((Common.VarType.t * Common.Var.t) list)
        with sexp
    ;;

    let get_info = function
        | Let(info, _, _, _, _)
        | If(info, _, _, _, _)
        | AllocTuple(info, _, _, _)
        | GetField(info, _, _, _)
        | Case(info, _, _, _)
        | Label(info, _, _, _, _, _)
        | Goto(info, _, _)
        | BinOp(info, _, _, _, _)
        | UnOp(info, _, _, _)
        | Apply(info, _, _, _)
        | Var(info, _, _)
        | Const(info, _, _)
        | CallExtern(info, _, _, _)
        -> info
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
        | Apply(_, ty, _, _)
        | Var(_, ty, _)
        | Const(_, ty, _)
        | CallExtern(_, ty, _, _)
        -> ty

        (* This should be 'a *)
        | Goto(_, _, _) -> Type.Base(Type.Unit)
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
        | LambdaLift.Expr.If(info, ty, x, y, z) ->
            If(info, ty, (convert x), (convert y), (convert z))
        | LambdaLift.Expr.AllocTuple(info, ty, tag, xs) ->
            let rec loop acc = function
                | [] -> AllocTuple(info, ty, tag, List.rev acc)
                | x :: xs -> lift_var (convert x)
                                        (fun v -> loop (v :: acc) xs)
            in
            loop [] xs
        | LambdaLift.Expr.GetField(info, ty, num, x) ->
            lift_var (convert x) (fun v -> GetField(info, ty, num, v))
        | LambdaLift.Expr.Case(info, ty, n, opts) ->
            let opts = List.map (fun (tag, x) -> tag, convert x) opts in
            Case(info, ty, n, opts)
        | LambdaLift.Expr.Label(info, ty, x, label, bindings, y) ->
            Label(info, ty, (convert x), label, bindings, (convert y))
        | LambdaLift.Expr.Goto(info, label, bindings) ->
            let rec loop bindings = function
                | [] -> Goto(info, label, bindings)
                | (k, x) :: xs -> lift_var (convert x)
                                    (fun v -> loop
                                                (Common.Var.Map.add k v
                                                                    bindings)
                                                xs)
            in
            loop Common.Var.Map.empty (Common.Var.Map.bindings bindings)
        | LambdaLift.Expr.BinOp(info, ty, x, op, y) ->
            BinOp(info, ty, (convert x), op, (convert y))
        | LambdaLift.Expr.UnOp(info, ty, op, x) ->
            UnOp(info, ty, op, (convert x))
        | LambdaLift.Expr.Apply(info, ty, f, x) ->
            let rec collapse xs = function
                | LambdaLift.Expr.Apply(i, t, g, y) ->
                    collapse ((i, t, y) :: xs) g
                | g -> g, xs
            in
            let f, xs = collapse [ (info, ty, x) ] f in
            let rec loop2 short f xs =
                let n = if short then
                            Config.max_args - 1
                        else
                            Config.max_args
                in
                if ((List.length xs) > n) then
                    let ys, zs = Utils.take_drop n xs in
                    let (i, t, _) = Utils.last ys in
                    let ys = List.map (fun (_, _, y) -> y) ys in
                    lift_var (Apply(i, t, f, ys))
                        (fun h -> loop2 true h zs)
                else
                    let (i, t, _) = Utils.last xs in
                    let xs = List.map (fun (_, _, y) -> y) xs in
                    Apply(i, t, f, xs)
            in

            let rec loop1 xs = function
                | [] ->
                    let xs = List.rev xs in
                    lift_var (convert f)
                        (fun g -> loop2 false g xs)
                | (i, t, y) :: ys ->
                    lift_var (convert y)
                        (fun y -> loop1 ((i, t, y) :: xs) ys)
            in
            loop1 [] xs
        | LambdaLift.Expr.Var(info, ty, v) ->
            Var(info, ty, v)
        | LambdaLift.Expr.Const(info, ty, c) ->
            Const(info, ty, c)
        | LambdaLift.Expr.CallExtern(info, ty, xtern, xs) ->
            CallExtern(info, ty, xtern, xs)
    ;;


end;;

type t =
    | TopFun of Info.t * Common.VarType.t * Common.Var.t
                    * Common.Arg.t list * Expr.t
    | TopVar of Info.t * Common.VarType.t * Common.Var.t * Expr.t
    | TopForward of Info.t * Common.VarType.t * Common.Var.t * int
    | TopExpr of Info.t * Common.VarType.t * Expr.t
    with sexp
;;

let convert = function
    | LambdaLift.TopFun(info, ty, v, args, x) ->
        TopFun(info, ty, v, args, Expr.convert x)
    | LambdaLift.TopVar(info, ty, v, x) ->
        TopVar(info, ty, v, Expr.convert x)
    | LambdaLift.TopForward(info, ty, n, nargs) ->
        TopForward(info, ty, n, nargs)
    | LambdaLift.TopExpr(info, ty, x) ->
        TopExpr(info, ty, Expr.convert x)
;;

module C : IL.Conversion with type input = LambdaLift.t
                                and type output = t =
struct
    type input = LambdaLift.t;;
    type output = t;;
    type state = unit;;

    let name = "simplify";;
    let sexp_of_output x = sexp_of_t x;;
    let dump_flag = ref false;;
    let check_flag = ref false;;
    let init_state () = ();;
    let convert _ input = (), [ convert input ];;
    let check _ = true;;
    let get_info _ = assert false;;
    let fini_state _ = ();;
end;;

module Convert : IL.Converter with type output = t =
    IL.Make(LambdaLift.Convert)(C);;


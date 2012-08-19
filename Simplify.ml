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
        | AllocTuple of Common.Tag.t * (Common.VarType.t * Common.Var.t) list
        | GetField of int * (Common.VarType.t * Common.Var.t)
        | Case of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of (Common.VarType.t * Common.Var.t)
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

    let lift_var v f =
        match v.body with
        | Var(n) -> f v.info v.typ n
        | _ ->
            let name = Common.Var.generate () in
            let r = f v.info v.typ name in
            { info = v.info; typ = r.typ;
                body = Let((v.typ, Some name), v, r) }
    ;;

    let make_var i t n = { info = i; typ = t; body = Var(n) };;

    let rec convert expr =
        let info = expr.LambdaLift.Expr.info in
        let typ = expr.LambdaLift.Expr.typ in
        let body =
            match expr.LambdaLift.Expr.body with
            | LambdaLift.Expr.Let(arg, x, y) ->
                Let(arg, (convert x), (convert y))
            | LambdaLift.Expr.If(x, y, z) ->
                If((convert x), (convert y), (convert z))
            | LambdaLift.Expr.AllocTuple(tag, xs) ->
                let rec loop acc = function
                    | [] -> { info; typ; body = AllocTuple(tag, List.rev acc) }
                    | x :: xs -> lift_var (convert x)
                                            (fun _ t v ->
                                                loop ((t, v) :: acc) xs)
                in
                (loop [] xs).body
            | LambdaLift.Expr.GetField(num, x) ->
                (lift_var (convert x)
                    (fun i t v ->
                        { info; typ;
                            body = GetField(num, (t, v)) })).body
            | LambdaLift.Expr.Case(n, opts) ->
                let opts = List.map (fun (tag, x) -> tag, convert x) opts in
                Case(n, opts)
            | LambdaLift.Expr.Label(x, label, bindings, y) ->
                Label((convert x), label, bindings, (convert y))
            | LambdaLift.Expr.Goto(label, bindings) ->
                let rec loop bindings = function
                    | [] -> { info; typ; body = Goto(label, bindings) }
                    | (k, x) :: xs
                        -> lift_var
                                (convert x)
                                (fun i t v ->
                                    loop
                                        (Common.Var.Map.add k
                                            (t, v)
                                            bindings)
                                        xs)
                in
                let r = loop Common.Var.Map.empty
                                    (Common.Var.Map.bindings bindings)
                in
                r.body
            | LambdaLift.Expr.BinOp(x, op, y) ->
                BinOp((convert x), op, (convert y))
            | LambdaLift.Expr.UnOp(op, x) ->
                UnOp(op, (convert x))
            | LambdaLift.Expr.Apply(f, x) ->
                let rec collapse xs expr =
                    match expr.LambdaLift.Expr.body with
                    | LambdaLift.Expr.Apply(g, y) ->
                        collapse (y :: xs) g
                    | _ -> expr, xs
                in
                let f, xs = collapse [ x ] f in
                let rec loop2 short f xs =
                    let n = if short then
                                Config.max_args - 1
                            else
                                Config.max_args
                    in
                    if ((List.length xs) > n) then
                        let ys, zs = Utils.take_drop n xs in
                        let z = Utils.last ys in
                        let t = (fst z) in
                        lift_var { info; typ = t; body = Apply(f, ys) }
                            (fun i t h -> loop2 true (t,h) zs)
                    else
                        let z = Utils.last xs in
                        let t = (fst z) in
                        { info; typ = t; body = Apply(f, xs) }
                in
    
                let rec loop1 xs = function
                    | [] ->
                        let xs = List.rev xs in
                        lift_var (convert f)
                            (fun i t g -> loop2 false 
                                (t, g)
                                xs)
                    | y :: ys ->
                        lift_var (convert y)
                            (fun i t y -> loop1 ((t, y) :: xs) ys)
                in
                (loop1 [] xs).body
            | LambdaLift.Expr.Var(v) ->
                Var(v)
            | LambdaLift.Expr.Const(c) ->
                Const(c)
            | LambdaLift.Expr.CallExtern(xtern, xs) ->
                CallExtern(xtern, xs)
        in
        { info; typ; body }
    ;;

end;;

type s =
    | TopFun of Common.Var.t * Common.Arg.t list * Expr.t
    | TopVar of Common.Var.t * Expr.t
    | TopForward of Common.Var.t * int
    | TopExpr of Expr.t
and t = {
    info : Info.t;
    typ : Common.VarType.t;
    body : s;
} with sexp;;

let convert top =
    let info = top.LambdaLift.info in
    let typ = top.LambdaLift.typ in
    let body =
        match top.LambdaLift.body with
        | LambdaLift.TopFun(v, args, x) ->
            TopFun(v, args, Expr.convert x)
        | LambdaLift.TopVar(v, x) ->
            TopVar(v, Expr.convert x)
        | LambdaLift.TopForward(n, nargs) ->
            TopForward(n, nargs)
        | LambdaLift.TopExpr(x) ->
            TopExpr(Expr.convert x)
    in
    { info; typ; body }
;;

module C : IL.Conversion with type input = LambdaLift.t
                                and type output = t =
struct
    type input = LambdaLift.t;;
    type output = t;;
    type state = unit;;
    type check_state = unit;;

    let name = "simplify";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = ();;
    let convert _ input = (), [ convert input ];;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _= (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert : IL.Converter with type output = t =
    IL.Make(LambdaLift.Convert)(C);;


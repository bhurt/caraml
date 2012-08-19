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

let rec flatten_lambdas args body = 
    match body.MatchReduce.Expr.body with
    | MatchReduce.Expr.Lambda(args2, y) ->
        flatten_lambdas (List.append args args2) y
    | _ -> args, body
;;

module rec Lambda: sig

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: Common.Arg.t list;
        body: Expr.t;
    } with sexp;;

    val make : Info.t -> Common.VarType.t -> Common.Var.t
                    -> Common.Arg.t list -> Expr.t -> t;;

    val convert : MatchReduce.Lambda.t -> t;;

end = struct

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: Common.Arg.t list;
        body: Expr.t;
    } with sexp;;

    let make info typ name args body = {
        info = info;
        typ = typ;
        name = name;
        args = args;
        body = body;
    };;


    let convert lambda =
        let args, x = flatten_lambdas lambda.MatchReduce.Lambda.args
                                        lambda.MatchReduce.Lambda.body
        in
        let x = Expr.convert x in
        make lambda.MatchReduce.Lambda.info
                lambda.MatchReduce.Lambda.typ
                lambda.MatchReduce.Lambda.name
                args
                x
    ;;

end and Expr : sig

    type s =
        | Let of Common.Arg.t * t * t
        | LetFn of Lambda.t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | GetField of int * t
        | Case of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t * (t Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val make : Info.t -> Common.VarType.t -> s -> t;;

    val convert : MatchReduce.Expr.t -> t;;

end = struct

    type s =
        | Let of Common.Arg.t * t * t
        | LetFn of Lambda.t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | GetField of int * t
        | Case of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t * (t Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;


    let make info typ body = {
        info = info;
        typ = typ;
        body = body;
    };;

    let rec convert expr =
        let info = expr.MatchReduce.Expr.info in
        let ty = expr.MatchReduce.Expr.typ in
        let body = match expr.MatchReduce.Expr.body with
            | MatchReduce.Expr.Lambda(args, x) ->
                let args, y = flatten_lambdas args x in
                let name = Common.Var.generate () in
                let y = convert y in
                LetFn((Lambda.make info ty name args y),
                        make info ty (Var(name)))

            | MatchReduce.Expr.Let((v_typ, Some name) as n, x, y) ->
                begin
                    let y = convert y in
                    match x.MatchReduce.Expr.body with
                    | MatchReduce.Expr.Lambda(args, z) ->
                        let args, z = flatten_lambdas args z in
                        let z = convert z in
                        LetFn((Lambda.make x.MatchReduce.Expr.info
                                    v_typ name args z), y)
                    | _ ->
                        let x = convert x in
                        Let(n, x, y)
                end

            | MatchReduce.Expr.Let(n, x, y) ->
                let x = convert x in
                let y = convert y in
                Let(n, x, y)
                
            | MatchReduce.Expr.LetRec(fns, x) ->
                let fns = List.map Lambda.convert fns in
                let x = convert x in
                LetRec(fns, x)

            | MatchReduce.Expr.If(x, y, z) ->
                let x = convert x in
                let y = convert y in
                let z = convert z in
                If(x, y, z)

            | MatchReduce.Expr.AllocTuple(tag, xs) ->
                let xs = List.map convert xs in
                AllocTuple(tag, xs)

            | MatchReduce.Expr.GetField(num, x) ->
                let x = convert x in
                GetField(num, x)

            | MatchReduce.Expr.Case(n, opts) ->
                let opts = List.map (fun (tag, x) -> tag, convert x) opts in
                Case(n, opts)

            | MatchReduce.Expr.Label(x, label, bindings, y) ->
                let x = convert x in
                let y = convert y in
                Label(x, label, bindings, y)

            | MatchReduce.Expr.Goto(label, bindings) ->
                let bindings = Common.Var.Map.map convert bindings in
                Goto(label, bindings)

            | MatchReduce.Expr.BinOp(x, op, y) ->
                let x = convert x in
                let y = convert y in
                BinOp(x, op, y)
            | MatchReduce.Expr.UnOp(op, x) ->
                let x = convert x in
                UnOp(op, x)
            | MatchReduce.Expr.Apply(x, y) ->
                let x = convert x in
                let y = convert y in
                Apply(x, y)
            | MatchReduce.Expr.Var(v) ->
                Var(v)
            | MatchReduce.Expr.Const(c) ->
                Const(c)
        in
        {   info = info;
            typ = ty;
            body = body;
        }
    ;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopFn of Common.VarType.t * Lambda.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
and t = {
    info : Info.t;
    body: s;
} with sexp;;

let convert top = 
    let info = top.MatchReduce.info in
    let body = match top.MatchReduce.body with
        | MatchReduce.Top(ty, name, x) ->
            begin
                match x.MatchReduce.Expr.body with
                | MatchReduce.Expr.Lambda(args, body) ->
                    let args, body = flatten_lambdas args body  in
                    let body = Expr.convert body in
                    let name = match name with
                                    | Some n -> n
                                    | None -> Common.Var.generate ()
                    in
                    TopFn(ty, Lambda.make info ty name args body)
                | _ ->
                    let x = Expr.convert x in
                    Top(ty, name, x)
            end
        | MatchReduce.TopRec(fns) ->
            let fns = List.map Lambda.convert fns in
            TopRec(fns)
        | MatchReduce.Extern(name, xtrn) -> Extern(name, xtrn)
    in {
        info = info;
        body = body;
    }
;;


module C : IL.Conversion with type input = MatchReduce.t and type output = t =
struct
    type input = MatchReduce.t;;
    type output = t;;
    type state = unit;;
    type check_state = unit;;

    let name = "lambda-conv";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = ();;
    let convert _ input = (), [ convert input ];;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert : IL.Converter with type output = t =
    IL.Make(MatchReduce.Convert)(C);;


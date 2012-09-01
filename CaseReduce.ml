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

module rec Lambda : sig

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;

    val convert : Common.Tag.t Common.Var.Map.t -> Alpha.Lambda.t -> t;;

end = struct

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;

    let convert tagmap x = {
        info = x.Alpha.Lambda.info;
        typ = x.Alpha.Lambda.typ;
        name = x.Alpha.Lambda.name;
        args = x.Alpha.Lambda.args;
        body = Expr.convert tagmap x.Alpha.Lambda.body;
    };;

end and Expr : sig

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
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

    val convert : Common.Tag.t Common.Var.Map.t -> Alpha.Expr.t -> t;;

end = struct

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
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


    let load_args info ty src body args =
        Utils.fold_righti
            (fun i (ty', v) z ->
                match v with
                | Some(n) ->
                    {   info = info;
                        typ = ty;
                        body = Let((ty', v),
                                    {   info = info;
                                        typ = ty';
                                        body = GetField(i, src) },
                                    z) }
                | None -> z)
            args
            body
    ;;

    let rec convert tagmap x =
        let info = x.Alpha.Expr.info in
        let typ = x.Alpha.Expr.typ in

        let body = match x.Alpha.Expr.body with
            | Alpha.Expr.Lambda(args, x) ->
                let x = convert tagmap x in
                Lambda(args, x)
            | Alpha.Expr.Let(arg, x, y) ->
                let x = convert tagmap x in
                let y = convert tagmap y in
                Let(arg, x, y)
            | Alpha.Expr.LetTuple(args, x, y) ->
                let name = Common.Var.generate () in
                let tuple_ty = Type.Tuple(List.map fst args) in
                let var = { info = info;
                            typ = tuple_ty;
                            body = Var(name) }
                in
                Let((tuple_ty, Some name),
                        (convert tagmap x),
                        (load_args info typ var (convert tagmap y) args))
            | Alpha.Expr.LetRec(fns, x) ->
                let fns = List.map (Lambda.convert tagmap) fns in
                let x = convert tagmap x in
                LetRec(fns, x)
            | Alpha.Expr.If(x, y, z) ->
                let x = convert tagmap x in
                let y = convert tagmap y in
                let z = convert tagmap z in
                If(x, y, z)
            | Alpha.Expr.Match(x, bindings) ->
                let name = Common.Var.generate () in
                let var = { info = info;
                            typ = x.Alpha.Expr.typ;
                            body = Var(name) }
                in
                let compile_match (pat, x) =
                    match pat.Alpha.Pattern.body with
                    | Alpha.Pattern.Pattern(name, args) ->
                        let tag = Common.Var.Map.find name tagmap in
                        let x = convert tagmap x in
                        tag, (load_args pat.Alpha.Pattern.info typ var x args)
                in

                Let((x.Alpha.Expr.typ, Some name), (convert tagmap x),
                    {   info = info;
                        typ = typ;
                        body = Case((x.Alpha.Expr.typ, name),
                                List.map compile_match bindings) })
            | Alpha.Expr.Tuple(xs) ->
                let xs = List.map (convert tagmap) xs in
                AllocTuple((Common.Tag.of_int 0), xs)
            | Alpha.Expr.BinOp(x, op, y) ->
                let x = convert tagmap x in
                let y = convert tagmap y in
                BinOp(x, op, y)
            | Alpha.Expr.UnOp(op, x) ->
                let x = convert tagmap x in
                UnOp(op, x)
            | Alpha.Expr.Apply(x, y) ->
                let x = convert tagmap x in
                let y = convert tagmap y in
                Apply(x, y)
            | Alpha.Expr.Var(v) -> Var(v)
            | Alpha.Expr.Const(c) -> Const(c)
    in
    {
        info = info;
        typ = typ;
        body = body;
    }
    ;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
and t = {
    info: Info.t;
    body: s;
} with sexp;;

let convert tagmap x =
    let inner_convert info = function
        | Alpha.Top(ty, v, x) ->
            let x = Expr.convert tagmap x in
            tagmap, [ Top(ty, v, x) ]
        | Alpha.TopRec(fns) ->
            let fns = List.map (Lambda.convert tagmap) fns in
            tagmap, [ TopRec(fns) ]
        | Alpha.Extern(name, xt) ->
            tagmap, [ Extern(name, xt) ]
        | Alpha.VariantDef(name, opts) ->
            let tagmap =
                Utils.fold_lefti
                    (fun i tagmap (_, n, _) ->
                        Common.Var.Map.add n (Common.Tag.of_int i) tagmap)
                    tagmap
                    opts
            in
            let rtype = Type.Named(name) in
            let fns =
                List.map
                    (fun (info, n, tys) ->
                        let names =
                            List.map (fun _ -> Common.Var.generate ()) tys
                        in
                        let fn_type = Type.fn_type tys rtype in
                        let args = List.map2 (fun ty n -> ty, Some n)
                                                tys names
                        in
                        let tag = Common.Var.Map.find n tagmap in
                        let body =
                            {   Expr.info = info;
                                Expr.typ = rtype;
                                Expr.body =
                                    Expr.AllocTuple(tag,
                                        List.map2
                                            (fun ty n -> {
                                                Expr.info = info;
                                                Expr.typ = ty;
                                                Expr.body = Expr.Var(n) })
                                            tys
                                            names) }
                        in
                        Top(fn_type, Some n,
                                {   Expr.info = info;
                                    Expr.typ = fn_type;
                                    Expr.body =
                                        Expr.Lambda(args, body) }))
                    opts
            in
            tagmap, fns
    in
    let tagmap, bodies = inner_convert x.Alpha.info x.Alpha.body in
    tagmap, List.map (fun b -> { info = x.Alpha.info; body = b }) bodies
    ;;

module C : IL.Conversion with type input = Alpha.t and type output = t =
struct
    type input = Alpha.t;;
    type output = t;;
    type state = Common.Tag.t Common.Var.Map.t;;
    type check_state = unit;;

    let name = "case-reduce";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = Common.Var.Map.empty;;
    let convert state input = convert state input;;
    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert: IL.Converter with type output = t
    = IL.Make(Alpha.Convert)(C);;


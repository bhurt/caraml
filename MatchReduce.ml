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
        | Lambda of Info.t * Common.VarType.t * Common.Arg.t list * t
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | LetRec of Info.t * Common.VarType.t * (lambda list) * t
        | If of Info.t * Common.VarType.t * t * t * t
        | AllocTuple of Info.t * Common.VarType.t * Common.Tag.t * (t list)
        | GetField of Info.t * Common.VarType.t * int * t
        | Case of Info.t * Common.VarType.t
                    * (Common.VarType.t * Common.Var.t)
                    * ((Common.Tag.t * t) list)
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | Apply of Info.t * Common.VarType.t * t * t
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        with sexp
    ;;

    let load_args info ty src body args =
        Utils.fold_righti
            (fun i (ty', v) z ->
                match v with
                | Some(n) ->
                    Let(info, ty, (ty', v),
                            GetField(info, ty', i, src), z)
                | None -> z)
            args
            body
    ;;

    let get_type = function
        | Alpha.Expr.Lambda(_, ty, _, _)
        | Alpha.Expr.Let(_, ty, _, _, _)
        | Alpha.Expr.LetTuple(_, ty, _, _, _)
        | Alpha.Expr.LetRec(_, ty, _, _)
        | Alpha.Expr.If(_, ty, _, _, _)
        | Alpha.Expr.Match(_, ty, _, _)
        | Alpha.Expr.Tuple(_, ty, _)
        | Alpha.Expr.BinOp(_, ty, _, _, _)
        | Alpha.Expr.UnOp(_, ty, _, _)
        | Alpha.Expr.Apply(_, ty, _, _)
        | Alpha.Expr.Var(_, ty, _)
        | Alpha.Expr.Const(_, ty, _)
        ->ty
    ;;

    let rec convert tagmap = function
        | Alpha.Expr.Lambda(info, ty, args, x) ->
            let x = convert tagmap x in
            Lambda(info, ty, args, x)
        | Alpha.Expr.Let(info, ty, arg, x, y) ->
            let x = convert tagmap x in
            let y = convert tagmap y in
            Let(info, ty, arg, x, y)
        | Alpha.Expr.LetTuple(info, ty, args, x, y) ->
            let name = Common.Var.generate () in
            let tuple_ty = Type.Tuple(List.map fst args) in
            let var = Var(info, tuple_ty, name) in
            Let(info, ty,
                    (tuple_ty, Some name),
                    (convert tagmap x),
                    (load_args info ty var (convert tagmap y) args))
        | Alpha.Expr.LetRec(info, ty, fns, x) ->
            let fns = List.map
                            (fun (i, ty, n, args, x) ->
                                (i, ty, n, args, convert tagmap x))
                            fns
            in
            let x = convert tagmap x in
            LetRec(info, ty, fns, x)
        | Alpha.Expr.If(info, ty, x, y, z) ->
            let x = convert tagmap x in
            let y = convert tagmap y in
            let z = convert tagmap z in
            If(info, ty, x, y, z)
        | Alpha.Expr.Match(info, ty, x, bindings) ->
            let x_ty = get_type x in
            let name = Common.Var.generate () in
            let var = Var(info, x_ty, name) in
            Let(info, ty,
                    (x_ty, Some name),
                    (convert tagmap x),
                    Case(info, ty, (x_ty, name), 
                            List.map
                                (fun (info, name, args, x) ->
                                    let tag =
                                        Common.Var.Map.find name tagmap
                                    in
                                    let x = convert tagmap x in
                                    tag, (load_args info ty var x args))
                                bindings))
        | Alpha.Expr.Tuple(info, ty, xs) ->
            let xs = List.map (convert tagmap) xs in
            AllocTuple(info, ty, (Common.Tag.of_int 0), xs)
        | Alpha.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert tagmap x in
            let y = convert tagmap y in
            BinOp(info, ty, x, op, y)
        | Alpha.Expr.UnOp(info, ty, op, x) ->
            let x = convert tagmap x in
            UnOp(info, ty, op, x)
        | Alpha.Expr.Apply(info, ty, x, y) ->
            let x = convert tagmap x in
            let y = convert tagmap y in
            Apply(info, ty, x, y)
        | Alpha.Expr.Var(info, ty, v) -> Var(info, ty, v)
        | Alpha.Expr.Const(info, ty, c) -> Const(info, ty, c)

end;;

type t =
    | Top of Info.t * Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.Var.t Common.External.t
    with sexp
;;

let convert tagmap = function
    | Alpha.Top(info, ty, v, x) ->
        let x = Expr.convert tagmap x in
        tagmap, [ Top(info, ty, v, x) ]
    | Alpha.TopRec(info, fns) ->
        let fns = List.map
                    (fun (i, ty, n, args, x) ->
                        (i, ty, n, args, Expr.convert tagmap x))
                    fns
        in
        tagmap, [ TopRec(info, fns) ]
    | Alpha.Extern(info, name, xt) ->
        tagmap, [ Extern(info, name, xt) ]
    | Alpha.VariantDef(info, name, opts) ->
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
                    let args = List.map2 (fun ty n -> ty, Some n) tys names in
                    let tag = Common.Var.Map.find n tagmap in
                    let body =
                        Expr.AllocTuple(info, rtype, tag,
                                            List.map2
                                                (fun ty n ->
                                                    Expr.Var(info, ty, n))
                                                tys
                                                names)
                    in
                    Top(info, fn_type, Some n,
                            (Expr.Lambda(info, fn_type, args, body))))
                opts
        in
        tagmap, fns
    ;;


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

module StringMap = Map.Make(String);;

let rec convert_type names = function
    | Type.Arrow(x, y) ->
        let x = convert_type names x in
        let y = convert_type names y in
        Type.Arrow(x, y)
    | Type.Tuple(xs) ->
        let xs = List.map (convert_type names) xs in
        Type.Tuple(xs)
    | Type.Named(n) ->
        let n = StringMap.find (Annot.CheckedString.to_string n) names in
        Type.Named(n)
    | Type.Base(b) -> Type.Base(b)
;;

module Expr = struct

    type lambda = Info.t * Common.VarType.t * Common.Var.t
                                            * (Common.Arg.t list) * t
    and t =
        | Lambda of Info.t * Common.VarType.t * Common.Arg.t list * t
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Common.VarType.t * Common.Arg.t list * t * t
        | LetRec of Info.t * Common.VarType.t * (lambda list) * t
        | If of Info.t * Common.VarType.t * t * t * t
        | Match of Info.t * Common.VarType.t * t *
                    ((Info.t * Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of Info.t * Common.VarType.t * t list
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | Apply of Info.t * Common.VarType.t * t * t
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        with sexp
    ;;

    let rename_arg names = function
        | (_, None) -> names
        | (_, Some name) ->
            let v = Common.Var.of_string name in
            StringMap.add name v names
    ;;

    let map_arg names = function
        | (ty, None) ->
            let ty = convert_type names ty in
            (ty, None)
        | (ty, Some name) ->
            let ty = convert_type names ty in
            let v = StringMap.find name names in
            (ty, Some v)
    ;;

    let rec convert names = function

        | Annot.Expr.Lambda(info, ty, args, x) ->
            let ty = convert_type names ty in
            let names = List.fold_left rename_arg names args in
            let args = List.map (map_arg names) args in
            let x = convert names x in
            Lambda(info, ty, args, x)

        | Annot.Expr.Let(info, ty, arg, x, y) ->
            let ty = convert_type names ty in
            let x = convert names x in
            let names = rename_arg names arg in
            let arg = map_arg names arg in
            let y = convert names y in
            Let(info, ty, arg, x, y)

        | Annot.Expr.LetTuple(info, ty, args, x, y) ->
            let ty = convert_type names ty in
            let x = convert names x in
            let names = List.fold_left rename_arg names args in
            let args = List.map (map_arg names) args in
            let y = convert names y in
            LetTuple(info, ty, args, x, y)

        | Annot.Expr.LetRec(info, ty, fns, x) ->
            let ty = convert_type names ty in
            let names, fns = convert_lambdas names fns in
            let x = convert names x in
            LetRec(info, ty, fns, x)

        | Annot.Expr.If(info, ty, x, y, z) ->
            let ty = convert_type names ty in
            let x = convert names x in
            let y = convert names y in
            let z = convert names z in
            If(info, ty, x, y, z)

        | Annot.Expr.Match(info, ty, x, bindings) ->
            let ty = convert_type names ty in
            let x = convert names x in
            let bindings =
                List.map
                    (fun ((Annot.Pattern.Pattern(info, name, args)), y) ->
                        let name = StringMap.find name names in
                        let names = List.fold_left rename_arg names args in
                        let args = List.map (map_arg names) args in
                        let y = convert names y in
                        (info, name, args, y))
                    bindings
            in
            Match(info, ty, x, bindings)

        | Annot.Expr.Tuple(info, ty, xs) ->
            let ty = convert_type names ty in
            let xs = List.map (convert names) xs in
            Tuple(info, ty, xs)

        | Annot.Expr.BinOp(info, ty, x, op, y) ->
            let ty = convert_type names ty in
            let x = convert names x in
            let y = convert names y in
            BinOp(info, ty, x, op, y)

        | Annot.Expr.UnOp(info, ty, op, x) ->
            let ty = convert_type names ty in
            let x = convert names x in
            UnOp(info, ty, op, x)

        | Annot.Expr.Apply(info, ty, f, x) ->
            let ty = convert_type names ty in
            let f = convert names f in
            let x = convert names x in
            Apply(info, ty, f, x)

        | Annot.Expr.Var(info, ty, v) ->
            let ty = convert_type names ty in
            Var(info, ty, StringMap.find v names)

        | Annot.Expr.Const(info, ty, c) ->
            let ty = convert_type names ty in
            Const(info, ty, c)

    and convert_lambdas names fns =
        let names =
            List.fold_left
                (fun names (_, _, n, _, _) ->
                    let v = Common.Var.of_string n in
                    StringMap.add n v names)
                names
                fns
        in
        let fns =
            List.map
                (fun (i, ty, n, args, x) ->
                    let ty = convert_type names ty in
                    let n = StringMap.find n names in
                    let names = List.fold_left rename_arg names args in
                    let args = List.map (map_arg names) args in
                    let x = convert names x in
                    (i, ty, n, args, x))
                fns
        in
        names, fns
    ;;
        
end;;

type t =
    | Top of Info.t * Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.Var.t Common.External.t
    | VariantDef of Info.t * Common.Var.t
                * ((Info.t * Common.Var.t * (Common.VarType.t list)) list)
    with sexp
;;

let convert names = function
    | Annot.Top(info, ty, None, x) ->
        let ty = convert_type names ty in
        let x = Expr.convert names x in
        names, (Top(info, ty, None, x))
    | Annot.Top(info, ty, Some v, x) ->
        let ty = convert_type names ty in
        let name = Common.Var.of_string v in
        let x = Expr.convert names x in
        (StringMap.add v name names), (Top(info, ty, Some name, x))
    | Annot.TopRec(info, fns) ->
        let names, fns = Expr.convert_lambdas names fns in
        names, TopRec(info, fns)
    | Annot.Extern(info, v, x) ->
        let name = Common.Var.of_string v in
        let x = { x with
                    Common.External.return_type =
                        convert_type names x.Common.External.return_type;
                    Common.External.arg_types =
                        List.map (convert_type names)
                            x.Common.External.arg_types }
        in
        (StringMap.add v name names), (Extern(info, name, x))
    | Annot.VariantDef(info, v, opts) ->
        let name = Common.Var.of_string v in
        let names = StringMap.add v name names in
        let opts = List.map
                    (fun (i, n, ts) ->
                        let n' = Common.Var.of_string n in
                        let ts = List.map (convert_type names) ts in
                        (i, n', ts))
                    opts
        in
        names, (VariantDef(info, name, opts))
;;

module C : IL.Conversion with type input = Annot.t and type output = t
= struct
    type input = Annot.t;;
    type output = t;;
    type state = Common.Var.t StringMap.t;;
    let name = "alpha";;
    let sexp_of_output x = sexp_of_t x;;
    let dump_flag = ref false;;
    let init_state () = StringMap.empty;;
    let convert state input =
        let state, output = convert state input in
        state, [ output ]
    ;;
    let fini_state _ = ();;
end;;

module Convert : IL.Converter with type output = t
        = IL.Make(Annot.Convert)(C);;

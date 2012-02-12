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


module StringMap = Map.Make(String);;

module Expr = struct

    type t =
        | Lambda of Info.t * Type.t * Common.Arg.t list * t
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * t * t
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let rename_arg names = function
        | (_, None) -> names
        | (_, Some name) ->
            let v = Common.Var.of_string name in
            StringMap.add name v names
    ;;

    let map_arg names = function
        | (ty, None) -> (ty, None)
        | (ty, Some name) ->
            let v = StringMap.find name names in
            (ty, Some v)
    ;;

    let rec convert names = function

        | Annot.Expr.Lambda(info, ty, args, x) ->
            let names = List.fold_left rename_arg names args in
            let args = List.map (map_arg names) args in
            let x = convert names x in
            Lambda(info, ty, args, x)

        | Annot.Expr.Let(info, ty, arg, x, y) ->
            let x = convert names x in
            let names = rename_arg names arg in
            let arg = map_arg names arg in
            let y = convert names y in
            Let(info, ty, arg, x, y)

        | Annot.Expr.LetTuple(info, ty, args, x, y) ->
            let x = convert names x in
            let names = List.fold_left rename_arg names args in
            let args = List.map (map_arg names) args in
            let y = convert names y in
            LetTuple(info, ty, args, x, y)

        | Annot.Expr.If(info, ty, x, y, z) ->
            let x = convert names x in
            let y = convert names y in
            let z = convert names z in
            If(info, ty, x, y, z)

        | Annot.Expr.Tuple(info, ty, xs) ->
            let xs = List.map (convert names) xs in
            Tuple(info, ty, xs)

        | Annot.Expr.BinOp(info, ty, x, op, y) ->
            let x = convert names x in
            let y = convert names y in
            BinOp(info, ty, x, op, y)

        | Annot.Expr.UnOp(info, ty, op, x) ->
            let x = convert names x in
            UnOp(info, ty, op, x)

        | Annot.Expr.Apply(info, ty, f, x) ->
            let f = convert names f in
            let x = convert names x in
            Apply(info, ty, f, x)

        | Annot.Expr.Var(info, ty, v) ->
            Var(info, ty, StringMap.find v names)

        | Annot.Expr.Const(info, ty, c) ->
            Const(info, ty, c)
    ;;
        
end;;

type t =
    | Top of Info.t * Type.t * Common.Var.t option * Expr.t with sexp
;;

let convert names = function
    | Annot.Top(info, ty, None, x) ->
        let x = Expr.convert names x in
        names, (Top(info, ty, None, x))
    | Annot.Top(info, ty, Some v, x) ->
        let name = Common.Var.of_string v in
        let x = Expr.convert names x in
        (StringMap.add v name names), (Top(info, ty, Some name, x))
;;


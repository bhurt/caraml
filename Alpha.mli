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

module StringMap : Map.S with type key = string;;

module Expr : sig

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

end;;

type t =
    | Top of Info.t * Type.t * Common.Var.t option * Expr.t
    | TopRec of Info.t * ((Info.t * Type.t * Common.Var.t
                            * (Common.Arg.t list) * Expr.t) list)
    | Extern of Info.t * Common.Var.t * Common.External.t
    with sexp
;;

val convert :
    Common.Var.t StringMap.t -> Annot.t -> (Common.Var.t StringMap.t * t)
;;


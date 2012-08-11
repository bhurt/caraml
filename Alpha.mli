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

module Expr : sig

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

end;;

type t =
    | Top of Info.t * Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.Var.t Common.External.t
    | VariantDef of Info.t * Common.Var.t
                * ((Info.t * Common.Var.t * (Common.VarType.t list)) list)
    with sexp
;;

module Convert: IL.Converter with type output = t;;


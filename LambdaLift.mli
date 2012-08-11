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

    type t =
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | If of Info.t * Common.VarType.t * t * t * t
        | AllocTuple of Info.t * Common.VarType.t * Common.Tag.t * (t list)
        | GetField of Info.t * Common.VarType.t * int * t
        | Case of Info.t * Common.VarType.t
                        * (Common.VarType.t * Common.Var.t)
                        * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t * (t Common.Var.Map.t)
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | Apply of Info.t * Common.VarType.t * t * t
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        | CallExtern of Info.t * Common.VarType.t
                    * Common.Var.t Common.External.t
                    * ((Common.VarType.t * Common.Var.t) list)
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

module Convert : IL.Converter with type output = t;;


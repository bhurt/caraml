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

type type_t = Common.Var.t Type.t with sexp;;

type tag_t = int with sexp;;

module Expr : sig

    type t =
        | Let of Info.t * type_t * Common.Arg.t * t * t
        | If of Info.t * type_t * t * t * t
        | AllocTuple of Info.t * type_t * tag_t * (type_t * Common.Var.t) list
        | GetField of Info.t * type_t * int * (type_t * Common.Var.t)
        | Case of Info.t * type_t * (type_t * Common.Var.t)
                    * ((tag_t * t) list)
        | BinOp of Info.t * type_t * t * Common.BinOp.t * t
        | UnOp of Info.t * type_t * Common.UnOp.t * t
        | Apply of Info.t * type_t * (type_t * Common.Var.t)
                                        * ((type_t * Common.Var.t) list)
        | Var of Info.t * type_t * Common.Var.t
        | Const of Info.t * type_t * Common.Const.t
        | CallExtern of Info.t * type_t * Common.Var.t Common.External.t
                            * ((type_t * Common.Var.t) list)
        with sexp
    ;;

end;;

type t =
    | TopFun of Info.t * type_t * Common.Var.t * Common.Arg.t list * Expr.t
    | TopVar of Info.t * type_t * Common.Var.t * Expr.t
    | TopForward of Info.t * type_t * Common.Var.t * int
    | TopExpr of Info.t * type_t * Expr.t
    with sexp
;;

val convert : LambdaLift.t -> t;;





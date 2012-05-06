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

    type lambda = Info.t * type_t * Common.Var.t
                            * (Common.Arg.t list) * t
    and t =
        | Lambda of Info.t * type_t * Common.Arg.t list * t
        | Let of Info.t * type_t * Common.Arg.t * t * t
        | LetRec of Info.t * type_t * (lambda list) * t
        | If of Info.t * type_t * t * t * t
        | AllocTuple of Info.t * type_t * tag_t * (t list)
        | GetField of Info.t * type_t * int * t
        | Case of Info.t * type_t * (type_t * Common.Var.t)
                                                * ((tag_t * t) list)
        | BinOp of Info.t * type_t * t * Common.BinOp.t * t
        | UnOp of Info.t * type_t * Common.UnOp.t * t
        | Apply of Info.t * type_t * t * t
        | Var of Info.t * type_t * Common.Var.t
        | Const of Info.t * type_t * Common.Const.t
        with sexp
    ;;

end;;

type t =
    | Top of Info.t * type_t * Common.Var.t option * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * Common.Var.t * Common.Var.t Common.External.t
    with sexp
;;

val convert :
    tag_t Common.Var.Map.t -> Alpha.t -> (int Common.Var.Map.t * (t list))
;;


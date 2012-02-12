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

module InnerExpr : sig
    type t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | InnerApply of Info.t * Type.t * t * t list
        | InnerSafeApply of Info.t * Type.t * Common.Var.t * int * t list
        | InnerCall of Info.t * Type.t * Common.Var.t * t list
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
    ;;

end;;

module TailExpr : sig

    type t =
        | Return of InnerExpr.t
        | Let of Info.t * Type.t * Common.Arg.t * InnerExpr.t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * InnerExpr.t * t
        | If of Info.t * Type.t * InnerExpr.t * t * t
        | TailCall of Info.t * Type.t * Common.Var.t * InnerExpr.t list
    ;;

end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * Common.Arg.t list * TailExpr.t
    | TopVar of Info.t * Type.t * Common.Var.t * InnerExpr.t
    | TopExpr of Info.t * Type.t * InnerExpr.t
;;

val t_of_sexp__ : Sexplib.Sexp.t -> t;;
val t_of_sexp : Sexplib.Sexp.t -> t;;
val sexp_of_t : t -> Sexplib.Sexp.t;;
val convert :
    int Common.Var.Map.t -> LambdaLift.t -> (int Common.Var.Map.t * t)
;;


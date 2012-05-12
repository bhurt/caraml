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
        | Let of Info.t * Common.VarType.t * Common.Arg.t * t * t
        | If of Info.t * Common.VarType.t * t * t * t
        | AllocTuple of Info.t * Common.VarType.t * Common.Tag.t
                            * (Common.VarType.t * Common.Var.t) list
        | GetField of Info.t * Common.VarType.t * int
                            * (Common.VarType.t * Common.Var.t)
        | Case of Info.t * Common.VarType.t
                    * (Common.VarType.t * Common.Var.t)
                    * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | BinOp of Info.t * Common.VarType.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Common.VarType.t * Common.UnOp.t * t
        | InnerApply of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | InnerSafeApply of Info.t * Common.VarType.t
                                * (Common.VarType.t * Common.Var.t) * int
                                * ((Common.VarType.t * Common.Var.t) list)
        | InnerCall of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | Var of Info.t * Common.VarType.t * Common.Var.t
        | Const of Info.t * Common.VarType.t * Common.Const.t
        | CallExtern of Info.t * Common.VarType.t
                            * Common.Var.t Common.External.t
                            * ((Common.VarType.t * Common.Var.t) list)
        with sexp
    ;;

    val get_type : t -> Common.VarType.t;;

end;;

module TailExpr : sig

    type t =
        | Return of InnerExpr.t
        | Let of Info.t * Common.VarType.t * Common.Arg.t * InnerExpr.t * t
        | If of Info.t * Common.VarType.t * InnerExpr.t * t * t
        | Case of Info.t * Common.VarType.t
                    * (Common.VarType.t * Common.Var.t)
                    * ((Common.Tag.t * t) list)
        | Label of Info.t * Common.VarType.t * t * Common.Var.t
                                * Common.VarType.t Common.Var.Map.t * t
        | Goto of Info.t * Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | TailCall of Info.t * Common.VarType.t
                            * (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | TailCallExtern of Info.t * Common.VarType.t
                                * Common.Var.t Common.External.t
                                * ((Common.VarType.t * Common.Var.t) list)
        with sexp

    ;;

    val get_type : t -> Common.VarType.t;;

end;;

type t =
    | TopFun of Info.t * Common.VarType.t * Common.Var.t
                    * Common.Arg.t list * TailExpr.t
    | TopVar of Info.t * Common.VarType.t * Common.Var.t * InnerExpr.t
    | TopForward of Info.t * Common.VarType.t * Common.Var.t * int
    | TopExpr of Info.t * Common.VarType.t * InnerExpr.t
    with sexp
;;

val convert :
    int Common.Var.Map.t -> Simplify.t -> (int Common.Var.Map.t * t)
;;


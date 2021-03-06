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

    type s =
        | Let of Common.Arg.t * t * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (Common.VarType.t * Common.Var.t) list
        | ConstantConstructor of Common.Tag.t
        | GetField of int * (Common.VarType.t * Common.Var.t)
        | IsConstantConstructor of t
        | ConstantConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | TupleConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | InnerApply of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | InnerSafeApply of (Common.VarType.t * Common.Var.t) * int
                                * ((Common.VarType.t * Common.Var.t) list)
        | InnerCall of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | Var of Common.Var.t
        | Const of Common.Const.t
        | CallExtern of Common.Var.t Common.External.t
                            * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info: Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val get_type : t -> Common.VarType.t;;

end;;

module TailExpr : sig

    type s =
        | Return of InnerExpr.t
        | Let of Common.Arg.t * InnerExpr.t * t
        | If of InnerExpr.t * t * t
        | ConstantConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | TupleConstructorCase of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t
                    * ((Common.VarType.t * Common.Var.t) Common.Var.Map.t)
        | TailCall of (Common.VarType.t * Common.Var.t)
                            * ((Common.VarType.t * Common.Var.t) list)
        | TailCallExtern of Common.Var.t Common.External.t
                                * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val get_type : t -> Common.VarType.t;;

end;;

type s =
    | TopFun of Common.Var.t * Common.Arg.t list * TailExpr.t
    | TopVar of Common.Var.t * InnerExpr.t
    | TopForward of Common.Var.t * int
    | TopExpr of InnerExpr.t
and t = {
    info: Info.t;
    typ: Common.VarType.t;
    body: s;
} with sexp;;

module Convert : IL.Converter with type output = t;;


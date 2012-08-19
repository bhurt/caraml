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

    type s =
        | Let of Common.Arg.t * t * t
        | If of t * t * t
        | AllocTuple of Common.Tag.t * (t list)
        | GetField of int * t
        | Case of (Common.VarType.t * Common.Var.t) * ((Common.Tag.t * t) list)
        | Label of t * Common.Var.t * Common.VarType.t Common.Var.Map.t * t
        | Goto of Common.Var.t * (t Common.Var.Map.t)
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
        | CallExtern of Common.Var.t Common.External.t
                    * ((Common.VarType.t * Common.Var.t) list)
    and t = {
        info : Info.t;
        typ : Common.VarType.t;
        body : s;
    } with sexp;;

end;;

type s =
    | TopFun of Common.Var.t * Common.Arg.t list * Expr.t
    | TopVar of Common.Var.t * Expr.t
    | TopForward of Common.Var.t * int
    | TopExpr of Expr.t
and t = {
    info : Info.t;
    typ: Common.VarType.t;
    body: s;
} with sexp;;

module Convert : IL.Converter with type output = t;;


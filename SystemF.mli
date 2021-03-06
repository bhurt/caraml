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

module rec Lambda : sig

    type t = {
        info : Info.t;
        typ : Common.VarType.t;
        name: Common.Var.t;
        args : Common.Arg.t list;
        body : Expr.t
    } with sexp;;

end and Expr : sig

    type s =
        | Lambda of Lambda.t
        | Let of Common.Arg.t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Case of t * ((Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of t list
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
        | Loop of Common.Var.t * ((Common.VarType.t * Common.Var.t) list) * t
        | Recur of Common.Var.t * ((Common.VarType.t * t) list)
    and t = {
        info : Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val make : Info.t -> Common.VarType.t -> s -> t;;
    val map : ?pre:(t -> t) -> ?post:(t -> t) -> t -> t;;
    val fold : ?pre:('a -> t -> 'a) -> ?post:(t -> 'a -> 'a) -> t -> 'a -> 'a;;
    val any : (t -> bool) -> t -> bool;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
    | VariantDef of Common.Var.t
                * ((Info.t * Common.Var.t * (Common.VarType.t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;



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

module rec Pattern : sig

    type s =
        | Discard
        | Variable of string
        | Tuple of (t list)
        | Constructor of string * (t list)
        | Or of t * t
        | When of t * Expr.t
        | With of t * ((string * Expr.t) list)
        | As of t * string
    and t = {
        info: Info.t;
        body: s;
    } with sexp;;

end and Arg: sig
    type t = Common.StringType.t * (string option) with sexp;;

end and Lambda : sig

    type t = {
        info: Info.t;
        name: string;
        args: (Arg.t list);
        rtype: Common.StringType.t;
        body: Expr.t
    } with sexp

    val make : Info.t -> string -> Arg.t list -> Common.StringType.t
                    -> Expr.t -> t;;

end and Expr : sig

    type s =
        | Lambda of Arg.t list * t
        | Let of (string option) * t * t
        | LetTuple of (string option list) * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Match of t * ((Pattern.t * t) list)
        | Tuple of t list
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of string
        | Const of Common.Const.t
    and t = {
        info: Info.t;
        body: s;
    } with sexp;;

    val make : Info.t -> s -> t;;

end;;

type s =
    | Top of (string option) * Expr.t
    | TopRec of Lambda.t list
    | Extern of string * string Common.External.t
    | VariantDef of string
                        * ((Info.t * string * (Common.StringType.t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;

val make : Info.t -> s -> t;;

type parse_result =
    | Form of t
    | EOF
    | SyntaxError
;;


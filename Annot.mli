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

module CheckedString : sig
    type t with sexp;;
    val to_string : t -> string;;
end;;

type type_t = CheckedString.t Type.t with sexp;;

module rec Pattern : sig

    type s = Pattern of string * ((type_t * string option) list)
    and t = {
        info: Info.t;
        match_type : type_t;
        body: s
    } with sexp;;

end and Arg : sig

    type t = (type_t * (string option)) with sexp;;

end and Lambda : sig

    type t = {
        info: Info.t;
        typ: type_t;
        name: string;
        args: Arg.t list;
        body: Expr.t
    } with sexp;;

end and Expr : sig

    type s =
        | Lambda of Arg.t list * t
        | Let of Arg.t * t * t
        | LetTuple of Arg.t list * t * t
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
        info : Info.t;
        typ: type_t;
        body: s;
    } with sexp;;

end;;

type s =
    | Top of type_t * string option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of string * CheckedString.t Common.External.t
    | VariantDef of string * ((Info.t * string * (type_t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;

module Convert: IL.Converter with type output = t;;


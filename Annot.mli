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

module StringMap : Map.S with type key = string;;

type type_t = CheckedString.t Type.t with sexp;;

type type_env_t = {
    type_map : type_t StringMap.t;
    type_defn : type_t list StringMap.t StringMap.t;
};;

module Pattern : sig

    type t = Pattern of Info.t * string * ((type_t * string option) list)
                with sexp;;

end;;

module Expr : sig

    type arg = (type_t * (string option)) with sexp;;

    type lambda = Info.t * type_t * string * (arg list) * t
    and t =
        | Lambda of Info.t * type_t * arg list * t
        | Let of Info.t * type_t * arg * t * t
        | LetTuple of Info.t * type_t * arg list * t * t
        | LetRec of Info.t * type_t * (lambda list) * t
        | If of Info.t * type_t * t * t * t
        | Match of Info.t * type_t * t * ((Pattern.t * t) list)
        | Tuple of Info.t * type_t * t list
        | BinOp of Info.t * type_t * t * Common.BinOp.t * t
        | UnOp of Info.t * type_t * Common.UnOp.t * t
        | Apply of Info.t * type_t * t * t
        | Var of Info.t * type_t * string
        | Const of Info.t * type_t * Common.Const.t
        with sexp
    ;;

end;;

type t =
    | Top of Info.t * type_t * string option * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * string * CheckedString.t Common.External.t
    | VariantDef of Info.t * string
                        * ((Info.t * string * (type_t list)) list)
    with sexp
;;

val convert : type_env_t -> AST.t -> (type_env_t * t);;


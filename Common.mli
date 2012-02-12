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

module Var : sig

    type t;;

    val t_of_sexp__ : Sexplib.Sexp.t -> t;;
    val t_of_sexp : Sexplib.Sexp.t -> t;;
    val sexp_of_t : t -> Sexplib.Sexp.t;;

    val of_string : string -> t;;
    val to_string : t -> string;; (* Returns a unique name! *)
    val orig_name : t -> string;;

    val generate : unit -> t;;

    module Map : Map.S with type key = t;;
    module Set : Set.S with type elt = t;;

end;;

module Arg : sig

    type t = Type.t * (Var.t option);;

    val t_of_sexp__ : Sexplib.Sexp.t -> t;;
    val t_of_sexp : Sexplib.Sexp.t -> t;;
    val sexp_of_t : t -> Sexplib.Sexp.t;;

end;;

module BinOp : sig

    type t =
        | Times
        | Divide
        | Add
        | Subtract
        | Le
        | Ge
        | Lt
        | Gt
        | Eq
        | Ne
        | And
        | Or
    ;;

    val t_of_sexp__ : Sexplib.Sexp.t -> t;;
    val t_of_sexp : Sexplib.Sexp.t -> t;;
    val sexp_of_t : t -> Sexplib.Sexp.t;;

    val get_types : t -> Type.t * Type.t * Type.t;;

end;;

module UnOp : sig

    type t =
        | Neg
        | Not
    ;;

    val t_of_sexp__ : Sexplib.Sexp.t -> t;;
    val t_of_sexp : Sexplib.Sexp.t -> t;;
    val sexp_of_t : t -> Sexplib.Sexp.t;;

    val get_types : t -> Type.t * Type.t;;

end;;

module Const : sig

    type t =
        | Boolean of bool
        | Int of int
        | Unit
    ;;

    val t_of_sexp__ : Sexplib.Sexp.t -> t;;
    val t_of_sexp : Sexplib.Sexp.t -> t;;
    val sexp_of_t : t -> Sexplib.Sexp.t;;

    val get_type : t -> Type.t;;

end;;


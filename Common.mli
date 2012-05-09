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

    type t with sexp;;

    val of_string : string -> t;;
    val to_string : t -> string;; (* Returns a unique name! *)
    val orig_name : t -> string;;

    val generate : unit -> t;;

    module Map : Map.S with type key = t;;
    module Set : Set.S with type elt = t;;

end;;

module Arg : sig

    type t = Var.t Type.t * (Var.t option) with sexp;;

end;;

module BinOp : sig

    type t =
        (* Integer Ops *)
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

        (* Boolean ops *)
        | And
        | Or

        (* Float ops *)
        | FTimes
        | FDivide
        | FAdd
        | FSubtract
        | FLe
        | FGe
        | FLt
        | FGt
        | FEq
        | FNe
        with sexp
    ;;

    val get_types : t -> 'a Type.t * 'a Type.t * 'a Type.t;;

end;;

module UnOp : sig

    type t =
        | Neg
        | Not
        | FNeg
        with sexp
    ;;

    val get_types : t -> 'a Type.t * 'a Type.t;;

end;;

module Const : sig

    type t =
        | Boolean of bool
        | Int of int
        | Float of float
        | Unit
        with sexp
    ;;

    val get_type : t -> 'a Type.t;;

end;;

module External : sig

    type 'a t = {
        real_name : string;
        return_type : 'a Type.t;
        arg_types : 'a Type.t list;
    } with sexp;;

end;;

module StringType : sig

    type t = string Type.t with sexp;;

end;;

module VarType : sig

    type t = Var.t Type.t with sexp;;

end;;

module Tag : sig

    type t with sexp;;

    val of_int : int -> t;;
    val to_int : t -> int;;

end;;


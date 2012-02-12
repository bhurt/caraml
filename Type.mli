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

type base =
    | Int
    | Boolean
    | Unit
;;

type t =
    | Arrow of t * t
    | Tuple of t list
    | Base of base
;;

val t_of_sexp__ : Sexplib.Sexp.t -> t;;
val t_of_sexp : Sexplib.Sexp.t -> t;;
val sexp_of_t : t -> Sexplib.Sexp.t;;

val pprint : int -> t -> unit;;

val equals : t -> t -> bool;;

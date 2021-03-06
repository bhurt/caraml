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
    | Float
    with sexp
;;

type 'a t =
    | Arrow of 'a t * 'a t
    | Tuple of 'a t list
    | Named of 'a
    | Base of base
    with sexp
;;

val pprint : ('a -> string) -> int -> 'a t -> unit;;

val equals : 'a t -> 'a t -> bool;;

val fn_type : 'a t list -> 'a t -> 'a t;;


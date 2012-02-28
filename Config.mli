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

(* The maximum number of arguments a function can take, or which can
 * be applied in one call.
 *)
val max_args : int;;

(* The name of a function to directly call it. *)
val direct_name : Common.Var.t -> string;;

(* The name of a function to apply it. *)
val apply_name : Common.Var.t -> string;;

(* The name of the initialization function for a given variable *)
val init_name : Common.Var.t -> string;;

(* The name of the apply table for functions with n args and m
 * already applied.
 *)
val apply_table_name: int -> int -> string;;

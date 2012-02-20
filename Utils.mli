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

val id : 'a -> 'a;;
val take : int -> 'a list -> 'a list;;
val drop : int -> 'a list -> 'a list;;
val take_drop : int -> 'a list -> 'a list * 'a list;;
val repeat : int -> 'a -> 'a list;;
val unfold_left : ('a -> ('b * 'a) option) -> 'a -> 'b list;;
val unfold_right : ('a -> ('a * 'b) option) -> 'a -> 'b list;;
val unfoldi : (int -> 'a) -> int -> 'a list;;
val range : ?start:int -> ?step:int -> length:int -> int list;;
val mapi : ?start:int -> ?step:int -> (int -> 'a -> 'b) -> 'a list -> 'b list;;
val freduce : ('a -> 'a) list -> 'a -> 'a;;


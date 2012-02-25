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

val get_member : Llvm.llvalue -> Type.t -> int -> Llvm.llvalue Block.t;;
val set_member : ptr:Llvm.llvalue -> Type.t -> int -> value:Llvm.llvalue
                                                    -> Llvm.llvalue Block.t;;
val make_tag_word : tag:int -> len:int -> Type.t list -> int64;;
val set_tag_word_length : len:int -> Llvm.llvalue -> Llvm.llvalue Block.t;;
val heap_alloc : Llvm.llbasicblock -> int
                    -> (Llvm.llvalue * Llvm.llbasicblock) Function.t;;


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

include Reader.S;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_context : data t;;
end;;

module type S = sig
    type 'a monad;;
    val void_type : Llvm.lltype monad;;
    val word_type : Llvm.lltype monad;;
    val unit_type : Llvm.lltype monad;;
    val bool_type : Llvm.lltype monad;;
    val int_type : Llvm.lltype monad;;
    val float_type : Llvm.lltype monad;;
    val intptr_type : Llvm.lltype monad;;
    val ptr_type : Llvm.lltype -> Llvm.lltype monad;;
    val func_type : (Llvm.lltype list) -> Llvm.lltype -> Llvm.lltype monad;;
    val struct_type : (Llvm.lltype list) -> Llvm.lltype monad;;

    val llvm_of_type : Type.t -> Llvm.lltype monad;;

    val int_const : int -> Llvm.llvalue monad;;
    val int64_const : Int64.t -> Llvm.llvalue monad;;
    val float_const : float -> Llvm.llvalue monad;;
    val bool_const : bool -> Llvm.llvalue monad;;
    val unit_const : unit -> Llvm.llvalue monad;;

    val int_init : Llvm.llvalue monad;;
    val float_init : Llvm.llvalue monad;;
    val bool_init : Llvm.llvalue monad;;
    val unit_init : Llvm.llvalue monad;;
    val ptr_init : Llvm.llvalue monad;;

    val init_of_type : Type.t -> Llvm.llvalue monad;;

    val get_context : Llvm.llcontext monad;;
end;;

module Make(M: Monad) : S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val with_context : 'a t -> 'a;;



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
    val get_block : data t;;
end;;

module type S = sig

    include Function.S;;

    val ret : Llvm.llvalue -> Llvm.llvalue monad;;
    val br : Llvm.llbasicblock -> Llvm.llvalue monad;;
    val cond_br : test:Llvm.llvalue -> on_true:Llvm.llbasicblock
                    -> on_false:Llvm.llbasicblock -> Llvm.llvalue monad;;

    val add : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val sub : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val mul : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val div : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val bool_and : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val bool_or : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val lt : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val le : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val gt : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val ge : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val eq : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;
    val ne : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;

    val neg : Llvm.llvalue -> Llvm.llvalue monad;;
    val bool_not : Llvm.llvalue -> Llvm.llvalue monad;;

    val phi : (Llvm.llvalue * Llvm.llbasicblock) list -> Llvm.llvalue monad;;

    val load : Llvm.llvalue -> Llvm.llvalue monad;;
    val store : ptr:Llvm.llvalue -> value:Llvm.llvalue -> Llvm.llvalue monad;;

    val offset : Llvm.llvalue -> int -> Llvm.llvalue monad;;

    val call : Llvm.llvalue -> (Llvm.llvalue list) -> Llvm.llvalue monad;;

    val int_to_bool : Llvm.llvalue -> Llvm.llvalue monad;;
    val bool_to_int : Llvm.llvalue -> Llvm.llvalue monad;;

    val box_unit : Llvm.llvalue -> Llvm.llvalue monad;;
    val box_bool : Llvm.llvalue -> Llvm.llvalue monad;;
    val box_int : Llvm.llvalue -> Llvm.llvalue monad;;
    val box_float : Llvm.llvalue -> Llvm.llvalue monad;;
    val box_ptr : Llvm.llvalue -> Llvm.llvalue monad;;
    val box : Type.t -> Llvm.llvalue -> Llvm.llvalue monad;;

    val unbox_unit : Llvm.llvalue -> Llvm.llvalue monad;;
    val unbox_bool : Llvm.llvalue -> Llvm.llvalue monad;;
    val unbox_int : Llvm.llvalue -> Llvm.llvalue monad;;
    val unbox_float : Llvm.llvalue -> Llvm.llvalue monad;;
    val unbox_ptr : Llvm.llvalue -> Llvm.llvalue monad;;
    val unbox : Type.t -> Llvm.llvalue -> Llvm.llvalue monad;;

    val bitcast : Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue monad;;

    val ptr_cmp_lt : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue monad;;

    val load_global : string -> Llvm.llvalue monad;;

end;;

module Make(M: Monad) : S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val in_block : Llvm.llbasicblock -> 'a t -> 'a Function.t;;


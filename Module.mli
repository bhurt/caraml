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
    val get_module : data t;;
end;;

module type S = sig

    include Context.S;;

    val dump_module : unit monad;;

    val get_module : Llvm.llmodule monad;;

    val lookup_global : string -> Llvm.llvalue option monad;;
    val define_global : string -> Llvm.llvalue -> Llvm.llvalue monad;;

end;;


module Make(M: Monad) :S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val with_module : string -> 'a t -> 'a Context.t;;


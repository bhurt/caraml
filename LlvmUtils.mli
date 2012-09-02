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

val load : LlvmIntf.block_t -> ?lltype:Llvm.lltype -> Llvm.llvalue -> int
            -> Llvm.llvalue;;
val store: LlvmIntf.block_t-> ?lltype:Llvm.lltype -> ptr:Llvm.llvalue -> int
                -> value:Llvm.llvalue -> Llvm.llvalue;;
val get_member : LlvmIntf.block_t -> Llvm.llvalue -> 'a Type.t -> int
                    -> Llvm.llvalue;;
val set_member : LlvmIntf.block_t -> ptr:Llvm.llvalue -> 'a Type.t -> int
                    -> value:Llvm.llvalue -> Llvm.llvalue;;

val make_tag_word : tag:int -> len:int -> 'a Type.t list -> int64;;
val set_tag_word_length : LlvmIntf.block_t -> len:int -> Llvm.llvalue
                                -> Llvm.llvalue;;
val heap_alloc : LlvmIntf.block_t -> int -> (Llvm.llvalue * LlvmIntf.block_t);;

type get_val_t = LlvmIntf.block_t -> (Llvm.llvalue * LlvmIntf.block_t);;

val alloc_closure : LlvmIntf.block_t
                        -> int
                        -> tag_word:get_val_t
                        -> fn_ptr:get_val_t
                        -> get_val_t list
                        -> (Llvm.llvalue * LlvmIntf.block_t)
;;

val apply : LlvmIntf.block_t -> Llvm.llvalue -> Llvm.llvalue list
                -> Llvm.llvalue
;;

val get_tag : LlvmIntf.block_t -> Llvm.llvalue
                        -> (Llvm.llvalue * LlvmIntf.block_t);;


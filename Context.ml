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

module X = struct
    type t = Llvm.llcontext;;
end;;

module R = Reader.Make(X);;

include R;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_context: data t;;
end;;

module type S = sig

    type 'a monad;;

    val word_type : Llvm.lltype monad;;
    val unit_type : Llvm.lltype monad;;
    val bool_type : Llvm.lltype monad;;
    val int_type : Llvm.lltype monad;;
    val float_type : Llvm.lltype monad;;
    val intptr_type : Llvm.lltype monad;;
    val ptr_type : Llvm.lltype -> Llvm.lltype monad;;
    val func_type : (Llvm.lltype list) -> Llvm.lltype -> Llvm.lltype monad;;
    val struct_type : (Llvm.lltype list) -> Llvm.lltype monad;;

    val int_const : int -> Llvm.llvalue monad;;
    val int64_const : Int64.t -> Llvm.llvalue monad;;
    val float_const : float -> Llvm.llvalue monad;;
    val bool_const : bool -> Llvm.llvalue monad;;
    val unit_const : unit -> Llvm.llvalue monad;;

    val get_context : Llvm.llcontext monad;;

end;;

    

module Make(M: Monad) = struct

    open M;;

    type 'a monad = 'a M.t;;

    let word_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.i64_type ctx)
    ;;

    let unit_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.i64_type ctx)
    ;;

    let bool_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.i1_type ctx)
    ;;

    let int_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.i64_type ctx)
    ;;

    let float_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.double_type ctx)
    ;;

    let intptr_type =
        perform
            ctx <-- M.get_context;
            return (Llvm.pointer_type (Llvm.i64_type ctx))
    ;;

    let ptr_type base = return (Llvm.pointer_type base);;

    let func_type arg_types ret_type =
        return (Llvm.function_type ret_type (Array.of_list arg_types))
    ;;

    let struct_type tys =
        perform
            ctx <-- M.get_context;
            return (Llvm.struct_type ctx (Array.of_list tys))
    ;;

    let int_const x =
        perform
            t <-- int_type;
            return (Llvm.const_int t x)
    ;;

    let int64_const x =
        perform
            t <-- int_type;
            return (Llvm.const_of_int64 t x true)
    ;;

    let float_const x =
        perform
            t <-- float_type;
            return (Llvm.const_float t x)
    ;;

    let bool_const x =
        perform
            t <-- bool_type;
            return (Llvm.const_int t (if x then 1 else 0))
    ;;

    let unit_const () =
        perform
            t <-- unit_type;
            return (Llvm.const_int t 0)
    ;;

    let get_context = M.get_context;;

end;;

module K = struct
    include R;;
    let get_context = read;;
end;;

include Make(K);;

let with_context m = run (Llvm.create_context ()) m;;

let get_context = read;;


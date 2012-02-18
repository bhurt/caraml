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

    type t = {
        fn: Function.data;
        block: Llvm.llbasicblock;
        builder: Llvm.llbuilder;
    };;

end;;

module R = Reader.Make(X);;

include R;;

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

end;;

module Make(M: Monad) = struct

    open M;;

    module Y = struct
        include M;;

        let get_function =
            perform
                b <-- M.get_block;
                return b.X.fn
        ;;

    end;;

    include Function.Make(Y);;

    let ret v =
        perform
            b <-- M.get_block;
            return (Llvm.build_ret v b.X.builder)
    ;;

    let br dest =
        perform
            b <-- M.get_block;
            return (Llvm.build_br dest b.X.builder)
    ;;

    let cond_br ~test ~on_true ~on_false =
        perform
            b <-- M.get_block;
            return (Llvm.build_cond_br test on_true on_false
                        b.X.builder)
    ;;

    let binop op x y =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (op x y name b.X.builder)
    ;;

    let add = binop Llvm.build_add;;
    let sub = binop Llvm.build_sub;;
    let mul = binop Llvm.build_mul;;
    let div = binop Llvm.build_sdiv;;
    let bool_and = binop Llvm.build_and;;
    let bool_or = binop Llvm.build_or;;

    let compare t = binop (Llvm.build_icmp t);;
    let lt = compare Llvm.Icmp.Slt;;
    let le = compare Llvm.Icmp.Sle;;
    let gt = compare Llvm.Icmp.Sgt;;
    let ge = compare Llvm.Icmp.Sge;;
    let eq = compare Llvm.Icmp.Eq;;
    let ne = compare Llvm.Icmp.Ne;;

    let unop op x =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (op x name b.X.builder)
    ;;

    let neg = unop Llvm.build_neg;;
    let bool_not = unop Llvm.build_not;;

    let phi vs =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (Llvm.build_phi vs name b.X.builder)
    ;;

    let load v =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (Llvm.build_load v name b.X.builder)
    ;;

    let store ~ptr ~value =
        perform
            b <-- M.get_block;
            return (Llvm.build_store value ptr b.X.builder)
    ;;

    let offset ptr idx =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (Llvm.build_struct_gep ptr idx name b.X.builder)
    ;;

    let call f xs =
        perform
            b <-- M.get_block;
            name <-- alloc_reg_name;
            return (Llvm.build_call f (Array.of_list xs) name b.X.builder)
    ;;

end;;

module K = struct
    include R;;
    let get_block = read;;
end;;

include Make(K);;

let in_block block m =
    let bind = Function.bind in
    let return = Function.return in
    perform
        ctx <-- Function.get_context;
        f <-- Function.read;
        let builder = Llvm.builder_at_end ctx block in
        let t = {
            X.fn = f;
            X.block = block;
            X.builder = builder;
        }
        in
        return (run t m)
;;

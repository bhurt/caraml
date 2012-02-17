include Reader.S;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_function : data t;;
end;;

module type S = sig

    include Module.S;;

    val alloc_reg_name : string monad;;
    val alloc_block_name : string monad;;
    val param : int -> Llvm.llvalue monad;;
    val params : Llvm.llvalue list monad;;
    val entry_block : Llvm.llbasicblock monad;;
    val new_block : Llvm.llbasicblock monad;;

end;;

module Make(M: Monad) : S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val with_function : string -> Llvm.lltype -> 'a t -> 'a Module.t;;



include Reader.S;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_context : data t;;
end;;

module type S = sig
    type 'a monad;;
    val bool_type : Llvm.lltype monad;;
    val int_type : Llvm.lltype monad;;
    val float_type : Llvm.lltype monad;;
    val intptr_type : Llvm.lltype monad;;
    val get_context : Llvm.llcontext monad;;
end;;

module Make(M: Monad) : S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val with_context : 'a t -> 'a;;



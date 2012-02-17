
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
end;;


module Make(M: Monad) :S with type 'a monad = 'a M.t;;

include S with type 'a monad = 'a t;;

val with_module : string -> 'a t -> 'a Context.t;;


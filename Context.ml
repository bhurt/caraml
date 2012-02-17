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

    val bool_type : Llvm.lltype monad;;
    val int_type : Llvm.lltype monad;;
    val float_type : Llvm.lltype monad;;
    val intptr_type : Llvm.lltype monad;;
    val get_context : Llvm.llcontext monad;;

end;;

    

module Make(M: Monad) = struct

    open M;;

    type 'a monad = 'a M.t;;

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

    let func_type arg_types ret_type =
        return (Llvm.function_type ret_type (Array.of_list arg_types))
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


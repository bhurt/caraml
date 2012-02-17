
module X = struct
    type t = {
        llmodule: Module.data;
        fn: Llvm.llvalue;
        mutable block_count: int;
        mutable reg_count : int;
    };;

end;;

module R = Reader.Make(X);;

include R;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_function: data t;;
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

module Make(M: Monad) = struct

    open M;;

    module Y = struct
        include M;;

        let get_module =
            perform
                f <-- M.get_function;
                return f.X.llmodule
        ;;

    end;;

    include Module.Make(Y);;

    let alloc_reg_name =
        perform
            f <-- M.get_function;
            let r = f.X.reg_count in
            let _ = f.X.reg_count <- (r + 1) in
            return (string_of_int r)
    ;;

    let alloc_block_name =
        perform
            f <-- M.get_function;
            let b = f.X.block_count in
            let _ = f.X.block_count <- b + 1 in
            return (Printf.sprintf "block%d" b)
    ;;

    let param n =
        perform
            f <-- M.get_function;
            return (Llvm.param f.X.fn n)
    ;;

    let params =
        perform
            f <-- M.get_function;
            return (Array.to_list (Llvm.params f.X.fn))
    ;;

    let entry_block =
        perform
            f <-- M.get_function;
            return (Llvm.entry_block f.X.fn)
    ;;

    let new_block =
        perform
            f <-- M.get_function;
            ctx <-- get_context;
            name <-- alloc_block_name;
            return (Llvm.append_block ctx name f.X.fn)
    ;;

end;;

module K = struct
    include R;;
    let get_function = read;;
end;;

include Make(K);;

let with_function name fn_type (m: 'a t) : 'a Module.t =
    let bind = Module.bind in
    let return = Module.return in
    perform
        llmdl <-- Module.get_module;
        mdl <-- Module.read;
        let v = Llvm.define_function name fn_type llmdl in
        let f = {
            X.llmodule = mdl;
            X.fn = v;
            X.block_count = 1;
            X.reg_count = 1;
        } in
        let x = run f m in
        return x
;;


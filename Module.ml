
module X = struct
    type t = Context.data * Llvm.llmodule;;
end;;

module R = Reader.Make(X);;

include R;;

module type Monad = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val get_module: data t;;
end;;

module type S = sig

    include Context.S;;

    val dump_module : unit monad;;
    val get_module : Llvm.llmodule monad;;

end;;

    

module Make(M: Monad) = struct

    open M;;

    module Y = struct
        include M;;

        let get_context =
            perform
                mdl <-- M.get_module;
                return (fst mdl)
        ;;
    end;;

    include Context.Make(Y);;

    let dump_module =
        perform
            mdl <-- M.get_module;
            return (Llvm.dump_module (snd mdl))
    ;;

    let get_module =
        perform
            mdl <-- M.get_module;
            return (snd mdl)
    ;;

end;;

module K = struct
    include R;;
    let get_module = read;;
end;;

include Make(K);;

let with_module name (m: 'a t) : 'a Context.t =
    Context.bind Context.get_context
        (fun ctx -> 
            Context.bind Context.read
            (fun data ->
                let mdl = Llvm.create_module ctx name in
                let x = run (data, mdl) m in
                Context.return x))
;;

let get_module =
    perform
        mdl <-- read;
        return (snd mdl)
;;

let define_function name fn_type =
    perform
        mdl <-- read;
        return (Llvm.define_function name fn_type (snd mdl))
;;



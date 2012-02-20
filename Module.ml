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
        context: Context.data;
        mdl: Llvm.llmodule
    };;

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

    val lookup_global : string -> Llvm.llvalue monad;;
    val define_global : string -> Llvm.llvalue -> Llvm.llvalue monad;;
    val app_table_type : Llvm.lltype monad;;

end;;



module Make(M: Monad) = struct

    open M;;

    module Y = struct
        include M;;

        let get_context =
            perform
                mdl <-- M.get_module;
                return mdl.X.context;
        ;;
    end;;

    include Context.Make(Y);;

    let dump_module =
        perform
            mdl <-- M.get_module;
            return (Llvm.dump_module mdl.X.mdl)
    ;;

    let get_module =
        perform
            mdl <-- M.get_module;
            return mdl.X.mdl
    ;;

    let lookup_global name =
        perform
            mdl <-- M.get_module;
            match (Llvm.lookup_global name mdl.X.mdl) with
            | None ->
                failwith
                    (Printf.sprintf "Unknown global named \"%s\""
                                            name)
            | Some v -> return v
    ;;

    let define_global name value =
        perform
            mdl <-- M.get_module;
            return (Llvm.define_global name value mdl.X.mdl)
    ;;

    let app_table_type =
        perform
            mdl <-- M.get_module;
            match Llvm.type_by_name mdl.X.mdl "caraml_app_table_t" with
            | Some t -> return t
            | None -> assert false
    ;;

end;;

module K = struct
    include R;;
    let get_module = read;;
end;;

include Make(K);;

let make_app_fn_type num_args =
    perform
        itype <-- int_type;
        ptrtype <-- intptr_type;
        func_type (ptrtype :: (Utils.repeat num_args itype)) itype
;;

let make_app_table_type =
    perform
        tys <-- seq (Utils.unfoldi (fun i -> make_app_fn_type (i + 1))
                                                            Config.max_args);
        struct_type tys
;;

let init_module m =
    perform
        ty <-- make_app_table_type;
        mdl <-- read;
        let _ = Llvm.define_type_name "caraml_app_table_t" ty mdl.X.mdl in
        m
;;

let with_module name (m: 'a t) : 'a Context.t =
    let bind = Context.bind in
    perform
        ctx <-- Context.get_context;
        data <-- Context.read;
        let mdl = Llvm.create_module ctx name in
        let m = init_module m in
        let x = run { X.context = data; X.mdl = mdl } m in
        Context.return x
;;

let get_module =
    perform
        mdl <-- read;
        return mdl.X.mdl
;;

let define_function name fn_type =
    perform
        mdl <-- read;
        return (Llvm.define_function name fn_type mdl.X.mdl)
;;



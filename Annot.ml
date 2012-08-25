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

open Sexplib.Conv;;

module CheckedString : sig
    type t with sexp;;
    val of_string : string -> t;;
    val to_string : t -> string;;
end = struct
    type t = string with sexp;;
    let of_string x = x;;
    let to_string x = x;;
end;;

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

type type_t = CheckedString.t Type.t with sexp;;

type type_env_t = {
    type_map : type_t StringMap.t;
    type_defn : type_t list StringMap.t StringMap.t;
};;

module Errors : sig
    val type_error : Info.t -> type_t -> type_t -> 'a;;
    val tuple_arity_error : Info.t -> int -> int -> 'a;;
    val not_tuple_type_error : Info.t -> type_t -> 'a;;
    val unknown_var : Info.t -> string -> 'a;;
    val not_a_function : Info.t -> type_t -> 'a;;
    val not_a_variant : Info.t -> type_t -> 'a;;
    val invalid_number_of_bindings : Info.t -> string -> int -> int -> 'a;;
    val constructor_not_found : Info.t -> string -> string -> 'a;;
    val type_not_found : Info.t -> string -> 'a;;
    val pattern_vars : Info.t -> string list -> 'a;;

end = struct

    let type_error info t1 t2 =
        let f indent =
            Format.print_string "Type error:";
            Format.print_space ();
            Format.print_string "can not unify ";
            Format.open_box indent;
            Type.pprint CheckedString.to_string indent t1;
            Format.close_box ();
            Format.print_space ();
            Format.print_string "with ";
            Format.open_box indent;
            Type.pprint CheckedString.to_string indent t2;
            Format.close_box ();
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let tuple_arity_error info expected got =
        let f _ =
            Format.print_string
                (Printf.sprintf "Tuple arity error, expected %d members, got %d"
                                    expected got);
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let not_tuple_type_error info ty =
        let f indent =
            Format.print_string "Type error:";
            Format.print_space ();
            Format.print_string "expected a tuple type, but got type:";
            Format.print_space ();
            Format.open_box indent;
            Type.pprint CheckedString.to_string indent ty;
            Format.close_box ();
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let unknown_var info v =
        let f indent =
            Format.print_string "Unknown name:";
            Format.print_space ();
            Format.print_string v;
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let not_a_function info f_ty =
        let f indent =
            Format.print_string "Type error:";
            Format.print_space ();
            Format.print_string "expected a function type, but got type:";
            Format.print_space ();
            Format.open_box indent;
            Type.pprint CheckedString.to_string indent f_ty;
            Format.close_box ();
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let not_a_variant info ty =
        let f indent =
            Format.print_string "Type error:";
            Format.print_space ();
            Format.print_string "expected a variant type, but got type:";
            Format.print_space ();
            Format.open_box indent;
            Type.pprint CheckedString.to_string indent ty;
            Format.close_box ();
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let invalid_number_of_bindings info name expected got =
        let f indent =
            Format.print_string "Error:";
            Format.print_space ();
            Format.print_string
                "Invalid number of parameters for constructor:";
            Format.print_space ();
            Format.print_string
                (Printf.sprintf "Expected %d, got %d." expected got);
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let constructor_not_found info tname cname =
        let f indent =
            Format.print_string "Error:";
            Format.print_space ();
            Format.print_string "Could not find constructor";
            Format.print_space ();
            Format.print_string cname;
            Format.print_space ();
            Format.print_string "in type";
            Format.print_space ();
            Format.print_string tname;
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let type_not_found info tname =
        let f indent =
            Format.print_string "Error:";
            Format.print_space ();
            Format.print_string "Could not find type";
            Format.print_space ();
            Format.print_string tname;
            ()
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let pattern_vars info vars =
        let f indent =
            Format.print_string "Error:";
            Format.print_space ();
            Format.print_string "The following variables were declared in one branch of a pattern, but not the other: ";
            let rec loop = function
                | x :: ((_ :: _) as xs) ->
                    let () = Format.open_box indent in
                    let _ = Format.print_string x in
                    let _ = Format.print_string "," in
                    let () = Format.close_box () in
                    let _ = Format.print_space () in
                    loop xs
                | [ x ] ->
                    let () = Format.open_box indent in
                    let _ = Format.print_string x in
                    let () = Format.close_box () in
                    ()
                | [] -> assert false
            in
            loop vars
        in
        raise (Error.Compiler_error(f, info))
    ;;

end

let unify info t1 t2 =
    if not (Type.equals t1 t2) then
        Errors.type_error info t1 t2
    else
        true
;;

let rec convert_type info type_env = function
    | Type.Arrow(f, x) ->
        let f = convert_type info type_env f in
        let x = convert_type info type_env x in
        Type.Arrow(f, x)
    | Type.Tuple(xs) ->
        let xs = List.map (convert_type info type_env) xs in
        Type.Tuple(xs)
    | Type.Named(name) ->
        if (StringMap.mem name type_env.type_defn) then
            let name = CheckedString.of_string name in
            Type.Named(name)
        else
            Errors.unknown_var info name
    | Type.Base(b) -> Type.Base(b)
;;

module rec Pattern : sig

    type s =
        | Discard
        | Variable of string
        | Tuple of t list
        | Constructor of string * (t list)
        | Or of t * t
        | When of t * Expr.t
        | With of t * ((string * Expr.t) list)
        | As of t * string
    and t = {
        info: Info.t;
        match_type : type_t;
        body: s
    } with sexp;;

    val defined_vars : t -> StringSet.t;;

    val convert : type_env_t -> type_t -> AST.Pattern.t -> type_env_t * t;;

end = struct

    type s =
        | Discard
        | Variable of string
        | Tuple of (t list)
        | Constructor of string * (t list)
        | Or of t * t
        | When of t * Expr.t
        | With of t * ((string * Expr.t) list)
        | As of t * string
    and t = {
        info: Info.t;
        match_type : type_t;
        body: s
    } with sexp;;

    let defined_vars patt =
        let rec loop vars patt =
            match patt.body with
            | Discard -> vars
            | Variable(s) -> StringSet.add s vars
            | Tuple(xs)
            | Constructor(_, xs) ->
                List.fold_left loop vars xs
            | Or(_, y) ->
                (* We assume both branchs have the same variables defined,
                 * so we only need to traverse one.  Due to how patterns
                 * get constructed, the right branch is generally
                 * shallower, so we traverse it.
                 *)
                loop vars y
            | When(x, _) -> loop vars x
            | With(x, ds) ->
                let vars = List.fold_left
                                (fun vars (n, _) -> StringSet.add n vars)
                                vars ds
                in
                loop vars x
            | As(x, n) -> loop (StringSet.add n vars) x
        in
        loop StringSet.empty patt
    ;;

    let rec convert type_env match_type ast =
        let info = ast.AST.Pattern.info in
        let type_env, body =
            match ast.AST.Pattern.body with
            | AST.Pattern.Discard -> type_env, Discard
            | AST.Pattern.Variable(s) ->
                { type_env with
                    type_map = StringMap.add s match_type type_env.type_map },
                Variable(s)

            | AST.Pattern.Tuple(xs) ->
                begin
                    match match_type with
                    | Type.Tuple(ts) ->
                        if (List.length xs) != (List.length ts) then
                            Errors.tuple_arity_error info (List.length ts)
                                    (List.length xs)
                        else
                            let type_env, xs =
                                Utils.map2_accum convert type_env ts xs
                            in
                            type_env, Tuple(xs)
                    | _ ->
                        Errors.not_tuple_type_error info match_type
                end

            | AST.Pattern.Constructor(name, xs) ->
                begin
                    match match_type with
                    | Type.Named(xname) ->
                        let ts =
                            try
                                let tmap = StringMap.find
                                            (CheckedString.to_string xname)
                                            type_env.type_defn
                                in
                                try
                                    StringMap.find name tmap
                                with
                                | Not_found ->
                                    Errors.constructor_not_found info
                                            (CheckedString.to_string xname)
                                            name
                            with
                            | Not_found ->
                                Errors.type_not_found info
                                        (CheckedString.to_string xname)
                        in
                        if (List.length xs) != (List.length ts) then
                            Errors.tuple_arity_error info (List.length ts)
                                    (List.length xs)
                        else
                            let type_env, xs =
                                Utils.map2_accum convert type_env ts xs
                            in
                            type_env, Tuple(xs)
                    | _ ->
                        Errors.not_a_variant info match_type
                end

            | AST.Pattern.Or(x, y) ->
                let type_env_1, x = convert type_env match_type x in
                let type_env_2, y = convert type_env match_type y in
                let x_vars = defined_vars x in
                let y_vars = defined_vars y in
                let all_vars = StringSet.union x_vars y_vars in
                let error_vars =
                    StringSet.diff all_vars (StringSet.inter x_vars y_vars)
                in
                if not (StringSet.is_empty error_vars) then
                    Errors.pattern_vars info (StringSet.elements error_vars)
                else
                    let () =
                        StringSet.iter
                            (fun v ->
                                let _ =
                                    unify info
                                        (StringMap.find v type_env_1.type_map)
                                        (StringMap.find v type_env_2.type_map)
                                in
                                ())
                        all_vars
                    in
                    type_env_1, Or(x, y)

            | AST.Pattern.When(patt, x) ->
                let type_env, patt = convert type_env match_type patt in
                let x = Expr.convert type_env x in
                type_env, When(patt, x)

            | AST.Pattern.With(patt, ds) ->
                let type_env, patt = convert type_env match_type patt in
                let type_env, ds =
                    Utils.map_accum
                        (fun type_env (n, x) ->
                            let x = Expr.convert type_env x in
                            let type_env = { type_env with
                                type_map = StringMap.add n
                                            x.Expr.typ type_env.type_map }
                            in
                            type_env, (n, x))
                        type_env
                        ds
                in
                type_env, With(patt, ds)

            | AST.Pattern.As(patt, n) ->
                let type_env, patt = convert type_env match_type patt in
                let type_env = { type_env with
                    type_map = StringMap.add n match_type
                                type_env.type_map }
                in
                type_env, As(patt, n)
        in
        type_env, { info; match_type; body }
    ;;

end and Arg : sig

    type t = (type_t * (string option)) with sexp;;

end = struct

    type t = (type_t * (string option)) with sexp;;

end and Lambda : sig

    type t = {
        info: Info.t;
        typ: type_t;
        name: string;
        args: Arg.t list;
        body: Expr.t
    } with sexp;;

    val add_lambda_type : type_env_t -> AST.Lambda.t -> type_env_t;;
    val convert : type_env_t -> AST.Lambda.t -> t;;

end = struct

    type t = {
        info: Info.t;
        typ: type_t;
        name: string;
        args: Arg.t list;
        body: Expr.t
    } with sexp;;

    let add_lambda_type type_env lambda =
        let ty = Type.fn_type
                    (List.map
                        (fun x -> convert_type lambda.AST.Lambda.info
                                                    type_env (fst x))
                        lambda.AST.Lambda.args)
                    (convert_type lambda.AST.Lambda.info type_env
                                            lambda.AST.Lambda.rtype)
        in
        { type_env with type_map =
            StringMap.add lambda.AST.Lambda.name ty type_env.type_map }
    ;;

    let convert type_env lambda =
        let info = lambda.AST.Lambda.info in
        let name = lambda.AST.Lambda.name in
        let args = lambda.AST.Lambda.args in
        let rtype = lambda.AST.Lambda.rtype in
        let x = lambda.AST.Lambda.body in
        let args = List.map
                        (fun (t, x) -> (convert_type info type_env t), x)
                        args
        in
        let rtype = convert_type info type_env rtype in
        let type_env =
            {   type_env with
                type_map =
                    List.fold_left
                        (fun m (t, a) ->
                            match a with
                            | Some v -> StringMap.add v t m
                            | None -> m)
                        type_env.type_map
                        args }
        in
        let x = Expr.convert type_env x in
        let _ = unify info x.Expr.typ rtype in
        let fty = Type.fn_type (List.map fst args) rtype in
        {   info = info;
            typ = fty;
            name = name;
            args = args;
            body = x }
    ;;


end and Expr : sig

    type s =
        | Lambda of Arg.t list * t
        | Let of Arg.t * t * t
        | LetTuple of Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Match of t * ((Pattern.t * t) list)
        | Tuple of t list
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of string
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: type_t;
        body: s;
    } with sexp;;

    val convert : type_env_t -> AST.Expr.t -> t;;

end = struct

    type s =
        | Lambda of Arg.t list * t
        | Let of Arg.t * t * t
        | LetTuple of Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Match of t * ((Pattern.t * t) list)
        | Tuple of t list
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of string
        | Const of Common.Const.t
    and t = {
        info : Info.t;
        typ: type_t;
        body: s;
    } with sexp;;

    let add_args type_env args =
        { type_env with
            type_map =
                List.fold_left
                    (fun m (t, a) ->
                        match a with
                        | Some v -> StringMap.add v t m
                        | None -> m)
                    type_env.type_map
                    args }
    ;;

    let add_bindings type_env v ty =
        { type_env with
            type_map = StringMap.add v ty type_env.type_map }
    ;;

    let calc_type type_env = function
        | Lambda(args, x) ->
            Type.fn_type (List.map fst args) x.typ
        | Let(_, _, y)
        | LetTuple(_, _, y)
        | LetRec(_, y)
        | If(_, y, _)
                -> y.typ
        | Match(_, bindings) -> (snd (List.hd bindings)).typ
        | Tuple(xs) ->
            let ts = List.map (fun x -> x.typ) xs in
            Type.Tuple(ts)
        | BinOp(_, op, _) ->
            let (_, _, ty) = Common.BinOp.get_types op in
            ty
        | UnOp(op, _) ->
            let (_, ty) = Common.UnOp.get_types op in
            ty
        | Apply(f, _) ->
            begin
                match f.typ with
                | Type.Arrow(_, ty) -> ty
                | _ -> assert false
            end
        | Var(v) -> StringMap.find v type_env.type_map
        | Const(c) -> Common.Const.get_type c
    ;;

    let rec convert type_env t =
        let info = t.AST.Expr.info in
        let body = match t.AST.Expr.body with
            | AST.Expr.Lambda(args, x) ->
                let args = List.map
                                (fun (t, x) ->
                                    (convert_type info type_env t), x)
                                args
                in
                let type_env = add_args type_env args in
                let x = convert type_env x in
                Lambda(args, x)

            | AST.Expr.Let(None, x, y) ->
                let x = convert type_env x in
                let y = convert type_env y in
                let v = (x.typ, None) in
                Let(v, x, y)

            | AST.Expr.Let(Some(v), x, y) ->
                let x = convert type_env x in
                let x_ty = x.typ in
                let type_env = add_bindings type_env v x_ty in
                let y = convert type_env y in
                let v = (x_ty, Some(v)) in
                Let(v, x, y)

            | AST.Expr.LetTuple(args, x, y) ->
                begin
                    let x = convert type_env x in
                    match x.typ with
                    | Type.Tuple(ts) ->
                        if (List.length ts) != (List.length args) then
                            Errors.tuple_arity_error x.info (List.length ts)
                                (List.length args)
                        else
                            let args = List.map2 (fun a b -> a,b) ts args in
                            let type_env = add_args type_env args in
                            let y = convert type_env y in
                            LetTuple(args, x, y)
                    | _ ->
                        Errors.not_tuple_type_error x.info x.typ
                end

            | AST.Expr.LetRec(fns, x) ->
                let type_env = List.fold_left Lambda.add_lambda_type
                                                        type_env fns
                in
                let fns = List.map (Lambda.convert type_env) fns in
                let x = convert type_env x in
                LetRec(fns, x)

            | AST.Expr.If(x, y, z) ->
                let x = convert type_env x in
                let y = convert type_env y in
                let z = convert type_env z in
                let _ = unify info x.typ (Type.Base(Type.Boolean)) in
                let _ = unify info y.typ z.typ in
                If(x, y, z)

            | AST.Expr.Match(x, bindings) ->
                let x = convert type_env x in
                let bindings =
                    List.map
                        (fun (patt, y) ->
                            let type_env, patt =
                                Pattern.convert type_env x.typ patt
                            in
                            let y = convert type_env y in
                            (patt, y))
                        bindings
                in
                let _ = List.fold_left (fun t x ->
                                            let _ = unify info t (snd x).typ
                                            in t)
                            (snd (List.hd bindings)).typ (List.tl bindings)
                in
                Match(x, bindings)

            | AST.Expr.Tuple(xs) ->
                let xs = List.map (convert type_env) xs in
                Tuple(xs)

            | AST.Expr.BinOp(x, op, y) ->
                let (x_ty, y_ty, _) = Common.BinOp.get_types op in
                let x = convert type_env x in
                let y = convert type_env y in
                let _ = unify x.info x.typ x_ty in
                let _ = unify y.info y.typ y_ty in
                BinOp(x, op, y)

            | AST.Expr.UnOp(op, x) ->
                let (x_ty, _) = Common.UnOp.get_types op in
                let x = convert type_env x in
                let _ = unify x.info x.typ x_ty in
                UnOp(op, x)

            | AST.Expr.Apply(f, x) ->
                begin
                    let f = convert type_env f in
                    let x = convert type_env x in
                    match f.typ with
                    | Type.Arrow(y_ty, _) ->
                        let _ = unify info x.typ y_ty in
                        Apply(f, x)
                    | _ ->
                        Errors.not_a_function info f.typ
                end

            | AST.Expr.Var(v) ->
                begin
                    try
                        let _ = StringMap.find v type_env.type_map in
                        Var(v)
                    with
                    | Not_found -> Errors.unknown_var info v
                end

            | AST.Expr.Const(c) -> Const(c)

        in
        let typ = calc_type type_env body in
        {   info = info;
            typ = typ;
            body = body; }
    ;;

end;;

type s =
    | Top of type_t * string option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of string * CheckedString.t Common.External.t
    | VariantDef of string * ((Info.t * string * (type_t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;

let convert type_env x =
    let info = x.AST.info in
    let type_env, body = match x.AST.body with
        | AST.Top(v, x) ->
            let x = Expr.convert type_env x in
            let ty = x.Expr.typ in
            let type_env =
                match v with
                | Some n -> { type_env with
                                type_map = StringMap.add n ty
                                                            type_env.type_map }
                | None -> type_env
            in
            type_env, Top(ty, v, x)

        | AST.TopRec(fns) ->
            let type_env =
                List.fold_left Lambda.add_lambda_type type_env fns
            in
            let fns = List.map (Lambda.convert type_env) fns in
            type_env, TopRec(fns)

        | AST.Extern(v, extern) ->
            let extern = { extern with
                                Common.External.return_type =
                                    convert_type info type_env
                                        extern.Common.External.return_type;
                                Common.External.arg_types =
                                    List.map
                                        (convert_type info type_env)
                                        extern.Common.External.arg_types }
            in
            let type_env =
                { type_env with
                    type_map = StringMap.add v
                                    (Type.fn_type
                                        extern.Common.External.arg_types
                                        extern.Common.External.return_type)
                                    type_env.type_map }
            in
            type_env, Extern(v, extern)
        | AST.VariantDef(name, opts) ->
            (* Add a dummy definition just long enough to check the types *)
            let type_env' = { type_env with
                                type_defn = StringMap.add name
                                                StringMap.empty
                                                type_env.type_defn }
            in
            let opts = List.map
                        (fun (i, n, ts) ->
                            i, n, (List.map (convert_type i type_env') ts))
                        opts
            in
            let type_defn =
                StringMap.add name
                    (List.fold_left
                        (fun s (_, n, tys) -> StringMap.add n tys s)
                        StringMap.empty
                        opts)
                    type_env.type_defn
            in
            let rtype = Type.Named(CheckedString.of_string name) in
            let type_map =
                List.fold_left
                    (fun type_map (_, n, tys) ->
                        StringMap.add n (Type.fn_type tys rtype) type_map)
                    type_env.type_map
                    opts
            in
            { type_defn = type_defn; type_map = type_map },
            VariantDef(name, opts)
    in
    type_env, { info = info; body = body }
;;

module C : IL.Conversion with type input = AST.t and type output = t = struct
    type input = AST.t;;
    type output = t;;
    type state = type_env_t;;
    type check_state = unit;;

    let name = "annot";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = {
        type_map = StringMap.empty;
        type_defn = StringMap.empty;
    };;

    let convert state input =
        let state, output = convert state input in
        state, [ output ]
    ;;

    let fini_state _ = ();;

    let init_check_state _ = ();;
    let check _ _ = (), true;;
    let get_info _ = assert false;;
    let fini_check_state _ = ();;

end;;

module Convert : IL.Converter with type output = t = IL.Make(IL.Base)(C);;



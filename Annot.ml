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

type type_t = CheckedString.t Type.t with sexp;;

type type_env_t = {
    type_map : type_t StringMap.t;
    type_defn : type_t list StringMap.t StringMap.t;
};;

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

let unify info t1 t2 =
    if not (Type.equals t1 t2) then
        type_error info t1 t2
    else
        true
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
            unknown_var info name
    | Type.Base(b) -> Type.Base(b)
;;

module Pattern = struct

    type t = Pattern of Info.t * string
                            * ((type_t * string option) list)
                with sexp;;

end;;

module Expr = struct

    type arg = (type_t * (string option)) with sexp;;

    type lambda = Info.t * type_t * string * (arg list) * t
    and t =
        | Lambda of Info.t * type_t * arg list * t
        | Let of Info.t * type_t * arg * t * t
        | LetTuple of Info.t * type_t * arg list * t * t
        | LetRec of Info.t * type_t * (lambda list) * t
        | If of Info.t * type_t * t * t * t
        | Match of Info.t * type_t * t * ((Pattern.t * t) list)
        | Tuple of Info.t * type_t * t list
        | BinOp of Info.t * type_t * t * Common.BinOp.t * t
        | UnOp of Info.t * type_t * Common.UnOp.t * t
        | Apply of Info.t * type_t * t * t
        | Var of Info.t * type_t * string
        | Const of Info.t * type_t * Common.Const.t
        with sexp
    ;;

    let get_type = function
        | Lambda(_, ty, _, _)
        | Let(_, ty, _, _, _)
        | LetTuple(_, ty, _, _, _)
        | LetRec(_, ty, _, _)
        | If(_, ty, _, _, _)
        | Match(_, ty, _, _)
        | Tuple(_, ty, _)
        | BinOp(_, ty, _, _, _)
        | UnOp(_, ty, _, _)
        | Apply(_, ty, _, _)
        | Var(_, ty, _)
        | Const(_, ty, _)
            -> ty
    ;;

    let get_info = function
        | Lambda(info, _, _, _)
        | Let(info, _, _, _, _)
        | LetTuple(info, _, _, _, _)
        | LetRec(info, _, _, _)
        | If(info, _, _, _, _)
        | Match(info, _, _, _)
        | Tuple(info, _, _)
        | BinOp(info, _, _, _, _)
        | UnOp(info, _, _, _)
        | Apply(info, _, _, _)
        | Var(info, _, _)
        | Const(info, _, _)
            -> info
    ;;

    let add_lambda_type type_env (info, name, args, rtype, _) =
        let ty = Type.fn_type
                    (List.map
                        (fun x -> convert_type info type_env (fst x))
                        args)
                    (convert_type info type_env rtype)
        in
        { type_env with type_map = StringMap.add name ty type_env.type_map }
    ;;

    let rec convert_lambda type_env (info, name, args, rtype, x) =
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
        let x = convert type_env x in
        let _ = unify info (get_type x) rtype in
        let fty = Type.fn_type (List.map fst args) rtype in
        (info, fty, name, args, x)

    and convert type_env = function

        | AST.Expr.Lambda(info, args, x) ->
            let args = List.map
                            (fun (t, x) -> (convert_type info type_env t), x)
                            args
            in
            let type_env =
                { type_env with
                    type_map =
                        List.fold_left
                            (fun m (t, a) ->
                                match a with
                                | Some v -> StringMap.add v t m
                                | None -> m)
                            type_env.type_map
                            args }
            in
            let x = convert type_env x in
            let ty = Type.fn_type (List.map fst args) (get_type x) in
            Lambda(info, ty, args, x)

        | AST.Expr.Let(info, None, x, y) ->
            let x = convert type_env x in
            let y = convert type_env y in
            let ty = get_type y in
            let v = (get_type x, None) in
            Let(info, ty, v, x, y)

        | AST.Expr.Let(info, Some(v), x, y) ->
            let x = convert type_env x in
            let x_ty = get_type x in
            let type_env =
                { type_env with
                    type_map = StringMap.add v x_ty type_env.type_map }
            in
            let y = convert type_env y in
            let ty = get_type y in
            let v = (x_ty, Some(v)) in
            Let(info, ty, v, x, y)

        | AST.Expr.LetTuple(info, args, x, y) ->
            begin
                let x = convert type_env x in
                let ty = get_type x in
                match ty with
                | Type.Tuple(ts) ->
                    if (List.length ts) != (List.length args) then
                        tuple_arity_error (get_info x) (List.length ts)
                            (List.length args)
                    else
                        let args = List.map2 (fun a b -> a,b) ts args in
                        let type_env =
                            { type_env with
                                type_map =
                                    List.fold_left
                                        (fun env (t, arg) ->
                                            match arg with
                                            | Some v -> StringMap.add v t env
                                            | None -> env)
                                        type_env.type_map
                                        args }
                        in
                        let y = convert type_env y in
                        LetTuple(info, get_type y, args, x, y)
                | _ ->
                    not_tuple_type_error (get_info x) ty
            end

        | AST.Expr.LetRec(info, fns, x) ->
            let type_env = List.fold_left add_lambda_type type_env fns in
            let fns = List.map (convert_lambda type_env) fns in
            let x = convert type_env x in
            LetRec(info, (get_type x), fns, x)

        | AST.Expr.If(info, x, y, z) ->
            let x = convert type_env x in
            let y = convert type_env y in
            let z = convert type_env z in
            let _ = unify info (get_type x) (Type.Base(Type.Boolean)) in
            let y_ty = get_type y in
            let _ = unify info y_ty (get_type z) in
            If(info, y_ty, x, y, z)

        | AST.Expr.Match(info, x, bindings) ->
            let (type_env, x, bindings) =
                convert_match type_env info x bindings
            in
            let ty = 
                List.fold_left
                    (fun ty (_, x) ->
                        let _ = unify info ty (get_type x) in
                        ty)
                    (get_type (snd (List.hd bindings)))
                    (List.tl bindings)
            in
            Match(info, ty, x, bindings)

        | AST.Expr.Tuple(info, xs) ->
            let xs = List.map (convert type_env) xs in
            let ts = List.map get_type xs in
            let ty = Type.Tuple(ts) in
            Tuple(info, ty, xs)

        | AST.Expr.BinOp(info, x, op, y) ->
            let (x_ty, y_ty, z_ty) = Common.BinOp.get_types op in
            let x = convert type_env x in
            let y = convert type_env y in
            let _ = unify (get_info x) (get_type x) x_ty in
            let _ = unify (get_info y) (get_type y) y_ty in
            BinOp(info, z_ty, x, op, y)

        | AST.Expr.UnOp(info, op, x) ->
            let (x_ty, y_ty) = Common.UnOp.get_types op in
            let x = convert type_env x in
            let _ = unify (get_info x) (get_type x) x_ty in
            UnOp(info, y_ty, op, x)

        | AST.Expr.Apply(info, f, x) ->
            begin
                let f = convert type_env f in
                let x = convert type_env x in
                let f_ty = get_type f in
                let x_ty = get_type x in
                match f_ty with
                | Type.Arrow(y_ty, z_ty) ->
                    let _ = unify info x_ty y_ty in
                    Apply(info, z_ty, f, x)
                | _ ->
                    not_a_function info f_ty
            end

        | AST.Expr.Var(info, v) ->
            begin
                try
                    let ty = StringMap.find v type_env.type_map in
                    Var(info, ty, v)
                with
                | Not_found -> unknown_var info v
            end

        | AST.Expr.Const(info, c)
            -> Const(info, Common.Const.get_type c, c)

    and convert_match type_env info x bindings = 
        let x = convert type_env x in
        let x_typ = get_type x in
        let type_defn =
            match x_typ with
            | Type.Named(n) ->
                begin
                    try
                        StringMap.find (CheckedString.to_string n)
                                        type_env.type_defn
                    with
                    | Not_found -> unknown_var (get_info x)
                                            (CheckedString.to_string n)
                end
            | _ -> not_a_variant (get_info x) x_typ
        in
        let bindings =
            List.map
                (fun (pat, y) ->
                    let (type_env, pat) = convert_pat type_env type_defn pat in
                    let y = convert type_env y in
                    (pat, y))
                bindings
        in
        type_env, x, bindings
    and convert_pat type_env type_def (AST.Pattern.Pattern(info, n, xs)) =
        let tys =
            try
                StringMap.find n type_def
            with
            | Not_found -> unknown_var info n
        in
        if (List.length xs) != (List.length tys) then
            invalid_number_of_bindings info n
                (List.length tys) (List.length xs)
        else
            let type_map =
                List.fold_left2
                    (fun s x t ->
                        match x with
                        | Some x -> StringMap.add x t s
                        | None -> s)
                    type_env.type_map
                    xs
                    tys
            in
            let xs = List.map2 (fun x t -> t, x) xs tys in
            { type_env with type_map = type_map },
            (Pattern.Pattern(info, n, xs))
    ;;

end;;

type t =
    | Top of Info.t * type_t * (string option) * Expr.t
    | TopRec of Info.t * (Expr.lambda list)
    | Extern of Info.t * string * CheckedString.t Common.External.t
    | VariantDef of Info.t * string
                        * ((Info.t * string * (type_t list)) list)
    with sexp;;

let convert type_env = function
    | AST.Top(info, v, x) ->
        let x = Expr.convert type_env x in
        let ty = Expr.get_type x in
        let type_env =
            match v with
            | Some n -> { type_env with
                            type_map = StringMap.add n ty type_env.type_map }
            | None -> type_env
        in
        type_env, Top(info, ty, v, x)
    | AST.TopRec(info, fns) ->
        let type_env = List.fold_left Expr.add_lambda_type type_env fns in
        let fns = List.map (Expr.convert_lambda type_env) fns in
        type_env, TopRec(info, fns)
                    
    | AST.Extern(info, v, extern) ->
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
        type_env, Extern(info, v, extern)
    | AST.VariantDef(info, name, opts) ->
        (* Add a dummy definition just long enough to check the types *)
        let type_env' = { type_env with
                            type_defn = StringMap.add name StringMap.empty
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
        VariantDef(info, name, opts)
;;

module C : IL.Conversion with type input = AST.t and type output = t = struct
    type input = AST.t;;
    type output = t;;
    type state = type_env_t;;

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

    let check _ = true;;
    let get_info _ = assert false;;

    let fini_state _ = ();;
end;;

module Convert : IL.Converter with type output = t = IL.Make(IL.Base)(C);;



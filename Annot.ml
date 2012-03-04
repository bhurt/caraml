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

module StringMap = Map.Make(String);;

let type_error info t1 t2 =
    let f indent =
        Format.print_string "Type error:";
        Format.print_space ();
        Format.print_string "can not unify ";
        Format.open_box indent;
        Type.pprint indent t1;
        Format.close_box ();
        Format.print_space ();
        Format.print_string "with ";
        Format.open_box indent;
        Type.pprint indent t2;
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
        Type.pprint indent ty;
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
        Type.pprint indent f_ty;
        Format.close_box ();
        ()
    in
    raise (Error.Compiler_error(f, info))
;;

module Expr = struct

    type t =
        | Lambda of Info.t * Type.t * (Type.t * (string option)) list * t
        | Let of Info.t * Type.t * (Type.t * (string option)) * t * t
        | LetTuple of Info.t * Type.t
                        * ((Type.t * (string option)) list) * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * t * t
        | Var of Info.t * Type.t * string
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let get_type = function
        | Lambda(_, ty, _, _)
        | Let(_, ty, _, _, _)
        | LetTuple(_, ty, _, _, _)
        | If(_, ty, _, _, _)
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
        | If(info, _, _, _, _)
        | Tuple(info, _, _)
        | BinOp(info, _, _, _, _)
        | UnOp(info, _, _, _)
        | Apply(info, _, _, _)
        | Var(info, _, _)
        | Const(info, _, _)
            -> info
    ;;

    let rec convert type_env = function

        | AST.Expr.Lambda(info, args, x) ->
            let type_env =
                List.fold_left
                    (fun m (t, a) ->
                        match a with
                        | Some v -> StringMap.add v t m
                        | None -> m)
                    type_env
                    args
            in
            let x = convert type_env x in
            let ty = get_type x in
            let ty = List.fold_right
                        (fun (x, _) y -> Type.Arrow(x,y))
                        args ty
            in
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
            let type_env = StringMap.add v x_ty type_env in
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
                            List.fold_left
                                (fun env (t, arg) ->
                                    match arg with
                                    | Some v -> StringMap.add v t env
                                    | None -> env)
                                type_env
                                args
                        in
                        let y = convert type_env y in
                        LetTuple(info, get_type y, args, x, y)
                | _ ->
                    not_tuple_type_error (get_info x) ty
            end

        | AST.Expr.If(info, x, y, z) ->
            let x = convert type_env x in
            let y = convert type_env y in
            let z = convert type_env z in
            let _ = unify info (get_type x) (Type.Base(Type.Boolean)) in
            let y_ty = get_type y in
            let _ = unify info y_ty (get_type z) in
            If(info, y_ty, x, y, z)

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
                    let ty = StringMap.find v type_env in
                    Var(info, ty, v)
                with
                | Not_found -> unknown_var info v
            end

        | AST.Expr.Const(info, c)
            -> Const(info, Common.Const.get_type c, c)
    ;;

end;;

type t =
    | Top of Info.t * Type.t * (string option) * Expr.t
    with sexp;;

let convert type_env = function
    | AST.Top(info, v, x) ->
        let x = Expr.convert type_env x in
        let ty = Expr.get_type x in
        let type_env =
            match v with
            | Some n -> StringMap.add n ty type_env
            | None -> type_env
        in
        type_env, Top(info, ty, v, x)
;;


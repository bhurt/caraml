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

let from_some = function
    | Some x -> x
    | None -> assert false
;;

module Expr = struct

    type t =
        | Let of Info.t * Type.t * Common.Arg.t * t * t
        | LetTuple of Info.t * Type.t * Common.Arg.t list * t * t
        | If of Info.t * Type.t * t * t * t
        | Tuple of Info.t * Type.t * t list
        | BinOp of Info.t * Type.t * t * Common.BinOp.t * t
        | UnOp of Info.t * Type.t * Common.UnOp.t * t
        | Apply of Info.t * Type.t * t * t
        | Var of Info.t * Type.t * Common.Var.t
        | Const of Info.t * Type.t * Common.Const.t
        with sexp
    ;;

    let rem_arg m = function
        | (_, None) -> m
        | (_, Some v) -> Common.Var.Map.remove v m
    ;;

    let add_arg s = function
        | (_, None) -> s
        | (_, Some v) -> Common.Var.Set.add v s
    ;;

    let rec freevars bound m = function
        | Let(_, _, v, x, y) ->
            let m = freevars bound m x in
            freevars (add_arg bound v) m y

        | LetTuple(_, _, args, x, y) ->
            let m = freevars bound m x in
            let bound = List.fold_left add_arg bound args in
            freevars bound m y
        | If(_, _, x, y, z) ->
            freevars bound (freevars bound (freevars bound m x) y) z
        | Tuple(_, _, xs) ->
            List.fold_left (freevars bound) m xs
        | BinOp(_, _, x, _, y) ->
            freevars bound (freevars bound m x) y
        | UnOp(_, _, _, x) -> freevars bound m x
        | Apply(_, _, f, x) -> freevars bound (freevars bound m f) x
        | Var(_, ty, v) ->
            if (Common.Var.Set.mem v bound) then
                m
            else
                Common.Var.Map.add v ty m
        | Const(_, _, _) -> m
    ;;

    let get_type = function
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

    let mk_apply info f x =
        match get_type f with
        | Type.Arrow(_, b) -> Apply(info, b, f, x)
        | _ -> assert false
    ;;

    let rec rename vmap = function
        | Let(info, ty, arg, x, y) ->
            (* You can only rename free variables. *)
            let _ = assert
                        (not
                            (Common.Var.Map.mem
                                (from_some (snd arg))
                                vmap))
            in
            Let(info, ty, arg, (rename vmap x), (rename vmap y))
        | LetTuple(info, ty, args, x, y) ->
            (* You can only rename free variables. *)
            let _ = assert
                        (not
                            (List.exists
                                (fun v ->
                                    match snd v with
                                    | Some x -> Common.Var.Map.mem x vmap
                                    | None -> false)
                                args))
            in
            LetTuple(info, ty, args, (rename vmap x), (rename vmap y))
        | If(info, ty, x, y, z) ->
            If(info, ty, (rename vmap x), (rename vmap y), (rename vmap z))
        | Tuple(info, ty, xs) ->
            Tuple(info, ty, List.map (rename vmap) xs)
        | BinOp(info, ty, x, op, y) ->
            BinOp(info, ty, (rename vmap x), op, (rename vmap y))
        | UnOp(info, ty, op, x) ->
            UnOp(info, ty, op, (rename vmap x))
        | Apply(info, ty, f, x) ->
            Apply(info, ty, (rename vmap f), (rename vmap x))
        | Var(info, ty, v) ->
            begin
                try
                    let v' = Common.Var.Map.find v vmap in
                    Var(info, ty, v')
                with
                | Not_found -> Var(info, ty, v)
            end
        | Const(info, ty, c) -> Const(info, ty, c)
    ;;

    let lift_fn info ty (args: Common.Arg.t list) x lifted globals =
        let name = Common.Var.generate () in
        let m = freevars globals Common.Var.Map.empty x in
        let vs = Common.Var.Map.fold
            (fun v ty lst -> (ty, v) :: lst) m []
        in
        match vs with
        | [] ->
            let lifted = (name, info, ty, args, x) :: lifted in
            lifted, Var(info, ty, name)
        | [ a ] ->
            let new_name = Common.Var.generate () in
            let x = rename (Common.Var.Map.singleton (snd a) new_name) x in
            let ty' = Type.Arrow(fst a, ty) in
            let lifted = (name, info, ty', ((fst a), Some new_name) :: args, x)
                                :: lifted
            in
            lifted, (mk_apply info
                            (Var(info, ty', name))
                            (Var(info, fst a, snd a)))
        | _ ->
            let new_names = List.map (fun _ -> Common.Var.generate ())
                                        vs
            in
            let vmap = List.fold_left2
                        (fun m k d -> Common.Var.Map.add (snd k) d m)
                        Common.Var.Map.empty
                        vs
                        new_names
            in
            let x = rename vmap x in
            if ((List.length vs) + (List.length args)) > Config.max_args
            then
                (* Too many args- capture the bound vars in a tuple *)
                let tuple_name = Common.Var.generate () in
                let tuple_ty = Type.Tuple (List.map fst vs) in
                let fn_ty = Type.Arrow(tuple_ty, ty) in
                let tuple_args =
                    List.map2 (fun v n -> (fst v), Some n) vs new_names
                in
                let x = LetTuple(info, get_type x, tuple_args,
                                    Var(info, tuple_ty, tuple_name),
                                    x)
                in
                let lifted = (name, info, fn_ty,
                                ((tuple_ty, Some tuple_name) :: args), x)
                                :: lifted
                in
                lifted,
                    Apply(info, ty, Var(info, fn_ty, name), 
                        Tuple(info, ty,
                                List.map
                                    (fun v -> Var(info, fst v, snd v))
                                    vs))
            else
                (* Apply extra arguments one at a time *)

                let ty' = List.fold_right
                    (fun a t -> Type.Arrow((fst a), t))
                    vs ty
                in
                let new_args = List.map (fun (x, y) -> x, Some y) vs in
                let lifted =
                    (name, info, ty', (List.append new_args args), x)
                    :: lifted
                in
                lifted,
                    List.fold_left
                        (fun f a -> 
                            mk_apply info f
                                        (Var(info, fst a, (snd a))))
                        (Var(info, ty', name))
                        vs
    ;;

    let rec convert globals lifted = function
        (* Flatten lambdas- convert \x -> \y -> ... to \x y -> ... *)
        | Alpha.Expr.Lambda(info, ty, args,
                                Alpha.Expr.Lambda(_, _, args', x))
            -> convert globals lifted
                (Alpha.Expr.Lambda(info, ty, (List.append args args'), x))

        | Alpha.Expr.Lambda(info, ty, args, x) ->
            begin
                let (lifted, x) = convert globals lifted x in
                lift_fn info ty args x lifted globals
            end
        | Alpha.Expr.Let(info, ty, v, x, y) ->
            let lifted, x = convert globals lifted x in
            let lifted, y = convert globals lifted y in
            lifted, Let(info, ty, v, x, y)
        | Alpha.Expr.LetTuple(info, ty, vs, x, y) ->
            let lifted, x = convert globals lifted x in
            let lifted, y = convert globals lifted y in
            lifted, LetTuple(info, ty, vs, x, y)
        | Alpha.Expr.If(info, ty, x, y, z) ->
            let lifted, x = convert globals lifted x in
            let lifted, y = convert globals lifted y in
            let lifted, z = convert globals lifted z in
            lifted, If(info, ty, x, y, z)
        | Alpha.Expr.Tuple(info, ty, xs) ->
            let lifted, xs =
                List.fold_left
                    (fun (lifted, xs) x ->
                        let lifted, x = convert globals lifted x in
                        lifted, (x :: xs))
                    (lifted, [])
                    xs
            in
            lifted, Tuple(info, ty, List.rev xs)
        | Alpha.Expr.BinOp(info, ty, x, op, y) ->
            let lifted, x = convert globals lifted x in
            let lifted, y = convert globals lifted y in
            lifted, BinOp(info, ty, x, op, y)
        | Alpha.Expr.UnOp(info, ty, op, x) ->
            let lifted, x = convert globals lifted x in
            lifted, UnOp(info, ty, op, x)
        | Alpha.Expr.Apply(info, ty, f, x) ->
            let lifted, f = convert globals lifted f in
            let lifted, x = convert globals lifted x in
            lifted, Apply(info, ty, f, x)
        | Alpha.Expr.Var(info, ty, v) ->
            lifted, Var(info, ty, v)
        | Alpha.Expr.Const(info, ty, c) ->
            lifted, Const(info, ty, c)

    ;;

end;;

type t =
    | TopFun of Info.t * Type.t * Common.Var.t * (Common.Arg.t list) * Expr.t
    | TopVar of Info.t * Type.t * Common.Var.t * Expr.t
    | TopExpr of Info.t * Type.t * Expr.t
    with sexp
;;

let rec make_fun info ty name args x =
    let n = List.length args in
    if (n <= Config.max_args) then
        [ TopFun(info, ty, name, args, x) ]
    else
        let c = n mod Config.max_args in
        let c = if (c == 0) then Config.max_args else
                    if (c == 1) then 2 else c
        in
        let taken_args, rem_args = Utils.take_drop c args in
        let inner_fn_name = Common.Var.generate () in
        let rec drop_arrows n = function
            | Type.Arrow(f, x) ->
                if (n == 1) then x else drop_arrows (n - 1) x
            | _ -> assert false
        in
        let inner_ty = drop_arrows c ty in
        let new_arg_names =
            List.map (fun _ -> Common.Var.generate ()) taken_args
        in
        let new_args =
            List.map2 (fun v n -> (fst v), Some n) taken_args new_arg_names
        in
        let tuple_arg_name = Common.Var.generate () in
        let tuple_type = Type.Tuple (List.map fst taken_args) in
        let y = Expr.Apply(info, ty,
                            (Expr.Var(info, inner_ty, inner_fn_name)),
                            (Expr.Tuple(info, tuple_type,
                                            List.map
                                                (fun (t, v) ->
                                                    Expr.Var(info, t,
                                                                from_some v))
                                                new_args)))
        in
        let rem_args = (tuple_type, Some tuple_arg_name) :: rem_args in
        let x =
            Expr.LetTuple(info, Expr.get_type x, taken_args,
                            Expr.Var(info, tuple_type, tuple_arg_name),
                            x)
        in
        (TopFun(info, ty, name, new_args, y)) :: 
            make_fun info inner_ty inner_fn_name rem_args x
;;

let rec convert globals = function
    | Alpha.Top(info, ty, None, Alpha.Expr.Lambda(_, _, _, _)) ->
        (* Code of the form let _ = \x -> ... doesn't do anything
         * worthwhile- elide it.
         *)
        globals, []

    (* Flatten lambdas- convert \x -> \y -> ... to \x y -> ... *)
    | Alpha.Top(info, ty, v,
                    Alpha.Expr.Lambda(info', ty', args,
                        Alpha.Expr.Lambda(_, _, args', x)))
    -> convert globals
            (Alpha.Top(info, ty, v, Alpha.Expr.Lambda(info', ty',
                                            (List.append args args'), x)))

    | Alpha.Top(info, ty, Some(v), Alpha.Expr.Lambda(_, _, args, x)) ->
        let lifted, x = Expr.convert globals [] x in
        (Common.Var.Set.add v globals),
        List.fold_left
            (fun fns (name, info, ty, args, x) ->
                List.append
                    (make_fun info ty name args x)
                    fns)
            (make_fun info ty v args x)
            lifted

    | Alpha.Top(info, ty, Some(v), x) ->
        let lifted, x = Expr.convert globals [] x in
        globals,
        List.fold_left
            (fun fns (name, info, ty, args, x) ->
                (TopFun(info, ty, name, args, x)) :: fns)
            [ TopVar(info, ty, v, x) ]
            lifted

    | Alpha.Top(info, ty, None, x) ->
        let lifted, x = Expr.convert globals [] x in
        globals,
        List.fold_left
            (fun fns (name, info, ty, args, x) ->
                (TopFun(info, ty, name, args, x)) :: fns)
            [ TopExpr(info, ty, x) ]
            lifted

;;


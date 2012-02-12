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

    let mk_apply info f x =
        let ty =
            match f with
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
        in
        match ty with
        | Type.Arrow(_, b) -> Apply(info, b, f, x)
        | _ -> assert false
    ;;

    let from_some = function
        | Some x -> x
        | None -> assert false
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
                let name = Common.Var.generate () in
                let m = freevars globals Common.Var.Map.empty x in
                let vs = Common.Var.Map.fold
                    (fun v ty lst -> (ty, Some v) :: lst) m []
                in
                match vs with
                | [] ->
                    let lifted = (name, info, ty, args, x) :: lifted in
                    lifted, Var(info, ty, name)
                | [ a ] ->
                    let ty' = Type.Arrow(fst a, ty) in
                    let lifted = (name, info, ty', a :: args, x) :: lifted in
                    lifted, (mk_apply info
                                    (Var(info, ty', name))
                                    (Var(info, (fst a), from_some (snd a))))
                | _ ->
                    let ty' = List.fold_right
                        (fun a t -> Type.Arrow((fst a), t))
                        vs ty
                    in
                    let lifted =
                        (name, info, ty', (List.append vs args), x) :: lifted
                    in
                    lifted,
                        List.fold_left
                            (fun f a -> 
                                mk_apply info f
                                            (Var(info, fst a,
                                                        from_some (snd a))))
                            (Var(info, ty', name))
                            vs
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
                (TopFun(info, ty, name, args, x)) :: fns)
            [ TopFun(info, ty, v, args, x) ]
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


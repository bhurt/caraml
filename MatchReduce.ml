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

module P : sig

    type typed_var = Common.Var.t * Common.VarType.t;;

    type t =
        | Undefined
        | Apply of typed_var * (typed_var list)
        | LetTuple of (typed_var list) * typed_var * t
        | Case of typed_var *
                    ((Common.Var.t * (typed_var list) * t) list)
        | Test of typed_var * (typed_var list) * t * t
        | Let of typed_var * typed_var * (typed_var list) * t
        | LetVar of typed_var * Common.Var.t * t
    ;;

    type v =
        | Constructor of (Common.Var.t * (Common.Var.t list))
        | Tuple of Common.Var.t list
    ;;

    val or_patterns : t -> t -> t;;

    val simplify : t -> t;;

    val has_undefined_patterns : t -> bool;;

    val undefined_patterns : t -> v Common.Var.Map.t list;;

    val format_undefined_pattern :
        v Common.Var.Map.t -> Common.Var.t -> int -> unit;;

end = struct

    type typed_var = Common.Var.t * Common.VarType.t;;

    type t =
        | Undefined
        | Apply of typed_var * (typed_var list)
        | LetTuple of (typed_var list) * typed_var * t
        | Case of typed_var *
                    ((Common.Var.t * (typed_var list) * t) list)
        | Test of typed_var * (typed_var list) * t * t
        | Let of typed_var * typed_var * (typed_var list) * t
        | LetVar of typed_var * Common.Var.t * t
    ;;

    type v =
        | Constructor of (Common.Var.t * (Common.Var.t list))
        | Tuple of Common.Var.t list
    ;;

    let rec or_patterns x y =
        match x with
        | Undefined -> y
        | Apply(_, _) -> x
        | LetTuple(args, v, p) -> LetTuple(args, v, (or_patterns p y))
        | Case(v, ps)
            -> Case(v, List.map
                            (fun (t, xs, p) -> (t, xs, or_patterns p y))
                            ps)
        | Test(f, xs, p1, p2)
            -> Test(f, xs, (or_patterns p1 y), (or_patterns p2 y))
        | Let(v, f, xs, p) -> Let(v, f, xs, (or_patterns p y))
        | LetVar(v, w, p) -> LetVar(v, w, or_patterns p y)
    ;;

    let simplify x =
        let rename renames v =
            try
                Common.Var.Map.find v renames
            with
            | Not_found -> v
        in
        let rec loop renames known_tuples known_constructors = function
            | Undefined -> Undefined
            | Apply(lab, vs) ->
                let vs =
                    List.map
                        (fun (n, t) -> (rename renames n), t)
                        vs
                in
                Apply(lab, vs)
            | LetTuple (vs, (x, t), p) ->
                begin
                    let x = rename renames x in
                    match
                        try
                            Some(Common.Var.Map.find x known_tuples)
                        with
                        | Not_found -> None
                    with
                    | Some ws ->
                        let renames =
                            List.fold_left2
                                (fun renames (v, _) (w, _) ->
                                    Common.Var.Map.add v w renames)
                                renames
                                vs
                                ws
                        in
                        loop renames known_tuples known_constructors p
                    | None ->
                        let known_tuples =
                            Common.Var.Map.add x vs known_tuples
                        in
                        let p =
                            loop renames known_tuples known_constructors p
                        in
                        LetTuple(vs, (x, t), p)
                end
            | Case((x, t), ds) ->
                begin
                    let x = rename renames x in
                    match
                        try
                            Some(Common.Var.Map.find x known_constructors)
                        with
                        | Not_found -> None
                    with
                    | Some(tag, ws) ->
                        let (_, vs, p) =
                            try
                                List.find
                                    (fun (t, _, _) ->
                                        (Common.Var.compare t tag) == 0)
                                    ds
                            with
                            | Not_found -> assert false
                        in
                        let renames =
                            List.fold_left2
                                (fun renames (v, _) (w, _) ->
                                    Common.Var.Map.add v w renames)
                                renames
                                vs
                                ws
                        in
                        loop renames known_tuples known_constructors p
                    | None ->
                        let ds =
                            List.map
                                (fun (tag, vs, p) ->
                                    let p = loop renames known_tuples
                                                (Common.Var.Map.add
                                                    x (tag, vs)
                                                    known_constructors)
                                                p
                                    in
                                    (tag, vs, p))
                                ds
                        in
                        Case((x, t), ds)
                end
            | Test(f, vs, p1, p2) ->
                let vs = List.map (fun (n, t) -> rename renames n, t) vs in
                let p1 = loop renames known_tuples known_constructors p1 in
                let p2 = loop renames known_tuples known_constructors p2 in
                Test(f, vs, p1, p2)
            | Let(v, f, xs, p) ->
                let xs = List.map (fun (n, t) -> rename renames n, t) xs in
                let p = loop renames known_tuples known_constructors p in
                Let(v, f, xs, p)
            | LetVar((v, t), w, p) ->
                let renames = Common.Var.Map.add v w renames in
                loop renames known_tuples known_constructors p
        in
        loop Common.Var.Map.empty Common.Var.Map.empty Common.Var.Map.empty x
    ;;

    let rec has_undefined_patterns = function
        | Undefined -> true
        | Apply(_, _) -> false
        | LetTuple(_, _, p) -> has_undefined_patterns p
        | Case(_, ds) ->
            List.exists
                (fun (_, _, p) -> has_undefined_patterns p)
                ds
        | Test(_, _, p1, p2) ->
            (has_undefined_patterns p1) || (has_undefined_patterns p2)
        | Let(_, _, _, p) -> has_undefined_patterns p
        | LetVar(_, _, p) -> has_undefined_patterns p
    ;;

    let rec undefined_patterns = function
        | Undefined -> [ Common.Var.Map.empty ]
        | Apply(_, _) -> []
        | LetTuple(vs, (x, _), p) ->
            let r = undefined_patterns p in
            (* Avoid allocating the closure if we don't need it. *)
            if (r == []) then
                []
            else
                List.map (Common.Var.Map.add x (Tuple (List.map fst vs))) r
        | Case((x, _), ds) ->
            Utils.mapcat
                (fun (t, xs, p) ->
                    (* Avoid allocating the closure if we don't need it. *)
                    let r = undefined_patterns p in
                    if (r == []) then
                        []
                    else
                        List.map (Common.Var.Map.add x
                                        (Constructor(t, List.map fst xs))) r)
                ds
        | Test(_, _, p1, p2) ->
            List.append (undefined_patterns p1) (undefined_patterns p2)
        | Let(_, _, _, p) -> undefined_patterns p
        | LetVar(_, _, p) -> undefined_patterns p
    ;;

    let format_undefined_pattern patt v indent =
        let rec loop indent v =
            match
                try
                    Some(Common.Var.Map.find v patt)
                with
                | Not_found -> None
            with
            | None -> Format.print_string "_"
            | Some(Constructor(tag, xs)) ->
                if (xs == []) then
                    Format.print_string (Common.Var.orig_name tag)
                else
                    begin
                        Format.open_box indent;
                        Format.print_string "(";
                        Format.print_string (Common.Var.orig_name tag);
                        List.iter
                            (fun x ->
                                Format.print_space ();
                                loop (indent + 4) x)
                            xs;
                        Format.print_string ")";
                        Format.close_box ()
                    end
            | Some(Tuple(xs)) ->
                begin
                    Format.open_box indent;
                    Format.print_string "(";
                    let _ =
                        List.fold_left
                            (fun need_space x ->
                                if need_space then
                                    Format.print_space ();
                                loop (indent + 4) x;
                                true)
                            false
                            xs
                    in
                    Format.print_string ")";
                    Format.close_box ()
                end
        in
        loop indent v
    ;;

end;;

type constructors_t =
    (Common.VarType.t list) Common.Var.Map.t Common.Var.Map.t
;;

module rec Lambda : sig

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: Common.Arg.t list;
        body: Expr.t;
    } with sexp;;

    val convert : constructors_t -> Alpha.Lambda.t -> t;;

end = struct

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: Common.Arg.t list;
        body: Expr.t;
    } with sexp;;

    let convert constructors lambda =
        {   info = lambda.Alpha.Lambda.info;
            typ = lambda.Alpha.Lambda.typ;
            name = lambda.Alpha.Lambda.name;
            args = lambda.Alpha.Lambda.args;
            body = Expr.convert constructors lambda.Alpha.Lambda.body;
        }
    ;;


end and Expr : sig

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | Case of t * ((Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of t list
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info: Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    val convert : constructors_t -> Alpha.Expr.t -> t;;

end = struct

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | Case of t * ((Common.Var.t * (Common.Arg.t list) * t) list)
        | Tuple of t list
        | BinOp of t * Common.BinOp.t * t
        | UnOp of Common.UnOp.t * t
        | Apply of t * t
        | Var of Common.Var.t
        | Const of Common.Const.t
    and t = {
        info: Info.t;
        typ: Common.VarType.t;
        body: s;
    } with sexp;;

    let bound_vars expr =
        let rec loop s expr =
            match expr.Alpha.Pattern.body with
            | Alpha.Pattern.Discard -> s
            | Alpha.Pattern.Variable(v) ->
                Common.Var.Map.add v expr.Alpha.Pattern.match_type s
            | Alpha.Pattern.Tuple(ps)
            | Alpha.Pattern.Constructor(_, ps) ->
                List.fold_left loop s ps
            | Alpha.Pattern.Or(_, y) -> loop s y
            | Alpha.Pattern.When(p, _) -> loop s p
            | Alpha.Pattern.With(p, ds) ->
                List.fold_left
                    (fun s (n, x) -> Common.Var.Map.add n x.Alpha.Expr.typ s)
                    (loop s p)
                    ds
            | Alpha.Pattern.As(p, v) ->
                loop (Common.Var.Map.add v expr.Alpha.Pattern.match_type s) p
        in
        loop Common.Var.Map.empty expr
    ;;

    let used_vars vars expr =
        let add s v t =
            if (Common.Var.Map.mem v vars) then
                Common.Var.Map.add v t s
            else
                s
        in
        let rec loop s expr =
            match expr.body with
            | Lambda(_, x) -> loop s x
            | Let(_, x, y)
            | LetTuple(_, x, y)
                -> loop (loop s x) y
            | LetRec(defns, x) ->
                let s =
                    List.fold_left
                        (fun s lambda -> loop s lambda.Lambda.body)
                        s
                        defns
                in
                loop s x
            | If(x, y, z) -> loop (loop (loop s x) y) z
            | Case(x, pats) ->
                let s = loop s x in
                List.fold_left
                        (fun s (_, _, p) -> loop s p)
                        s
                        pats
            | Tuple(xs) ->
                List.fold_left loop s xs
            | BinOp(x, _, y) -> loop (loop s x) y
            | UnOp(_, x) -> loop s x
            | Apply(x, y) -> loop (loop s x) y
            | Var(v) -> add s v expr.typ
            | Const(_) -> s
        in
        loop Common.Var.Map.empty expr
    ;;

    let rec rename renames expr =
        let body =
            match expr.body with
            | Lambda(xs, x) -> Lambda(xs, rename renames x)
            | Let(v, x, y) -> Let(v, rename renames x, rename renames y)
            | LetRec(defns, x) ->
                let defns =
                    List.map
                        (fun (l: Lambda.t) ->
                            { l with Lambda.body =
                                            rename renames l.Lambda.body })
                        defns
                in
                LetRec(defns, rename renames x)
            | If(x, y, z) ->
                let x = rename renames x in
                let y = rename renames y in
                let z = rename renames z in
                If(x, y, z)
            | LetTuple(args, x, y) ->
                let x = rename renames x in
                let y = rename renames y in
                LetTuple(args, x, y)
            | Case(x, ys) ->
                let x = rename renames x in
                let ys =
                    List.map
                        (fun (t, xs, y) -> t, xs, rename renames y)
                        ys
                in
                Case(x, ys)
            | Tuple(xs) -> Tuple (List.map (rename renames) xs)
            | BinOp(x, op, y) ->
                let x = rename renames x in
                let y = rename renames y in
                BinOp(x, op, y)
            | UnOp(op, x) -> UnOp(op, rename renames x)
            | Apply(x, y) ->
                let x = rename renames x in
                let y = rename renames y in
                Apply(x, y)
            | Var(v) ->
                begin
                    try
                        let v = Common.Var.Map.find v renames in
                        Var(v)
                    with
                    | Not_found -> Var(v)
                end
            | Const(_) as x -> x
        in
        { expr with body = body }
    ;;

    let incomplete_pattern_match info patterns v =
        let f indent =
            Format.print_string "Incomplete pattern matching. ";
            Format.print_space ();
            Format.print_string "Here are the patterns not matched:";
            List.iter
                (fun pat ->
                    Format.print_newline ();
                    P.format_undefined_pattern pat v indent)
                patterns
        in
        raise (Error.Compiler_error(f, info))
    ;;

    let make_var expr (name, ty) = { expr with typ = ty; body = Var(name) };;

    let make_apply expr f x =
        let typ =
            match f.typ with
            | Type.Arrow(_, r) -> r
            | _ -> assert false
        in
        { expr with typ = typ; body = Apply(f, x) }
    ;;

    let rec compile_pattern expr = function
        | P.Undefined -> assert false
        | P.Apply(f, bindings) ->
            (List.fold_left (fun f x -> make_apply expr f (make_var expr x))
                (make_var expr f) bindings).body
        | P.LetTuple(args, v, p) ->
            let p = { expr with body = compile_pattern expr p } in
            let v = make_var expr v in
            let args = List.map (fun (n, t) -> (t, Some n)) args in
            LetTuple(args, v, p)
        | P.Case(m, defns) ->
            let m = make_var expr m in
            let defns =
                List.map
                    (fun (tag, args, p) ->
                        let p = { expr with body = compile_pattern expr p } in
                        let args = List.map (fun (n, t) -> t, Some n) args in
                        (tag, args, p))
                defns
            in
            Case(m, defns)
        | P.Test((f, t), args, p1, p2) ->
            let p1 = { expr with body = compile_pattern expr p1 } in
            let p2 = { expr with body = compile_pattern expr p2 } in
            let args =
                if (args == []) then
                    [ { expr with typ = Type.Base Type.Unit;
                            body = Const Common.Const.Unit } ]
                else
                    List.map (make_var expr) args
            in
            let f, t = List.fold_left
                            (fun (f, t) x ->
                                Apply({expr with body = f; typ = t}, x),
                                match t with
                                    | Type.Arrow(_, r) -> r
                                    | _ -> assert false)
                            (Var(f), t)
                            args
            in
            If({ expr with body = f; typ = t }, p1, p2)
        | P.Let((v,t), (f, fty), args, p) ->
            let p = compile_pattern expr p in
            let f, t = List.fold_left
                            (fun (f, t) (x, xty) ->
                                Apply({expr with body = f; typ = t },
                                        {expr with body = Var(x); typ = xty }),
                                match t with
                                | Type.Arrow(_, r) -> r
                                | _ -> assert false)
                            (Var(f), fty)
                            args
            in
            Let((t, Some v), { expr with body = f; typ = t },
                                { expr with body = p })
        | P.LetVar((v, t), w, p) ->
            let p = compile_pattern expr p in
            Let((t, Some v), { expr with body = Var(w); typ = t },
                        { expr with body = p })
    ;;

    let make_wrapper expr bound =
        let used = used_vars bound expr in
        let apply_bindings = Common.Var.Map.bindings used in
        let fn_name = Common.Var.generate () in
        let fn_args =
            List.map
                (fun (n, t) -> (Common.Var.derived n), t)
                apply_bindings
        in
        let f_type = Type.fn_type (List.map snd fn_args) expr.typ in
        let renames =
            List.fold_left2
                (fun m (n1, _) (n2, _) -> Common.Var.Map.add n1 n2 m)
                Common.Var.Map.empty
                apply_bindings
                fn_args
        in
        let expr = rename renames expr in
        let fn_args = List.map (fun (n, ty) -> ty, Some n) fn_args in
        let wrapper =
            (fun y z ->
                Let((f_type, Some fn_name),
                    { y with typ = f_type;
                        body = Lambda(fn_args, expr) },
                    { y with body = z }))
        in
        wrapper, (fn_name, f_type), apply_bindings
    ;;

    let rec convert constructors expr =
        let info = expr.Alpha.Expr.info in
        let typ = expr.Alpha.Expr.typ in
        let body =
            match expr.Alpha.Expr.body with
            | Alpha.Expr.Lambda(args, x) ->
                Lambda(args, convert constructors x)
            | Alpha.Expr.Let(v, x, y) ->
                Let(v, convert constructors x, convert constructors y)
            | Alpha.Expr.LetRec(defns, x) ->
                let defns = List.map (Lambda.convert constructors) defns in
                LetRec(defns, convert constructors x)
            | Alpha.Expr.If(x, y, z) ->
                let x = convert constructors x in
                let y = convert constructors y in
                let z = convert constructors z in
                If(x, y, z)
            | Alpha.Expr.Match(x, ps) ->
                let x = convert constructors x in
                let x_name = Common.Var.generate () in
                let wrappers, pat =
                    List.fold_right
                        (convert_bindings constructors x.typ x_name)
                        ps
                        ([], P.Undefined)
                in
                if (P.has_undefined_patterns pat) then
                    incomplete_pattern_match info (P.undefined_patterns pat)
                        x_name
                else
                    (List.fold_left (fun z f -> f x z)
                        (Let((x.typ, Some(x_name)), x,
                            { x with body = (compile_pattern x pat) }))
                        wrappers)
            | Alpha.Expr.Tuple(xs) ->
                Tuple(List.map (convert constructors) xs)
            | Alpha.Expr.BinOp(x, op, y) ->
                let x = convert constructors x in
                let y = convert constructors y in
                BinOp(x, op, y)
            | Alpha.Expr.UnOp(op, x) ->
                let x = convert constructors x in
                UnOp(op, x)
            | Alpha.Expr.Apply(x, y) ->
                let x = convert constructors x in
                let y = convert constructors y in
                Apply(x, y)
            | Alpha.Expr.Var(v) -> Var(v)
            | Alpha.Expr.Const(c) -> Const(c)
        in
        { info; typ; body }
    and convert_bindings constructors x_typ x_name (p, y) (wrappers, pat) =
        let y = convert constructors y in
        let wrapper, f, xs = make_wrapper y (bound_vars p) in
        let wrappers, p = create_pattern (wrapper :: wrappers) constructors
                                x_typ x_name (P.Apply(f, xs)) p
        in
        wrappers, P.or_patterns p pat
    and create_pattern wrappers constructors x_typ x_name base_pattern p =
        match p.Alpha.Pattern.body with
        | Alpha.Pattern.Discard -> wrappers, base_pattern
        | Alpha.Pattern.Variable(v) ->
            wrappers, P.LetVar((v, x_typ), x_name, base_pattern)
        | Alpha.Pattern.Tuple(ts) ->
            let tvars = List.map
                (fun t -> (Common.Var.generate ()),
                            t.Alpha.Pattern.match_type)
                ts
            in
            let wrappers, base_pattern =
                List.fold_right2
                    (fun t (n, ty) (wrappers, base) ->
                        create_pattern wrappers constructors ty n base t)
                    ts
                    tvars
                    (wrappers, base_pattern)
            in
            wrappers, P.LetTuple(tvars, (x_name, x_typ), base_pattern)
        | Alpha.Pattern.Constructor(tag, ps) ->
            let type_def =
                match p.Alpha.Pattern.match_type with
                | Type.Named(n) ->
                    begin
                        try
                            Common.Var.Map.find n constructors
                        with
                        | Not_found -> assert false
                    end
                | _ -> assert false
            in
            let wrappers, cases =
                Common.Var.Map.fold
                    (fun ctag tys (wrappers, cases) ->
                        let vs = List.map
                                    (fun ty -> (Common.Var.generate ()), ty)
                                    tys
                        in
                        if (Common.Var.compare tag ctag) == 0 then
                            let wrappers, r =
                                List.fold_right2
                                    (fun (v, ty) p (wrappers, r) ->
                                        create_pattern wrappers constructors
                                            ty v r p)
                                    vs ps (wrappers, base_pattern)
                            in
                            wrappers, ((ctag, vs, r) :: cases)
                        else
                            wrappers, ((ctag, vs, P.Undefined) :: cases))
                    type_def
                    (wrappers, [])
            in
            wrappers, (P.Case((x_name, x_typ), cases))

        | Alpha.Pattern.Or(a, b) ->
            let wrappers, a = create_pattern wrappers constructors
                                                x_typ x_name base_pattern a
            in
            let wrappers, b = create_pattern wrappers constructors
                                                x_typ x_name base_pattern b
            in
            wrappers, P.or_patterns a b

        | Alpha.Pattern.When(p, expr) ->
            let expr = convert constructors expr in
            let wrapper, f, xs = make_wrapper expr (bound_vars p) in
            create_pattern (wrapper :: wrappers)
                            constructors x_typ x_name
                                (P.Test(f, xs, base_pattern,
                                            P.Undefined))
                                p
        | Alpha.Pattern.With(p, defns) ->
            let bound = bound_vars p in
            let wrappers, base_pattern =
                List.fold_right
                    (fun (v, y) (wrappers, base_pattern) ->
                        let y = convert constructors y in
                        let wrapper, f, xs = make_wrapper y bound in
                        (wrapper :: wrappers),
                            (P.Let((v, y.typ), f, xs, base_pattern)))
                    defns
                    (wrappers, base_pattern)
            in
            create_pattern wrappers constructors x_typ x_name base_pattern p

        | Alpha.Pattern.As(p, v) ->
            create_pattern wrappers constructors x_typ x_name
                (P.LetVar((v, x_typ), x_name, base_pattern))
                p
    ;;

end;;

type s =
    | Top of Common.VarType.t * Common.Var.t option * Expr.t
    | TopRec of (Lambda.t list)
    | Extern of Common.Var.t * Common.Var.t Common.External.t
    | VariantDef of Common.Var.t
                * ((Info.t * Common.Var.t * (Common.VarType.t list)) list)
and t = {
    info: Info.t;
    body: s;
} with sexp;;

let convert constructors top =
    let info = top.Alpha.info in
    let constructors, body =
        match top.Alpha.body with
        | Alpha.Top(ty, n, x) -> constructors,
                                    (Top(ty, n, Expr.convert constructors x))
        | Alpha.TopRec(lambdas) -> constructors,
                                    (TopRec(
                                        List.map (Lambda.convert constructors)
                                            lambdas))
        | Alpha.Extern(n, x) -> constructors, Extern(n, x)
        | Alpha.VariantDef(n, defns) ->
            Common.Var.Map.add n
                (List.fold_left
                    (fun m (_, tag, vals) ->
                        Common.Var.Map.add tag vals m)
                    Common.Var.Map.empty
                    defns)
                constructors,
            VariantDef(n, defns)
    in
    constructors, { info; body }
;;

module C : IL.Conversion with type input = Alpha.t and type output = t
= struct
    type input = Alpha.t;;
    type output = t;;
    type state = constructors_t;;
    type check_state = unit;;

    let name = "match-reduce";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = Common.Var.Map.empty;;
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

module Convert : IL.Converter with type output = t
        = IL.Make(Alpha.Convert)(C);;



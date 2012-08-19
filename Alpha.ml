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

let rec convert_type names = function
    | Type.Arrow(x, y) ->
        let x = convert_type names x in
        let y = convert_type names y in
        Type.Arrow(x, y)
    | Type.Tuple(xs) ->
        let xs = List.map (convert_type names) xs in
        Type.Tuple(xs)
    | Type.Named(n) ->
        let n = StringMap.find (Annot.CheckedString.to_string n) names in
        Type.Named(n)
    | Type.Base(b) -> Type.Base(b)
;;

let rename_arg names = function
    | (_, None) -> names
    | (_, Some name) ->
        let v = Common.Var.of_string name in
        StringMap.add name v names
;;

let map_arg names = function
    | (ty, None) ->
        let ty = convert_type names ty in
        (ty, None)
    | (ty, Some name) ->
        let ty = convert_type names ty in
        let v = StringMap.find name names in
        (ty, Some v)
;;

module rec Lambda : sig

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;

    val convert : Common.Var.t StringMap.t -> Annot.Lambda.t list
                            -> (Common.Var.t StringMap.t) * (t list);;

end = struct

    type t = {
        info: Info.t;
        typ: Common.VarType.t;
        name: Common.Var.t;
        args: (Common.Arg.t list);
        body: Expr.t;
    } with sexp;;


    let convert names fns = 
        let names =
            List.fold_left
                (fun names fn ->
                    let n = fn.Annot.Lambda.name in
                    let v = Common.Var.of_string n in
                    StringMap.add n v names)
                names
                fns
        in
        let fns =
            List.map
                (fun fn ->
                    let names' = List.fold_left rename_arg names
                                        fn.Annot.Lambda.args
                    in
                    {
                        info = fn.Annot.Lambda.info;
                        typ = convert_type names fn.Annot.Lambda.typ;
                        name = StringMap.find fn.Annot.Lambda.name names;
                        args = List.map (map_arg names') fn.Annot.Lambda.args;
                        body = Expr.convert names' fn.Annot.Lambda.body;
                    })
                fns
        in
        names, fns
    ;;

end and Pattern : sig

    type s = Pattern of Common.Var.t * (Common.Arg.t list)
    and t = {
        info: Info.t;
        match_type: Common.VarType.t;
        body: s;
    } with sexp;;

    val convert : Common.Var.t StringMap.t -> Annot.Pattern.t
                    -> (Common.Var.t StringMap.t) * t
    ;;

end = struct

    type s = Pattern of Common.Var.t * (Common.Arg.t list)
    and t = {
        info: Info.t;
        match_type: Common.VarType.t;
        body: s;
    } with sexp;;

    let convert names pat =
        let names', body = match pat.Annot.Pattern.body with
            | Annot.Pattern.Pattern(name, args) ->
                let name = StringMap.find name names in
                let names = List.fold_left rename_arg names args in
                let args = List.map (map_arg names) args in
                names, Pattern(name, args)
        in
        names',
        {   info = pat.Annot.Pattern.info;
            match_type = convert_type names pat.Annot.Pattern.match_type;
            body = body }
    ;;

end and Expr : sig

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Match of t * ((Pattern.t * t) list)
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

    val convert: Common.Var.t StringMap.t -> Annot.Expr.t -> t;;

end = struct

    type s =
        | Lambda of Common.Arg.t list * t
        | Let of Common.Arg.t * t * t
        | LetTuple of Common.Arg.t list * t * t
        | LetRec of (Lambda.t list) * t
        | If of t * t * t
        | Match of t * ((Pattern.t * t) list)
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

    let rec convert names x = 
        let body = match x.Annot.Expr.body with

            | Annot.Expr.Lambda(args, x) ->
                let names = List.fold_left rename_arg names args in
                let args = List.map (map_arg names) args in
                let x = convert names x in
                Lambda(args, x)
    
            | Annot.Expr.Let(arg, x, y) ->
                let x = convert names x in
                let names = rename_arg names arg in
                let arg = map_arg names arg in
                let y = convert names y in
                Let(arg, x, y)
    
            | Annot.Expr.LetTuple(args, x, y) ->
                let x = convert names x in
                let names = List.fold_left rename_arg names args in
                let args = List.map (map_arg names) args in
                let y = convert names y in
                LetTuple(args, x, y)
    
            | Annot.Expr.LetRec(fns, x) ->
                let names, fns = Lambda.convert names fns in
                let x = convert names x in
                LetRec(fns, x)
    
            | Annot.Expr.If(x, y, z) ->
                let x = convert names x in
                let y = convert names y in
                let z = convert names z in
                If(x, y, z)
    
            | Annot.Expr.Match(x, bindings) ->
                let x = convert names x in
                let bindings =
                    List.map
                        (fun (pat, y) ->
                            let names, pat = Pattern.convert names pat in
                            let y = convert names y in
                            (pat, y))
                        bindings
                in
                Match(x, bindings)
    
            | Annot.Expr.Tuple(xs) ->
                let xs = List.map (convert names) xs in
                Tuple(xs)
    
            | Annot.Expr.BinOp(x, op, y) ->
                let x = convert names x in
                let y = convert names y in
                BinOp(x, op, y)
    
            | Annot.Expr.UnOp(op, x) ->
                let x = convert names x in
                UnOp(op, x)
    
            | Annot.Expr.Apply(f, x) ->
                let f = convert names f in
                let x = convert names x in
                Apply(f, x)
    
            | Annot.Expr.Var(v) ->
                Var(StringMap.find v names)
    
            | Annot.Expr.Const(c) ->
                Const(c)
    in
    {
        info = x.Annot.Expr.info;
        typ = convert_type names x.Annot.Expr.typ;
        body = body;
    } 
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

let convert names defn = 
    let names, body = match defn.Annot.body with
        | Annot.Top(ty, None, x) ->
            let ty = convert_type names ty in
            let x = Expr.convert names x in
            names, (Top(ty, None, x))
        | Annot.Top(ty, Some v, x) ->
            let ty = convert_type names ty in
            let name = Common.Var.of_string v in
            let x = Expr.convert names x in
            (StringMap.add v name names), (Top(ty, Some name, x))
        | Annot.TopRec(fns) ->
            let names, fns = Lambda.convert names fns in
            names, TopRec(fns)
        | Annot.Extern(v, x) ->
            let name = Common.Var.of_string v in
            let x = { x with
                        Common.External.return_type =
                            convert_type names x.Common.External.return_type;
                        Common.External.arg_types =
                            List.map (convert_type names)
                                x.Common.External.arg_types }
            in
            (StringMap.add v name names), (Extern(name, x))
        | Annot.VariantDef(v, opts) ->
            let name = Common.Var.of_string v in
            let names = StringMap.add v name names in
            let opts = List.map
                        (fun (i, n, ts) ->
                            let n' = Common.Var.of_string n in
                            let ts = List.map (convert_type names) ts in
                            (i, n', ts))
                        opts
            in
            names, (VariantDef(name, opts))
    in
    names,
    {
        info = defn.Annot.info;
        body = body;
    }
;;

module C : IL.Conversion with type input = Annot.t and type output = t
= struct
    type input = Annot.t;;
    type output = t;;
    type state = Common.Var.t StringMap.t;;
    type check_state = unit;;

    let name = "alpha";;
    let sexp_of_output x = sexp_of_t x;;

    let dump_flag = ref false;;
    let check_flag = ref false;;

    let init_state () = StringMap.empty;;
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
        = IL.Make(Annot.Convert)(C);;

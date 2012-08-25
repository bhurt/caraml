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

module Var = struct

    type t = string with sexp;;

    let counter = ref 0;;

    let generate () =
        incr counter;
        let i = !counter in
        Printf.sprintf "_.%d" i
    ;;

    module XMap = Map.Make(String);;

    let seen_names : int XMap.t ref = ref XMap.empty;;

    let of_string s =
        try
            begin
                let c = XMap.find s !seen_names in
                seen_names := XMap.add s (c + 1) !seen_names;
                Printf.sprintf "%s.%d" s (c + 1)
            end
        with
        | Not_found ->
            begin
                seen_names := XMap.add s 1 !seen_names;
                Printf.sprintf "%s.1" s
            end

    let to_string s = s;;

    let orig_name s =
        String.sub s 0 (String.index s '.')
    ;;

    let derived s = of_string (orig_name s);;

    module Map = struct
        include XMap;;

        let t_of_sexp f sexp = 
            let pairs =
                Sexplib.Conv.list_of_sexp
                    (Sexplib.Conv.pair_of_sexp t_of_sexp f) sexp
            in
            List.fold_left (fun m (k, v) -> XMap.add k v m) XMap.empty
                pairs
        ;;

        let sexp_of_t f x =
            let pairs = XMap.bindings x in
            Sexplib.Conv.sexp_of_list
                (Sexplib.Conv.sexp_of_pair sexp_of_t f) pairs
        ;;

    end;;

    module Set = Set.Make(String);;

end;;

module Arg = struct

    type t = Var.t Type.t * (Var.t option) with sexp;;

end;;

module BinOp = struct

    type t =
        (* Integer Ops *)
        | Times
        | Divide
        | Add
        | Subtract
        | Le
        | Ge
        | Lt
        | Gt
        | Eq
        | Ne

        (* Boolean ops *)
        | And
        | Or

        (* Float ops *)
        | FTimes
        | FDivide
        | FAdd
        | FSubtract
        | FLe
        | FGe
        | FLt
        | FGt
        | FEq
        | FNe
        with sexp
    ;;

    let get_types = function

        | Times | Divide | Add | Subtract
            -> (Type.Base(Type.Int), Type.Base(Type.Int),
                Type.Base(Type.Int))

        | Le | Ge | Lt | Gt | Eq | Ne
            -> (Type.Base(Type.Int), Type.Base(Type.Int),
                Type.Base(Type.Boolean))

        | And | Or
            -> (Type.Base(Type.Boolean), Type.Base(Type.Boolean),
                Type.Base(Type.Boolean))

        | FTimes | FDivide | FAdd | FSubtract
            -> (Type.Base(Type.Float), Type.Base(Type.Float),
                Type.Base(Type.Float))

        | FLe | FGe | FLt | FGt | FEq | FNe
            -> (Type.Base(Type.Float), Type.Base(Type.Float),
                Type.Base(Type.Boolean))
    ;;


end;;

module UnOp = struct

    type t =
        | Neg
        | Not
        | FNeg
        with sexp
    ;;

    let get_types = function
        | Neg -> (Type.Base(Type.Int), Type.Base(Type.Int))
        | Not -> (Type.Base(Type.Boolean), Type.Base(Type.Boolean))
        | FNeg -> (Type.Base(Type.Float), Type.Base(Type.Float))
    ;;


end;;


module Const = struct

    type t =
        | Boolean of bool
        | Int of int
        | Float of float
        | Unit
        with sexp
    ;;

    let get_type = function
        | Boolean(_) -> Type.Base(Type.Boolean)
        | Int(_) -> Type.Base(Type.Int)
        | Float(_) -> Type.Base(Type.Float)
        | Unit -> Type.Base(Type.Unit)
    ;;

end;;


module External = struct

    type 'a t = {
        real_name : string;
        return_type : 'a Type.t;
        arg_types : 'a Type.t list;
    } with sexp;;


end;;

module StringType = struct

    type t = string Type.t with sexp;;

end;;

module VarType = struct

    type t = Var.t Type.t with sexp;;

end;;

module Tag = struct

    type t = int with sexp;;

    let of_int (i : int) : t = i;;
    let to_int (i : t) : int = i;;

end;;


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

type base =
    | Int
    | Boolean
    | Unit
    with sexp
;;

type t =
    | Arrow of t * t
    | Tuple of t list
    | Base of base
    with sexp
;;

let rec pprint indent = function
    | Arrow(f, x) ->
        begin
            pprint indent f;
            Format.print_cut ();
            Format.open_box indent;
            Format.print_string " -> ";
            pprint indent x;
            Format.close_box ();
        end
    | Tuple([]) -> assert false
    | Tuple(t :: ts) ->
        begin
            let f t =
                Format.print_string ",";
                Format.print_space ();
                Format.open_box indent;
                pprint indent t;
                Format.close_box ();
                ()
            in
            pprint indent t;
            List.iter f ts
        end
    | Base(Int) -> Format.print_string "int"
    | Base(Boolean) -> Format.print_string "boolean"
    | Base(Unit) -> Format.print_string "unit"
;;


let rec equals t1 t2 : bool =
    match t1, t2 with
    | Arrow(f1, x1), Arrow(f2, x2) ->
        (equals f1 f2) && (equals x1 x2)
    | Tuple(t1s), Tuple(t2s) ->
        begin
            if (List.length t1s) != (List.length t2s) then
                false
            else
                List.for_all2 equals t1s t2s
        end
    | Base(Int), Base(Int)
    | Base(Boolean), Base(Boolean)
    | Base(Unit), Base(Unit)
        -> true
    | _ -> false
;;

let fn_type arg_tys rty =
    List.fold_right (fun f x -> Arrow(f, x)) arg_tys rty
;;


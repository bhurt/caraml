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

module Position = struct

    type t = {
        file_name: string;
        line_num: int;
        bol: int;
        col_num: int;
    } with sexp;;

    let of_lexing_position p = {
        file_name = p.Lexing.pos_fname;
        line_num = p.Lexing.pos_lnum;
        bol = p.Lexing.pos_bol;
        col_num = p.Lexing.pos_cnum;
    };;

end;;


type t = Position.t * Position.t with sexp;;

let from_positions p1 p2 =
    (Position.of_lexing_position p1),
    (Position.of_lexing_position p2)
;;

let pprint indent (loc1, loc2) =
    if (loc1.Position.file_name != "") then
        begin
            Format.print_string loc1.Position.file_name;
            Format.print_string "#"
        end;
    Format.print_int loc1.Position.line_num;
    Format.print_string ".";
    Format.print_int loc1.Position.col_num;
    Format.print_string "-";
    if (loc2.Position.file_name != loc1.Position.file_name) then
        begin
            Format.print_string loc2.Position.file_name;
            Format.print_string "#"
        end;
    Format.print_int loc2.Position.line_num;
    Format.print_string ".";
    Format.print_int loc2.Position.col_num;
    ()
;;



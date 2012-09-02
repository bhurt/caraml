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

exception Compiler_error of (int -> unit) * Info.t;;

let print_error msg loc =
    Format.set_formatter_out_channel stderr;
    Format.print_string "ERROR: ";
    Format.open_box 4;
    Info.pprint 4 loc;
    Format.print_string ": ";
    Format.close_box ();
    Format.open_box 4;
    msg 4;
    Format.close_box ();
    Format.print_newline ();
    ()
;;
    


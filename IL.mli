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

module type Converter = sig
    type output;;
    type state;;

    val cmd_args : (Arg.key * Arg.spec * Arg.doc) list;;
    val init_state : dump_all:bool -> file_name:string -> state;;
    val convert : state -> AST.t -> (state * (output list));;
    val fini_state : state -> unit;;
end;;

module type Conversion = sig
    type input;;
    type output;;
    type state;;

    val name : string;;
    val sexp_of_output : output -> Sexplib.Sexp.t;;
    val dump_flag : bool ref;;
    val init_state : unit -> state;;
    val convert : state -> input -> (state * (output list));;
    val fini_state : state -> unit;;
end;;

module Make(I: Converter)(M: Conversion with type input=I.output)
    : Converter with type output = M.output;;

module Base : Converter with type output = AST.t;;




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

module StringMap = Map.Make(String);;

let rec repl env lexbuf =
    print_string "> ";
    flush stdout;
    match Parser.top_level Lexer.token lexbuf with
    | None -> repl env lexbuf
    | Some t ->
        Sexplib.Sexp.output_hum stdout (AST.sexp_of_t t);
        print_newline();
        flush stdout;
        match t with
            | AST.Top(p, None, x) -> repl env lexbuf
            | AST.Top(p, Some(v), x) ->
                repl (StringMap.add v (p, x) env) lexbuf
in
repl StringMap.empty (Lexing.from_channel stdin);;



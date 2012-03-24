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

{

module StringMap = Map.Make(String);;

let keywords : Parser.token StringMap.t =
    List.fold_left
        (fun m (k,v) -> StringMap.add k v m)
        StringMap.empty
        [   "and", Parser.AND;
            "boolean", Parser.BOOLEAN;
            "bool", Parser.BOOLEAN;
            "else", Parser.ELSE;
            "false", Parser.BOOLEAN_VAL(false);
            "fn", Parser.LAMBDA;
            "fun", Parser.LAMBDA;
            "if", Parser.IF;
            "in", Parser.IN;
            "integer", Parser.INT;
            "int", Parser.INT;
            "lambda", Parser.LAMBDA;
            "let", Parser.LET;
            "not", Parser.NOT;
            "or", Parser.OR;
            "then", Parser.THEN;
            "true", Parser.BOOLEAN_VAL(true);
            "unit", Parser.UNIT;
            "_", Parser.DISCARD ]
;;

let keyword lexbuf =
    let s = Lexing.lexeme lexbuf in
    try
        StringMap.find s keywords
    with
    | Not_found -> Parser.VAR s
;;

}

let blank = [ ' ' '\009' '\010' '\012' '\013' ]
let lowercase = [ 'a'-'z' '_' ]
let uppercase = [ 'A'-'Z' ]
let digit = [ '0'-'9' ]
let prime = '\'' 
let identchar = (lowercase | uppercase | digit | prime)

rule token = parse
    | blank+ { token lexbuf }
    | digit+ {
            Parser.INT_VAL (int_of_string (Lexing.lexeme lexbuf))
        } 

    | (uppercase | lowercase) identchar* { keyword lexbuf }
    | "&&" { Parser.AND }
    | "->" { Parser.ARROW }
    | ")" { Parser.CLOSE_PAREN }
    | ":" { Parser.COLON }
    | "," { Parser.COMMA }
    | "/" { Parser.DIVIDE }
    | "." { Parser.DOT }
    | ";;" { Parser.DOUBLE_SEMI }
    | "=" { Parser.EQUALS }
    | "==" { Parser.EQUALS_EQUALS }
    | ">=" { Parser.GREATER_EQUALS }
    | ">" { Parser.GREATER_THAN }
    | ".\\" { Parser.LAMBDA }
    | "\\" { Parser.LAMBDA }
    | "<=" { Parser.LESS_EQUALS }
    | "<" { Parser.LESS_THAN }
    | "-" { Parser.MINUS }
    | "~" { Parser.NEGATE }
    | "!" { Parser.NOT }
    | "!=" { Parser.NOT_EQUALS }
    | "/=" { Parser.NOT_EQUALS }
    | "(" { Parser.OPEN_PAREN }
    | "||" { Parser.OR }
    | "+" { Parser.PLUS }
    | "*" { Parser.TIMES }

    | eof { Parser.EOF }
    | _ {
            Printf.printf "Unknown character \"%s\"!\n"
                                    (Lexing.lexeme lexbuf);
            raise Parsing.Parse_error
        }


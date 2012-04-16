/*
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
*/

%{

let info () : Info.t =
    Info.from_positions
        (Parsing.symbol_start_pos ())
        (Parsing.symbol_end_pos ())
;;

let parse_error s =
    let start = Parsing.symbol_start_pos () in
    let stop = Parsing.symbol_end_pos () in
    Printf.printf "%s: %s:%d.%d-%d.%d\n" s start.Lexing.pos_fname
        start.Lexing.pos_lnum start.Lexing.pos_cnum stop.Lexing.pos_lnum
        stop.Lexing.pos_cnum;
    ()
;;

%}

%token <string> VAR
%token <bool> BOOLEAN_VAL
%token <int> INT_VAL

%token EOF

%token AND
%token ARROW
%token BOOL_AND
%token BOOLEAN
%token BOOL_NOT
%token BOOL_OR
%token CLOSE_PAREN
%token COLON
%token COMMA
%token DISCARD
%token DIVIDE
%token DOT
%token DOUBLE_SEMI
%token ELSE
%token EQUALS
%token EQUALS_EQUALS
%token GREATER_EQUALS
%token GREATER_THAN
%token IF
%token IN
%token INT
%token LAMBDA
%token LESS_EQUALS
%token LESS_THAN
%token LET
%token MINUS
%token NEGATE
%token NOT_EQUALS
%token OPEN_PAREN
%token PLUS
%token REC
%token THEN
%token TIMES
%token UNIT

%start top_level
%type <AST.parse_result> top_level

%%

top_level:
      LET var_or_discard EQUALS expr DOUBLE_SEMI {
            AST.Form(AST.Top(info (), $2, $4))
        }
    | LET VAR arglist EQUALS expr DOUBLE_SEMI {
            let i = info () in
            AST.Form(AST.Top(i, Some($2), AST.Expr.Lambda(i, List.rev $3, $5)))
        }
    | LET REC rec_list DOUBLE_SEMI {
            let i = info () in
            AST.Form(AST.TopRec(i, List.rev $3))
        }
    | error DOUBLE_SEMI {
            Printf.printf("Syntax Error.\n");
            AST.SyntaxError
        }
    | EOF { AST.EOF }
    | error EOF { Printf.printf("Syntax Error.\n"); AST.SyntaxError }
;

rec_list:
      rec_defn { [ $1 ] }
    | rec_list AND rec_defn { $3 :: $1 }
;

rec_defn:
      VAR arglist COLON type_expr EQUALS expr {
            info (), $1, List.rev $2, $4, $6
        }
;

var_or_discard:
      VAR { Some $1 }
    | DISCARD { None }
;

arglist:
    | arglist arg { $2 :: $1 }
    | arg { [ $1 ] }
;

arg:
      var_or_discard COLON type_expr { $3, $1 }
;

type_expr:
      tuple_type_expr ARROW type_expr { 
            Type.Arrow($1, $3)
        }
    | tuple_type_expr {
            $1
        }
;

tuple_type_expr:
      tuple_type_list_expr { Type.Tuple(List.rev $1) }
    | base_type_expr { $1 }
;

tuple_type_list_expr:
      tuple_type_list_expr TIMES base_type_expr { $3 :: $1 }
    | base_type_expr TIMES base_type_expr { [ $3; $1 ] }
;

base_type_expr:
      INT { Type.Base(Type.Int) }
    | BOOLEAN { Type.Base(Type.Boolean) }
    | UNIT { Type.Base(Type.Unit) }
    | OPEN_PAREN type_expr CLOSE_PAREN { $2 }
;

expr:
      LET var_or_discard EQUALS expr IN expr {
            AST.Expr.Let(info (), $2, $4, $6)
        }
    | LET tuple_pattern EQUALS expr IN expr {
            AST.Expr.LetTuple(info (), List.rev $2, $4, $6)
        }
    | LET REC rec_list IN expr {
            AST.Expr.LetRec(info (), List.rev $3, $5)
        }
    | LAMBDA arglist DOT expr {
            AST.Expr.Lambda(info (), List.rev $2, $4)
        }
    | IF expr THEN expr ELSE expr {
            AST.Expr.If(info (), $2, $4, $6)
        }
    | tuple_expr { $1 }
;

tuple_pattern:
      tuple_pattern COMMA var_or_discard { $3 :: $1 }
    | var_or_discard COMMA var_or_discard { [ $3; $1 ] }
;

tuple_expr:
      tuple_list_expr {
            AST.Expr.Tuple(info (), List.rev $1)
        }
    | bool_expr { $1 }
;

tuple_list_expr:
      tuple_list_expr COMMA bool_expr { $3 :: $1 }
    | bool_expr COMMA bool_expr { [ $3; $1 ] }
;

bool_expr:
      bool_expr BOOL_OR and_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.Or, $3)
        }
    | and_expr { $1 }
;

and_expr:
      and_expr BOOL_AND not_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.And, $3)
        }
    | not_expr { $1 }
;

not_expr:
      BOOL_NOT not_expr {
            AST.Expr.UnOp(info (), Common.UnOp.Not, $2)
        }
    | comp_expr { $1 }
;

comp_expr:
      times_expr comp_op add_expr {
            AST.Expr.BinOp(info (), $1, $2, $3)
        }
    | add_expr { $1 }
;

comp_op:
      LESS_THAN { Common.BinOp.Lt }
    | GREATER_THAN { Common.BinOp.Gt }
    | LESS_EQUALS { Common.BinOp.Le }
    | GREATER_EQUALS { Common.BinOp.Ge }
    | EQUALS_EQUALS { Common.BinOp.Eq }
    | NOT_EQUALS { Common.BinOp.Ne }
;

add_expr:
      add_expr PLUS times_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.Add, $3)
        }
    | add_expr MINUS times_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.Subtract, $3)
        }
    | times_expr { $1 }
;

times_expr:
      times_expr TIMES neg_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.Times, $3)
        }
    | times_expr DIVIDE neg_expr {
            AST.Expr.BinOp(info (), $1, Common.BinOp.Divide, $3)
        }
    | neg_expr { $1 }
;

neg_expr:
      NEGATE neg_expr {
            AST.Expr.UnOp(info (), Common.UnOp.Neg, $2)
        }
    | apply_expr { $1 }
;

apply_expr:
      apply_expr base_expr {
            AST.Expr.Apply(info (), $1, $2)
        }
    | base_expr { $1 }
;

base_expr:
      VAR { AST.Expr.Var(info (), $1) }
    | BOOLEAN_VAL { AST.Expr.Const(info(), Common.Const.Boolean $1) }
    | INT_VAL { AST.Expr.Const(info(), Common.Const.Int $1) }
    | OPEN_PAREN CLOSE_PAREN { AST.Expr.Const(info(), Common.Const.Unit) }
    | OPEN_PAREN expr CLOSE_PAREN { $2 }
;


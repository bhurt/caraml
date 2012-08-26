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

let expr body = AST.Expr.make (info ()) body;;

%}

%token <string> VAR
%token <string> CVAR
%token <bool> BOOLEAN_VAL
%token <int> INT_VAL
%token <float> FLOAT_VAL

%token EOF

%token AND
%token ARROW
%token AS
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
%token END
%token EQUALS
%token EQUALS_EQUALS
%token FDIVIDE
%token FEQ
%token FGE
%token FGT
%token FLE
%token FLT
%token FMINUS
%token FNE
%token FNEGATE
%token FPLUS
%token FTIMES
%token GREATER_EQUALS
%token GREATER_THAN
%token IF
%token IN
%token INT
%token LAMBDA
%token LESS_EQUALS
%token LESS_THAN
%token LET
%token MATCH
%token MINUS
%token NEGATE
%token NOT_EQUALS
%token OPEN_PAREN
%token PIPE
%token PLUS
%token REC
%token THEN
%token TIMES
%token TYPE
%token UNIT
%token WHEN
%token WITH

%start top_level
%type <AST.parse_result> top_level

%%

top_level:
      LET var_or_discard EQUALS expr DOUBLE_SEMI {
            AST.Form(AST.make (info ()) (AST.Top($2, $4)))
        }
    | LET VAR arglist EQUALS expr DOUBLE_SEMI {
            let i = info () in
            AST.Form(
                AST.make i
                    (AST.Top(Some($2),
                                (AST.Expr.make i
                                    (AST.Expr.Lambda(List.rev $3, $5))))))
        }
    | LET REC rec_list DOUBLE_SEMI {
            AST.Form(AST.make  (info ()) (AST.TopRec(List.rev $3)))
        }
    | TYPE VAR EQUALS var_def DOUBLE_SEMI {
            AST.Form(AST.make (info ()) (AST.VariantDef($2, List.rev $4)))
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
            AST.Lambda.make (info ()) $1 (List.rev $2) $4 $6
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
    | VAR { Type.Named($1) }
    | OPEN_PAREN type_expr CLOSE_PAREN { $2 }
;

expr:
      LET var_or_discard EQUALS expr IN expr {
            expr (AST.Expr.Let($2, $4, $6))
        }
    | LET tuple_pattern EQUALS expr IN expr {
            expr (AST.Expr.LetTuple(List.rev $2, $4, $6))
        }
    | LET REC rec_list IN expr {
            expr (AST.Expr.LetRec(List.rev $3, $5))
        }
    | LAMBDA arglist DOT expr {
            expr (AST.Expr.Lambda(List.rev $2, $4))
        }
    | IF expr THEN expr ELSE expr {
            expr (AST.Expr.If($2, $4, $6))
        }
    | MATCH expr WITH match_list END {
            expr (AST.Expr.Match($2, List.rev $4))
        }
    | tuple_expr { $1 }
;

match_list:
      PIPE match_clause { [ $2 ] }
    | match_clause { [ $1 ] }
    | match_list PIPE match_clause { $3 :: $1 }
;

match_clause:
      pattern ARROW expr { $1, $3 }
;

pattern:
      pattern PIPE as_pattern {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.Or($1, $3) }
        }
    | as_pattern { $1 }
;

as_pattern:
      as_pattern AS VAR { 
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.As($1, $3) }
        }
    | as_pattern WHEN expr {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.When($1, $3) }
        }
    | as_pattern WITH var_defns {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.With($1, List.rev $3) }
        }
    | tuple_match { $1 }
;

var_defns:
      VAR EQUALS expr { [ $1, $3 ] }
    | var_defns AND VAR EQUALS expr { ($3, $5) :: $1 }
;

tuple_match:
      tuple_match COMMA constructor_pattern {
            match $1.AST.Pattern.body with
            | AST.Pattern.Tuple(xs) ->
                {   AST.Pattern.info = info();
                    AST.Pattern.body =
                        AST.Pattern.Tuple(List.append xs [ $3 ]) }
            | x ->
                {   AST.Pattern.info = info ();
                    AST.Pattern.body =
                        AST.Pattern.Tuple([ $1; $3 ]) }
        }
    | constructor_pattern { $1 }
;

constructor_pattern:
      CVAR constructor_args {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.Constructor($1, List.rev $2) }
        }
    | base_pattern { $1 }
;

constructor_args:
      constructor_args base_pattern { $2 :: $1 }
    | base_pattern { [ $1 ] }
;

base_pattern:
      VAR {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.Variable($1) }
        }
    | CVAR {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.Constructor($1, []); }
        }
    | DISCARD {
            {   AST.Pattern.info = info ();
                AST.Pattern.body = AST.Pattern.Discard }
        }
    | OPEN_PAREN pattern CLOSE_PAREN { $2 }
;

tuple_pattern:
      tuple_pattern COMMA var_or_discard { $3 :: $1 }
    | var_or_discard COMMA var_or_discard { [ $3; $1 ] }
;

tuple_expr:
      tuple_list_expr {
            expr (AST.Expr.Tuple(List.rev $1))
        }
    | bool_expr { $1 }
;

tuple_list_expr:
      tuple_list_expr COMMA bool_expr { $3 :: $1 }
    | bool_expr COMMA bool_expr { [ $3; $1 ] }
;

bool_expr:
      bool_expr BOOL_OR and_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.Or, $3))
        }
    | and_expr { $1 }
;

and_expr:
      and_expr BOOL_AND not_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.And, $3))
        }
    | not_expr { $1 }
;

not_expr:
      BOOL_NOT not_expr {
            expr (AST.Expr.UnOp(Common.UnOp.Not, $2))
        }
    | comp_expr { $1 }
;

comp_expr:
      times_expr comp_op add_expr {
            expr (AST.Expr.BinOp($1, $2, $3))
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
    | FEQ { Common.BinOp.FEq }
    | FNE { Common.BinOp.FNe }
    | FLT { Common.BinOp.FLt }
    | FGT { Common.BinOp.FGt }
    | FLE { Common.BinOp.FLe }
    | FGE { Common.BinOp.FGe }
;

add_expr:
      add_expr PLUS times_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.Add, $3))
        }
    | add_expr MINUS times_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.Subtract, $3))
        }
    | add_expr FPLUS times_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.FAdd, $3))
        }
    | add_expr FMINUS times_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.FSubtract, $3))
        }
    | times_expr { $1 }
;

times_expr:
      times_expr TIMES neg_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.Times, $3))
        }
    | times_expr DIVIDE neg_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.Divide, $3))
        }
    | times_expr FTIMES neg_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.FTimes, $3))
        }
    | times_expr FDIVIDE neg_expr {
            expr (AST.Expr.BinOp($1, Common.BinOp.FDivide, $3))
        }
    | neg_expr { $1 }
;

neg_expr:
      NEGATE neg_expr {
            expr (AST.Expr.UnOp(Common.UnOp.Neg, $2))
        }
    | FNEGATE neg_expr {
            expr (AST.Expr.UnOp(Common.UnOp.FNeg, $2))
        }
    | apply_expr { $1 }
;

apply_expr:
      apply_expr base_expr {
            expr (AST.Expr.Apply($1, $2))
        }
    | base_expr { $1 }
;

base_expr:
      VAR { expr (AST.Expr.Var($1)) }
    | CVAR { expr (AST.Expr.Var($1)) }
    | BOOLEAN_VAL { expr (AST.Expr.Const(Common.Const.Boolean $1)) }
    | INT_VAL { expr (AST.Expr.Const(Common.Const.Int $1)) }
    | FLOAT_VAL { expr (AST.Expr.Const(Common.Const.Float $1)) }
    | OPEN_PAREN CLOSE_PAREN { expr (AST.Expr.Const(Common.Const.Unit)) }
    | OPEN_PAREN expr CLOSE_PAREN { $2 }
;

var_def:
      variant { [ $1 ] }
    | PIPE variant { [ $2 ] }
    | var_def PIPE variant { $3 :: $1 }
;

variant:
      CVAR { (info(), $1, []) }
    | CVAR type_list {
            (info (), $1, List.rev $2)
        }
;

type_list:
      type_expr { [ $1 ] }
    | type_list base_type_expr { $2 :: $1 }
;

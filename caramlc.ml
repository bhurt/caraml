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

let dump_ast = ref false;;
let dump_annot = ref false;;
let dump_alpha = ref false;;
let dump_lambda_lift = ref false;;
let dump_simplify = ref false;;
let dump_callopt = ref false;;
let dump_llvm = ref false;;

type dumps_t = {
    ast_out : out_channel option;
    annot_out : out_channel option;
    alpha_out : out_channel option;
    lambda_out : out_channel option;
    simplify_out : out_channel option;
    callopt_out : out_channel option;
};;

type state_t = {
    annot_map : Type.t Annot.StringMap.t;
    alpha_map : Common.Var.t Alpha.StringMap.t;
    lambda_set : Common.Var.Set.t;
    callopt_map : int Common.Var.Map.t;
    init_fns : string list;
};;

let maybe_dump ocopt f x =
    match ocopt with
    | None -> ()
    | Some oc -> Sexplib.Sexp.output_hum oc (f x)
;;

let rec parse_loop lexbuf dumps state =
    match Parser.top_level Lexer.token lexbuf with
    | AST.EOF -> state
    | AST.SyntaxError -> raise Exit;
    | AST.Form ast ->
        let _ = maybe_dump dumps.ast_out AST.sexp_of_t ast in
        let (annot_map, annot) = Annot.convert state.annot_map ast in
        let _ = maybe_dump dumps.annot_out Annot.sexp_of_t annot in
        let (alpha_map, alpha) = Alpha.convert state.alpha_map annot in
        let _ = maybe_dump dumps.alpha_out Alpha.sexp_of_t alpha in
        let (lambda_set, lambdas) =
            LambdaLift.convert  state.lambda_set alpha
        in
        let state =
            List.fold_left
                (fun state lambda ->
                    maybe_dump dumps.lambda_out LambdaLift.sexp_of_t lambda;
                    let simplify = Simplify.convert lambda in
                    maybe_dump dumps.simplify_out Simplify.sexp_of_t simplify;
                    let (callopt_map, callopt) =
                        CallOpt.convert state.callopt_map simplify
                    in
                    maybe_dump dumps.callopt_out CallOpt.sexp_of_t callopt;
                    let init_fn = Assembly.assemble callopt in
                    {
                        annot_map = annot_map;
                        alpha_map = alpha_map;
                        lambda_set = lambda_set;
                        callopt_map = callopt_map;
                        init_fns = init_fn :: state.init_fns;
                    })
                state
                lambdas
        in
        parse_loop lexbuf dumps state
;;

let maybe_out r ext name =
    if !r then
        Some(open_out (name ^ ext))
    else
        None
;;

let make_dumps name = {
    ast_out = maybe_out dump_ast ".ast.sexp" name;
    annot_out = maybe_out dump_annot ".annot.sexp" name;
    alpha_out = maybe_out dump_alpha ".alpha.sexp" name;
    lambda_out = maybe_out dump_lambda_lift ".lambda-lift.sexp" name;
    simplify_out = maybe_out dump_simplify ".simplify.sexp" name;
    callopt_out = maybe_out dump_callopt ".callopt.sexp" name;
};;

let maybe_close = function
    | None -> ()
    | Some oc ->
        let _ = close_out oc in
        ()
;;

let close_dumps dumps =
    maybe_close dumps.ast_out;
    maybe_close dumps.annot_out;
    maybe_close dumps.alpha_out;
    maybe_close dumps.lambda_out;
    maybe_close dumps.simplify_out;
    maybe_close dumps.callopt_out;
    ()
;;

let init_state name = 
    LlvmIntf.with_module name;
    {
        annot_map = Annot.StringMap.empty;
        alpha_map = Alpha.StringMap.empty;
        lambda_set = Common.Var.Set.empty;
        callopt_map = Common.Var.Map.empty;
        init_fns = [];
    };;

let base_name name =
    try
        let i = String.rindex name '.' in
        if (String.sub name i ((String.length name) - i)) = ".cml" then
            String.sub name 0 i
        else
            name
    with
    | Not_found -> name
;;

let parse_file name =
    let base = base_name name in
    let dumps = make_dumps base in
    begin
        try
            begin
                let state = init_state name in
                let inchan = open_in name in
                let lexbuf = Lexing.from_channel inchan in
                lexbuf.Lexing.lex_start_p <-
                    {   lexbuf.Lexing.lex_start_p with
                        Lexing.pos_fname = name };
                lexbuf.Lexing.lex_curr_p <-
                    {   lexbuf.Lexing.lex_curr_p with
                        Lexing.pos_fname = name };
                let _ = parse_loop lexbuf dumps state in
                if !dump_llvm then
                    begin
                        LlvmIntf.dump_module ();
                        flush stderr
                    end;
                close_in inchan;
                let _ = LlvmIntf.write_bitcode_file (base ^ ".o") in
                ()
            end
        with
            | Error.Compiler_error(msg, loc) ->
                begin
                    Error.print_error msg loc;
                    close_dumps dumps;
                    raise Exit
                end
    end;
    close_dumps dumps;
    ()
;;
        

let arg_spec = [
    "-dump-ast", Arg.Set dump_ast, "Dump the AST.";
    "-dump-annot", Arg.Set dump_annot, "Dump the type-annotated AST.";
    "-dump-alpha", Arg.Set dump_alpha, "Dump the alpha-renamed AST.";
    "-dump-lambda-lifted", Arg.Set dump_lambda_lift, 
        "Dump the lambda-lifted AST.";
    "-dump-simplify", Arg.Set dump_simplify, "Dump the simplified AST.";
    "-dump-callopt", Arg.Set dump_callopt, "Dump the call optimized AST.";
    "-dump-llvm", Arg.Set dump_llvm, "Dump the LLVM assembly to stderr.";
    "-dump-all", Arg.Unit
                    (fun () ->
                        dump_ast := true;
                        dump_annot := true;
                        dump_alpha := true;
                        dump_lambda_lift := true;
                        dump_simplify := true;
                        dump_callopt := true;
                        dump_llvm := true),
        "Dump all intermediate represetations.";
    "-", Arg.String parse_file, "Parse a file that begins with a -"
];;

let doc = "A native-mode compiler for the toy language caraml.";;

try
    let _ = Arg.parse arg_spec parse_file doc in ()
with
    | Exit -> ()
    | a -> Printf.printf "Unexpected exception!\n%!"
;;


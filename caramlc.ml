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

let dump_llvm = ref false;;
let dump_all = ref false;;
let check_all = ref false;;
let echo = ref false;

(* So that when this changes, it only needs to change in one place. *)
module FinalConv = CallOpt;;

type state_t = FinalConv.Convert.state * (string option list);;

let init_state name =
    LlvmIntf.with_module name;
    (FinalConv.Convert.init_state ~dump_all:(!dump_all)
            ~check_all:(!check_all) ~file_name:name), []
;;

let handle_ast (state, init_fns) ast =
    let state, callopts = FinalConv.Convert.convert state ast in
    let init_fns =
        List.fold_left
            (fun init_fns callopt ->
                let init_fn = Assembly.assemble callopt in
                init_fn :: init_fns)
            init_fns callopts
    in
    state, init_fns
;;

let rec parse_loop lexbuf state =
    match Parser.top_level Lexer.token lexbuf with
    | AST.EOF ->
        let _ = FinalConv.Convert.fini_state (fst state) in
        (snd state)
    | AST.SyntaxError ->
        let _ = FinalConv.Convert.fini_state (fst state) in
        raise Exit;
    | AST.Form ast ->
        let state = handle_ast state ast in
        parse_loop lexbuf state
;;

let add_externs state =
    let no_position = {
        Info.Position.file_name = "no file";
        Info.Position.line_num = 0;
        Info.Position.bol = 0;
        Info.Position.col_num = 0;
    } in
    let info = no_position, no_position in
    let state = handle_ast state
                (AST.Extern(info, "print_int",
                            {   Common.External.real_name = "print_int";
                                Common.External.return_type =
                                    Type.Base(Type.Unit);
                                Common.External.arg_types = [
                                    Type.Base(Type.Int) ] }))
    in
    let state = handle_ast state
                (AST.Extern(info, "print_newline",
                            {   Common.External.real_name = "print_newline";
                                Common.External.return_type =
                                    Type.Base(Type.Unit);
                                Common.External.arg_types = [
                                    Type.Base(Type.Unit) ] }))
    in
    state
;;

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

let link_native base_name =
    let cmd = Printf.sprintf "llvm-ld -native -o '%s' '%s.bc' caraml.bc"
                    base_name base_name
    in
    let _ = if !echo then
                print_endline cmd
            else
                ()
    in
    let res = Unix.system cmd in
    match res with
    | Unix.WEXITED(0) -> ()
    | _ ->
        begin
            Printf.fprintf stderr "llvm-ld failed.\n";
            exit (-1)
        end
;;

let parse_file name =
    let base = base_name name in
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
                let state = add_externs state in
                let init_fns = parse_loop lexbuf state in
                let _ = Assembly.create_main (List.rev init_fns) in
                if !dump_llvm then
                    begin
                        LlvmIntf.dump_module ();
                        flush stderr
                    end;
                close_in inchan;
                let _ = LlvmIntf.write_bitcode_file (base ^ ".bc") in
                let _ = link_native base in
                ()
            end
        with
            | Error.Compiler_error(msg, loc) ->
                begin
                    Error.print_error msg loc;
                    raise Exit
                end
    end;
    ()
;;

let arg_spec = List.flatten [
    [   "--echo", Arg.Set echo, "Echo the ld command to stdout.";
        "--dump-all", Arg.Unit
                        (fun () ->
                            dump_all := true;
                            dump_llvm := true),
            "Dump all intermediate represetations." ];
    FinalConv.Convert.dump_cmd_args;
    [   "--dump-llvm", Arg.Set dump_llvm, "Dump the LLVM assembly to stderr.";
        "--check-all", Arg.Set(check_all),
            "Type check all intermediate represetations." ];
    FinalConv.Convert.check_cmd_args;
    [
        "-", Arg.String parse_file, "Parse a file that begins with a -" ]
];;

let doc = "A native-mode compiler for the toy language caraml.";;

try
    let _ = Arg.parse arg_spec parse_file doc in ()
with
    | Exit -> ()
    | a -> Printf.printf "Unexpected exception! %s\n%!"
                        (Printexc.to_string a)
;;


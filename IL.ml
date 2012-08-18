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

module type Converter = sig
    type output;;
    type state;;

    val dump_cmd_args : (Arg.key * Arg.spec * Arg.doc) list;;
    val check_cmd_args : (Arg.key * Arg.spec * Arg.doc) list;;
    val init_state : dump_all:bool -> check_all:bool
                            -> file_name:string -> state;;
    val convert : state -> AST.t -> (state * (output list));;
    val fini_state : state -> unit;;
end;;

module type Conversion = sig
    type input;;
    type output;;
    type state;;
    type check_state;;

    val name : string;;
    val sexp_of_output : output -> Sexplib.Sexp.t;;
    val dump_flag : bool ref;;
    val check_flag : bool ref;;

    val init_state : unit -> state;;
    val convert : state -> input -> (state * (output list));;
    val fini_state : state -> unit;;

    val init_check_state : unit -> check_state;;
    val check : check_state -> output -> (check_state * bool);;
    val get_info : output -> Info.t;;
    val fini_check_state : check_state -> unit;;

end;;

module Make(I: Converter)(M: Conversion with type input=I.output)
    : Converter with type output = M.output
= struct

    type output = M.output;;
    type state = {
        inner_state : I.state;
        local_state : M.state;
        sexp_file : out_channel option;
        check_state : M.check_state option;
    };;

    let dump_cmd_args =
        List.append I.dump_cmd_args
            [   (Printf.sprintf "--dump-%s" M.name),
                (Arg.Set M.dump_flag),
                (Printf.sprintf
                    "Dump the Sexp representation of %s" M.name) ]
    ;;

    let check_cmd_args =
        List.append I.check_cmd_args
            [   (Printf.sprintf "--check-%s" M.name),
                (Arg.Set M.check_flag),
                (Printf.sprintf
                    "Type check the result of the %s conversion" M.name) ]
    ;;

    let init_state ~dump_all ~check_all ~file_name = {
        inner_state = (I.init_state ~dump_all ~check_all ~file_name);
        local_state = (M.init_state ());
        sexp_file =
            (if (dump_all || !M.dump_flag) then
                    Some(
                        open_out(
                            Printf.sprintf "%s.%s.sexp" file_name M.name))
                else
                    None);
        check_state =
            (if (check_all || !M.check_flag) then
                    Some(M.init_check_state ())
                else
                    None)
    };;

    let convert state ast =
        let (i_state, inputs) = I.convert state.inner_state ast in
        let (m_state, outputs) =
            Utils.map_accum M.convert state.local_state inputs
        in
        let outputs = List.flatten outputs in
        let () =
            match state.sexp_file with
            | None -> ()
            | Some(ostr) ->
                List.iter
                    (fun out ->
                        let sexp = M.sexp_of_output out in
                        Sexplib.Sexp.output_hum ostr sexp;
                        output_char ostr '\n';
                        output_char ostr '\n';
                        flush ostr;
                        ())
                    outputs
        in
        let check_state =
            match state.check_state with
                | None -> None
                | Some(check_state) ->
                    Some(List.fold_left
                            (fun check_state out ->
                                let (check_state, ok) =
                                    M.check check_state out
                                in
                                if not ok then
                                    let f _ =
                                        Format.print_string
                                            "Internal compiler error in ";
                                        Format.print_string M.name
                                    in
                                    raise (Error.Compiler_error(f,
                                                    (M.get_info out)))
                                else
                                    check_state)
                            check_state
                            outputs)
        in
        {   inner_state = i_state;
            local_state = m_state;
            sexp_file = state.sexp_file;
            check_state = check_state; },
        outputs
    ;;

    let fini_state state =
        let () =
            match state.check_state with
            | None -> ()
            | Some(check_state) -> M.fini_check_state check_state
        in
        let () =
            match state.sexp_file with
            | None -> ()
            | Some(ostr) -> close_out ostr
        in
        M.fini_state state.local_state;
        I.fini_state state.inner_state
    ;;

end;;

module Base : Converter with type output = AST.t = struct
    type output = AST.t;;
    let dump_flag = ref false;;
    type state = out_channel option;;

    let dump_cmd_args =
        [   "--dump-ast",
            (Arg.Set dump_flag),
            "Dump the Sexp representation of ast" ]
    ;;

    let check_cmd_args = [];;

    let init_state ~dump_all ~check_all ~file_name =
        if (dump_all || !dump_flag) then
                Some(
                    open_out(
                        Printf.sprintf "%s.ast.sexp" file_name))
            else
                None
    ;;

    let convert ostream ast =
        match ostream with
        | None -> ostream, [ ast ]
        | Some(ostr) ->
            begin
                let sexp = AST.sexp_of_t ast in
                Sexplib.Sexp.output_hum ostr sexp;
                output_char ostr '\n';
                output_char ostr '\n';
                flush ostr;
                ostream, [ ast ]
            end
    ;;

    let fini_state = function
        | None -> ()
        | Some(ostr) -> close_out ostr
    ;;

end;;


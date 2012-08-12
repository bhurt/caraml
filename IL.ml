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

    val name : string;;
    val sexp_of_output : output -> Sexplib.Sexp.t;;
    val dump_flag : bool ref;;
    val check_flag : bool ref;;
    val init_state : unit -> state;;
    val convert : state -> input -> (state * (output list));;
    val check : output -> bool;;
    val get_info : output -> Info.t;;
    val fini_state : state -> unit;;
end;;

module Make(I: Converter)(M: Conversion with type input=I.output)
    : Converter with type output = M.output
    = struct

        type output = M.output;;
        type state = I.state * M.state * (out_channel option) * bool;;

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

        let init_state ~dump_all ~check_all ~file_name =
            (I.init_state ~dump_all ~check_all ~file_name),
            (M.init_state ()),
            (if (dump_all || !M.dump_flag) then
                    Some(
                        open_out(
                            Printf.sprintf "%s.%s.sexp" file_name M.name))
                else
                    None),
            (check_all || !M.check_flag)
        ;;

        let convert (i_state, m_state, ostream, check) ast =
            let (i_state, inputs) = I.convert i_state ast in
            let (m_state, outputs) =
                Utils.map_accum M.convert m_state inputs
            in
            let outputs = List.flatten outputs in
            let () =
                match ostream with
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
            let _ =
                if check then
                    List.iter
                        (fun x ->
                            if not (M.check x) then
                                let f _ =
                                    Format.print_string
                                        "Internal compiler error in ";
                                    Format.print_string M.name
                                in
                                raise (Error.Compiler_error(f,
                                                (M.get_info x)))
                            else ())
                        outputs
                else ()
            in
            (i_state, m_state, ostream, check), outputs
        ;;

    let fini_state (i_state, m_state, ostream, check) =
        begin
            match ostream with
            | None -> ()
            | Some(ostr) -> close_out ostr
        end;
        M.fini_state m_state;
        I.fini_state i_state
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


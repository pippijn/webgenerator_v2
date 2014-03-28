let slurp ic =
  let lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file -> List.rev !lst |> String.concat "\n"


let split chrs lexbuf =
  let open Lexing in

  let rec split_rec state =
    if not state then (
      (* First entry. *)
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
    );

    (* See if we need a refill. *)
    if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len then
      if lexbuf.lex_eof_reached then
        raise End_of_file
      else
        lexbuf.refill_buff lexbuf;

    Printf.printf "=> reading '%s'\n"
      (Char.escaped lexbuf.lex_buffer.[lexbuf.lex_curr_pos]);
    Printf.printf "buffer_len  = %d\n" lexbuf.lex_buffer_len;
    Printf.printf "abs_pos     = %d\n" lexbuf.lex_abs_pos;
    Printf.printf "start_pos   = %d\n" lexbuf.lex_start_pos;
    Printf.printf "curr_pos    = %d\n" lexbuf.lex_curr_pos;
    Printf.printf "last_pos    = %d\n" lexbuf.lex_last_pos;
    Printf.printf "last_action = %d\n" lexbuf.lex_last_action;
    Printf.printf "eof_reached = %s\n" (string_of_bool lexbuf.lex_eof_reached);

    if List.memq lexbuf.lex_buffer.[lexbuf.lex_curr_pos] chrs then (
      (* found a word *)
      Printf.printf "\027[1;33mLexeme:\027[0m \"%s\"\n"
        (Lexing.lexeme lexbuf |> String.escaped);
      lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1;
    ) else (
      lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + 1;
      split_rec true
    )
  in

  let continue = ref true in
  while !continue do
    try
      split_rec false
    with End_of_file ->
      continue := false
  done
;;

let test_lexbuf () =
  let fh = open_in "test.txt" in
  let lexbuf = Lexing.from_channel fh in
  split [' '; '\n'] lexbuf;
  close_in fh


let () =
  Printexc.record_backtrace true;

  match Sys.argv with
  | [|_; mll; input|] ->
      let ast =
        try
          let fh = open_in Sys.argv.(1) in
          let lexbuf = Lexing.from_channel fh in
          let ast = Oparser.parse Olexer.(token (make ())) lexbuf in
          close_in fh;
          ast
        with
        | Oparser.StateError (token, state) ->
            Printf.printf "in state %d, unexpected %s\n"
              state (Olexer.to_string token);
            exit 1
      in

      let ast = Resolve.resolve ast in
      let ast = Simplify.simplify ast in
      (*print_endline @@ Show.show<Ast.t> ast;*)

      let pat = ExtractPattern.extract_rules_program ast in
      let pat = Language.compute_nullable pat in
      let npat, varmap = Pattern.number_pattern pat in

      (*print_endline @@ Show.show<string Types.pattern> pat;*)

      let (nfa, start) = NfaConstruct.build Util.identity varmap npat in
      Printf.printf "%d states\n" (Hashtbl.length nfa);

      let nfa = NfaConstruct.optimised (nfa, start) in

      let fh = open_in input in
      let lexbuf = Lexing.from_channel fh in
      NfaSimulate.run_loop_opt Util.identity nfa varmap lexbuf;
      (*NfaSimulate.run_loop_opt nfa varmap (slurp fh);*)
      close_in fh

  | _ ->
      failwith "Usage: drelex <lexer.mll> <input>"

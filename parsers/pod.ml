let process file =
  let fh = open_in file in
  let lexbuf = Lexing.from_channel fh in
  lexbuf.Lexing.lex_abs_pos <- -1;

  let rec next () =
    match PodLexer.token lexbuf with
    | PodParser.EOF ->
        ()
    | token ->
        Printf.printf "position %d: %s\n"
          Lexing.((lexeme_start_p lexbuf).pos_cnum)
          (PodLexer.show token);
        next ()
  in

  begin try
    next ();
  with Failure msg ->
    Printf.printf "error in position %d: %s\n"
      Lexing.((lexeme_start_p lexbuf).pos_cnum)
      msg;
    exit 1
  end;

  close_in fh

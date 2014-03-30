open Types
open Nfa


let time label f x =
  if Options._timing then
    let s = Unix.gettimeofday () in
    let r = f x in
    let e = Unix.gettimeofday () in
    Printf.printf "%s: %.06f sec\n" label (e -. s);
    flush stdout;
    r
  else
    f x


let string_of_pattern varmap =
  Print.string_of_pattern @@
    fun label ->
      (Array.get varmap (Label.value label))


let show ?(pre="") nfa lexbuf p env =
  let is_final =
    if Language.nullable p = Tribool.Yes then
      "\t(FINAL)"
    else
      ""
  in
  print_endline (
    "  state: " ^
    pre ^
    string_of_pattern nfa.varmap p ^
    is_final
  );
  print_endline (
    "  env:   [" ^
    String.concat ", " (
      List.rev_map (fun (Pos (x, start_p, end_p)) ->
        Printf.sprintf "(%s: \"%s\")"
          (Label.name nfa.varmap x)
          (String.escaped (String.sub lexbuf.lex_buffer
                             (lexbuf.lex_start_pos + start_p)
                             (end_p - start_p + 1)))
      ) env
    ) ^
    "]"
  )


let show_list ?(pre="") nfa lexbuf states =
  List.iter
    (fun (p, env) -> show nfa lexbuf p env)
    states


let show_internal nfa lexbuf states =
  List.iter (fun (p, env) ->
    let p' = nfa.inversion.(Label.value p) in
    show_list ~pre:(Label.to_string p ^ ": ")
      nfa lexbuf [p', env]
  ) states


let lexbuf_debug lexbuf =
  let open Lexing in
  Printf.printf "=> reading '%s'\n"
    (Char.escaped lexbuf.lex_buffer.[lexbuf.lex_curr_pos]);
  Printf.printf "buffer_len  = %d\n" lexbuf.lex_buffer_len;
  Printf.printf "abs_pos     = %d\n" lexbuf.lex_abs_pos;
  Printf.printf "start_pos   = %d\n" lexbuf.lex_start_pos;
  Printf.printf "curr_pos    = %d\n" lexbuf.lex_curr_pos;
  Printf.printf "last_pos    = %d\n" lexbuf.lex_last_pos;
  Printf.printf "last_action = %d\n" lexbuf.lex_last_action;
  Printf.printf "eof_reached = %s\n" (string_of_bool lexbuf.lex_eof_reached);
;;

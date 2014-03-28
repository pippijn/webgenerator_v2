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


let string_of_int_label string_of_label varmap label =
  string_of_label (Array.get varmap (label - 1))


let string_of_pattern string_of_label varmap =
  Print.string_of_pattern (string_of_int_label string_of_label varmap)


let string_of_int_label string_of_label varmap x =
  if x < 0 then
    string_of_label varmap.(-x - 1) ^ "'"
  else
    string_of_label varmap.( x - 1)


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
    string_of_pattern nfa.string_of_label nfa.varmap p ^
    is_final
  );
  print_endline (
    "  env:   [" ^
    String.concat ", " (
      List.rev_map (fun (x, pos) ->
        let Pos (start_p, end_p) = decode_pos pos in
        Printf.sprintf "(%s: \"%s\")"
          (string_of_int_label nfa.string_of_label nfa.varmap x)
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
    let p' = nfa.inversion.(p) in
    show_list ~pre:(string_of_int p ^ ": ")
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

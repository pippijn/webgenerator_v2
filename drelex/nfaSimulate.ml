open BatPervasives
open Nfa

let rec update_envs0 seen pos env states next idx =
  if idx = Array.length next then
    states
  else
    let states =
      let (pd, f) = Array.unsafe_get next idx in
      if Bitset.mem seen pd then (
        (* Only go into each state once (left-most matching). *)
        states
      ) else (
        Bitset.set seen pd;
        (* This is slow if we are in many states, but there are
           rarely more than 10, and usually less than 4. *)
        states @ [(pd, Tag.execute f pos env)]
      )
    in

    update_envs0 seen pos env states next (idx + 1)

let update_envs seen pos env states next =
  update_envs0 seen pos env states next 0


let rec goto_next_states0 nfa lexbuf pos c next_states = function
  | (state, env) :: tl ->
      let state = Label.value state in

      (* find all transitions on 'c' for 'state' *)
      let next =
        Array.unsafe_get nfa.tables
          (state * CharSet.size + Char.code c)
      in

      if Options._trace_run then (
        Printf.printf "state %d -> [%s]\n"
          state (next
                 |> Array.map (Label.to_string % fst)
                 |> Array.to_list
                 |> String.concat ";")
      );

      (* update envs *)
      let next_states =
        update_envs
          nfa.seen
          (pos - lexbuf.lex_start_pos)
          env
          next_states
          next
      in

      (* recursive call *)
      goto_next_states0 nfa lexbuf pos c next_states tl

  | [] ->
      next_states

let goto_next_states nfa lexbuf pos c curr_states =
  goto_next_states0 nfa lexbuf pos c [] curr_states


let rec clear_seen seen = function
  | [] -> ()
  | (number, _) :: states ->
      Bitset.unset seen number;
      clear_seen seen states


let iteration nfa lexbuf pos states c =
  let next = goto_next_states nfa lexbuf pos c states in
  clear_seen nfa.seen next;
  next


let rec update_final nfa lexbuf pos = function
  | (p, env) :: states ->
      if Bitset.mem nfa.final p then (
        nfa.last_env           <- env;
        lexbuf.lex_last_action <- Label.value p;
        lexbuf.lex_last_pos    <- pos;
      ) else (
        update_final nfa lexbuf pos states
      )
  | [] ->
      ()


let rec read_token nfa lexbuf states =
  if Options._trace_run then
    print_newline ();

  if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len then
    if not lexbuf.lex_eof_reached then
      lexbuf.refill_buff lexbuf;

  if Options._trace_lexbuf then
    Debug.lexbuf_debug lexbuf;

  let pos = lexbuf.lex_curr_pos in

  if pos <> lexbuf.lex_buffer_len && states <> [] then (
    let c = String.unsafe_get lexbuf.lex_buffer pos in
    let states = iteration nfa lexbuf pos states c in

    if Options._trace_run then (
      Printf.printf "after %s: in %d states\n"
        (Char.escaped c)
        (List.length states);
      Debug.show_internal nfa lexbuf states;
    );

    update_final nfa lexbuf pos states;

    lexbuf.lex_curr_pos <- pos + 1;
    read_token nfa lexbuf states;
  );
;;


let rec backtrack_loop nfa lexbuf =
  lexbuf.lex_last_pos <- -1;
  lexbuf.lex_last_action <- -1;
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;

  read_token nfa lexbuf nfa.start;

  if lexbuf.lex_last_action <> -1 then (
    if Options._trace_lex then (
      Printf.printf "\027[1;33mLexeme:\027[0m (at pos = %d/%d)\n"
        lexbuf.lex_last_pos
        lexbuf.lex_buffer_len;
      Debug.show nfa lexbuf
        nfa.inversion.(lexbuf.lex_last_action)
        nfa.last_env;
    );

    lexbuf.lex_curr_pos <- lexbuf.lex_last_pos + 1;

    backtrack_loop nfa lexbuf
  )
;;


let run nfa lexbuf =
  Gc.compact ();
  Debug.time "backtrack_loop"
    (backtrack_loop nfa) lexbuf

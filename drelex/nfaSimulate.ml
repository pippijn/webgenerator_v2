open BatPervasives
open Nfa

let _trace_run = false
let _trace_lex = true

type state = int * Types.env

type lexbuf = Lexing.lexbuf = {
  refill_buff : lexbuf -> unit;
  mutable lex_buffer : string;
  mutable lex_buffer_len : int;
  mutable lex_abs_pos : int;
  mutable lex_start_pos : int;
  mutable lex_curr_pos : int;
  mutable lex_last_pos : int;
  mutable lex_last_action : int;
  mutable lex_eof_reached : bool;
  mutable lex_mem : int array;
  mutable lex_start_p : Lexing.position;
  mutable lex_curr_p : Lexing.position;
}

type 'tag nfa = {
  seen               : Bitset.t;
  final              : Bitset.t;
  tables             : (int * int Instruction.t) list array;
  input              : string;
  len                : int;
  varmap             : 'tag array;
  inversion          : int Types.pattern array;
  start              : state list;
  string_of_tag      : 'tag -> string;
  lexbuf             : lexbuf;
  mutable last_env   : Types.env;
}


let rec update_envs0 seen pos env states = function
  | (pd, f) :: tl ->
      let states =
        if Bitset.mem seen pd then (
          states
        ) else (
          Bitset.set seen pd;
          (* This is slow if there are many states. *)
          states @ [(pd, Instruction.execute f pos env)]
        )
      in
      update_envs0 seen pos env states tl

  | [] ->
      states

let update_envs seen pos env states next =
  update_envs0 seen pos env states next


let rec goto_next_states0 nfa pos c next_states = function
  | (state, env) :: tl ->
      (* find all transitions on 'c' for 'state' *)
      let next =
        Array.unsafe_get nfa.tables
          (state * CharClass.set_end + Char.code c)
      in

      if _trace_run then (
        Printf.printf "state %d -> [%s]\n"
          state (String.concat ";" (List.map (string_of_int % fst) next))
      );

      (* update envs *)
      let next_states = update_envs nfa.seen pos env next_states next in

      (* recursive call *)
      goto_next_states0 nfa pos c next_states tl

  | [] ->
      next_states

let goto_next_states nfa pos c curr_states =
  goto_next_states0 nfa pos c [] curr_states


let rec clear_seen seen = function
  | [] -> ()
  | (number, _) :: states ->
      Bitset.unset seen number;
      clear_seen seen states


let iteration nfa pos states c =
  let next = goto_next_states nfa pos c states in
  clear_seen nfa.seen next;
  next


let rec update_final nfa pos = function
  | (p, env) :: states ->
      if Bitset.mem nfa.final p then (
        nfa.last_env               <- env;
        nfa.lexbuf.lex_last_action <- p;
        nfa.lexbuf.lex_last_pos    <- pos;
      ) else (
        update_final nfa pos states
      )
  | [] ->
      ()


let rec main_loop nfa states =
  if _trace_run then
    print_newline ();

  let pos = nfa.lexbuf.lex_curr_pos in

  if pos <> nfa.len && states <> [] then
    let c = String.unsafe_get nfa.input pos in
    let states = iteration nfa pos states c in

    if _trace_run then (
      Printf.printf "after %s: in %d states\n"
        (Char.escaped c)
        (List.length states);
      Debug.show_internal
        nfa.string_of_tag nfa.varmap
        nfa.inversion
        nfa.input
        states;
    );

    update_final nfa pos states;

    nfa.lexbuf.lex_curr_pos <- pos + 1;
    main_loop nfa states


let rec backtrack_loop nfa input =
  nfa.lexbuf.lex_last_action <- -1;

  main_loop nfa nfa.start;

  if nfa.lexbuf.lex_last_action <> -1 then (
      if _trace_lex then (
        Printf.printf "\027[1;33mLexeme:\027[0m (at pos = %d/%d)\n"
          nfa.lexbuf.lex_last_pos nfa.len;
        Debug.show
          nfa.string_of_tag nfa.varmap
          input
          nfa.inversion.(nfa.lexbuf.lex_last_action)
          nfa.last_env;
      );
      nfa.lexbuf.lex_last_pos <- nfa.lexbuf.lex_last_pos + 1;
      nfa.lexbuf.lex_curr_pos <- nfa.lexbuf.lex_last_pos;
      backtrack_loop nfa input
  )


let slurp_lexbuf lexbuf =
  let open Lexing in
  while not lexbuf.lex_eof_reached do
    lexbuf.lex_curr_pos <- lexbuf.lex_buffer_len;
    lexbuf.refill_buff lexbuf
  done;
  String.sub lexbuf.lex_buffer
    0 lexbuf.lex_buffer_len


let run_loop_opt string_of_tag nfa varmap lexbuf =
  let input = slurp_lexbuf lexbuf in

  lexbuf.lex_curr_pos <- 0;
  lexbuf.lex_last_pos <- 0;

  let seen = Bitset.create (Array.length nfa.o_nfa / CharClass.set_end) in

  let nfa = {
    tables     = nfa.o_nfa;
    final      = nfa.o_final;
    inversion  = nfa.o_inversion;

    seen;
    input;
    varmap;
    string_of_tag;
    lexbuf;

    start      = [(nfa.o_start, Types.empty_env)];
    len        = String.length input;
    last_env   = [];
  } in

  Debug.time "backtrack_loop" (fun () ->
    backtrack_loop nfa input
  )

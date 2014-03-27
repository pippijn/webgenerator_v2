open BatPervasives
open Nfa

let _trace_run = false
let _trace_lex = true

type 'tag nfa = {
  seen      : Bitset.t;
  final     : Bitset.t;
  nfa       : (int * int Instruction.t) list array;
  input     : string;
  len       : int;
  varmap    : 'tag array;
  inversion : int Types.pattern array;
  mutable last_final : int * Types.env;
  mutable last_pos   : int;
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


let goto_next_states nfa pos c curr_states =
  let rec goto_next_states0 nfa pos c next_states curr_states =
    match curr_states with
    | (state, env) :: tl ->
        (* find all transitions on 'c' for 'state' *)
        let next = Array.unsafe_get nfa.nfa (state * CharClass.set_end + (Char.code c)) in

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
  in
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
  | (p, _ as state) :: states ->
      if Bitset.mem nfa.final p then (
        nfa.last_final <- state;
        nfa.last_pos <- pos;
      ) else (
        update_final nfa pos states
      )
  | [] ->
      ()


let rec main_loop nfa pos states =
  if _trace_run then
    print_newline ();

  if pos = nfa.len || states = [] then
    (nfa.last_final, nfa.last_pos)
  else
    let c = String.unsafe_get nfa.input pos in
    let states = iteration nfa pos states c in

    if _trace_run then (
      Printf.printf "after %s: in %d states\n"
        (Char.escaped c)
        (List.length states);
      Debug.show_internal nfa.inversion nfa.varmap nfa.input states;
    );

    update_final nfa pos states;

    main_loop nfa (pos + 1) states


let run seen inversion varmap nfa final start input pos =
  let nfa = {
    nfa;
    final;
    seen;
    input;
    len = String.length input;
    varmap;
    inversion;
    last_final = (-1, []);
    last_pos = -1;
  } in

  main_loop nfa pos [(start, Types.empty_env)]


let run_optimised pos { o_nfa; o_start; o_inversion; o_final; } seen varmap input =
  let result =
(*  Debug.time "run" (fun () -> *)
    run seen o_inversion varmap o_nfa o_final o_start input pos
(*  ) *)
  in
  result


let rec run_optimised_loop pos nfa seen varmap input =
  let state = run_optimised pos nfa seen varmap input in

  match state with
  | ((-1, []), -1) ->
      ()
  | ((state, env), pos) ->
      if _trace_lex then
        Debug.show varmap input nfa.o_inversion.(state) env pos;
      run_optimised_loop (pos + 1) nfa seen varmap input


let run_loop_opt pos nfa varmap input =
  let seen = Bitset.create (Array.length nfa.o_nfa / CharClass.set_end) in
  Debug.time "hashcons" (fun () ->
    run_optimised_loop pos nfa seen varmap input
  )

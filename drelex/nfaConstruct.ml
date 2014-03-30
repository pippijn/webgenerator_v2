open Nfa
open Types

(**********************************************************
 * :: NFA Construction.
 **********************************************************)

let filter_final states =
  List.filter (fun (p, _) ->
    Language.nullable p = Tribool.Yes
  ) states


let transitions varmap p =
  if Options._trace_con then (
    Printf.printf "transitions for %s:\n"
      (Debug.string_of_pattern varmap p);
  );
  let transitions_for_char n =
    let chr = Char.chr n in
    (*
    if chr <> 'a' && chr <> 'b' && chr <> 'c' then
      []
    else
    *)

    let pds = Derivative.derive_pat chr p in

    if Options._trace_con && pds != [] then (
      Printf.printf "  on '%s':\n"
        (Char.escaped chr);
      List.iter (fun (pd, f) ->
        Printf.printf "    %-30s\t%s\n"
          (Debug.string_of_pattern varmap pd)
          (Tag.to_string (Label.name varmap) f)
      ) pds;
    );

    pds
  in
  Array.init 256 transitions_for_char


let rec build_next varmap nfa = function
  | [] -> ()
  | (pd, _) :: xs ->
      build varmap nfa pd;
      build_next varmap nfa xs

and build varmap nfa p =
  if not (Hashtbl.mem nfa p) then (
    let xs = transitions varmap p in
    Hashtbl.add nfa p xs;
    for i = 0 to Array.length xs - 1 do
      build_next varmap nfa (Array.unsafe_get xs i)
    done;
  )


let build varmap start =
  let nfa = Hashtbl.create 10 in
  build varmap nfa start;
  nfa, start



let state_count (nfa, start) =
  Hashtbl.length nfa

let transition_count (nfa, start) =
  Hashtbl.fold (fun p xs count ->
    Array.fold_left (fun count xs ->
      count + List.length xs
    ) count xs
  ) nfa 0


let optimised (nfa, start) =
  let nstates = Hashtbl.length nfa in

  let o_final = Bitset.create nstates in

  (* first, build hashcons and nullable bitset *)
  let hashcons = Hashtbl.create nstates in
  Hashtbl.iter (fun p xs ->
    assert (not (Hashtbl.mem hashcons p));
    let id = Label.make (Hashtbl.length hashcons) in
    Hashtbl.add hashcons p id;
    if Language.nullable p = Tribool.Yes then
      Bitset.set o_final id;
  ) nfa;

  (* and the inversion *)
  let o_inversion = Array.make nstates start in
  Hashtbl.iter (fun p id ->
    o_inversion.(Label.value id) <- p
  ) hashcons;

  (* next, map all patterns to their id *)
  let o_tables = Array.make (nstates * 256) [||] in
  Hashtbl.iter (fun p xs ->
    let p = Label.value @@ Hashtbl.find hashcons p in
    Array.iteri (fun i x ->
      o_tables.(p * 256 + i) <-
        BatList.map (fun (pd, f) ->
          Hashtbl.find hashcons pd, f
        ) x
        |> Array.of_list
    ) xs;
  ) nfa;

  let o_start = Hashtbl.find hashcons start in

  {
    o_tables;
    o_start;
    o_inversion;
    o_final;
  }


let construct pat =
  let pat = Language.compute_nullable pat in
  let pat, varmap = Pattern.number_pattern pat in

  let nfa =
    Debug.time "build nfa"
      (build varmap) pat
  in
  Printf.printf "%d states, %d transitions\n"
    (state_count nfa)
    (transition_count nfa);

  let nfa =
    Debug.time "build table"
      optimised nfa
  in

  let seen = Bitset.create (Array.length nfa.o_tables / CharSet.size) in

  {
    tables     = nfa.o_tables;
    final      = nfa.o_final;
    inversion  = nfa.o_inversion;

    seen;
    varmap;

    start      = [(nfa.o_start, Types.empty_env)];
    last_env   = [];
  }

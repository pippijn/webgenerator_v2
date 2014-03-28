open Nfa
open Types

let _trace_con = false

(**********************************************************
 * :: NFA Construction.
 **********************************************************)

let filter_final states =
  List.filter (fun (p, _) ->
    Language.nullable p = Tribool.Yes
  ) states


let transitions string_of_label varmap p =
  if _trace_con then (
    Printf.printf "transitions for %s:\n"
      (Debug.string_of_pattern string_of_label varmap p);
  );
  let transitions_for_char n =
    let chr = Char.chr n in
    (*
    if chr <> 'a' && chr <> 'b' && chr <> 'c' then
      []
    else
    *)

    let pds = Derivative.derive_pat chr p in

    if _trace_con && pds != [] then (
      Printf.printf "  on '%s':\n"
        (Char.escaped chr);
      List.iter (fun (pd, f) ->
        Printf.printf "    %-30s\t%s\n"
          (Debug.string_of_pattern string_of_label varmap pd)
          (Tag.to_string (Debug.string_of_int_label string_of_label varmap) f)
      ) pds;
    );

    pds
  in
  Array.init 256 transitions_for_char


let rec build_next string_of_label varmap nfa = function
  | [] -> ()
  | (pd, _) :: xs ->
      build string_of_label varmap nfa pd;
      build_next string_of_label varmap nfa xs

and build string_of_label varmap nfa p =
  if not (Hashtbl.mem nfa p) then (
    let xs = transitions string_of_label varmap p in
    Hashtbl.add nfa p xs;
    for i = 0 to Array.length xs - 1 do
      build_next string_of_label varmap nfa (Array.unsafe_get xs i)
    done;
  )


let build string_of_label varmap start =
  let nfa = Hashtbl.create 10 in
  build string_of_label varmap nfa start;
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
    let id = Hashtbl.length hashcons in
    Hashtbl.add hashcons p id;
    if Language.nullable p = Tribool.Yes then
      Bitset.set o_final id;
  ) nfa;

  (* and the inversion *)
  let o_inversion = Array.make nstates start in
  Hashtbl.iter (fun p id ->
    o_inversion.(id) <- p
  ) hashcons;

  (* next, map all patterns to their id *)
  let o_tables = Array.make (nstates * 256) [||] in
  Hashtbl.iter (fun p xs ->
    let p = Hashtbl.find hashcons p in
    Array.iteri (fun i x ->
      o_tables.(p * 256 + i) <-
        BatList.map (fun (pd, f) ->
          Hashtbl.find hashcons pd, f
        ) x
        |> Array.of_list
    ) xs;
  ) nfa;

  let o_start = Hashtbl.find hashcons start in

  { o_tables; o_start; o_inversion; o_final; }


let construct string_of_label pat =
  let pat = Language.compute_nullable pat in
  let pat, varmap = Pattern.number_pattern pat in

  let nfa =
    Debug.time "build nfa"
      (build string_of_label varmap) pat
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
    string_of_label;

    start      = [(nfa.o_start, Types.empty_env)];
    last_env   = [];
  }

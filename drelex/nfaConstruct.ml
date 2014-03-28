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


let transitions string_of_tag varmap p =
  if _trace_con then (
    Printf.printf "transitions for %s:\n"
      (Debug.string_of_pattern string_of_tag varmap p);
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
          (Debug.string_of_pattern string_of_tag varmap pd)
          (Tag.to_string (Debug.string_of_label string_of_tag varmap) f)
      ) pds;
    );

    pds
  in
  Array.init 256 transitions_for_char


let rec build_next string_of_tag varmap nfa = function
  | [] -> ()
  | (pd, _) :: xs ->
      build string_of_tag varmap nfa pd;
      build_next string_of_tag varmap nfa xs

and build string_of_tag varmap nfa p =
  if not (Hashtbl.mem nfa p) then (
    let xs = transitions string_of_tag varmap p in
    Hashtbl.add nfa p xs;
    for i = 0 to Array.length xs - 1 do
      build_next string_of_tag varmap nfa (Array.unsafe_get xs i)
    done;
  )


let build string_of_tag varmap start =
  Debug.time "build" (fun () ->
    let nfa = Hashtbl.create 10 in
    build string_of_tag varmap nfa start;
    nfa, start
  )



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
  let o_nfa = Array.make (nstates * 256) [] in
  Hashtbl.iter (fun p xs ->
    let p = Hashtbl.find hashcons p in
    Array.iteri (fun i x ->
      o_nfa.(p * 256 + i) <-
        BatList.map (fun (pd, f) ->
          Hashtbl.find hashcons pd, f
        ) x;
    ) xs;
  ) nfa;

  let o_start = Hashtbl.find hashcons start in

  { o_nfa; o_start; o_inversion; o_final; }

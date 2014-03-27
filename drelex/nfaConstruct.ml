open Types

let _trace_con = false

type 'tag nfa = {
  seen : Bitset.t;
  nullable : Bitset.t;
  nfa : (int * 'tag) list array;
  input : string;
  len : int;
  mutable last_final : int * env;
  mutable last_pos : int;
}

type 'a optimised_nfa = {
  o_nfa       : (int * int Types.instruction) list array;
  o_start     : int;
  o_inversion : 'a Types.pattern array;
  o_nullable  : Bitset.t;
} deriving (Show)

(**********************************************************
 * :: NFA Construction.
 **********************************************************)

let filter_final states =
  List.filter (fun (p, _) ->
    Language.nullable p = Yes
  ) states


let transitions varmap p =
  if _trace_con then (
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

    if _trace_con && pds != [] then (
      Printf.printf "  on '%s':\n"
        (Char.escaped chr);
      List.iter (fun (pd, f) ->
        Printf.printf "    %-30s\t%s\n"
          (Debug.string_of_pattern varmap pd)
          (Instruction.to_string (Debug.string_of_label varmap) f)
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
  Debug.time "build" (fun () ->
    let nfa = Hashtbl.create 10 in
    build varmap nfa start;
    nfa, start
  )



let cardinal (nfa, start) =
  Hashtbl.length nfa


let optimised (nfa, start) =
  let nstates = Hashtbl.length nfa in

  let o_nullable = Bitset.create nstates in

  (* first, build hashcons and nullable bitset *)
  let hashcons = Hashtbl.create nstates in
  Hashtbl.iter (fun p xs ->
    assert (not (Hashtbl.mem hashcons p));
    let id = Hashtbl.length hashcons in
    Hashtbl.add hashcons p id;
    if Language.nullable p = Yes then
      Bitset.set o_nullable id;
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

  { o_nfa; o_start; o_inversion; o_nullable; }

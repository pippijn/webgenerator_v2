open Types


let precedence = function
  | LetterSet _ | Letter _ | Phi | Epsilon -> 140
  | VarGroup    _ -> 100
  | Intersect   _ -> 100
  | Choice      _ -> 100
  | Concat      _ -> 100
  | Star        _ -> 90
  | Repeat      _ -> 100
  | Not         _ -> 120


let rec vars_of_pattern vars = function
  | VarGroup (_, x, p) ->
      x :: vars_of_pattern vars p
  | Intersect (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | Choice (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | Concat (_, p1, p2) ->
      vars_of_pattern (vars_of_pattern vars p2) p1
  | Star (p) ->
      vars_of_pattern vars p
  | Repeat (_, p, _) ->
      vars_of_pattern vars p
  | Not (_, p) ->
      vars_of_pattern vars p
  | LetterSet _ | Letter _ | Phi | Epsilon ->
      vars

let vars_of_pattern p = vars_of_pattern [] p


let variable_clashes p =
  let vars = vars_of_pattern p in
  let seen = Hashtbl.create (List.length vars) in
  BatList.sort_unique compare (List.fold_left (fun clashes var ->
    if Hashtbl.mem seen var then
      var :: clashes
    else (
      Hashtbl.add seen var true;
      clashes
    )
  ) [] vars)


let rec number_pattern names = function
  | VarGroup (null, x, p) ->
      VarGroup (null, Hashtbl.find names x, number_pattern names p)
  | Intersect (null, p1, p2) ->
      Intersect (null, number_pattern names p1, number_pattern names p2)
  | Choice (null, p1, p2) ->
      Choice (null, number_pattern names p1, number_pattern names p2)
  | Concat (null, p1, p2) ->
      Concat (null, number_pattern names p1, number_pattern names p2)
  | Star (p) ->
      Star (number_pattern names p)
  | Repeat (null, p, n) ->
      Repeat (null, number_pattern names p, n)
  | Not (null, p) ->
      Not (null, number_pattern names p)
  | LetterSet _ | Letter _ | Phi | Epsilon as p ->
      p


let number_pattern p =
  let vars = vars_of_pattern p in
  (* 'label -> int *)
  let names = Hashtbl.create (List.length vars) in
  (* int -> 'label *)
  let map = Array.make (List.length vars) "" in

  BatList.iteri (fun i var ->
    Hashtbl.add names var (i + 1);
    map.(i) <- var;
  ) vars;

  let numbered = number_pattern names p in

  numbered, map

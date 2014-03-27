open Types

let not3 = function
  | No -> Yes
  | Yes -> No
  | Maybe -> failwith "Maybe in not3"

let (|||) a b =
  match a, b with
  | No, No -> No
  | _, Yes
  | Yes, _ -> Yes
  | _, Maybe
  | Maybe, _ -> failwith "Maybe in |||"

let (&&&) a b =
  match a, b with
  | Yes, Yes -> Yes
  | No, _
  | _, No -> No
  | _, Maybe
  | Maybe, _ -> failwith "Maybe in &&&"


let rec exprset_of_regex = function
  | Intersect (_, r1, r2) -> exprset_of_regex r1 @ exprset_of_regex r2
  | r -> [r]

let rec exprsets_of_regex = function
  | Choice (_, r1, r2) -> exprsets_of_regex r1 @ exprsets_of_regex r2
  | r -> [exprset_of_regex r]


let rec pattern_of_exprset = function
  | [] -> failwith "empty exprset" (* Phi? *)
  | [p] -> p
  | p1 :: ps ->
      let p2 = pattern_of_exprset ps in
      Intersect (
        Language.nullable p1 &&& Language.nullable p2,
        p1, p2
      )


let set_union a b =
  let table = Hashtbl.create (List.length a + List.length b) in

  let unite =
    List.fold_left (fun union x ->
      if Hashtbl.mem table x then (
        union
      ) else (
        Hashtbl.add table x ();
        x :: union
      )
    )
  in

  let union = unite (unite [] a) b in

  List.rev union


let set_union_pat a b =
  let table = ExprsetTbl.create (List.length a + List.length b) in

  let unite =
    List.fold_left (fun union x ->
      if ExprsetTbl.mem table x then (
        union
      ) else (
        ExprsetTbl.add table x ();
        x :: union
      )
    )
  in

  let union = unite (unite [] a) b in

  List.rev union


(*let set_union = (@)*)
(*let set_union_pat = (@)*)


let rec mapx1 f x1 = function
  | [] -> []
  | a :: l ->
      f x1 a :: mapx1 f x1 l

(*
let rec rev_mapx1 accu f x1 = function
  | [] -> accu
  | a :: l ->
      rev_mapx1 (f x1 a :: accu) f x1 l
*)


let rec mapx2 f x1 x2 = function
  | [] -> []
  | a :: l ->
      f x1 x2 a :: mapx2 f x1 x2 l

(*
let rec rev_mapx2 accu f x1 x2 = function
  | [] -> accu
  | a :: l ->
      rev_mapx2 (f x1 x2 a :: accu) f x1 x2 l
*)


(* circled · *)
let mul_exprsets_expr p1ss p2 =
  let fun_2 p2 p1 =
    Concat (Language.nullable p1 &&& Language.nullable p2, p1, p2)
  in
  let fun_1 p2 (p1s, f) =
    (mapx1 fun_2 p2 p1s, f)
  in
  mapx1 fun_1 p2 p1ss

let mul_exprsets_expr_iter iterate p1ss p2 =
  let fun_2 p2 p1 =
    Concat (Language.nullable p1 &&& Language.nullable p2, p1, p2)
  in
  let fun_1 p2 iterate (p1s, f) =
    (mapx1 fun_2 p2 p1s, Instruction.compose f iterate)
  in
  mapx2 fun_1 p2 iterate p1ss


(* circled ∩ *)
let intersect_exprsets ess fss =
  let fun_1 fss es =
    (* for each F set, combine with the E set *)
    mapx1 set_union es fss
  in
  let union =
    (* for each E set *)
    mapx1 fun_1 fss ess
  in
  (* unite every es with every fs *)
  Util.reduce set_union union


let intersect_exprsets_pat ess fss =
  let fun_2 (es, e_tag) (fs, f_tag) =
    (* combine the two sets and associated
     * transition functions *)
    set_union es fs, Instruction.compose e_tag f_tag
  in
  let fun_1 fss es =
    (* for each F set *)
    mapx1 fun_2 es fss
  in
  let union =
    (* for each E set *)
    mapx1 fun_1 fss ess
  in
  (* unite every es with every fs *)
  Util.reduce set_union_pat union


let sigma_star = [[Not (Yes, Phi)]]
(* circled ¬ *)
let not_exprsets = function
  | [] -> sigma_star
  | sets ->
      let fun_2 e =
        [Not (not3 (Language.nullable e), e)]
      in
      let fun_1 set =
        List.map fun_2 set
      in
      Util.reduce intersect_exprsets (
        List.map fun_1 sets
      )


(*
let filter_valid =
  List.filter (not % List.exists (Simplify.simplify %> Language.is_empty_language))

let simplify =
  List.map (List.map Simplify.simplify)
*)


(* ·\p· :: l -> p -> [p] *)
let rec derive_pat l p =
  List.map (fun (p, t) ->
    List.map SimplifyPattern.simplify p, t
  )
  begin match p with
  | VarGroup (_, x, p) ->
      derive_VarGroup l x p
  | Choice (_, p1, p2) ->
      derive_Choice l p1 p2
  | Concat (_, p1, p2) ->
      derive_Concat l p1 p2
  | Intersect (_, p1, p2) ->
      derive_Intersect l p1 p2
  | Star (p) as star ->
      derive_Star l p star
  | Repeat (_, p, 1) ->
      derive_Repeat1 l p
  | Repeat (null, p, n) ->
      derive_Repeat l null p n
  | Not _ ->
      failwith "Negation in pattern must be resolved before derivation"

  (* 2 *)
  | LetterSet set when CharSet.mem set l ->
      [[Epsilon], Instruction.nop]
  | Letter l2 when l = l2 ->
      [[Epsilon], Instruction.nop]
  (* 1 *)
  | Phi
  | Epsilon
  | LetterSet _
  | Letter _ (* otherwise *) -> []

  end

and derive_VarGroup l x p =
  let update = Instruction.update x in
  mapx2 (fun x update (p', f) ->
    let p' = pattern_of_exprset p' in
    [VarGroup (Language.nullable p', x, p')],
    Instruction.compose update f
  ) x update (derive_pat l p)

and derive_Choice l p1 p2 =
  set_union
    (derive_pat l p1)
    (derive_pat l p2)

and derive_Concat l p1 p2 =
  let p1' = derive_pat l p1 in
  let p1'p2 = mul_exprsets_expr p1' p2 in
  if Language.nullable p1 = Yes then
    set_union_pat
      p1'p2
      (derive_pat l p2)
  else
    p1'p2

and derive_Intersect l p1 p2 =
  intersect_exprsets_pat
    (derive_pat l p1)
    (derive_pat l p2)

and derive_Star l p star =
  let iterate = Instruction.iterate p in
  mul_exprsets_expr_iter iterate (derive_pat l p) star

and derive_Repeat1 l p =
  let iterate = Instruction.iterate p in
  mapx1 (fun iterate (p', f) ->
    p', Instruction.compose f iterate
  ) iterate (derive_pat l p)

and derive_Repeat l null p n =
  assert (n > 1);
  let iterate = Instruction.iterate p in
  let repeat = Repeat (null, p, n - 1) in
  mul_exprsets_expr_iter iterate (derive_pat l p) repeat


let derive_pat l p =
  let fun_1 (p, f) =
    pattern_of_exprset p, f
  in
  List.map fun_1 (derive_pat l p)

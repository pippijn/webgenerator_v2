open Types
open Tribool


let nullable = function
  | VarGroup  ((Yes | No as null), _, _)
  | Not       ((Yes | No as null), _   )
  | Choice    ((Yes | No as null), _, _)
  | Intersect ((Yes | No as null), _, _)
  | Concat    ((Yes | No as null), _, _)
  | Repeat    ((Yes | No as null), _, _) ->
      null

  | Epsilon
  | Star _ -> Yes

  | Phi
  | LetterSet _
  | Letter _ -> No

  | VarGroup  (Maybe, _, _)
  | Not       (Maybe, _   )
  | Choice    (Maybe, _, _)
  | Intersect (Maybe, _, _)
  | Concat    (Maybe, _, _)
  | Repeat    (Maybe, _, _) ->
      failwith "nullable not computed"


let rec compute_nullable = function
  | Epsilon -> Epsilon, Yes
  | Star p -> Star (fst (compute_nullable p)), Yes

  | VarGroup (Maybe, x, p) ->
      let p, null = compute_nullable p in
      VarGroup (null, x, p), null

  | Phi
  | LetterSet _
  | Letter _ as p -> p, No

  | Not (Maybe, r) ->
      let r, null = compute_nullable r in
      let null = not3 null in
      Not (null, r), null

  (* e.g. a | a* is nullable *)
  | Choice (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 ||| null2 in
      Choice (null, r1, r2), null

  (* e.g. a & a* is not nullable *)
  | Intersect (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 &&& null2 in
      Intersect (null, r1, r2), null

  (* e.g. a a* is not nullable *)
  | Concat (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 &&& null2 in
      Concat (null, r1, r2), null

  | Repeat (Maybe, r, n) ->
      assert (n > 0);
      let r, null = compute_nullable r in
      assert (null != Maybe);
      Repeat (null, r, n), null

  | VarGroup  ((Yes | No as null), _, _)
  | Not       ((Yes | No as null), _   )
  | Choice    ((Yes | No as null), _, _)
  | Intersect ((Yes | No as null), _, _)
  | Concat    ((Yes | No as null), _, _)
  | Repeat    ((Yes | No as null), _, _) as p ->
      p, null


let compute_nullable r = fst (compute_nullable r)

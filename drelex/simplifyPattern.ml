open Types


let rec need_simplify = function
  (* & *)
  | Intersect (_, Phi, _)
  | Intersect (_, _, Phi)

  | Intersect (_, Not (_, Phi), _)
  | Intersect (_, _, Not (_, Phi)) -> true

  (* juxtaposition: ab *)
  | Concat (_, Phi, _)
  | Concat (_, _, Phi)

  | Concat (_, Epsilon, _)
  | Concat (_, _, Epsilon) -> true

  (* + *)
  | Choice (_, Not (_, Phi), _)
  | Choice (_, _, Not (_, Phi))

  | Choice (_, Phi, _)
  | Choice (_, _, Phi)

  (* * *)
  | Star (Star _)
  | Star Epsilon
  | Star Phi

  (* ~(~r) -> r *)
  | Not (_, Not (_, _))

  (* ~eps -> phi *)
  | Not (_, Epsilon)


  (* XXX: this simplification makes this testcase trivial and useless:
     (a: ((a+b)*a(a+b)^3) & ((~((~a)&(~b)))*a(a+b)^3) )
  *)

  (* remove negation by De Morgan, if possible *)
  | Not (_, Intersect (_, Not _, Not _))
  | Not (_, Choice (_, Not _, Not _))

  | Repeat (_, _, 0)
  | Repeat (_, _, 1) -> true

  (* \Sigma^* => \neg\phi *)
  | Star (LetterSet set) -> CharSet.is_wildcard set

  (* Recursive simplify *)
  | VarGroup (_, _, p) -> need_simplify p
  | Concat (_, r1, r2) -> need_simplify r1 || need_simplify r2
  | Choice (_, r1, r2) -> need_simplify r1 || need_simplify r2
  | Intersect (_, r1, r2) -> need_simplify r1 || need_simplify r2
  | Star r -> need_simplify r
  | Repeat (_, r, _) -> need_simplify r
  | Not (_, r) -> need_simplify r

  (* Keep atoms *)
  | Phi
  | Epsilon
  | LetterSet _
  | Letter _ -> false


let wildcard = Not (Tribool.Yes, Phi)

(* 4.1 Weaker notions of RE equivalence *)
let rec simplify_step = function
  (* & *)
  | Intersect (_, Phi, _)
  | Intersect (_, _, Phi) -> Phi

  (* \neg\phi = \Sigma^* *)
  | Intersect (_, Not (_, Phi), r)
  | Intersect (_, r, Not (_, Phi)) -> r

  (* juxtaposition: ab *)
  | Concat (_, Phi, _)
  | Concat (_, _, Phi) -> Phi

  | Concat (_, Epsilon, r)
  | Concat (_, r, Epsilon) -> r

  (* + *)
  | Choice (_, (Not (_, Phi) as r), _)
  | Choice (_, _, (Not (_, Phi) as r)) -> r

  | Choice (_, Phi, r)
  | Choice (_, r, Phi) -> r

  (* * *)
  | Star (Star _ as inner) -> inner
  | Star Epsilon -> Epsilon
  | Star Phi -> Epsilon

  (* \Sigma^* => \neg\phi *)
  | Star (LetterSet set) when CharSet.is_wildcard set -> wildcard

  (* ~(~r) -> r *)
  | Not (_, Not (_, r)) -> r

  (* ~eps -> phi *)
  | Not (_, Epsilon) -> Phi


  (* XXX: this simplification makes this testcase trivial and useless:
     (a: ((a+b)*a(a+b)^3) & ((~((~a)&(~b)))*a(a+b)^3) )
  *)

  (* remove negation by De Morgan, if possible *)
  | Not (null, Intersect (_, Not (_, r1), Not (_, r2))) -> Choice (null, r1, r2)
  | Not (null, Choice (_, Not (_, r1), Not (_, r2))) -> Intersect (null, r1, r2)

  | Repeat (_, _, 0) -> Epsilon
  | Repeat (_, r, 1) -> r

  (* Recursive simplify *)
  | VarGroup (null, x, p) -> VarGroup (null, x, simplify_step p)
  | Concat (null, r1, r2) -> Concat (null, simplify_step r1, simplify_step r2)
  | Choice (null, r1, r2) -> Choice (null, simplify_step r1, simplify_step r2)
  | Intersect (null, r1, r2) -> Intersect (null, simplify_step r1, simplify_step r2)
  | Star r -> Star (simplify_step r)
  | Repeat (null, r, n) -> Repeat (null, simplify_step r, n)
  | Not (null, r) -> Not (null, simplify_step r)

  (* Keep atoms *)
  | Phi
  | Epsilon
  | LetterSet _
  | Letter _ as r -> r

let rec simplify x =
  if need_simplify x then
    simplify (simplify_step x)
  else
    x

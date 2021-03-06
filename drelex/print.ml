open Types


let superscript_of_digit = function
  | '0' -> "⁰"
  | '1' -> "¹"
  | '2' -> "²"
  | '3' -> "³"
  | '4' -> "⁴"
  | '5' -> "⁵"
  | '6' -> "⁶"
  | '7' -> "⁷"
  | '8' -> "⁸"
  | '9' -> "⁹"
  | _ -> assert false


let subscript_of_digit = function
  | '0' -> "₀"
  | '1' -> "₁"
  | '2' -> "₂"
  | '3' -> "₃"
  | '4' -> "₄"
  | '5' -> "₅"
  | '6' -> "₆"
  | '7' -> "₇"
  | '8' -> "₈"
  | '9' -> "₉"
  | _ -> assert false


let transform_int f i =
  BatString.fold_right (fun c str ->
    f c :: str
  ) (string_of_int i) []
  |> String.concat ""


let superscript_int = transform_int superscript_of_digit
let subscript_int = transform_int subscript_of_digit


let rec is_range = function
  | Choice (_, Letter _, r) -> is_range r
  | Letter _ -> true
  | _ -> false


let rec last_consecutive l1 = function
  | Choice (_, Letter l2, r2) as r ->
      if Char.code l1 + 1 = Char.code l2 then
        last_consecutive l2 r2
      else
        (l1, Some r)
  | Letter l1 ->
      (l1, None)
  | _ -> assert false


let rec string_of_range = function
  | Choice (_, Letter l1, r) ->
      let (l2, next) = last_consecutive l1 r in
      if l1 = l2 then
        Char.escaped l1 ^ string_of_range r
      else
        begin match next with
        | None ->
            if Char.code l1 + 1 = Char.code l2 then
              Char.escaped l1 ^ Char.escaped l2
            else
              Char.escaped l1 ^ "-" ^ Char.escaped l2
        | Some next ->
            Char.escaped l1 ^ "-" ^ Char.escaped l2 ^ string_of_range next
        end

  | Letter l -> Char.escaped l
  | _ -> assert false

(*let and_op = "∩"*)
(*let or_op  = "+"*)

let and_op = "&"
let or_op  = "|"

let string_of_pattern string_of_label p =
  let rec string_of_pattern indent inprec p =
    let prec = Pattern.precedence p in

    let result =
      match p with
      | VarGroup (_, f, p) ->
          string_of_label f ^ ":" ^ string_of_pattern false prec p
      | Intersect (_, p1, p2) ->
          string_of_pattern indent prec p1 ^ and_op ^ string_of_pattern indent prec p2
      | Concat (_, p1, p2) ->
          string_of_pattern indent prec p1 ^ string_of_pattern indent prec p2
      | Choice (_, p1, p2) ->
          if indent then
            string_of_pattern indent prec p1 ^ "\n  " ^ or_op ^ " " ^ string_of_pattern indent prec p2
          else
            string_of_pattern indent prec p1 ^ or_op ^ string_of_pattern indent prec p2
      | Star (p) ->
          if indent then
            "(\n    " ^ string_of_pattern indent prec p ^ "\n)*"
          else
            string_of_pattern indent prec p ^ "*"
      | Repeat (_, p, n) ->
          string_of_pattern indent prec p ^ superscript_int n
      (*| Not (_, Phi) -> ".*"*)
      | Not (_, p) ->
          "¬" ^ string_of_pattern indent prec p
      | Epsilon -> "ε"
      | Phi -> "Ø"
      | Letter l -> Char.escaped l
      | LetterSet set ->
          CharSet.to_string set
    in

    (* We don't need >= since all operators are associative. *)
    if inprec > prec then
      "(" ^ result ^ ")"
    else
      result
  in

  string_of_pattern true 1000 p
  (*Show.show<int pattern> p*)

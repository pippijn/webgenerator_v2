open BatPervasives
open Types
open Ast


let rec extract_pattern = function
  | Eof -> Epsilon
  | Char chr -> Letter (Sloc.value chr)
  | Sequence [] ->
      Epsilon
  | Sequence l ->
      BatList.reduce
        (fun l r -> Concat (Maybe, r, l))
        (List.rev_map extract_pattern l)
  | Alternation [] -> failwith "Empty Alternation"
  | Alternation l ->
      BatList.reduce
        (fun l r -> Choice (Maybe, r, l))
        (List.rev_map extract_pattern l)
  | Intersection [] -> failwith "Empty Intersection"
  | Intersection l ->
      BatList.reduce
        (fun l r -> Intersect (Maybe, r, l))
        (List.rev_map extract_pattern l)
  | CharClass (Positive []) -> failwith "Empty CharClass"
  | CharClass (Positive l) ->
      let set = CharSet.create () in
      List.iter
        (function
          | Single chr ->
              CharSet.add set (Sloc.value chr)
          | _ -> assert false
        ) l;
      LetterSet set
  | Star re ->
      Types.Star (extract_pattern re)
  | Negation re ->
      Not (Maybe, extract_pattern re)
  | Binding (re, name) ->
      VarGroup (Maybe, Sloc.value name, extract_pattern re)

  | r -> failwith @@ Show.show<Ast.regexp> r


let make_name =
  let next = ref 0 in
  fun () ->
    incr next;
    string_of_int !next


let extract_rules = function
  | Rule (re, code) ->
      VarGroup (Maybe, Sloc.value code, extract_pattern re)

let extract_rules_lexers = function
  | Lexer (_, _, rules) ->
      BatList.map extract_rules rules

let extract_rules_program = function
  | Program (_, _, [lexers], _) ->
      BatList.reduce
        (fun l r -> Choice (Maybe, r, l))
        (List.rev (extract_rules_lexers lexers))
  | _ -> assert false

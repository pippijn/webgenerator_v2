let (%) = BatPervasives.(%)

open Ast
open Nfa
open Types


let codegen ast =
  let out = open_out "drelex/testNfa.ml" in
  let fmt = Format.formatter_of_out_channel out in
  let pr x =
    Format.kfprintf
      (fun fmt -> Format.pp_print_char fmt '\n')
      fmt x
  in

  Format.pp_set_margin fmt 90;

  let pre, lexers, post =
    match ast with
    | Program (pre, [], lexers, post) ->
        pre, lexers, post
    | _ ->
        failwith "unresolved aliases in codegen"
  in

  BatOption.may (pr "%s" % Sloc.value) pre;
  pr "";

  List.iter (fun (Lexer (name, args, rules)) ->
      let patterns =
        List.map ExtractPattern.extract_rules rules
        |> List.map Language.compute_nullable
      in

      let lexer =
        BatList.reduce
          (fun l r -> Choice (Tribool.Maybe, r, l))
          (List.rev patterns)
        |> Language.compute_nullable
      in

      let lexer, varmap = Pattern.number_pattern lexer in

      let nfa = NfaConstruct.build varmap lexer in

      pr "";
      pr "module NFA_%a = struct" Sloc.pp name;
      pr "  open Nfa";
      pr "  open NfaSimulate";
      pr "  open Lexing";
      pr "  %a" Show.format<Label.t pattern> lexer;
      pr "end";
      pr "";
    ) lexers;

  pr "";
  (*
  pr "let nfa : nfa = Marshal.from_string \"%s\" 0"
    (Marshal.to_string nfa [] |> String.escaped);
  pr "";
  pr "let token lexbuf =";
  pr "  lexbuf.lex_last_pos <- -1;";
  pr "  lexbuf.lex_last_action <- -1;";
  pr "  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;";
  pr "";
  pr "  read_token nfa lexbuf nfa.start;";
  pr "";
  pr "  if lexbuf.lex_last_action = -1 then";
  pr "    raise End_of_file;";
  pr "";
  pr "  let env = nfa.last_env in";
  (*
  Array.iteri (fun i p ->
      if Bitset.mem nfa.final (Label.make i) then
        match p with
        | p ->
            print_endline @@ Show.show<Label.t pattern> p
    ) nfa.inversion;
  *)
  (*
  Array.iteri (fun i var ->
      pr "  | %d -> failwith \"%s\""
        i (String.escaped var)
    ) nfa.varmap;
  pr "  | n -> print_endline @@ Show.show<Types.env> nfa.last_env";
  *)
  pr "  ()";
  pr ";;";
  *)

  pr "";
  BatOption.may (pr "%s" % Sloc.value) post;

  close_out out;
;;

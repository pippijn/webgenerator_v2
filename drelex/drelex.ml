let slurp file =
  let ic = open_in file in
  let result =
    let lst = ref [] in
    try while true do lst := input_line ic :: !lst done; assert false
    with End_of_file -> List.rev !lst |> String.concat "\n"
  in
  close_in ic;
  result


let () =
  Printexc.record_backtrace true;

  match Sys.argv with
  | [|_; mll; input|] ->
      let ast =
        try
          let fh = open_in Sys.argv.(1) in
          let lexbuf = Lexing.from_channel fh in
          let ast = Oparser.parse Olexer.(token (make ())) lexbuf in
          close_in fh;
          ast
        with
        | Oparser.StateError (token, state) ->
            Printf.printf "in state %d, unexpected %s\n"
              state (Olexer.to_string token);
            exit 1
      in

      let ast = Resolve.resolve ast in
      let ast = Simplify.simplify ast in
      (*print_endline @@ Show.show<Ast.t> ast;*)

      let pat = ExtractPattern.extract_rules_program ast in
      let pat = Language.compute_nullable pat in
      let npat, varmap = Pattern.number_pattern pat in

      (*print_endline @@ Show.show<string Types.pattern> pat;*)

      let (nfa, start) = NfaConstruct.build varmap npat in
      Printf.printf "%d states\n" (Hashtbl.length nfa);

      let nfa = NfaConstruct.optimised (nfa, start) in

      let input = slurp input in
      NfaSimulate.run_loop_opt 0 nfa varmap input

  | _ ->
      failwith "Usage: drelex <lexer.mll> <input>"

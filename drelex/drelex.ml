let () =
  Printexc.record_backtrace true;

  let fh = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel fh in

  let ast = Oparser.parse Olexer.(token (make ())) lexbuf in
  let ast = Resolve.resolve ast in
  let ast = Simplify.simplify ast in
  (*print_endline @@ Show.show<Ast.t> ast;*)

  let pat = ExtractPattern.extract_rules_program ast in
  let pat = Language.compute_nullable pat in
  let npat, varmap = Pattern.number_pattern pat in

  print_endline @@ Show.show<string Types.pattern> pat;

  let (nfa, start) = NfaConstruct.build varmap npat in

  close_in fh

{
type t =
  | Comment
  | Any
  | Eof
}

rule token = parse
| "/*" ~( _* "*/" _* ) "*/"		{ Comment }
(*| "/*" ([^'*']|'*'[^'/'])* "*/"		{ Comment }*)
| _					{ Any }

| eof					{ Eof }
| _ as c				{ failwith (Char.escaped c) }


{
  let () =
    let fh = open_in "comments.txt" in
    let lexbuf = Lexing.from_channel fh in

    while token lexbuf <> Eof do () done
}

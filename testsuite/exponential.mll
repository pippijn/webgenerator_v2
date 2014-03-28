{
type t =
  | AStarB
  | A
  | Eof
}

let a = ['a''\n']

rule token = parse
| a*'b'					{ AStarB }
| a					{ A }

| eof					{ Eof }
| _ as c				{ failwith (Char.escaped c) }


{
  let () =
    let fh = open_in "exponential.txt" in
    let lexbuf = Lexing.from_channel fh in

    while token lexbuf <> Eof do () done
}

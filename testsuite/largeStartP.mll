{
type t =
  | AStarB
  | A
  | Eof
}

let a = ['a''\n']

rule token = parse
| a*('a' as x)				{ AStarB }

| eof					{ Eof }
| _ as c				{ failwith (Char.escaped c) }


{
  let () =
    let fh = open_in "exponential.txt" in
    let lexbuf = Lexing.from_channel fh in

    while token lexbuf <> Eof do () done
}

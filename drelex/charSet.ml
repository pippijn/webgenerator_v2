type t = bool array

let create () =
  Array.make CharClass.set_end false

let add set chr =
  set.(Char.code chr) <- true

let mem set chr =
  set.(Char.code chr)

let invert set =
  Array.map (not) set

let is_mostly_true set =
  let t = ref 0 in
  let f = ref 0 in
  Array.iter (function
      | true  -> incr t
      | false -> incr f
    ) set;
  !t > !f

let is_wildcard set =
  try
    (* If the character set is all true, it's the wildcard ".". *)
    Array.iter (fun x -> if not x then raise Exit) set;
    true
  with Exit ->
    false

let to_string set =
  if is_wildcard set then
    "."
  else
    let buf = Buffer.create CharClass.set_end in

    Buffer.add_char buf '[';

    let set =
      if is_mostly_true set then (
        Buffer.add_char buf '^';
        invert set
      ) else (
        set
      )
    in

    Array.iteri (fun i -> function
        | true ->
            Buffer.add_string buf
              (match Char.chr i with
               | '[' -> "\\["
               | ']' -> "\\]"
               | c -> Char.escaped c)
        | false ->
            ()
      ) set;

    Buffer.add_char buf ']';

    Buffer.contents buf


module Show_t = Deriving_Show.Defaults(struct

  type a = t

  let format fmt set =
    Format.pp_print_char fmt '(';
    Format.pp_print_string fmt (to_string set);
    Format.pp_print_char fmt ')';
  ;;

end)

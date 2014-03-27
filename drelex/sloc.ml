type position = Lexing.position = {
  pos_fname : string;
  pos_lnum  : int;
  pos_bol   : int;
  pos_cnum  : int;
} deriving (Show)

let dummy_pos = Lexing.dummy_pos
let generated_pos = Lexing.({ dummy_pos with pos_fname = "<generated>" })

let to_string pos =
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)


type 'a t =
  'a * position * position

module Show_t(S : Deriving_Show.Show) = Deriving_Show.Defaults(struct

  type a = S.a t

  let format fmt (t, s, e : a) =
    S.format fmt t

end)


let value   (t, s, e : 'a t) = t
let start_p (t, s, e : 'a t) = s
let end_p   (t, s, e : 'a t) = e

let compare ?(cmp=Pervasives.compare) (a, _, _) (b, _, _) = cmp a b
let equal   ?(eq =Pervasives.(=))     (a, _, _) (b, _, _) = eq  a b

let at (_, s, e) t =
  (t, s, e)

let map f (t, s, e : 'a t) : 'b t =
  (f t, s, e)

let dummy t =
  t, dummy_pos, dummy_pos

let generated t =
  t, generated_pos, generated_pos

let empty_string = dummy ""

let output_string out (t, s, e) =
  output_string out t

let pp_print_string fmt (t, s, e) =
  Format.pp_print_string fmt t

open Types

type t = int -> env -> env

module Show_t = Deriving_Show.Defaults(struct

  type a = t

  let format fmt _ =
    Format.pp_print_string fmt "<fun>"

end)


let rec update x : t = fun pos -> function
  | [] -> raise Not_found
  | (y, position) :: env ->
      if y = x then
        let lo = decode_start_p position in
        (y, encode_pos lo pos) :: env
      else
        (y, position) :: update x pos env


let update x : t = fun pos l ->
  try
    update x pos l
  with Not_found ->
    (x, encode_pos pos pos) :: l


let rec rename0 vars = function
  | [] -> []
  | (x, w as var) :: l ->
      if List.memq x vars then
        (-x, w) :: rename0 vars l
      else if List.memq (-x) vars then
        (* Throw away all previous versions of this var. *)
        rename0 vars l
      else
        var :: rename0 vars l

let rename vars pos l =
  (* rename the previous match *)
  rename0 vars l


let identity pos a = a


let iterate p =
  (* find all variable names in the pattern *)
  rename (Pattern.vars_of_pattern p)


let compose f g : t =
  fun pos env -> f pos (g pos env)


let execute f pos env =
  f pos env


let compile f = f


let to_string varmap f = ""

open Types

type t = int -> env -> env

module Show_t = Deriving_Show.Defaults(struct

  type a = t

  let format fmt _ =
    Format.pp_print_string fmt "<fun>"

end)


let rec update x : t = fun end_p -> function
  | [] -> raise Not_found
  | Pos (y, start_p, _) as p :: env ->
      if y = x then
        Pos (y, start_p, end_p) :: env
      else
        p :: update x end_p env


let update x : t = fun pos l ->
  try
    update x pos l
  with Not_found ->
    Pos (x, pos, pos) :: l


let rec rename0 vars = function
  | [] -> []
  | Pos (x, s, e) as var :: l ->
      if List.memq x vars then
        Pos (-x, s, e) :: rename0 vars l
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

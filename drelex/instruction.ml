open Types

type t = instruction
  deriving (Show)

let identity = Identity
let update x = Update x
let iterate p = Iterate (Pattern.vars_of_pattern p)
let compose f g =
  match f, g with
  | Identity, f
  | f, Identity ->
      f
  | f, g ->
      Compose (f, g)

let rec compile f =
  match f with
  | Identity -> Transition.identity
  | Update x -> Transition.update x
  | Iterate vars -> Transition.rename vars
  | Compose (Identity, f)
  | Compose (f, Identity) -> compile f
  | Compose (f, g) -> Transition.compose (compile f) (compile g)

let rec execute f pos env =
  match f with
  | Identity -> env
  | Update x -> Transition.update x pos env
  | Iterate vars -> Transition.rename vars pos env
  | Compose (f, g) -> execute f pos (execute g pos env)

(*let execute f pos env = env*)


let to_string string_of_label f =
  let rec to_string = function
    | Identity -> ""
    | Update x -> "update(" ^ string_of_label x ^ ")"
    | Iterate vars -> "iterate_{" ^ String.concat "," (List.map string_of_label vars) ^ "}"
    | Compose (f, Identity)
    | Compose (Identity, f) -> to_string f
    | Compose (f, g) -> to_string f ^ "; " ^ to_string g
  in

  "{" ^ to_string f ^ "}"

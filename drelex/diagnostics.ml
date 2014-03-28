open BatPervasives

type severity =
  | Info
  | Warning
  | Error

type diagnostic = {
  severity  : severity;
  location  : Sloc.position;
  message   : string;
}


let want_colour = Unix.(isatty stdout)
let want_colour = true


let string_of_severity terminal =
  let module T = (val terminal : TermColour.S) in
  let module Colour = TermColour.Make(T) in

  let open Colour in function
  | Info    -> bcyan "info"
  | Warning -> bpink "warning"
  | Error   -> bred  "error"


let string_of_severity =
  if want_colour then
    string_of_severity (module TermColour.ANSI : TermColour.S)
  else
    string_of_severity (module TermColour.None : TermColour.S)


let message_colour terminal =
  let module T = (val terminal : TermColour.S) in
  let module Colour = TermColour.Make(T) in

  let open Colour in
  fun msg -> bwhite msg


let message_colour =
  if want_colour then
    message_colour (module TermColour.ANSI : TermColour.S)
  else
    message_colour (module TermColour.None : TermColour.S)


exception Exit

let diagnostics = Stack.create ()

let diagnostic severity (_, location, _) =
  (* Let the client code do the actual formatting. *)
  Format.ksprintf (fun message ->
    (* Add the diagnostic to the list. *)
    Stack.push {
      severity;
      location;
      message;
    } diagnostics
  )

let info    at = diagnostic Info    at
let warning at = diagnostic Warning at
let error   at = diagnostic Error   at


let exit_on_error () =
  Stack.iter (function
    | { severity = Error } -> raise Exit
    | _ -> ()
  ) diagnostics


let print () =
  (* Reverse the diagnostics stack, so we show the oldest first. *)
  let reverse = Stack.create () in
  Stack.iter (flip Stack.push reverse) diagnostics;

  Stack.iter (fun { severity; location; message } ->
    (* show diagnostic on stdout *)
    Printf.printf "%s: %s: %s\n"
      (Sloc.to_string location |> message_colour)
      (string_of_severity severity)
      (message_colour message)
  ) reverse

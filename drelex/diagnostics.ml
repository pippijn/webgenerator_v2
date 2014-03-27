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


let string_of_severity terminal =
  let module T = (val terminal : TermColour.S) in
  let module Colour = TermColour.Make(T) in

  let open Colour in function
  | Info    -> cyan   "Info"
  | Warning -> yellow "Warning"
  | Error   -> red    "Error"


let string_of_severity =
  if Unix.(isatty stdout) then
    string_of_severity (module TermColour.ANSI : TermColour.S)
  else
    string_of_severity (module TermColour.None : TermColour.S)


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
    Printf.printf "%s:\n%s: %s\n"
      (Sloc.to_string location)
      (string_of_severity severity)
      message
  ) reverse

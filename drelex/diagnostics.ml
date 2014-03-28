module Colour = TermColour.Make(TermColour.ANSI)

type severity =
  | Info
  | Warning
  | Error

type diagnostic = {
  severity  : severity;
  location  : Sloc.position;
  message   : string;
}


let string_of_severity =
  let open Colour in function
  | Info    -> bcyan "info"
  | Warning -> bpink "warning"
  | Error   -> bred  "error"


exception Exit

let diagnostics = ref []

let diagnostic severity (_, location, _) =
  (* Let the client code do the actual formatting. *)
  Format.ksprintf (fun message ->
    (* Add the diagnostic to the list. *)
    diagnostics := {
      severity;
      location;
      message;
    } :: !diagnostics
  )

let info    at = diagnostic Info    at
let warning at = diagnostic Warning at
let error   at = diagnostic Error   at


let exit_on_error () =
  List.iter (function
    | { severity = Error } -> raise Exit
    | _ -> ()
  ) !diagnostics


let print () =
  (* Reverse the diagnostics stack, so we show the oldest first. *)
  let reverse = List.rev !diagnostics in

  List.iter (fun { severity; location; message } ->
    (* show diagnostic on stdout *)
    Printf.printf "%s: %s: %s\n"
      (Sloc.to_string location |> Colour.bwhite)
      (string_of_severity severity)
      (Colour.bwhite message)
  ) reverse

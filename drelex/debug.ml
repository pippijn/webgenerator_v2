open Types


let _timing = false


let time label f =
  if _timing then
    let s = Unix.gettimeofday () in
    let r = f () in
    let e = Unix.gettimeofday () in
    Printf.printf "%s: %.06f sec\n" label (e -. s);
    flush stdout;
    r
  else
    f ()


let string_of_label varmap label =
  Array.get varmap (label - 1)


let string_of_pattern varmap =
  Print.string_of_pattern (string_of_label varmap)


let string_of_label varmap x =
  if x < 0 then
    varmap.(-x - 1) ^ "'"
  else
    varmap.( x - 1)


let show ?(pre="") varmap input states =
  match states with
  | (p, env, pos) ->
      let is_final =
        if Language.nullable p = Yes then
          "\t(FINAL)"
        else
          ""
      in
      print_endline (
        "  state: " ^
        pre ^
        string_of_pattern varmap p ^
        is_final
      );
      print_endline (
        "  env:   [" ^
        String.concat ", " (
          List.rev_map (fun (x, (Pos (start_p, end_p))) ->
            Printf.sprintf "(%s: \"%s\")"
              (string_of_label varmap x)
              (String.escaped (String.sub input start_p (end_p - start_p + 1)))
          ) env
        ) ^
        "]"
      )


let show_list ?(pre="") varmap input states =
  List.iter (fun (p, env) ->
    let is_final =
      if Language.nullable p = Yes then
        "\t(FINAL)"
      else
        ""
    in
    print_endline (
      "  state: " ^
      pre ^
      string_of_pattern varmap p ^
      is_final
    );
    print_endline (
      "  env:   [" ^
      String.concat ", " (
        List.rev_map (fun (x, (Pos (start_p, end_p))) ->
          Printf.sprintf "(%s: \"%s\")"
            (string_of_label varmap x)
            (String.escaped (String.sub input start_p (end_p - start_p + 1)))
        ) env
      ) ^
      "]"
    );
  ) states


let show_internal inversion varmap input states =
  List.iter (fun (p, env) ->
    let p' = inversion.(p) in
    show_list ~pre:(string_of_int p ^ ": ") varmap input [p', env]
  ) states

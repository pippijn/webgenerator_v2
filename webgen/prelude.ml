module Filename = struct
  include Filename

  let extension name =
    if not @@ String.contains (basename name) '.' then
      ""
    else
      let base = chop_extension name in
      let base_len = String.length base in
      String.sub name base_len (String.length name - base_len)


  let () =
    assert (extension "hello.c" = ".c");
    assert (extension "foo/hello.bar" = ".bar");
  ;;
end


module Sys = struct
  include Sys

  let exec cmd =
    let cmd = Array.of_list cmd in

    let failure how number =
      failwith (
        "process " ^ cmd.(0) ^ how ^ " " ^ string_of_int number
      )
    in

    match Unix.fork () with
    | 0 ->
        (* child *)
        Unix.execv cmd.(0) cmd
    | pid ->
        match snd @@ Unix.waitpid [] pid with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED   s -> failure "exited with status" s
        | Unix.WSIGNALED s -> failure "killed by signal"   s
        | Unix.WSTOPPED  s -> failure "stopped by signal"  s
end


let (%) f g x = f (g x)

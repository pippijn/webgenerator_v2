let dir =
  let prog = Sys.argv.(0) in
  if Sys.file_exists prog then
    (* Easy case: relative or absolute path in program name. *)
    Filename.dirname @@ Cwd.abs_path prog
  else
    (* Difficult case: find program in PATH. *)
    let path = Sys.getenv "PATH" in
    failwith path


let () =
  assert (Sys.file_exists dir);
  assert (Sys.is_directory dir);
;;

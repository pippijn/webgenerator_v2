let concat =
  List.fold_left Filename.concat ""


let split path =
  let rec split_recursive parts path =
    match Filename.dirname path with
    | "." ->
        Filename.basename path :: parts
    | "/" ->
        "/" :: Filename.basename path :: parts
    | dirname ->
        split_recursive (Filename.basename path :: parts) dirname
  in

  split_recursive [] path


let remove_tree path =
  if Sys.file_exists path then
    FileFind.find ~order:FileFind.PostOrder
      (fun path ->
         if Sys.file_exists path &&
            Sys.is_directory path then
          Unix.rmdir path
        else
          Unix.unlink path
      ) path


let make_path path =
  List.fold_left (fun path part ->
      let path = Filename.concat path part in
      if not @@ Sys.file_exists path then
        Unix.mkdir path 0o700;
      path
    ) "" (split path)
  |> ignore


let relative_to which what =
  let what_len  = String.length what  in
  let which_len = String.length which in

  if what_len > which_len &&
     String.sub what 0 which_len = which then
    String.sub what (which_len + 1) (what_len - which_len - 1)
  else
    failwith what

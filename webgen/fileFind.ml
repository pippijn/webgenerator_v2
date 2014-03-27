type order =
  | PreOrder
    (* Call user function before descending. *)
  | PostOrder
    (* Call user function after descending. *)


let find ?(order=PreOrder) wanted dir =
  let rec find_recursive path file =
    let name = Filename.concat path file in

    if order = PreOrder then
      wanted name;

    if Sys.file_exists name &&
       Sys.is_directory name then
      Array.iter
        (find_recursive name)
        (Sys.readdir name);

    if order = PostOrder then
      wanted name;
  in

  Array.iter
    (find_recursive dir)
    (Sys.readdir dir)

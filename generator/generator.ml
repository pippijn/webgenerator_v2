open Prelude


type env = {
  target : string;
}


(* Absolute current directory. *)
let pwd = Cwd.abs_path "."


(* Common content directory. *)
let global_content =
  let dir = Filename.concat DirSelf.dir "content" in
  assert (Sys.is_directory @@ Cwd.abs_path dir);
  dir


(* Debug messages. *)
let debug =
  Printf.kprintf (fun s ->
      print_endline @@ "%% " ^ s
    )


(***********************************************************************
 * :: Prepare staging directory
 ***********************************************************************)

let link_file base file =
  assert (Sys.file_exists file);

  let relpath = FilePath.relative_to base file in
  let staging_path = Filename.concat "staging" relpath in

  if Sys.is_directory file then
    FilePath.make_path staging_path
  else
    Unix.symlink (Cwd.abs_path file) staging_path;

  assert (Sys.file_exists staging_path);
;;


let prepare env =
  debug "clean staging directory";
  List.iter FilePath.remove_tree ["staging"; env.target];

  debug "link common files to staging directory";
  FileFind.find (link_file global_content) global_content;

  debug "link local files and overrides";
  FileFind.find (link_file "content") "content";

  debug "make target path";
  FilePath.make_path env.target;
;;


(***********************************************************************
 * :: Parse all source files and copy unparsed files verbatim.
 ***********************************************************************)

let parse env =
  let rebase file =
    match FilePath.split file with
    | "staging" :: path ->
        FilePath.concat (env.target :: path)
    | base :: _ ->
        failwith @@ "Invalid staging base: " ^ base
    | [] ->
        failwith "Empty file name"
  in

  FileFind.find (fun file ->
      match Filename.extension file with
      | ".css" | ".js" | ".png" | ".ico" | ".cpp" ->
          (* Just copy it to the target directory. *)
          Sys.exec ["/bin/cp"; file; rebase file]

      | ".pod" ->
          print_endline file;
          Parsers.Pod.process file

      | ".md" ->
          ()

      | ext ->
          if Sys.is_directory file then
            Unix.mkdir (rebase file) 0o700
          else
            failwith @@ "Unknown extension: " ^ ext

    ) "staging"
;;


(***********************************************************************
 * :: Main entry point
 ***********************************************************************)

let main env =
  prepare env;
  parse env;
;;


let () =
  Printexc.record_backtrace true;

  try
    main (
      match Sys.argv with
      | [|_; target|] ->
          { target }
      | _ ->
          { target = "home" }
    )
  with
  | Unix.Unix_error (errno, operation, arg) ->
      Printf.printf "Error in %s (%s): %s\n"
        operation arg (Unix.error_message errno)

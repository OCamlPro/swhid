open Swhid
open Cmdliner

(* Various helpers *)

let str =
  let doc = "object" in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let handle_result = function
  | Error msg ->
    Format.eprintf "%s@\n" msg;
    exit 1
  | Ok id -> Format.printf "%a@\n" Swhid_core.Object.pp id

let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const []))

let mk_info ~cmd ~doc ~man =
  let version = "%%VERSION%%" in
  let shared_man =
    [ `S Cmdliner.Manpage.s_bugs; `P "Email them to <contact@ndrs>." ]
  in
  let man = man @ shared_man in
  Cmd.info cmd ~version ~doc ~sdocs:Manpage.s_common_options ~man

(* Actual code of commands *)

let compute_content_file filename =
  Compute.content_identifier_from_file filename |> handle_result

let compute_content_string s = Compute.content_identifier s |> handle_result

let compute_directory s = Compute.directory_identifier_deep s |> handle_result

let download_any s =
  match Swhid_core.Object.of_string s with
  | Error _msg as e -> handle_result e
  | Ok id -> (
    match Download.any id with
    | Error l ->
      List.iter (fun msg -> Format.eprintf "%s@\n" msg) l;
      exit 1
    | Ok l -> List.iter (fun url -> Format.printf "%s@\n" url) l )

(* Sub-sub-commands definitions *)

let compute_content_file_cmd =
  let cmd = "content-file" in
  let doc = "Compute the swhid of an object of type content from a file" in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.v info Term.(const compute_content_file $ str)

let compute_content_string_cmd =
  let cmd = "content-string" in
  let doc = "Compute the swhid of an object of type content from a string" in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.v info Term.(const compute_content_string $ str)

let compute_directory_cmd =
  let cmd = "content-directory" in
  let doc = "Compute the swhid of an object of type directory" in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.v info Term.(const compute_directory $ str)

(* Sub-commands definitions *)

let compute_cmd =
  let cmd = "compute" in
  let doc = "Compute the swhid of a given object" in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.group info ~default
    [ compute_content_file_cmd
    ; compute_content_string_cmd
    ; compute_directory_cmd
    ]

let download_cmd =
  let cmd = "download" in
  let doc = "Get a download URL for a given swhid" in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.v info Term.(const download_any $ str)

(* Main command definition *)

let cli =
  let cmd = "swhid" in
  let doc =
    "Toolkit to work with persistent identifiers found in Softwae Heritage, \
     also known as swhid."
  in
  let man = [] in
  let info = mk_info ~cmd ~doc ~man in
  Cmd.group info ~default [ compute_cmd; download_cmd ]

let exit_code = Cmd.eval cli

let () = exit exit_code

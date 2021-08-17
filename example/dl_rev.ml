let id = "swh:1:rev:db21f0afdb54c16b265754ca599869fda0ca4bfc"

let id = Swhid.Parse.from_string id

let url =
  match id with
  | Error _e -> None
  | Ok id -> (
    match Swhid.Download.revision (Swhid.Lang.get_object_id id) with
    | Ok url -> Some url
    | Error _e -> None )

let () =
  match url with
  | None -> Format.eprintf "can't get download URL@."
  | Some url ->
    Format.printf "you can download the revision at the URL: `%s`@." url

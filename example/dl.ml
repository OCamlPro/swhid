(* A given identifier we want to download, here it's a file from the source
    code of FreeDink, note that
    "swh:1:cnt:80131a360f0ae3d4d643f9e222591db8d4aa744c" would have been enough *)
let id = "swh:1:cnt:80131a360f0ae3d4d643f9e222591db8d4aa744c"

(* We parse the string identifier to get a Swhid.Lang.identifier *)
let id = Swhid.Parse.from_string id

(* Here's another way to do it *)
let url =
  match id with
  | Error _e -> None
  | Ok id -> (
    match
      Swhid.Download.content ~hash_type:"sha1_git" (Swhid.Lang.get_object_id id)
    with
    | Ok url -> Some url
    | Error _e -> None )

let () =
  match url with
  | None ->
    Format.eprintf
      "Can't get a download URL, maybe the file hasn't been archived on SWH or \
       was invalid ?@.";
    exit 1
  | Some url -> Format.printf "The file can be downloaded at url `%s`.@." url

(* test content *)
let () =
  match
    Swhids.Download.content ~hash_type:"sha1_git"
      "7bdf38d4468c114206c9b6ebd9cf1176e085d346"
  with
  | Ok result ->
    assert (
      result
      = "https://archive.softwareheritage.org/api/1/content/sha1_git:7bdf38d4468c114206c9b6ebd9cf1176e085d346/raw/" )
  | Error _e -> assert false

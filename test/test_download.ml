(* test content *)
let () =
  (* TODO: we disable tests on windows because SSL certs are wrong on windows CI, see https://github.com/ocaml/setup-ocaml/issues/205 *)
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    let id = "7bdf38d4468c114206c9b6ebd9cf1176e085d346" in
    begin
      match Swhids.Download.content ~hash_type:"sha1_git" id with
      | Ok result ->
        assert (
          result
          = "https://archive.softwareheritage.org/api/1/content/sha1_git:7bdf38d4468c114206c9b6ebd9cf1176e085d346/raw/" )
      | Error e ->
        Format.eprintf "Test for id `%s` failed with error `%s`@." id e;
        assert false
    end;
    let id = "208f61cc7a5dbc9879ae6e5c2f95891e270f09ef" in
    match Swhids.Download.release id with
    | Ok result ->
      assert (
        result
        = "https://archive.softwareheritage.org/api/1/vault/directory/4453cfbdab1a996658cd1a815711664ee7742380/raw/" )
    | Error e ->
      Format.eprintf "Test for id `%s` failed with error `%s`@." id e;
      assert false

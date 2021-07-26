(* test content_identifier *)
let () =
  let test_cases =
    [| ("coucou\n", "swh:1:cnt:28d0af969b32e69a389087d7a267a2ecc05f1350")
     ; ("coucoucou\n", "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b")
    |]
  in
  Array.iter
    (fun (content, expected_identifier) ->
      let result = Swhids.Compute.content_identifier content in
      let result = Format.asprintf "%a" Swhids.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected identifier `%s` from content `%s` but got \
           identifier `%s`@."
          expected_identifier content result;
      assert ok )
    test_cases

(* test release_identifier *)
let () =
  let test_cases =
    [| ( "741b2252a5e14d6c60a913c77a6099abe73a854a"
       , Swhids.Lang.Revision
       , "v2.6.14"
       , Some "Linus Torvalds <torvalds@g5.osdl.org>"
       , Some (1130457753, 0, -420, false)
       , Some
           {|
Linux 2.6.14 release
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.1 (GNU/Linux)

iD8DBQBDYWq6F3YsRnbiHLsRAmaeAJ9RCez0y8rOBbhSv344h86l/VVcugCeIhO1
wdLOnvj91G4wxYqrvThthbE=
=7VeT
-----END PGP SIGNATURE-----
|}
       , "swh:1:rel:2b10839e32c4c476e9d94492756bb1a3e1ec4aa8" )
    |]
  in

  Array.iter
    (fun (target, target_type, name, author, date, message, expected_identifier) ->
      let result =
        Swhids.Compute.release_identifier target target_type name author date
          message
      in
      let result = Format.asprintf "%a" Swhids.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        (* TODO: be able to print a revision *)
        Format.eprintf
          "error: expected_identifier `%s` from revision but got identifier \
           `%s`@."
          expected_identifier result;
      assert ok )
    test_cases

(* test revision_identifier *)
let () =
  let test_cases =
    [| ( "swh:1:rev:bc0195aad0daa2ad5b0d76cce22b167bc3435590"
       , "85a74718d377195e1efd0843ba4f3260bad4fe07"
       , [ "01e2d0627a9a6edb24c37db45db5ecb31e9de808" ]
       , Some "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some (1436739030, 0, -420, false)
       , Some "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some (1436739030, 0, -420, false)
       , Some "Linux 4.2-rc2\n" )
    |]
  in
  Array.iter
    (fun ( expected_identifier
         , directory
         , parents
         , author
         , author_date
         , committer
         , committer_date
         , message ) ->
      let result =
        Swhids.Compute.revision_identifier directory parents author author_date
          committer committer_date [||] message
      in
      let result = Format.asprintf "%a" Swhids.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected_identifier `%s` from revision but got identifier \
           `%s`@."
          expected_identifier result;
      assert ok )
    test_cases

let () =
  let test_cases =
    [| (* empty directory *)
       ("swh:1:dir:4b825dc642cb6eb9a060e54bf8d69288fbee4904", [])
     ; (* swh example *)
       ( "swh:1:dir:d7ed3d2c31d608823be58b1cbe57605310615231"
       , [ ("file", 33188, "README", "37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21")
         ; ( "file"
           , 33188
           , "Rakefile"
           , "3bb0e8592a41ae3185ee32266c860714980dbed7" )
         ; ("dir", 16384, "app", "61e6e867f5d7ba3b40540869bc050b0c4fed9e95")
         ; ( "file"
           , 33188
           , "1.megabyte"
           , "7c2b2fbdd57d6765cdc9d84c2d7d333f11be7fb3" )
         ; ("dir", 16384, "config", "591dfe784a2e9ccc63aaba1cb68a765734310d98")
         ; ("dir", 16384, "public", "9588bf4522c2b4648bfd1c61d175d1f88c1ad4a5")
         ; ( "file"
           , 33188
           , "development.sqlite3"
           , "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" )
         ; ("dir", 16384, "doc", "154705c6aa1c8ead8c99c7915373e3c44012057f")
         ; ("dir", 16384, "db", "85f157bdc39356b7bc7de9d0099b4ced8b3b382c")
         ; ("dir", 16384, "log", "5e3d3941c51cce73352dff89c805a304ba96fffe")
         ; ("dir", 16384, "script", "1b278423caf176da3f3533592012502aa10f566c")
         ; ("dir", 16384, "test", "035f0437c080bfd8711670b3e8677e686c69c763")
         ; ("dir", 16384, "vendor", "7c0dc9ad978c1af3f9a4ce061e50f5918bd27138")
         ; ( "rev"
           , 57344
           , "will_paginate"
           , "3d531e169db92a16a9a8974f0ae6edf52e52659e" )
         ; ("dir", 16384, "order", "62cdb7020ff920e5aa642c3d4066950dd1f01f4d")
         ; ("file", 16384, "order.", "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")
         ; ("file", 16384, "order0", "bbe960a25ea311d21d40669e93df2003ba9b90a2")
         ] )
    |]
  in
  Array.iter
    (fun (expected_identifier, entries) ->
      let result = Swhids.Compute.directory_identifier entries in
      let result = Format.asprintf "%a" Swhids.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected_identifier `%s` from directory but got identifier \
           `%s`@."
          expected_identifier result;
      assert ok )
    test_cases

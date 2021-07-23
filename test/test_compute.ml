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

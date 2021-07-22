let () =
  let test_cases =
    [| ("coucou\n", "swh:1:cnt:28d0af969b32e69a389087d7a267a2ecc05f1350")
     ; ("coucoucou\n", "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b")
    |]
  in
  Array.iter
    (fun (content, expected) ->
      let result = Swhids.Compute.content_identifier content in
      let result = Format.asprintf "%a" Swhids.Pp.identifier result in
      let ok = result = expected in
      if not ok then
        Format.eprintf "error: expected `%s` from content `%s` but got `%s`@."
          expected content result;
      assert ok )
    test_cases

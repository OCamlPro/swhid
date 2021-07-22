let () =
  let test_cases =
    [| "swh:1:cnt:28d0af969b32e69a389087d7a267a2ecc05f1350"
     ; "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b"
     ; "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
     ; "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
     ; "swh:1:rel:22ece559cc7cc2364edc5e5593d63ae8bd229f9f"
     ; "swh:1:snp:c7c108084bc0bf3d81436bf980b46e98bd338453"
     ; "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
    |]
  in
  Array.iter
    (fun input ->
      let result =
        Swhids.Parser.from_string input
        |> Format.asprintf "%a" Swhids.Pp.identifier
      in
      let ok = input = result in
      if not ok then
        Format.eprintf
          "error: expected `%s` when parsing and printing `%s` but got `%s`@."
          input input result;
      assert ok )
    test_cases

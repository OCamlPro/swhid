let test_cases =
  [ ( Swhids.Lang.content ~hash_type:"sha1_git"
        "7bdf38d4468c114206c9b6ebd9cf1176e085d346" []
    , Ok
        [ Ok
            "https://archive.softwareheritage.org/api/1/content/sha1_git:7bdf38d4468c114206c9b6ebd9cf1176e085d346/raw/"
        ] )
  ; ( Swhids.Lang.release "208f61cc7a5dbc9879ae6e5c2f95891e270f09ef" []
    , Ok
        [ Ok
            "https://archive.softwareheritage.org/api/1/vault/directory/4453cfbdab1a996658cd1a815711664ee7742380/raw/"
        ] )
  ; ( Swhids.Lang.snapshot "6a3a2cf0b2b90ce7ae1cf0a221ed68035b686f5a" []
    , Ok
        [ Ok
            "https://archive.softwareheritage.org/api/1/vault/directory/eb4f88b555061f611d4d7182a0a36e5e771a73ad/raw/"
        ; Ok
            "https://archive.softwareheritage.org/api/1/vault/directory/b53b0637f5ced64109d78c7bc32ea3bccbb6106c/raw/"
        ; Ok
            "https://archive.softwareheritage.org/api/1/vault/directory/fcbc6c0ea6d27a85d6fcb3cbf9d9168e3dafd096/raw/"
        ; Ok
            "https://archive.softwareheritage.org/api/1/vault/directory/3eb35765545dcca55cb5a7f30ab31d794cc36c95/raw/"
        ] )
  ]

let pp_result_l fmt l =
  List.iter
    (function
      | Ok s -> Format.fprintf fmt "  OK: %s@." s
      | Error s -> Format.fprintf fmt "  ERROR: %s@." s )
    l

let pp_result fmt = function
  | Ok l -> Format.fprintf fmt "OK: %a@." pp_result_l l
  | Error e -> Format.fprintf fmt "ERROR: %s@." e

let () =
  (* TODO: we disable tests on windows because SSL certs are wrong on windows CI, see https://github.com/ocaml/setup-ocaml/issues/205 *)
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    List.iter
      (fun (identifier, expected_result) ->
        let result = Swhids.Download.any identifier in
        let ok = result = expected_result in
        if not ok then begin
          Format.eprintf "test failed for identifier %a@." Swhids.Pp.identifier
            identifier;
          Format.eprintf "expected: %a@." pp_result expected_result;
          Format.eprintf "got: %a@." pp_result result;
          assert false
        end )
      test_cases

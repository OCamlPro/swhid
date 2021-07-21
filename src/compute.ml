open Lang

let of_content content =
  (* Quoting SWH documentation:
   * for contents, the intrinsic identifier is the sha1_git hash returned by
   * swh.model.identifiers.content_identifier(), i.e., the SHA1 of a byte sequence
   * obtained by juxtaposing the ASCII string "blob" (without quotes), a space, the
   * length of the content as decimal digits, a NULL byte, and the actual content
   * of the file. *)
  let content = Format.sprintf "blob %d\x00%s" (String.length content) content in
  let digest = Digestif.SHA1.digest_string content |> Digestif.SHA1.to_hex in
  let object_id = Array.init 40 (String.get digest) in
  ((1, Content, object_id) , [])

let of_directory _d =
  ((1, Directory, [||]) , [])

let of_release _r =
  ((1, Release, [||]) , [])

let of_revision _r =
  ((1, Revision, [||]) , [])

let of_snapshot _s =
  ((1, Snapshot, [||]) , [])

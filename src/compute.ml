open Lang

let target_type_to_git = function
  | Content -> "blob"
  | Directory -> "tree"
  | Release -> "tag"
  | Revision -> "commit"
  | Snapshot -> "refs"

let array_from_hexdigest digest = Array.init 40 (String.get digest)

let hexdigest_from_git_object git_object =
  Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string git_object

let git_object_header fmt (git_type, len) =
  match git_type with
  | "blob"
  | "commit"
  | "extid"
  | "raw_extrinsic_metadata"
  | "snapshot"
  | "tag"
  | "tree" ->
    Format.fprintf fmt "%s %d\x00" git_type len
  | git_type ->
    Utils.error
      (Format.sprintf "invalid git object type `%s` (Compute.git_object_header)"
         git_type )

let escape_newlines snippet =
  String.concat "\n " (String.split_on_char '\n' snippet)

let format_git_object_from_headers fmt (git_type, headers, message) =
  let entries = Buffer.create 512 in

  let buff_fmt = Format.formatter_of_buffer entries in

  Array.iter
    (fun (k, v) -> Format.fprintf buff_fmt "%s %s@." k (escape_newlines v))
    headers;

  begin
    match message with
    | None -> ()
    | Some message -> Format.fprintf buff_fmt "@.%s" message
  end;

  Format.pp_print_flush buff_fmt ();

  let entries = Buffer.contents entries in

  Format.fprintf fmt "%a%s" git_object_header
    (git_type, String.length entries)
    entries

let format_author fmt = function
  | None -> ()
  | Some author -> Format.fprintf fmt "%s" author

let normalize_timestamp _date_offset = Some ("TODO", "TODO", "TODO")

let format_date fmt _date = Format.fprintf fmt "TODO"

let format_offset fmt (_offset, _negative_utc) = Format.fprintf fmt "TODO"

let format_author_data fmt (author, date_offset) =
  Format.fprintf fmt "%a" format_author author;
  let date_offset = normalize_timestamp date_offset in
  match date_offset with
  | None -> ()
  | Some (timestamp, offset, negative_utc) ->
    Format.fprintf fmt " %a %a" format_date timestamp format_offset
      (offset, negative_utc)

let content_identifier content =
  (* Quoting SWH documentation:
   * for contents, the intrinsic identifier is the sha1_git hash returned by
   * swh.model.identifiers.content_identifier(), i.e., the SHA1 of a byte sequence
   * obtained by juxtaposing the ASCII string "blob" (without quotes), a space, the
   * length of the content as decimal digits, a NULL byte, and the actual content
   * of the file. *)
  let object_type = target_type_to_git Content in
  let len = String.length content in
  let git_object =
    Format.asprintf "%a%s" git_object_header (object_type, len) content
  in
  let hexdigest = hexdigest_from_git_object git_object in
  let object_id = array_from_hexdigest hexdigest in
  ((1, Content, object_id), [])

let directory_identifier _d = ((1, Directory, [||]), [])

let release_identifier target target_type name author date message =
  let target_len = String.length target in
  if target_len <> 40 then
    Utils.error
      (Format.sprintf "target must be of length 40 but `%s` has a length of %d"
         target target_len );
  let git_object =
    let headers =
      [| ("object", target)
       ; ("type", target_type_to_git target_type)
       ; ("tag", name)
      |]
    in
    let _headers =
      match author with
      | None -> headers
      | Some _release_author ->
        Array.append headers
          [| ("tagger", Format.asprintf "%a" format_author_data (author, date))
          |]
    in
    Format.asprintf "%a" format_git_object_from_headers ("tag", headers, message)
  in
  let hexdigest = hexdigest_from_git_object git_object in
  ((1, Release, hexdigest), [])

let revision_identifier directory parents author author_date committer
    committer_date extra_headers message =
  (* the directory identifier is the ascii representation of its hexadecimal encoding *)
  let pp_tree fmt _tree = Format.fprintf fmt "TODO" in
  let pp_parents fmt _parents = Format.fprintf fmt "TODO" in
  let pp_author fmt (_author, _author_date) = Format.fprintf fmt "TODO" in
  let pp_committer fmt (_committer, _committer_date) =
    Format.fprintf fmt "TODO"
  in
  let pp_extra_headers fmt _extra_headers = Format.fprintf fmt "TODO" in

  (* if the message is None, the manifest ends with the last header ; else, the message is appended to the headers after an empty line. *)
  let pp_message fmt = function
    | None -> ()
    | Some message -> Format.fprintf fmt "@.%s" message
  in

  let commit_manifest =
    Format.asprintf {|%a@.%a@.%a@.%a@.%a@.%a|} pp_tree directory pp_parents
      parents pp_author (author, author_date) pp_committer
      (committer, committer_date)
      pp_extra_headers extra_headers pp_message message
  in

  (* the checksum of the full manifest is computed using the ‘commit’ git object type *)
  let hexdigest = hexdigest_from_git_object commit_manifest in
  let object_id = array_from_hexdigest hexdigest in

  (*

Author and committer are formatted with the format_author() function. Dates are formatted with the format_offset() function.

Extra headers are an ordered list of [key, value] pairs. Keys are strings and get encoded to utf-8 for identifier computation. Values are either byte strings, unicode strings (that get encoded to utf-8), or integers (that get encoded to their utf-8 decimal representation).

Multiline extra header values are escaped by indenting the continuation lines with one ascii space.


*)
  ((1, Revision, object_id), [])

let snapshot_identifier _s = ((1, Snapshot, [||]), [])

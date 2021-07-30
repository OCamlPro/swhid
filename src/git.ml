let object_to_hexdigest git_object =
  Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string git_object

let object_header fmt (git_type, len) =
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
    raise
    @@ Invalid_argument
         (Format.sprintf "invalid git object type `%s` (Git.object_header)"
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
    | Some message -> Format.fprintf buff_fmt "%s" message
  end;

  Format.pp_print_flush buff_fmt ();

  let entries = Buffer.contents entries in

  Format.fprintf fmt "%a%s" object_header
    (git_type, String.length entries)
    entries

let format_author fmt author = Format.fprintf fmt "%s" author

let normalize_timestamp = function
  | None -> None
  | Some time_representation ->
    let seconds, microseconds, offset, negative_utc = time_representation in
    Some ((seconds, microseconds), offset, negative_utc)

let format_date fmt (seconds, microseconds) =
  match microseconds with
  | 0 -> Format.fprintf fmt "%d" seconds
  | microseconds ->
    (* TODO: this should be the equivalent of:
     * float_value = "%d.%06d" % (seconds, microseconds)
     * return float_value.rstrip("0").encode()
     * *)
    Format.fprintf fmt "%d.%06d" seconds microseconds

let format_offset fmt (offset, negative_utc) =
  let sign =
    if offset < 0 || (offset = 0 && negative_utc) then
      "-"
    else
      "+"
  in
  let offset = Int.abs offset in
  let hours = offset / 60 in
  let minutes = offset mod 60 in
  Format.fprintf fmt "%s%02d%02d" sign hours minutes

let format_author_data fmt (author, date_offset) =
  Format.fprintf fmt "%a" format_author author;
  let date_offset = normalize_timestamp date_offset in
  match date_offset with
  | None -> ()
  | Some (timestamp, offset, negative_utc) ->
    Format.fprintf fmt " %a %a" format_date timestamp format_offset
      (offset, negative_utc)

let target_invalid target = String.length target <> 40

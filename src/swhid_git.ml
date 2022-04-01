module Make (SHA1 : sig
  val digest_string_to_hex : string -> string
end) =
struct
  let target_type_to_git = function
    | Swhid_types.Content _hash_type -> "blob"
    | Directory -> "tree"
    | Release -> "tag"
    | Revision -> "commit"
    | Snapshot -> "refs"

  let id_to_bytes id =
    String.init
      (String.length id / 2)
      (fun i ->
        let s = String.sub id (2 * i) 2 in
        Char.chr @@ int_of_string @@ "0x" ^ s )

  let object_to_swhid (obj : string) (qualifiers : Swhid_types.qualifier list)
      mk_id : Swhid_types.identifier option =
    let hexdigest = SHA1.digest_string_to_hex obj in
    Option.map
      (fun obj -> mk_id obj qualifiers)
      (Swhid_types.object_id_from_string hexdigest)

  let object_header fmt (git_type, len) =
    match git_type with
    | "blob" | "commit" | "extid" | "raw_extrinsic_metadata" | "snapshot"
    | "tag" | "tree" ->
      Format.fprintf fmt "%s %d\x00" git_type len
    | git_type ->
      raise
      @@ Invalid_argument
           (Format.sprintf "invalid git object type `%s` (Git.object_header)"
              git_type )

  let object_from_contents_strtarget target_type contents =
    let len = String.length contents in
    Format.asprintf "%a%s" object_header (target_type, len) contents

  let object_from_contents target_type contents =
    object_from_contents_strtarget (target_type_to_git target_type) contents

  let escape_newlines snippet =
    String.concat "\n " (String.split_on_char '\n' snippet)

  let format_offset fmt (offset, negative_utc) =
    let sign =
      if offset < 0 || (offset = 0 && negative_utc) then "-" else "+"
    in
    let offset = Int.abs offset in
    let hours = offset / 60 in
    let minutes = offset mod 60 in
    Format.fprintf fmt "%s%02d%02d" sign hours minutes

  let format_author_data fmt (author, date) =
    Format.fprintf fmt "%s" author;
    match date with
    | None -> ()
    | Some (timestamp, tz_offset, negative_utc) ->
      Format.fprintf fmt " %d %a" timestamp format_offset
        (tz_offset, negative_utc)
end

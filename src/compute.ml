open Lang

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
    Format.asprintf "%a%s" Git.object_header (object_type, len) content
  in
  let hexdigest = Git.object_to_hexdigest git_object in
  Option.map
    (fun object_id -> Lang.content object_id [])
    (object_id_from_string hexdigest)

let id_to_bytes id =
  let buff = Buffer.create 512 in
  String.iteri
    (fun i c ->
      if i mod 2 = 0 then
        let c2 = String.get id (i + 1) in
        let s = Format.sprintf "%c%c" c c2 in
        let n = int_of_string (Format.sprintf "0x%s" s) in
        Buffer.add_char buff (Char.chr n) )
    id;
  Buffer.contents buff

let directory_identifier entries =
  List.iter
    (fun (_typ, _perms, _name, target) ->
      if Git.target_invalid target then
        raise @@ Invalid_argument "target must be of length 40" )
    entries;
  let entries =
    List.sort
      (fun (typ1, _perms, name1, _target1) (typ2, _perms2, name2, _target2) ->
        String.compare
          ( name1
          ^
          if typ1 = "dir" then
            "/"
          else
            "" )
          ( name2
          ^
          if typ2 = "dir" then
            "/"
          else
            "" ) )
      entries
  in
  let object_type = target_type_to_git Directory in
  let content =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _fmt () -> ())
         (fun fmt (_typ, perms, name, target) ->
           Format.fprintf fmt "%o %s%c%s" perms name '\x00' (id_to_bytes target)
           ) )
      entries
  in

  let len = String.length content in
  let git_object =
    Format.asprintf "%a%s" Git.object_header (object_type, len) content
  in
  let hexdigest = Git.object_to_hexdigest git_object in
  Option.map
    (fun object_id -> Lang.directory object_id [])
    (object_id_from_string hexdigest)

let release_identifier target target_type name author date message =
  if Git.target_invalid target then
    raise @@ Invalid_argument "target must be of length 40";
  let git_object =
    let headers =
      [| ("object", target)
       ; ("type", target_type_to_git target_type)
       ; ("tag", name)
      |]
    in
    let headers =
      match author with
      | None -> headers
      | Some author ->
        Array.append headers
          [| ( "tagger"
             , Format.asprintf "%a" Git.format_author_data (author, date) )
          |]
    in
    Format.asprintf "%a" Git.format_git_object_from_headers
      ("tag", headers, message)
  in
  let hexdigest = Git.object_to_hexdigest git_object in
  Option.map
    (fun object_id -> Lang.release object_id [])
    (object_id_from_string hexdigest)

let revision_identifier directory parents author author_date committer
    committer_date extra_headers message =
  if List.exists Git.target_invalid (directory :: parents) then
    raise
    @@ Invalid_argument "target (directory and parents) must be of length 40";

  (* TODO: this is probably false :-) *)
  let rec pp_parents fmt = function
    | [] -> ()
    | parent :: parents ->
      Format.fprintf fmt "%s" parent;
      pp_parents fmt parents
  in

  let git_object =
    let headers = [| ("tree", directory) |] in
    let headers =
      match parents with
      | [] -> headers
      | parents ->
        Array.append headers
          [| ("parent", Format.asprintf "%a" pp_parents parents) |]
    in
    let headers =
      match author with
      | None -> headers
      | Some author ->
        Array.append headers
          [| ( "author"
             , Format.asprintf "%a" Git.format_author_data (author, author_date)
             )
          |]
    in
    let headers =
      match committer with
      | None -> headers
      | Some committer ->
        Array.append headers
          [| ( "committer"
             , Format.asprintf "%a" Git.format_author_data
                 (committer, committer_date) )
          |]
    in
    let headers = Array.append headers extra_headers in

    let message =
      match message with
      | None -> None
      | Some msg -> Some (Format.sprintf "@.%s" msg)
    in

    Format.asprintf "%a" Git.format_git_object_from_headers
      ("commit", headers, message)
  in

  let hexdigest = Git.object_to_hexdigest git_object in

  Option.map
    (fun object_id -> Lang.revision object_id [])
    (object_id_from_string hexdigest)

let snapshot_identifier branches =
  let branches =
    List.sort
      (fun (name1, _target) (name2, _target) -> String.compare name1 name2)
      branches
  in
  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in
  List.iter
    (fun (branch_name, target) ->
      let target, target_type, target_id_len =
        match target with
        | None -> ("", "dangling", 0)
        | Some (target, target_type) -> (
          match target_type with
          | "content"
          | "directory"
          | "revision"
          | "release"
          | "snapshot" ->
            (id_to_bytes target, target_type, 20)
          | "alias" -> (target, "alias", String.length target)
          | _type -> assert false )
      in
      Format.fprintf fmt "%s %s%c%d:%s" target_type branch_name '\x00'
        target_id_len target )
    branches;
  Format.pp_print_flush fmt ();
  let content = Buffer.contents buff in
  let len = String.length content in
  let git_object =
    Format.asprintf "%a%s" Git.object_header ("snapshot", len) content
  in
  let hexdigest = Git.object_to_hexdigest git_object in

  Option.map
    (fun object_id -> Lang.snapshot object_id [])
    (object_id_from_string hexdigest)

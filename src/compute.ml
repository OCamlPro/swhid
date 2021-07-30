open Lang

(** Compute the software heritage identifier for a given content *)
let content_identifier content : Lang.identifier option =
  let git_object = Git.object_from_contents Content content in
  Git.object_to_swhid git_object [] Lang.content

(** Compute the software heritage identifier for a given directory *)
let directory_identifier entries : Lang.identifier option =
  List.iter
    (fun (_typ, _perms, _name, target) ->
      if Git.target_invalid target then
        raise @@ Invalid_argument "target must be of length 40" )
    entries;
  let entries =
    List.sort
      (fun (typ1, _perms, name1, _target1) (typ2, _perms2, name2, _target2) ->
        String.compare
          ( if typ1 = "dir" then
            name1 ^ "/"
          else
            name1 )
          ( if typ2 = "dir" then
            name2 ^ "/"
          else
            name2 ) )
      entries
  in
  let content =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _fmt () -> ())
         (fun fmt (_typ, perms, name, target) ->
           Format.fprintf fmt "%o %s%c%s" perms name '\x00'
             (Git.id_to_bytes target) ) )
      entries
  in
  let git_object = Git.object_from_contents Directory content in
  Git.object_to_swhid git_object [] Lang.directory

(** Compute the software heritage identifier for a given release *)
let release_identifier ~target target_type ~name ~author ~date ~message :
    Lang.identifier option =
  if Git.target_invalid target then
    raise @@ Invalid_argument "target must be of length 40";

  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in

  Format.fprintf fmt "object %s@.type %s@.tag %s@." target
    (Git.target_type_to_git target_type)
    (Git.escape_newlines name);

  begin
    match author with
    | None -> ()
    | Some author ->
      Format.fprintf fmt "tagger %a@." Git.format_author_data
        (Git.escape_newlines author, date)
  end;

  begin
    match message with
    | None -> ()
    | Some message -> Format.fprintf fmt "@.%s" message
  end;

  Format.pp_print_flush fmt ();

  let content = Buffer.contents buff in

  let git_object = Git.object_from_contents Release content in

  Git.object_to_swhid git_object [] Lang.release

(** Compute the software heritage identifier for a given revision *)
let revision_identifier directory parents ~author ~author_date ~committer
    ~committer_date _extra_headers message : Lang.identifier option =
  if List.exists Git.target_invalid (directory :: parents) then
    raise
    @@ Invalid_argument "target (directory and parents) must be of length 40";

  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in

  Format.fprintf fmt "tree %s@." directory;

  List.iter (fun parent -> Format.fprintf fmt "parent %s@." parent) parents;

  begin
    match author with
    | None -> ()
    | Some author ->
      Format.fprintf fmt "author %a@." Git.format_author_data
        (Git.escape_newlines author, author_date)
  end;

  begin
    match committer with
    | None -> ()
    | Some committer ->
      Format.fprintf fmt "committer %a@." Git.format_author_data
        (Git.escape_newlines committer, committer_date)
  end;

  (* TODO: extra headers *)
  begin
    match message with
    | None -> ()
    | Some message -> Format.fprintf fmt "@.%s" message
  end;

  Format.pp_print_flush fmt ();

  let content = Buffer.contents buff in

  let git_object = Git.object_from_contents Revision content in

  Git.object_to_swhid git_object [] Lang.revision

(** Compute the software heritage identifier for a given snapshot *)
let snapshot_identifier branches : Lang.identifier option =
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
            (Git.id_to_bytes target, target_type, 20)
          | "alias" -> (target, "alias", String.length target)
          | target_type ->
            raise
            @@ Invalid_argument
                 (Format.sprintf
                    "invalid target type: `%s` (Compute.snapshot_identifier)"
                    target_type ) )
      in
      Format.fprintf fmt "%s %s%c%d:%s" target_type branch_name '\x00'
        target_id_len target )
    branches;
  Format.pp_print_flush fmt ();
  let content = Buffer.contents buff in
  let git_object = Git.object_from_contents_strtarget "snapshot" content in
  Git.object_to_swhid git_object [] Lang.snapshot

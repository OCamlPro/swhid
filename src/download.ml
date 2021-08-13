(** The base URL of the software heritage instance used, defaults to
    `https://archive.softwareheritage.org` *)
let instance = ref "https://archive.softwareheritage.org"

let url endpoint = Format.sprintf "%s/api/1%s" !instance endpoint

let field_not_found f =
  Error (Format.sprintf "field `%s` not found in the JSON response" f)

let on_response url f =
  match Ezcurl.get ~url () with
  | Error (code, msg) ->
    Error (Format.sprintf "curl error: code `%s` (%s)" (Curl.strerror code) msg)
  | Ok response -> (
    match Json.json_of_src (`String response.body) with
    | Error (_loc, _e) ->
      Error (Format.sprintf "error while parsing JSON response")
    | Ok response -> f response )

(** For a given content identifier, compute an URL from which the content can be
    downloaded *)
let content ?hash_type hash =
  let url =
    match hash_type with
    | None -> url (Format.sprintf "/content/%s/" hash)
    | Some hash_type -> url (Format.sprintf "/content/%s:%s/" hash_type hash)
  in
  on_response url (fun response ->
      let field = "data_url" in
      match Json.find_string field response with
      | Some data_url -> Ok data_url
      | None -> field_not_found field )

(** For a given directory identifier, compute an URL from which the directory
    can be downloaded *)
let directory hash =
  let url = url (Format.sprintf "/vault/directory/%s/" hash) in
  match Ezcurl.post ~params:[] ~url () with
  | Error (code, msg) ->
    Error (Format.sprintf "curl error: code `%s` (%s)" (Curl.strerror code) msg)
  | Ok response -> (
    match Json.json_of_src (`String response.body) with
    | Error (_loc, _e) ->
      Error (Format.sprintf "error while parsing JSON response")
    | Ok _response ->
      on_response url (fun response ->
          let field = "fetch_url" in
          match Json.find_string field response with
          | Some data_url -> Ok data_url
          | None -> field_not_found field ) )

(** For a given revision identifier, compute an URL from which the revision can
    be downloaded *)
let revision hash =
  let url = url (Format.sprintf "/revision/%s/" hash) in
  on_response url (fun response ->
      let field = "directory" in
      match Json.find_string field response with
      | None -> field_not_found field
      | Some dir -> directory dir )

(** For a given release identifier, compute an URL from which the release can be
    downloaded *)
let rec release hash =
  let url = url (Format.sprintf "/release/%s/" hash) in

  on_response url (fun response ->
      let field = "target_type" in
      match Json.find_string field response with
      | None -> field_not_found field
      | Some target_type -> (
        let field = "target" in
        match Json.find_string field response with
        | None -> field_not_found field
        | Some target -> begin
          match target_type with
          | "release" -> release target
          | "revision" -> revision target
          | "content" -> content target
          | "directory" -> directory target
          | target_type ->
            Error (Format.sprintf "unknown target type: `%s`" target_type)
        end ) )

let go_through_objs = function
  | Json.Object o ->
    let rec aux target_type target jsonl =
      match (target_type, target) with
      | Some target_type, Some target -> begin
        match target_type with
        | "revision" -> Some (revision, target)
        | "release" -> Some (release, target)
        | "content" -> Some (content ~hash_type:"sha1", target)
        | "directory" -> Some (directory, target)
        | _ -> None
      end
      | _ -> (
        match jsonl with
        | [] -> None
        | ("target_type", Json.String value) :: r -> aux (Some value) target r
        | ("target", Json.String value) :: r -> aux target_type (Some value) r
        | (_, _) :: r -> aux target_type target r )
    in
    aux None None o
  | _ -> None

let snapshot hash =
  let url = url (Format.sprintf "/snapshot/%s/" hash) in

  on_response url (fun response ->
      let field = "branches" in
      match Json.find_obj field response with
      | None -> field_not_found field
      | Some branch ->
        let requests =
          List.filter_map (fun f -> go_through_objs @@ snd f) branch
        in
        Ok (List.map (fun (f, x) -> f x) requests) )

(** For any object identifier, compute an URL from which object can be
    downloaded *)
let any (identifier : Lang.identifier) :
    ((string, string) result list, string) Result.t =
  let object_id = Lang.get_object_id identifier in
  match Lang.get_object_type identifier with
  | Lang.Content hash_type -> Ok [ content ~hash_type object_id ]
  | Directory -> Ok [ directory object_id ]
  | Release -> Ok [ release object_id ]
  | Revision -> Ok [ revision object_id ]
  | Snapshot -> snapshot object_id

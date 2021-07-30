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

(** For a given content identifier, compute an URL from which the content can we
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
    can we downloaded *)
let directory hash =
  let url = url (Format.sprintf "/vault/directory/%s/" hash) in
  on_response url (fun response ->
      let field = "fetch_url" in
      match Json.find_string field response with
      | Some fetch_url -> Ok fetch_url
      | None -> field_not_found field )

(** For a given revision identifier, compute an URL from which the revision can
    we downloaded *)
let revision hash =
  let url = url (Format.sprintf "/revision/%s/" hash) in
  on_response url (fun response ->
      let field = "directory" in
      match Json.find_string field response with
      | None -> field_not_found field
      | Some dir -> directory dir )

(** For a given release identifier, compute an URL from which the release can we
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

(** For any object identifier, compute an URL from which object can we
    downloaded *)
let any (((_scheme, object_type, object_id), _qualifiers) : Lang.identifier) =
  let open Lang in
  match object_type with
  | Content -> content object_id
  | Directory -> directory object_id
  | Release -> release object_id
  | Revision -> revision object_id
  | Snapshot -> failwith "TODO :-)"

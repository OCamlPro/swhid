type url_escaped = string

type path_absolute_escaped = string

type scheme_version = int

type object_type =
  | Content of string (* hash *)
  | Directory
  | Release
  | Revision
  | Snapshot

type object_id = string (* this always has a length of 40 digit *)

type line_number = int

type identifier_core = scheme_version * object_type * object_id

type context_qualifier =
  | Anchor of identifier_core
  | Origin of url_escaped
  | Path of path_absolute_escaped
  | Visit of identifier_core

type qualifier =
  | Context of context_qualifier
  | Fragment of (line_number * line_number option)

type qualifiers = qualifier list

type identifier = identifier_core * qualifiers

let object_type_of_string = function
  | "snp" -> Some Snapshot
  | "rel" -> Some Release
  | "rev" -> Some Revision
  | "dir" -> Some Directory
  | "cnt" -> Some (Content "sha1")
  | _s -> None

let target_invalid target =
  String.length target <> 40
  ||
  try
    String.iter
      (function
        | 'a' .. 'f'
        | '0' .. '9' ->
          ()
        | _invalid_char -> raise Exit )
      target;
    false
  with
  | Exit -> true

let object_id_from_string s =
  if target_invalid s then
    None
  else
    Some s

let content ?(hash_type = "sha1") id qualifiers =
  ((1, Content hash_type, id), qualifiers)

let directory id qualifiers = ((1, Directory, id), qualifiers)

let snapshot id qualifiers = ((1, Snapshot, id), qualifiers)

let revision id qualifiers = ((1, Revision, id), qualifiers)

let release id qualifiers = ((1, Release, id), qualifiers)

let get_object_id
    (((_scheme_version, _object_type, object_id), _qualifiers) : identifier) :
    string =
  object_id

let get_object_type
    (((_scheme_version, object_type, _object_type), _qualifiers) : identifier) :
    object_type =
  object_type

exception Parser_error of string

exception Lexer_error of string

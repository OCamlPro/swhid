type object_type =
  | Content of string (* hash *)
  | Directory
  | Release
  | Revision
  | Snapshot

(** this always has a length of 40 digit *)
type object_id = string

(** scheme_version, object_id and object_id*)
type identifier_core = int * object_type * object_id

type context_qualifier =
  | Anchor of identifier_core
  | Origin of string
  | Path of string
  | Visit of identifier_core

type qualifier =
  | Context of context_qualifier  (** either a context *)
  | Fragment of (int * int option)  (** or a fragment (a line number or two) *)

type qualifiers = qualifier list

type identifier = identifier_core * qualifiers

let object_type_of_string = function
  | "snp" -> Some Snapshot
  | "rel" -> Some Release
  | "rev" -> Some Revision
  | "dir" -> Some Directory
  | "cnt" -> Some (Content "sha1")
  | _s -> None

let object_id_invalid (target : object_id) =
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

let object_id_from_string (s : string) : object_id option =
  if object_id_invalid s then
    None
  else
    Some s

let content ?(hash_type = "sha1") id qualifiers : identifier =
  ((1, Content hash_type, id), qualifiers)

let directory id qualifiers : identifier = ((1, Directory, id), qualifiers)

let snapshot id qualifiers : identifier = ((1, Snapshot, id), qualifiers)

let revision id qualifiers : identifier = ((1, Revision, id), qualifiers)

let release id qualifiers : identifier = ((1, Release, id), qualifiers)

let get_object_id
    (((_scheme_version, _object_type, object_id), _qualifiers) : identifier) :
    object_id =
  object_id

let get_object_type
    (((_scheme_version, object_type, _object_type), _qualifiers) : identifier) :
    object_type =
  object_type

exception Parser_error of string

exception Lexer_error of string

type directory_entry =
  { typ : string
  ; permissions : int
  ; name : string
  ; target : object_id
  }

type date =
  { timestamp : int
  ; tz_offset : int
  ; negative_utc : bool
  }

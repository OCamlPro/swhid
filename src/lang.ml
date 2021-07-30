type url_escaped = string

type path_absolute_escaped = string

type scheme_version = int

type object_type =
  | Content
  | Directory
  | Release
  | Revision
  | Snapshot

type object_id = char array (* this always has a length of 40 digit *)

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
  | "cnt" -> Some Content
  | _s -> None

let object_id_from_string s =
  let expected_size = 40 in
  if String.length s = expected_size then
    try
      String.iter
        (function
          | 'a' .. 'f'
          | '0' .. '9' ->
            ()
          | _invalid_char -> raise Exit )
        s;
      Some (Array.init expected_size (String.get s))
    with
    | Exit -> None
  else
    None

let content id qualifiers = ((1, Content, id), qualifiers)

let directory id qualifiers = ((1, Directory, id), qualifiers)

let snapshot id qualifiers = ((1, Snapshot, id), qualifiers)

let revision id qualifiers = ((1, Revision, id), qualifiers)

let release id qualifiers = ((1, Release, id), qualifiers)

exception Parser_error of string

exception Lexer_error of string

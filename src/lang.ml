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

type line_number = char list

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

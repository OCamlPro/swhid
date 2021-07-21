type url_escaped = string

type path_absolute_escaped = string

type scheme_version = int

type object_type =
  | Content
  | Directory
  | Release
  | Revision
  | Snapshot

type dec_digit =
  | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

type hex_digit =
  | DA | DB | DC | DD | DE | DF | D_dec of dec_digit

type object_id = hex_digit array (* this always has a length of 40 digit *)

type line_number = dec_digit list

type identifier_core = scheme_version * object_type * object_id

type context_qualifier =
  | Anchor of identifier_core
  | Origin of url_escaped
  | Path of path_absolute_escaped
  | Visit of identifier_core

type qualifier =
  | Context of context_qualifier
  | Fragment of (line_number * (line_number option))

type qualifiers = qualifier list

type identifier = identifier_core * qualifiers

open Lang

let string_of_object_type = function
  | Content _hash_type -> "cnt"
  | Directory -> "dir"
  | Release -> "rel"
  | Revision -> "rev"
  | Snapshot -> "snp"

let url_escaped fmt url = Format.fprintf fmt "%s" url

let object_id fmt object_id = Format.fprintf fmt "%s" object_id

let object_type fmt object_type =
  Format.fprintf fmt "%s" (string_of_object_type object_type)

let scheme_version fmt version = Format.fprintf fmt "%d" version

let identifier_core fmt (v, t, id) =
  Format.fprintf fmt "swh:%a:%a:%a" scheme_version v object_type t object_id id

let context_qualifier fmt = function
  | Anchor identifier ->
    Format.fprintf fmt "anchor=%a" identifier_core identifier
  | Origin url -> Format.fprintf fmt "origin=%a" url_escaped url
  | Path url -> Format.fprintf fmt "path=%a" url_escaped url
  | Visit identifier -> Format.fprintf fmt "visit=%a" identifier_core identifier

let line_number fmt line_number = Format.fprintf fmt "%d" line_number

let fragment_qualifier fmt (fst_line, snd_line) =
  Format.fprintf fmt "lines=%a" line_number fst_line;
  match snd_line with
  | None -> ()
  | Some snd_line -> Format.fprintf fmt "-%a" line_number snd_line

let qualifier fmt = function
  | Context q -> Format.fprintf fmt "%a" context_qualifier q
  | Fragment q -> Format.fprintf fmt "%a" fragment_qualifier q

let qualifiers fmt q =
  List.iter (fun q -> Format.fprintf fmt ";%a" qualifier q) q

let identifier fmt (i, q) =
  Format.fprintf fmt "%a%a" identifier_core i qualifiers q

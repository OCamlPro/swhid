open Lang

let string_of_object_type = function
  | Content -> "cnt"
  | Directory -> "dir"
  | Release -> "rel"
  | Revision -> "rev"
  | Snapshot -> "snp"

let string_of_dec_digit = function
  | D0 -> "0"
  | D1 -> "1"
  | D2 -> "2"
  | D3 -> "3"
  | D4 -> "4"
  | D5 -> "5"
  | D6 -> "6"
  | D7 -> "7"
  | D8 -> "8"
  | D9 -> "9"

let string_of_hex_digit = function
  | DA -> "a"
  | DB -> "b"
  | DC -> "c"
  | DD -> "d"
  | DE -> "e"
  | DF -> "f"
  | D_dec d -> string_of_dec_digit d

let url_escaped fmt url =
  Format.fprintf fmt "%s" url

let object_id fmt object_id =
  Array.iter (fun hex_digit -> Format.fprintf fmt "%s" (string_of_hex_digit hex_digit)) object_id

let object_type fmt object_type =
  Format.fprintf fmt "%s" (string_of_object_type object_type)

let scheme_version fmt version =
  Format.fprintf fmt "%d" version

let rec context_qualifier fmt = function
  | Anchor identifier -> Format.fprintf fmt "anchor=%a" identifier_core identifier
  | Origin url -> Format.fprintf fmt "origin=%a" url_escaped url
  | Path url -> Format.fprintf fmt "path=%a" url_escaped url
  | Visit identifier -> Format.fprintf fmt "visit=%a" identifier_core identifier

and identifier_core fmt (v, t, id) =
  Format.fprintf fmt "swh:%a:%a:%a" scheme_version v object_type t object_id id

let line_number fmt line_number =
  List.iter (fun digit -> Format.fprintf fmt "%s" (string_of_dec_digit digit)) line_number

let fragment_qualifier fmt (fst_line, snd_line) =
  Format.fprintf fmt "lines=%a" line_number fst_line;
    begin match snd_line with
  | None -> ()
  | Some snd_line -> Format.fprintf fmt "-%a" line_number snd_line
    end

let qualifier fmt = function
  | Context q -> Format.fprintf fmt "%a" context_qualifier q
  | Fragment q -> Format.fprintf fmt "%a" fragment_qualifier q

let qualifiers fmt q =
  List.iter (fun q -> Format.fprintf fmt ";%a" qualifier q) q

let identifier fmt (i, q) =
  Format.fprintf fmt "%a%a" identifier_core i qualifiers q

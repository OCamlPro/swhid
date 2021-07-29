let parse buf =
  try Ok (Menhir_parser.identifier Lexer.token buf) with
  | Lang.Parser_error s -> Error (Format.sprintf "parser error: %s" s)
  | Menhir_parser.Error -> Error (Format.sprintf "parser error: Syntax error")
  | Lang.Lexer_error s -> Error (Format.sprintf "lexer error: %s" s)

let from_string s = parse (Lexing.from_string s)

let from_channel c = parse (Lexing.from_channel c)

let from_file f =
  let chan = open_in f in
  let result = parse (Lexing.from_channel chan) in
  close_in chan;
  result

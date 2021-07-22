let parse buf = Menhir_parser.identifier Lexer.token buf

let from_string s = parse (Lexing.from_string s)

let from_channel c = parse (Lexing.from_channel c)

let from_file f =
  let chan = open_in f in
  let result = parse (Lexing.from_channel chan) in
  close_in chan;
  result

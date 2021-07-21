let error e =
  Format.eprintf "error: %s@." e;
  exit 1

let dec_digit_of_char = function
  | '0' -> Lang.D0
  | '1' -> Lang.D1
  | '2' -> Lang.D2
  | '3' -> Lang.D3
  | '4' -> Lang.D4
  | '5' -> Lang.D5
  | '6' -> Lang.D6
  | '7' -> Lang.D7
  | '8' -> Lang.D8
  | '9' -> Lang.D9
  | c -> error (Format.sprintf "invalid value: character `%c` (Utils.dec_digit_of_char)" c)

let hex_digit_of_char = function
  | 'a' -> Lang.DA
  | 'b' -> Lang.DB
  | 'c' -> Lang.DC
  | 'd' -> Lang.DD
  | 'e' -> Lang.DE
  | 'f' -> Lang.DF
  | c -> Lang.D_dec (dec_digit_of_char c)

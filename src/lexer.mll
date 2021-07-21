{ open Menhir_parser

}

let linebreak = ['\n' '\r']

    rule token = parse
              | eof             { EOF }
              | _               {  EOF }

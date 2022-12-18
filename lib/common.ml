open Core

let read_file fp = In_channel.read_all fp
let split_by str = Str.split (Str.regexp str)

let parse_day13 s =
  let lexbuf = Lexing.from_string s in
  Day13_parser.expr Day13_lexer.read lexbuf

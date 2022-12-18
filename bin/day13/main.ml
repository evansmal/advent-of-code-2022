open Core

let read_input = Common.read_file "data/13.txt"
let split_lines input = Common.split_by "\n\n" input

type atom = Value of int | ValueList of atom list

let print_list lst =
  printf "[";
  List.iter ~f:(printf "%d ") lst;
  printf "]"

let print_signal signal = printf "|%d|" signal
let print_node = function l -> print_list l

let find_missing_bracket str =
  let f (pos, level) ch =
    if level = 0 then (pos + 1, level)
    else
      match ch with
      | '[' -> (pos + 1, level + 1)
      | ']' -> (pos + 1, level - 1)
      | _ -> (pos + 1, level)
  in
  let str' = String.slice str 1 (String.length str) in
  let index, _ = List.fold_left ~f ~init:(0, 1) (String.to_list str') in
  index

let rec parse_signal str =
  let fst = String.get str 0 in
  match fst with
  | '[' ->
      let closing = find_missing_bracket str in
      ValueList []
  | ',' ->
      printf "Got a comma\n";
      parse_signal (String.slice str 1 (String.length str))
  | n ->
      printf "Its a num\n";
      Value (int_of_char n)

let parse block =
  let l, r =
    printf "------------ starting to parse block ------\n%s\n\n" block;
    match Common.split_by "\n" block with
    | [ left; right ] -> (parse_signal left, parse_signal right)
    | _ -> failwith "Cannot parse signal block"
  in
  (l, r)

let split = split_lines read_input
let signals = List.map ~f:parse split

let parse s =
  let lexbuf = Lexing.from_string s in
  Parse.Parser.expr Parse.Lexer.read lexbuf

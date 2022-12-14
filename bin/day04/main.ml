open Core

let read_input = Common.read_file "data/04.txt"
let split_lines input = Common.split_by "\n" input

type range = { starting : int; ending : int }
type pair = { first : range; second : range }

let parse_range str =
  let values = Common.split_by "-" str in
  match values with
  | [ fst; sec ] -> { starting = int_of_string fst; ending = int_of_string sec }
  | _ -> failwith "Expected two values in range"

let parse_pair str =
  let values = Common.split_by "," str in
  match values with
  | [ fst; sec ] -> { first = parse_range fst; second = parse_range sec }
  | _ -> failwith "Expected two values in pair"

let is_totally_contained_within r1 r2 =
  r1.starting >= r2.starting && r1.ending <= r2.ending

let does_totally_overlap p =
  is_totally_contained_within p.second p.first
  || is_totally_contained_within p.first p.second

let between value p1 p2 = value >= p1 && value <= p2

let is_overlapping r1 r2 =
  between r1.starting r2.starting r2.ending
  || between r1.ending r2.starting r2.ending

let does_overlap_any p =
  is_overlapping p.second p.first || is_overlapping p.first p.second

let part_one =
  let pairs = split_lines read_input |> List.map ~f:parse_pair in
  let overlap = List.filter ~f:does_totally_overlap pairs in
  printf "Part one: %d\n" (List.length overlap)

let part_two =
  let pairs = split_lines read_input |> List.map ~f:parse_pair in
  let overlap = List.filter ~f:does_overlap_any pairs in
  printf "Part two: %d\n" (List.length overlap)

let () = part_one
let () = part_two

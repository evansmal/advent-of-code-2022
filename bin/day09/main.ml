open Core

let read_input = Common.read_file "data/09.txt"
let split_lines input = Common.split_by "\n" input

type direction = Up | Down | Left | Right
type motion = { direction : direction; distance : int }

let parse_direction = function
  | "U" -> Up
  | "D" -> Down
  | "L" -> Left
  | "R" -> Right
  | _ -> failwith "Invalid direction"

let parse_motion str =
  match Common.split_by " " str with
  | [ dir; dist ] ->
      let dir = parse_direction dir in
      let dist = int_of_string dist in
      List.init dist ~f:(fun _ -> dir)
  | _ -> failwith "Expected two values in motion"

let parse_input input = input |> split_lines |> List.map ~f:parse_motion
let parse input = List.concat (parse_input input)

type position = { x : int; y : int }

let initial_pos = { x = 0; y = 0 }
let delta a b = max (abs (a.x - b.x)) (abs (a.y - b.y))
let tail_needs_to_move head tail = delta head tail > 1

let step direction head tail =
  let head' =
    match direction with
    | Up -> { x = head.x; y = head.y + 1 }
    | Down -> { x = head.x; y = head.y - 1 }
    | Left -> { x = head.x - 1; y = head.y }
    | Right -> { x = head.x + 1; y = head.y }
  in
  let tail' = if tail_needs_to_move head' tail then head else tail in
  (head', tail')

let print_position pos = printf "X: %d, Y: %d\n" pos.x pos.y

let execute rope path directions =
  let head, tail =
    match rope with
    | [ head; tail ] -> (head, tail)
    | _ -> failwith "Expected two knots"
  in
  let rec execute' head tail path = function
    | [] -> path
    | direction :: rest ->
        let h', t' = step direction head tail in
        execute' h' t' (List.concat [ t' :: path ]) rest
  in
  execute' head tail [] directions

let contains lst a = List.mem ~equal:(fun a b -> a.x = b.x && a.y = b.y) lst a

let rec remove_dups acc = function
  | hd :: tl ->
      if contains acc hd then remove_dups acc tl else remove_dups (hd :: acc) tl
  | [] -> acc

let create_rope n = List.init n ~f:(fun _ -> { x = 0; y = 0 })

let part_one =
  let directions = parse read_input in
  let tail_positions = execute (create_rope 2) [] directions in
  printf "Part one: %d\n" (List.length (remove_dups [] tail_positions))

let () = part_one

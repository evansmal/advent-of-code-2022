open Core

(* Read the input *)
let get_input = Common.read_file "data/02.txt"

(* Split the input into blocks *)
let split_lines input =
  let lines = Common.split_by "\n" input in
  List.map ~f:(Common.split_by " ") lines

type choice = Rock | Paper | Scissors
type round = { first : choice; second : choice }

(* Decrypt strategy guide *)
let to_choice = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> raise (Invalid_argument "Invalid choice")

(* Convert raw input into a single round *)
let to_round (input : string list) =
  let choices = List.map ~f:to_choice input in
  match choices with
  | [] | _ :: [] -> raise (Invalid_argument "Expect two values")
  | hd :: tl :: rst -> { first = hd; second = tl }

(* Points for each outcome *)
let loss = 0
let draw = 3
let win = 6

(* Compute the score based on the round *)
let get_score = function
  | { first = Rock; second = Rock } -> draw + 1
  | { first = Paper; second = Paper } -> draw + 2
  | { first = Scissors; second = Scissors } -> draw + 3
  | { first = Paper; second = Rock } -> loss + 1
  | { first = Scissors; second = Rock } -> win + 1
  | { first = Scissors; second = Paper } -> loss + 2
  | { first = Rock; second = Paper } -> win + 2
  | { first = Rock; second = Scissors } -> loss + 3
  | { first = Paper; second = Scissors } -> win + 3

let sum l = List.fold ~init:0 ~f:( + ) l

let part_one =
  let scores =
    get_input |> split_lines |> List.map ~f:to_round |> List.map ~f:get_score
  in
  printf "Part one: %d\n" (sum scores)

let () = part_one

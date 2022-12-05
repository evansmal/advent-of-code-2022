open Core

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
  | _ -> failwith "Invalid choice"

(* Convert raw input into a single round *)
let to_round (input : string list) =
  let choices = List.map ~f:to_choice input in
  match choices with
  | hd :: tl :: _ -> { first = hd; second = tl }
  | _ -> failwith "Expect two values"

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

type outcome = Loss | Tie | Win
type instruction = { choice : choice; outcome : outcome }

let to_outcome = function
  | "X" -> Loss
  | "Y" -> Tie
  | "Z" -> Win
  | _ -> failwith "Invalid choice"

(* Convert raw input into a instruction *)
let to_constraint (input : string list) =
  match input with
  | hd :: tl :: _ -> { choice = to_choice hd; outcome = to_outcome tl }
  | _ -> failwith "Expect two values"

let should_pick = function
  | { choice = x; outcome = Tie } -> x
  | { choice = Rock; outcome = Loss } -> Scissors
  | { choice = Rock; outcome = Win } -> Paper
  | { choice = Paper; outcome = Loss } -> Rock
  | { choice = Paper; outcome = Win } -> Scissors
  | { choice = Scissors; outcome = Loss } -> Paper
  | { choice = Scissors; outcome = Win } -> Rock

let instruction_to_round i = { first = i.choice; second = should_pick i }

let part_one =
  let input = Common.read_file "data/02.txt" in
  let scores =
    input |> split_lines |> List.map ~f:to_round |> List.map ~f:get_score
  in
  printf "Part one: %d\n" (sum scores)

let part_two =
  let input = Common.read_file "data/02.txt" in
  let scores =
    input |> split_lines |> List.map ~f:to_constraint
    |> List.map ~f:instruction_to_round
    |> List.map ~f:get_score
  in
  printf "Part two: %d\n" (sum scores)

let () = part_one
let () = part_two

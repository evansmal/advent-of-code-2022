open Core

(* Read the input *)
let input = Common.read_file "data/01.txt"

(* Split the input into blocks *)
let split_blocks input =
  let seg = Common.split "\n\n" input in
  let blocks = List.map ~f:(Common.split "\n") seg in
  List.map ~f:(List.map ~f:int_of_string) blocks

(* Find the max value in a string *)
let rec max : int list -> int =
 fun lst ->
  match lst with
  | [] -> 0
  | h :: [] -> h
  | h :: t ->
      let a = max t in
      if h > a then h else a

(* Sum all values in the list *)
let rec sum l = match l with [] -> 0 | h :: t -> h + sum t

let part_one =
  let sums = List.map ~f:sum (split_blocks input) in
  printf "Part 1: %d\n" (max sums)

let part_two =
  let sums = List.map ~f:sum (split_blocks input) in
  let sorted = List.sort sums ~compare:Int.descending in
  let top3 = List.take sorted 3 in
  printf "Part 2: %d \n" (sum top3)

let () = part_one
let () = part_two

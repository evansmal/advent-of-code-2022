open Core

let read_input = Common.read_file "data/03.txt"
let split_lines input = Common.split_by "\n" input
let string_to_char_list s = s |> String.to_list
let sort s = List.sort s ~compare:Char.descending

let rec find_dup (l: char list) = 
    let () = List.iter ~f:(printf "%c") l in
    let res = match l with 
    | [] -> raise(Invalid_argument " ")
    | last :: [] -> raise(Invalid_argument " ")
    | fst :: sec :: rest -> if Char.equal fst sec then fst else find_dup(sec::rest) in
    let () = printf "\n" in
    res

let part_one =
  let input =
    read_input |> split_lines
    |> List.map ~f:string_to_char_list
    |> List.map ~f:sort
    |> List.map ~f:find_dup
  in
  List.iter ~f:(printf "Part one: %c\n") input

let () = part_one

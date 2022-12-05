open Core

let read_input = Common.read_file "data/03.txt"
let split_lines input = Common.split_by "\n" input
let string_to_char_list s = s |> String.to_list

let split list n =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if i = 0 then (List.rev acc, l) else aux (i - 1) (h :: acc) t
  in
  aux n [] list

type rucksack = { first : char list; second : char list }

let to_rucksack seq =
  let s = split seq (List.length seq / 2) in
  match s with fst, sec -> { first = fst; second = sec }

(* Get the single common char in both compartments *)
let find_common_rucksack (x : rucksack) =
  let f = Set.of_list (module Char) x.first in
  let s = Set.of_list (module Char) x.second in
  let inter = Set.inter f s |> Set.to_list in
  match List.hd inter with Some x -> x | None -> failwith "Expect one value"

(*
     Scale and shift the given char to that a->z = 1->26 and
     A->Z = 27->52
*)
let to_priority = function
  | c when Char.is_lowercase c -> Char.to_int c - 96
  | c when Char.is_uppercase c -> Char.to_int c - (64 - 26)
  | _ -> failwith "Expect always a->z, A->Z"

let sum l = List.fold ~init:0 ~f:( + ) l

type group = { first : char list; second : char list; third : char list }

let list_to_group = function
  | [ fst; sec; thd ] -> { first = fst; second = sec; third = thd }
  | _ -> failwith "Expected 3 values in a group"

let to_groups l = List.chunks_of l ~length:3 |> List.map ~f:list_to_group

(* Get the single common char across the group *)
let find_common_in_group (x : group) =
  let f = Set.of_list (module Char) x.first in
  let s = Set.of_list (module Char) x.second in
  let t = Set.of_list (module Char) x.third in
  let inter = Set.inter f s |> Set.inter t |> Set.to_list in
  match List.hd inter with
  | Some x -> x
  | None -> failwith "Expect one common value in group"

let part_one =
  let answer =
    read_input |> split_lines
    |> List.map ~f:string_to_char_list
    |> List.map ~f:to_rucksack
    |> List.map ~f:find_common_rucksack
    |> List.map ~f:to_priority
  in
  printf "Part one: %d\n" (sum answer)

let part_two =
  let answer =
    read_input |> split_lines
    |> List.map ~f:string_to_char_list
    |> to_groups
    |> List.map ~f:find_common_in_group
    |> List.map ~f:to_priority
  in
  printf "Part two: %d\n" (sum answer)

let () = part_one
let () = part_two

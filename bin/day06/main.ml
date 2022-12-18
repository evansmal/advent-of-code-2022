open Core

let read_input = Common.read_file "data/06.txt"
let has_dups l = List.contains_dup ~compare:Char.compare l

let is_marker size l i _ =
  let slice = List.slice l i (List.length l - 1) in
  let buf = List.take slice size in
  (not (has_dups buf), i + size)

let search size input =
  input
  |> List.mapi ~f:(is_marker size input)
  |> List.filter ~f:(fun (x, _) -> x)
  |> List.map ~f:(fun (_, y) -> y)
  |> List.hd_exn

let part_one =
  let output = read_input |> String.to_list |> search 4 in
  printf "Part one: %d\n" output

let part_two =
  let output = read_input |> String.to_list |> search 14 in
  printf "Part two: %d\n" output

let () = part_one
let () = part_two

open Core

let read_input = Common.read_file "data/18.txt"
let split_lines input = Common.split_by "\n" input

type cube = { x : int; y : int; z : int }

let does_cover c0 c1 =
  let same = [ c0.x = c1.x; c0.y = c1.y; c0.z = c1.z ] in
  match same with
  | [ false; true; true ] -> abs (c1.x - c0.x) = 1
  | [ true; false; true ] -> abs (c1.y - c0.y) = 1
  | [ true; true; false ] -> abs (c1.z - c0.z) = 1
  | _ -> false

let rec count_sides total cubes cube =
  match cubes with
  | curr :: rest ->
      let covers = does_cover cube curr in
      if covers then count_sides (total + 1) rest cube
      else count_sides total rest cube
  | [] -> total

let parse line =
  let values = line |> Common.split_by "," |> List.map ~f:int_of_string in
  {
    x = List.nth_exn values 0;
    y = List.nth_exn values 1;
    z = List.nth_exn values 2;
  }

let compute_surface_area cubes =
  let sides =
    cubes |> List.map ~f:(count_sides 0 cubes) |> List.map ~f:(fun c -> 6 - c)
  in
  List.fold ~f:( + ) ~init:0 sides

let part_one () =
  let area =
    read_input |> split_lines |> List.map ~f:parse |> compute_surface_area
  in
  printf "Part one: %d\n" area

let () = part_one () 

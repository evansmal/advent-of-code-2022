open Core

let read_input = Common.read_file "data/18.txt"
let split_lines input = Common.split_by "\n" input

type cube = { x : int; y : int; z : int }

let parse line =
  let values = line |> Common.split_by "," |> List.map ~f:int_of_string in
  {
    x = List.nth_exn values 0;
    y = List.nth_exn values 1;
    z = List.nth_exn values 2;
  }

let does_cover c0 c1 =
  let same = [ c0.x = c1.x; c0.y = c1.y; c0.z = c1.z ] in
  match same with
  | [ false; true; true ] -> abs (c1.x - c0.x) = 1
  | [ true; false; true ] -> abs (c1.y - c0.y) = 1
  | [ true; true; false ] -> abs (c1.z - c0.z) = 1
  | _ -> false

let rec count_covered_sides total cubes cube =
  match cubes with
  | curr :: rest ->
      let covers = does_cover cube curr in
      if covers then count_covered_sides (total + 1) rest cube
      else count_covered_sides total rest cube
  | [] -> total

let compute_surface_area cubes =
  let sides =
    cubes
    |> List.map ~f:(count_covered_sides 0 cubes)
    |> List.map ~f:(fun c -> 6 - c)
  in
  List.fold ~f:( + ) ~init:0 sides

let dfs cubes start final =
  let visited = [] in
  let rec search cubes visited node =
    let neighbours =
      [
        { x = node.x + 1; y = node.y; z = node.z };
        { x = node.x - 1; y = node.y; z = node.z };
        { x = node.x; y = node.y + 1; z = node.z };
        { x = node.x; y = node.y - 1; z = node.z };
        { x = node.x; y = node.y; z = node.z + 1 };
        { x = node.x; y = node.y; z = node.z - 1 };
      ]
    in
    List.iter ~f:(fun n -> search cubes visited n) neighbours
  in
  search cubes visited start

let compute_surface_area_and_ignore_inner cubes =
  let covered_sides =
    cubes
    |> List.map ~f:(count_covered_sides 0 cubes)
    |> List.map ~f:(fun c -> 6 - c)
    |> List.fold ~f:( + ) ~init:0
  in
  let internal_sides = 6 in
  covered_sides - internal_sides

let part_one () =
  let area =
    read_input |> split_lines |> List.map ~f:parse |> compute_surface_area
  in
  printf "Part one: %d\n" area

let part_two () =
  let area =
    read_input |> split_lines |> List.map ~f:parse
    |> compute_surface_area_and_ignore_inner
  in
  printf "Part two: %d\n" area

let () = part_one ()
let () = part_two ()

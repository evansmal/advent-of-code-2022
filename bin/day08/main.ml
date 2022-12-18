open Core

let read_input = Common.read_file "data/08.txt"
let split_lines input = Common.split_by "\n" input
let to_int_list c = List.map ~f:int_of_char c

type grid = int array array

let part_one =
  let spatial_dim = 5 in
  let grid =
    read_input |> split_lines |> List.map ~f:String.to_list
    |> List.map ~f:to_int_list |> List.map ~f:Array.of_list |> Array.of_list
  in
  for i = 1 to spatial_dim - 1 do
    for j = 1 to spatial_dim - 1 do
      printf " %d "
    done
  done

let () = part_one

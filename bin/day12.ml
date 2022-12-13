open Core

let read_input = Common.read_file "data/12.txt"
let split_lines input = Common.split_by "\n" input
let char_to_int lst = List.map ~f:int_of_char lst
let get_dims grid = (Array.length grid, Array.length grid.(0))

let print_grid grid =
  let rows, cols = get_dims grid in
  for row = 0 to rows - 1 do
    let () =
      for col = 0 to cols - 1 do
        printf "%d " grid.(row).(col)
      done
    in
    printf "\n"
  done

type position = { row : int; col : int }
type node = { height : int }

let parse input =
  input |> split_lines |> List.map ~f:String.to_list |> List.map ~f:char_to_int
  |> List.map ~f:Array.of_list |> Array.of_list

let pad' value r =
  let r' = Array.append [| value |] r in
  Array.append r' [| value |]

let pad grid value =
  let _, cols = get_dims grid in
  let vertical_padding = Array.create ~len:(cols + 2) value in
  grid |> Array.map ~f:(pad' value) |> pad' vertical_padding

let get_id grid row col =
  let _, cols = get_dims grid in
  (row * cols) + col

let get_pos grid id =
  let rows, cols = get_dims grid in
  { row = id / cols; col = id mod cols }

let get_height grid id =
  let { row; col } = get_pos grid id in
  grid.(row).(col)

let get_height_from_pos grid { row; col } = grid.(row).(col)

let neighbors grid { row = r; col = c } =
  [ (r, c + 1); (r, c - 1); (r + 1, c); (r - 1, c) ]
  |> List.map ~f:(fun (r, c) -> get_id grid r c)
  |> List.filter ~f:(fun nb ->
         get_height_from_pos grid { row = r; col = c } + 1 >= get_height grid nb)

let find_value grid value =
  let rows, cols = get_dims grid in
  let pos = ref { row = 0; col = 0 } in
  let () =
    (* Ignore padded regions *)
    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let height = get_height_from_pos grid { row; col } in
        pos := if height = value then { row; col } else !pos
      done
    done
  in
  !pos

let print_nb (cc, r, c) = printf "(%d, %d, %d)" cc r c

let print_nbs nbs =
  printf "NB: ";
  List.iter ~f:print_nb nbs;
  printf "\n"

let djk grid destination () =
  let rows, cols = get_dims grid in
  let visited = Array.create ~len:(rows * cols) false in
  let queue = Batteries.Heap.of_list [ (0, 1, 1) ] in

  let rec djk_aux queue =
    let count, row, col = Batteries.Heap.find_min queue in
    let queue = Batteries.Heap.del_min queue in
    let () =
      if row = destination.row && col = destination.col then
        printf "%d %d %d\n" count row col
    in
    let () = visited.(get_id grid row col) <- true in
    let nb =
      neighbors grid { row; col }
      |> List.filter ~f:(fun n -> not visited.(n))
      |> List.map ~f:(fun n ->
             let pos = get_pos grid n in
             (count + 1, pos.row, pos.col))
    in
    let new_heap = Batteries.Heap.of_list nb in
    djk_aux (Batteries.Heap.merge queue new_heap)
  in
  djk_aux queue

let start_character = 83
let end_character = 69
let a_char = 97
let z_char = 122

let part_one =
  let grid = parse read_input in
  let start = find_value grid start_character in
  let dest = find_value grid end_character in
  let () =
    grid.(start.row).(start.col) <- a_char;
    grid.(dest.row).(dest.col) <- z_char
  in
  djk (pad grid 999) { row = dest.row + 1; col = dest.col + 1 }

let () = part_one ()


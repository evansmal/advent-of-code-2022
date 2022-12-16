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

let get_height grid { row; col } = grid.(row).(col)

let find_value grid value =
  let rows, cols = get_dims grid in
  let pos = ref { row = 0; col = 0 } in
  let () =
    (* Ignore padded regions *)
    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let height = get_height grid { row; col } in
        pos := if height = value then { row; col } else !pos
      done
    done
  in
  !pos

let print_nb (cc, r, c) = printf "(%d, %d, %d)" cc r c

let print_nbs nbs =
  printf "QUEUE: ";
  List.iter ~f:print_nb nbs;
  printf "\n"

let djk grid start dest () =
  let rows, cols = get_dims grid in
  let d = Array.make_matrix ~dimx:rows ~dimy:cols Int.max_value in
  let queue =
    Pairing_heap.of_list
      [ (start.row, start.col) ]
      ~cmp:(fun (i, j) (k, l) -> Int.compare d.(i).(j) d.(k).(l))
  in
  d.(start.row).(start.col) <- 0;

  let rec run () =
    if Pairing_heap.is_empty queue then None
    else
      let i, j = Pairing_heap.pop_exn queue in
      if i = dest.row && j = dest.col then Some d.(i).(j)
      else
        let neighbours = [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ] in
        List.iter neighbours ~f:(fun (i', j') ->
            if i' >= 0 && i' < rows && j' >= 0 && j' < cols then
              let step = grid.(i').(j') - grid.(i).(j) in
              if step <= 1 then
                let dist = d.(i).(j) + 1 in
                if dist < d.(i').(j') then (
                  d.(i').(j') <- dist;
                  Pairing_heap.add queue (i', j')));
        run ()
  in
  run ()

let start_character = 83
let end_character = 69
let a_char = 97
let z_char = 122

let part_one () =
  let grid = parse read_input in
  let start = find_value grid start_character in
  let dest = find_value grid end_character in
  let () = printf "Start: %d, %d\n" start.row start.col in
  let () = printf "Dest: %d, %d\n" dest.row dest.col in
  let () =
    grid.(start.row).(start.col) <- a_char;
    grid.(dest.row).(dest.col) <- z_char
  in
  let count =
    djk grid
      { row = start.row; col = start.col }
      { row = dest.row; col = dest.col }
      ()
  in
  printf "%d\n" (Option.value ~default:0 count);
  ()

let () = part_one ()

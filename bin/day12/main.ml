open Core

let read_input = Common.read_file "data/12.txt"
let split_lines input = Common.split_by "\n" input
let char_to_int lst = List.map ~f:int_of_char lst

type position = { row : int; col : int }

let parse input =
  input |> split_lines |> List.map ~f:String.to_list |> List.map ~f:char_to_int
  |> List.map ~f:Array.of_list |> Array.of_list

let get_dims grid = (Array.length grid, Array.length grid.(0))

let find_value grid value =
  let rows, cols = get_dims grid in
  let pos = ref { row = 0; col = 0 } in
  let () =
    (* Ignore padded regions *)
    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let height = grid.(row).(col) in
        pos := if height = value then { row; col } else !pos
      done
    done
  in
  !pos

let dijkstra grid start finish () =
  let rows, cols = get_dims grid in
  let distances = Array.make_matrix ~dimx:rows ~dimy:cols Int.max_value in
  let queue =
    Pairing_heap.of_list
      [ (start.row, start.col) ]
      ~cmp:(fun (i, j) (k, l) ->
        Int.compare distances.(i).(j) distances.(k).(l))
  in
  distances.(start.row).(start.col) <- 0;
  let rec run () =
    if Pairing_heap.is_empty queue then None
    else
      let i, j = Pairing_heap.pop_exn queue in
      if i = finish.row && j = finish.col then Some distances.(i).(j)
      else
        let neighbours = [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ] in
        List.iter neighbours ~f:(fun (i', j') ->
            if i' >= 0 && i' < rows && j' >= 0 && j' < cols then
              let step = grid.(i').(j') - grid.(i).(j) in
              if step <= 1 then
                let dist = distances.(i).(j) + 1 in
                if dist < distances.(i').(j') then (
                  distances.(i').(j') <- dist;
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
  let count =
    grid.(start.row).(start.col) <- a_char;
    grid.(dest.row).(dest.col) <- z_char;
    dijkstra grid
      { row = start.row; col = start.col }
      { row = dest.row; col = dest.col }
      ()
  in
  printf "Part one: %d\n" (Option.value ~default:0 count);
  ()

let part_two () =
  let grid = parse read_input in
  let start = find_value grid start_character in
  let dest = find_value grid end_character in
  let () =
    grid.(start.row).(start.col) <- a_char;
    grid.(dest.row).(dest.col) <- z_char
  in
  let values =
    Array.mapi
      ~f:(fun r row ->
        Array.filter_mapi
          ~f:(fun c height ->
            if height = a_char then
              dijkstra grid { row = r; col = c }
                { row = dest.row; col = dest.col }
                ()
            else None)
          row)
      grid
  in
  let shortest =
    values
    |> Array.map ~f:(Array.min_elt ~compare:Int.compare)
    |> Array.map ~f:(fun v -> Option.value_exn v)
    |> Array.min_elt ~compare:Int.compare
  in
  printf "Part two: %d\n" (Option.value_exn shortest)

let () = part_one ()
let () = part_two ()

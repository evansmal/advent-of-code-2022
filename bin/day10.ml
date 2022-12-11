open Core

let read_input = Common.read_file "data/10.txt"
let split_lines input = Common.split_by "\n" input

type instruction = Noop | AddX of int
type state = { x : int }

let print_state (id, state) = printf "%d {x = %d}\n" id state.x
let print_list printer lst = List.iter ~f:printer lst

let parse_instruction line =
  let chunks = Common.split_by " " line in
  match chunks with
  | [ "addx"; n ] -> AddX (int_of_string n)
  | [ "noop" ] -> Noop
  | _ -> failwith "Cannot parse"

let tick state cycle_id = function
  | Noop -> [ (cycle_id + 1, state) ]
  | AddX n -> [ (cycle_id + 1, state); (cycle_id + 2, { x = state.x + n }) ]

let rec execute instructions =
  let rec execute' state cycle_id instructions acc =
    match instructions with
    | curr :: rest ->
        let states' = tick state cycle_id curr in
        let cycle_id', state' = List.last_exn states' in
        execute' state' cycle_id' rest (acc @ states')
    | [] -> acc
  in
  execute' { x = 1 } 1 instructions []

let part_one =
  let states =
    read_input |> split_lines |> List.map ~f:parse_instruction |> execute
  in
  let readings =
    List.filter
      ~f:(fun (c, s) ->
        c = 20 || c = 60 || c = 100 || c = 140 || c = 180 || c = 220)
      states
  in
  let signal =
    List.fold ~f:(fun sum (c, s) -> (s.x * c) + sum) ~init:0 readings
  in
  printf "Part one: %d\n" signal

let should_draw sprite_position crt =
  crt <= sprite_position + 1 && crt >= sprite_position - 1

let draw_row row states =
  let width = 40 in
  let () =
    for i = 0 to width - 1 do
      let cycle, state = List.nth_exn states i in
      let sprite_position = state.x in
      if should_draw sprite_position i then printf "█" else printf "."
    done
  in
  ()

let draw states =
  let width = 40 in
  let () =
    for row = 0 to 5 do
      let slice = List.slice states (row * width) ((row * width) + width) in
      draw_row row slice;
      printf "\n"
    done
  in
  ()

let part_one =
  let states =
    read_input |> split_lines |> List.map ~f:parse_instruction |> execute
  in
  let states' = (1, { x = 1 }) :: states in
  printf "Part two: \n";
  draw states'

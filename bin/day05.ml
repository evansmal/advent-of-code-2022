open Core

let read_input = Common.read_file "data/05.txt"
let split_lines input = Common.split_by "\n\n" input

type instructions = { amount : int; first : int; second : int }

let parse_single_instruction line =
  let parts = Common.split_by " " line in
  match parts with
  | [ _; stack; _; first; _; second ] ->
      {
        amount = int_of_string stack;
        first = int_of_string first;
        second = int_of_string second;
      }
  | _ -> failwith "Malformed instruction"

let parse_instructions input =
  let lines = Common.split_by "\n" input in
  List.map ~f:parse_single_instruction lines

let get_initial_stacks =
  let input =
    [
      "QHCTNSVB";
      "GBDW";
      "BQSTRWF";
      "NDJZSWGL";
      "FVDPM";
      "JWF";
      "VJBQNL";
      "NSQJCRTQ";
      "MDWCQSJ";
    ]
  in
  List.map ~f:String.to_list input

let parse_input input =
  let pairs = split_lines input in
  match pairs with
  | [ _; sec ] -> (get_initial_stacks, parse_instructions sec)
  | _ -> failwith "Cannot parse input"

let to_stacks (s : char list) = Stack.of_list s

let rec pop (stack : char Stack.t) (num : int) (acc : char list) =
  match num with 0 -> acc | n -> pop stack (n - 1) (Stack.pop_exn stack :: acc)

let push (stack : char Stack.t) (values : char list) =
  for i = 0 to List.length values - 1 do
    Stack.push stack (List.nth_exn values i)
  done

let execute stacks instruction =
  let get_stack id = List.nth_exn stacks (id - 1) in
  for _ = 0 to instruction.amount - 1 do
    let values = pop (get_stack instruction.first) 1 [] in
    push (get_stack instruction.second) values
  done

let execute' stacks instruction =
  let get_stack id = List.nth_exn stacks (id - 1) in
  let values = pop (get_stack instruction.first) instruction.amount [] in
  push (get_stack instruction.second) values

let solve executor =
  let s, i = parse_input read_input in
  let stacks = List.map ~f:to_stacks s in
  let () = List.iter ~f:(executor stacks) i in
  stacks |> List.map ~f:Stack.pop_exn
  |> List.map ~f:(String.make 1)
  |> String.concat

let part_one = printf "Part one: %s\n" (solve execute)
let part_two = printf "Part two: %s\n" (solve execute')
let () = part_one
let () = part_two

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

let strip_brackets l =
  String.filter
    ~f:(fun c -> not (Char.equal c '[' || Char.equal c ']' || Char.equal c ' '))
    l

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

let execute stacks instruction =
  let get_stack id = List.nth_exn stacks (id - 1) in
  for i = 0 to instruction.amount - 1 do
    match instruction with
    | { amount; first; second } ->
        let value = Stack.pop_exn (get_stack first) in
        Stack.push (get_stack second) value
  done

let execute' stacks instruction =
  let get_stack id = List.nth_exn stacks (id - 1) in
  for i = 0 to instruction.amount - 1 do
    match instruction with
    | { amount; first; second } -> Stack.pop_exn (get_stack first)
  done

let part_one =
  let s, i = parse_input read_input in
  let stacks = List.map ~f:to_stacks s in
  let () = List.iter ~f:(execute stacks) i in
  let tops =
    stacks |> List.map ~f:Stack.pop_exn
    |> List.map ~f:(String.make 1)
    |> String.concat
  in
  printf "Part one: %s" tops

let part_two =
  let s, i = parse_input read_input in
  let stacks = List.map ~f:to_stacks s in
  let () = List.iter ~f:(execute' stacks) i in
  let tops =
    stacks |> List.map ~f:Stack.pop_exn
    |> List.map ~f:(String.make 1)
    |> String.concat
  in
  printf "Part two: %s" tops

let () = part_one
let () = part_two

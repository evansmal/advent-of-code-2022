open Core

let read_input = Common.read_file "data/10.txt"
let split_lines input = Common.split_by "\n" input

type monkey = {
  mutable items : int list;
  mutable inspects : int;
  operation : int -> int;
  divisible_by : int;
  test : bool -> int;
}

let get_starting_monkeys_test () =
  Array.of_list
    [
      {
        items = [ 79; 98 ];
        inspects = 0;
        operation = (fun item -> item * 19);
        divisible_by = 23;
        test = (fun c -> if c then 2 else 3);
      };
      {
        items = [ 54; 65; 75; 74 ];
        inspects = 0;
        operation = (fun item -> item + 6);
        divisible_by = 19;
        test = (fun c -> if c then 2 else 0);
      };
      {
        items = [ 79; 60; 97 ];
        inspects = 0;
        operation = (fun item -> item * item);
        divisible_by = 13;
        test = (fun c -> if c then 1 else 3);
      };
      {
        items = [ 74 ];
        inspects = 0;
        operation = (fun item -> item + 3);
        divisible_by = 17;
        test = (fun c -> if c then 0 else 1);
      };
    ]

let get_starting_monkeys () =
  Array.of_list
    [
      {
        items = [ 59; 65; 86; 56; 74; 57; 56 ];
        inspects = 0;
        operation = (fun item -> item * 17);
        divisible_by = 3;
        test = (fun c -> if c then 3 else 6);
      };
      {
        items = [ 63; 83; 50; 63; 56 ];
        inspects = 0;
        operation = (fun item -> item + 2);
        divisible_by = 13;
        test = (fun c -> if c then 3 else 0);
      };
      {
        items = [ 93; 79; 74; 55 ];
        inspects = 0;
        operation = (fun item -> item + 1);
        divisible_by = 2;
        test = (fun c -> if c then 0 else 1);
      };
      {
        items = [ 86; 61; 67; 88; 94; 69; 56; 91 ];
        inspects = 0;
        operation = (fun item -> item + 7);
        divisible_by = 11;
        test = (fun c -> if c then 6 else 7);
      };
      {
        items = [ 76; 50; 51 ];
        inspects = 0;
        operation = (fun item -> item * item);
        divisible_by = 19;
        test = (fun c -> if c then 2 else 5);
      };
      {
        items = [ 77; 76 ];
        inspects = 0;
        operation = (fun item -> item + 8);
        divisible_by = 17;
        test = (fun c -> if c then 2 else 1);
      };
      {
        items = [ 74 ];
        inspects = 0;
        operation = (fun item -> item * 2);
        divisible_by = 5;
        test = (fun c -> if c then 4 else 7);
      };
      {
        items = [ 86; 85; 52; 86; 91; 95 ];
        inspects = 0;
        operation = (fun item -> item + 6);
        divisible_by = 7;
        test = (fun c -> if c then 4 else 5);
      };
    ]

let worry w = w / 3

let inspect_item modifier monkey item =
  let worry' = modifier (monkey.operation item) in
  let dest = monkey.test (worry' mod monkey.divisible_by = 0) in
  let () = monkey.inspects <- monkey.inspects + 1 in
  (worry', dest)

let update_monkey monkeys (item, dest) =
  let monkey = monkeys.(dest) in
  monkey.items <- item :: monkey.items

let turn monkeys modifier monkey =
  let destinations = List.map ~f:(inspect_item modifier monkey) monkey.items in
  let () = monkey.items <- [] in
  List.iter ~f:(update_monkey monkeys) destinations

let round monkeys divn = Array.iter ~f:(turn monkeys divn) monkeys

let run monkeys modifier rounds =
  let () =
    for i = 1 to rounds do
      round monkeys modifier
    done
  in
  let inspected =
    monkeys |> Array.to_list
    |> List.map ~f:(fun m -> m.inspects)
    |> List.sort ~compare:Int.descending
  in
  let top_n = 2 in
  List.fold ~init:1 ~f:( * ) (List.take inspected top_n)

let part_one =
  let total = run (get_starting_monkeys ()) (fun w -> w / 3) 20 in
  printf "Part one: %d\n" total

let part_two =
  let monkeys = get_starting_monkeys () in
  let factor =
    monkeys
    |> Array.map ~f:(fun m -> m.divisible_by)
    |> Array.fold ~init:1 ~f:( * )
  in
  let total = run monkeys (fun w -> w mod factor) 10000 in
  printf "Part two: %d\n" total

open Core

let read_input = Common.read_file "data/07.txt"
let split_lines input = Common.split_by "\n" input

type command = ChangeDirectory of string | ReadDirectory

type inode =
  | File of { name : string; size : int }
  | Directory of { name : string }
  | Empty

type entry = INode of inode | Command of command

let split_by str = Str.split (Str.regexp str)

let parse_command = function
  | [ "cd"; dest ] -> ChangeDirectory dest
  | [ "ls" ] -> ReadDirectory
  | _ -> failwith "Cannot parse command"

let parse_output = function
  | [ "dir"; name ] -> Directory { name }
  | [ size; name ] -> File { name; size = int_of_string size }
  | _ -> failwith "Cannot parse output"

let parse_input line =
  let lst = Common.split_by " " line in
  match lst with
  | "$" :: rst -> Command (parse_command rst)
  | rst -> INode (parse_output rst)

type state = { mutable current_direction : string }

let process_entry dir fs entry =
  let add_node_to_current_dir node =
    let current_directory = Stack.top_exn dir in
    if Map.mem fs current_directory then
      let old = Map.find_exn fs current_directory in
      Map.set ~key:current_directory ~data:(node :: old) fs
    else Map.set ~key:current_directory ~data:[ node ] fs
  in
  match entry with
  | Command command -> (
      match command with
      | ChangeDirectory name ->
          if String.equal name ".." then
            let _ = Stack.pop_exn dir in
            fs
          else
            let () = Stack.push dir name in
            fs
      | ReadDirectory -> fs)
  | INode node -> (
      match node with
      | File file ->
          let file = File file in
          add_node_to_current_dir file
      | Directory dir ->
          let dir = Directory dir in
          add_node_to_current_dir dir
      | Empty -> fs)

let get_input = read_input |> split_lines |> List.map ~f:parse_input

let rec compute_directory_size fs nodes =
  let rec compute fs sum nodes =
    match nodes with
    | File file :: rest -> compute fs (sum + file.size) rest
    | Directory dir :: rest ->
        let dirs = compute fs sum (Map.find_exn fs dir.name) in
        compute fs dirs rest 
    | [] -> sum
    | _ -> failwith "Unexpected node type"
  in
  compute fs 0 nodes

let exe =
  let input = get_input in
  let dirs = Stack.create () in
  let fs = Map.empty (module String) in
  let fs' = List.fold ~init:fs ~f:(process_entry dirs) input in
  let total_file_sizes = Map.map ~f:(compute_directory_size fs') fs' in
  let t = Map.filter ~f:(fun v -> v <= 100000) total_file_sizes in
  Map.fold ~f:(fun ~key:_ ~data:v sum -> sum + v) ~init:0 t

let part_one =
  let map = exe in
  printf "Part one: %d \n" map

let () = part_one

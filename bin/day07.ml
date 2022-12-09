open Core

let read_input = Common.read_file "data/07.txt"
let split_lines input = Common.split_by "\n" input

type command = ChangeDirectory of string | ReadDirectory

type inode =
  | File of { name : string; size : int }
  | Directory of { name : string; nodes : inode list }
  | Empty

type entry = INode of inode | Command of command

let split_by str = Str.split (Str.regexp str)

let parse_command = function
  | [ "cd"; dest ] -> ChangeDirectory dest
  | [ "ls" ] -> ReadDirectory
  | _ -> failwith "Cannot parse command"

let parse_output = function
  | [ "dir"; name ] -> Directory { name; nodes = [] }
  | [ size; name ] -> File { name; size = int_of_string size }
  | _ -> failwith "Cannot parse output"

let parse_input line =
  let lst = Common.split_by " " line in
  match lst with
  | "$" :: rst -> Command (parse_command rst)
  | rst -> INode (parse_output rst)

let proc sd sf (e : entry) =
  match e with
  | Command command -> (
      match command with
      | ChangeDirectory name ->
          if String.equal name ".." then
            let _ = Stack.pop_exn sd in
            ()
          else Stack.push sd name
      | ReadDirectory -> ())
  | INode node -> (
      match node with
      | File { name; size } -> Stack.push sf name
      | Directory { name } -> Stack.push sf name
      | Empty -> ())

let get_input = read_input |> split_lines |> List.map ~f:parse_input

let part_one =
  let input = get_input in
  let stack = Stack.create in
  let r = List.map ~f:(proc stack) input in
  ()

let () = part_one

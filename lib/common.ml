open Core

let read_file fp = In_channel.read_all fp
let split_by str = Str.split (Str.regexp str)

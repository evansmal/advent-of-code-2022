open Core

let read_file fp = In_channel.read_all fp;;

let tokenize str = String.split_on_chars ~on:['\n'] str;;

let rec join separator = function
  | [] -> ""
  | [str] -> str
  | ""::strs -> join separator strs
  | str::strs -> str ^ separator ^ join separator strs


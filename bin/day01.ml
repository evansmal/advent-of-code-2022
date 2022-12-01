
let txt = Common.read_file "./test.txt";;
let res = Common.tokenize txt;;
let () = print_string (Common.join "," res);;


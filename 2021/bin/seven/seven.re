let fh = open_in(Array.get(Sys.argv, 1))
let initstateline = input_line(fh)
print_endline(initstateline)
let comma_re = Str.regexp(",")
let initstate = List.map((x) => { int_of_string(x) }, Str.split(comma_re, initstateline))
let state = Array.of_list(initstate)
let tbl = Hashtbl.create(~random=false, Array.length(state))
let sum = ref(0)
Array.iter((x) => {
    sum := sum^ + x
    if(Hashtbl.mem(tbl, x)) {
        Hashtbl.replace(tbl, x, Hashtbl.find(tbl, x) + 1)
    } else {
        Hashtbl.add(tbl, x, 1)
    }
}, state)
print_endline(string_of_int(sum^ / Array.length(state)))
Hashtbl.iter((v, c) => {
    print_endline("item " ++ string_of_int(v) ++ " exists: " ++ string_of_int(c))
}, tbl)

let fh = open_in(Array.get(Sys.argv, 1))
let avg = int_of_string(Array.get(Sys.argv, 2))
let initstateline = input_line(fh)
print_endline(initstateline)
let comma_re = Str.regexp(",")
let initstate = List.map((x) => { int_of_string(x) }, Str.split(comma_re, initstateline))
let state = Array.of_list(initstate)
let tbl = Hashtbl.create(~random=false, Array.length(state))
let sum = ref(0)
// the cost towards the average
let costm = ref(0)
// the cost towards the most common
let costc = ref(0)
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
sum := sum^ / Array.length(state)
Array.iter((x) => {
    switch(compare(x, sum^)) {
        | -1 => {
            costm := costm^ + (sum^ - x)
        }
        | 0 => {
            costm := costm^ + 0
        }
        | _ => {
            costm := costm^ + (x - sum^)
        }
    }

    switch(compare(x, avg)) {
        | -1 => {
            costc := costc^ + (avg - x)
        }
        | 0 => {
            costc := costc^ + 0
        }
        | _ => {
            costc := costc^ + (x - avg)
        }
    }
}, state)
print_endline("cost towards average: " ++ string_of_int(costm^))
print_endline("cost towards most common: " ++ string_of_int(costc^))

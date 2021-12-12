let array_iter_while = (fn:('a => unit), pred:('a => bool), arr:array('a)):unit => {
    let rec inner = (idx:int) => {
        switch(idx) {
            | n when n >= Array.length(arr) => ()
            | m when pred(Array.get(arr, m)) => {
                fn(Array.get(arr, m))
                inner(idx + 1)
            }
            | _ => ()
        }

    }
    inner(0)
}

let clock = (fishes:array(int)):array(int) => {
    // count the number of zeros in current state
    let zedcnt = Array.fold_right((x, y) => {if(x == 0) { y + 1 } else { y } }, fishes, 0);
    let fresh_fishes = Array.make((Array.length(fishes) + zedcnt), 0)
    let offset = ref(Array.length(fishes))
    //print_newline()
    //print_endline("0 count: " ++ string_of_int(zedcnt))
    //print_endline("offset: " ++ string_of_int(offset^))
    Array.iteri((idx, x) => {
        //print_endline("idx: " ++ string_of_int(idx))
        switch(x) {
            | -1 => {
                ()
            }
            | 0 => {
                Array.set(fresh_fishes, idx, 6)
                Array.set(fresh_fishes, offset^, 8)
                offset := offset^ + 1
            }
            | n => {
                Array.set(fresh_fishes, idx, n - 1)
            }
        }
    }, fishes)
    fresh_fishes
}

let fh = open_in(Array.get(Sys.argv, 1))
let days = int_of_string(Array.get(Sys.argv, 2))
let initstateline = input_line(fh)
print_endline(initstateline)
let comma_re = Str.regexp(",")
let initstate = List.map((x) => { int_of_string(x) }, Str.split(comma_re, initstateline))
let state = ref(Array.of_list(initstate))
/*
 * We can do this with two counters:
 *
 * - the current fish's state
 * - the number of fish spawned
 *
 * and then, instead of creating an array, we just iterate over the number of fish
 * we have, and simulate the full life cycle of the fish from there
 *
 * I do suspect that there's an even easier way, since we know that basically modulo 7
 * fish will do certain things, so give an initial state we can likely detect what that
 * individual fish will do in a given number of days
 */
for(day in 1 to days) {
    if(day == 1) {
        print_string("After  1 day: ")
    } else if(day < 10) {
        print_string("After  " ++ string_of_int(day) ++ " days: ")
    } else {
        print_string("After " ++ string_of_int(day) ++ " days: ")
    }
    state := clock(state^)
    print_endline("Current fish count: " ++ string_of_int(Array.length(state^)))
    //Array.iter((x) => { print_string(string_of_int(x) ++ ",") }, state^) 
    //print_newline()
}
print_endline("Current fish count: " ++ string_of_int(Array.length(state^)))

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(int_of_string("0b" ++ f))
            | exception End_of_file => None
        }
    })
}

let oxygen_filter = (h:array(int), l:array(int), v:int, i:int) => {
    let binval = 2 lsl (Array.length(h) - 2)
    switch((Array.get(h, i), Array.get(l, i)) {
        | (n, m) when n == m => {
            (v land binval) == binval
        }
        | (x, y) when x > m => {
            (v land binval) == binval
        }
        | (z, a) when z < a => {
            (v land binval) == 0
        }
    }
}

let compare_bits = (h:array(int), l:array(int), hl:bool):int => {
    let bits = ref(0)
    print_endline("array length: " ++ string_of_int(Array.length(h)))
    Array.iteri((i, x) => {
        let v = if(hl) { x >= Array.get(l, i) } else { x < Array.get(l, i) }
        print_endline(string_of_bool(hl))
        print_endline("bits: " ++ string_of_int(bits^))
        if(v) {
            print_endline("1 h: " ++ string_of_int(x) ++ " l: " ++ string_of_int(Array.get(l, i)))
            bits := bits^ + (2048 lsr i)
        } else {
            print_endline("0 h: " ++ string_of_int(x) ++ " l: " ++ string_of_int(Array.get(l, i)))
        }
    }, h)
    bits^
}

let fh = open_in(Array.get(Sys.argv, 1))
//let hbitcounts = [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0|]
//let lbitcounts = [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0|]
let hbitcounts = [|0, 0, 0, 0, 0|]
let lbitcounts = [|0, 0, 0, 0, 0|]

Stream.iter((x) => {
    //let cur = ref(2048)
    let cur = ref(16)
    //for(idx in 0 to 11) { 
    for(idx in 0 to 4) {
        if ((x land cur^) == cur^) {
            Array.set(hbitcounts, idx, Array.get(hbitcounts, idx) + 1)
        } else {
            Array.set(lbitcounts, idx, Array.get(lbitcounts, idx) + 1)
        }
        cur := cur^ lsr 1
    }
}, iter_channel(fh))

print_endline("high counts:")
Array.iter((x) => {print_endline(string_of_int(x));}, hbitcounts)
print_endline("low counts:")
Array.iter((x) => {print_endline(string_of_int(x));}, lbitcounts)

let gamma = compare_bits(hbitcounts, lbitcounts, true)
let epsilon = compare_bits(hbitcounts, lbitcounts, false)

print_endline("gamma: " ++ string_of_int(gamma));
print_endline("epsilon: " ++ string_of_int(epsilon));

print_endline("Power rate: " ++ string_of_int(gamma * epsilon))

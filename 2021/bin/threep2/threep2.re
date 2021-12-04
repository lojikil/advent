let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(int_of_string("0b" ++ f))
            | exception End_of_file => None
        }
    })
}

let oxygen_filter = (h:array(int), l:array(int), v:int, i:int) => {
    let lbinval = 2 lsl (Array.length(h) - 2 - i)
    let binval = if(lbinval == 0) { 1 } else { lbinval }
    print_endline("oxygen filter, binval: " ++ string_of_int(binval))
    print_endline(string_of_int(Array.get(h, i)) ++ " " ++ string_of_int(Array.get(l, i)))
    switch((Array.get(h, i), Array.get(l, i))) {
        | (n, m) when n == m => {
            print_endline("here on 16");
            (v land binval) == binval
        }
        | (x, y) when x > y => {
            print_endline("here on 20");
            (v land binval) == binval
        }
        | _ => {
            print_endline("here on 24");
            (v land binval) == 0
        }
    }
}

let co2_filter = (h:array(int), l:array(int), v:int, i:int) => {
    print_endline("here on 32")
    let lbinval = 2 lsl (Array.length(h) - 2 - i)
    let binval = if(lbinval == 0) { 1 } else { lbinval }
    print_endline("co2 filter, binval: " ++ string_of_int(binval))
    print_endline(string_of_int(Array.get(h, i)) ++ " " ++ string_of_int(Array.get(l, i)))
    switch((Array.get(h, i), Array.get(l, i))) {
        | (n, m) when n == m => {
            print_endline("here on 38");
            (v land binval) == 0
        }
        | (x, y) when x < y => {
            print_endline("here on 42");
            (v land binval) == binval
        }
        | _ => {
            print_endline("here on 46");
            (v land binval) == 0
        }
    }
}

let clone_list = (original:list(int)):list(int) => {
    List.map((x) => { x }, original)
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
let hbitcounts = [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0|]
let lbitcounts = [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0|]
//let hbitcounts = [|0, 0, 0, 0, 0|]
//let lbitcounts = [|0, 0, 0, 0, 0|]
let values = ref([])

let count_bits = (x) => {
    let cur = ref(2048)
    for(idx in 0 to 11) {
        if ((x land cur^) == cur^) {
            Array.set(hbitcounts, idx, Array.get(hbitcounts, idx) + 1)
        } else {
            Array.set(lbitcounts, idx, Array.get(lbitcounts, idx) + 1)
        }
        cur := cur^ lsr 1
    }
}

let clear_bits = () => {
    for(idx in 0 to 11) {
        Array.set(hbitcounts, idx, 0)
        Array.set(lbitcounts, idx, 0)
    }
}

Stream.iter((x) => {
    values := List.append([x], values^)
    count_bits(x)
}, iter_channel(fh))

print_endline("high counts:")
Array.iter((x) => {
        print_endline(string_of_int(x));
}, hbitcounts)
print_endline("low counts:")
Array.iter((x) => {print_endline(string_of_int(x));}, lbitcounts)
let cont = ref(true)
let idx = ref(0)
let oxygen_values = ref(clone_list(values^))
print_endline("oxygen filter hunt:")
while(cont^) {
    oxygen_values := List.filter(oxygen_filter(hbitcounts, lbitcounts, _, idx^), oxygen_values^)
    if(List.length(oxygen_values^) <= 1) {
        cont := false
    } else {
        cont := true
    }
    clear_bits();
    List.iter(count_bits, oxygen_values^)
    idx := idx^ + 1
}
idx := 0
clear_bits()
cont := true
List.iter(count_bits, values^)
print_endline("oxygen value: " ++ string_of_int(List.nth(oxygen_values^, 0)))
let oxygen = List.nth(oxygen_values^, 0)
print_endline("length of values: " ++ string_of_int(List.length(values^)))
let co2_values = ref(clone_list(values^))
print_endline("CO2 scrubber hunt:")
while(cont^) {
    co2_values := List.filter(co2_filter(hbitcounts, lbitcounts, _, idx^), co2_values^)
    if(List.length(co2_values^) <= 1) {
        cont := false
    } else {
        cont := true
    }
    clear_bits();
    List.iter(count_bits, co2_values^)
    idx := idx^ + 1
}
print_endline("co2 value: " ++ string_of_int(List.nth(co2_values^, 0)))
let co2 = List.nth(co2_values^, 0)
print_endline("lifesupport value: " ++ string_of_int(co2 * oxygen))

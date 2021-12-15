let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

// I really feel like I shouldn't have had to write this function...
let array_every = (fn: ('a => bool), a:array('a)):bool => {
    let rec inner = (idx:int):bool => {
        if(idx >= Array.length(a)) {
            true
        } else if(fn(Array.get(a, idx))) {
            inner(idx + 1)
        } else {
            false
        }
    }
    inner(0)
}

let make_grams = (src:string):array(string) => {
    // if I did my maths correctly, there is always
    // String.length(src) - 1 2-grams in a string,
    // but it's easier and less error prone to just
    // do it this way
    let res = ref([])
    for(idx in 0 to String.length(src) - 2) {
        res := List.append(res^, [String.sub(src, idx, 2)])
    }
    Array.of_list(res^)
}

let build_replace_grams = (gram_store:Hashtbl.t('a, 'b), grams:array(string)):string => {
    let res = ref([])
    Array.iter((x) => {
        let insert_part = Hashtbl.find(gram_store, x)
        let exploded_grams = [|Char.escaped(String.get(x, 0)), Char.escaped(String.get(x, 1))|]
        res := List.append(res^, [Array.get(exploded_grams, 0) ++ insert_part])
    }, grams)
    res := List.append(res^, [Char.escaped(String.get(Array.get(grams, Array.length(grams) - 1), 1))])
    String.concat("", res^)
}

let rec countdown = (gram_store:Hashtbl.t('a, 'b), cur:string, count:int):string => {
    let grams = make_grams(cur)
    let next_round = build_replace_grams(gram_store, grams)
    print_endline("current round: " ++ string_of_int(count))
    if(count < 1) {
        next_round 
    } else {
        countdown(gram_store, next_round, count - 1)
    }
}

let count_1grams = (final:string):int => {
    let counts = Hashtbl.create(~random=true, 100)
    let hicount = ref(-1)
    let lowcount = ref(-1)
    String.iter((x) => {
        if(Hashtbl.mem(counts, x)) {
            Hashtbl.replace(counts, x, Hashtbl.find(counts, x) + 1)
        } else {
            Hashtbl.add(counts, x, 1)
        }
    }, final)
    Hashtbl.iter((_, v) => {
        if(v < lowcount^ || lowcount^ == -1) {
            lowcount := v
        } else {
            ()
        }
        if(v >= hicount^ || hicount^ == -1) {
            hicount := v
        } else {
            ()
        }
    }, counts)
    hicount^ - lowcount^
}

let fh = open_in(Array.get(Sys.argv, 1))
let count = int_of_string(Array.get(Sys.argv, 2))
let linestream = iter_channel(fh)
let line_re = Str.regexp("\\([A-Z]+\\) -> \\([A-Z]\\)")
let initial = Stream.next(linestream)
let grams = Hashtbl.create(~random=false, 1000) 
print_endline("initial object: " ++ initial)
Stream.iter((x) => {
    if(Str.string_match(line_re, x, 0)) {
        let original_pair = Str.matched_group(1, x)
        let insertion = Str.matched_group(2, x)
        Hashtbl.add(grams, original_pair, insertion)
    } else {
        ()
    }
}, linestream)
let final = countdown(grams, initial, count)
print_endline("final: " ++ final)
print_endline("score: " ++ string_of_int(count_1grams(final)))

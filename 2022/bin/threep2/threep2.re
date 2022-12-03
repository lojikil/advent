let rec search_score = (needle:string, haystack:string, idx:int):int => {
    if(idx >= String.length(needle)) {
        0
    } else {
        switch(String.index(haystack, String.get(needle, idx))) {
            | n when (Char.compare(String.get(haystack, n), 'a') > 0) => {
                print_string("matched: ");
                print_char(String.get(haystack, n));
                print_endline("");
                (Char.code(String.get(haystack, n)) - Char.code('a')) + 1;
            }
            /*| n when (Char.compare(String.get(needle, n), 'A') > 0) => { */
            | n => {
                print_string("matched: ");
                print_char(String.get(haystack, n));
                print_endline("");
                (Char.code(String.get(haystack, n)) - Char.code('A')) + 27 ;
            }
            | exception Not_found => search_score(needle, haystack, idx + 1);
        }
    }
};

let score = (c:char):int => {
    switch(c) {
        | n when (Char.compare(n, 'a') > 0) => Char.code(n) - Char.code('a') + 1;
        | n => Char.code(n) - Char.code('A') + 27;
    }
};

// List has `sort_uniq` which actually does this, so I probably should have
// used that...
let rec uniq = (a:array(char), idx:int, accum:list(char)):list(char) => {
    switch(Array.get(a, idx)) {
        | c when List.length(accum) > 0 && List.hd(accum) == c => uniq(a, idx + 1, accum);
        | c => uniq(a, idx + 1, List.cons(c, accum));
        | exception Invalid_argument(_) => accum;
    }
};

let uniqify = (src:string):list(char) => {
    String.to_seq(src) |> List.of_seq |> List.sort_uniq(Char.compare, _)
};

let combine_triples = (src:string, src1:string, src2:string):list(char) => {
    List.concat([uniqify(src), uniqify(src1), uniqify(src2)]) |> List.sort(Char.compare, _);
};

let counter = (state:(int, char, list((char, int))), src:char) => {
    let (cur_count, cur_char, cur_assq) = state;

    if(src == cur_char) {
        (cur_count + 1, cur_char, cur_assq)
    } else {
        (1, src, List.cons((cur_char, cur_count), cur_assq))
    }
};

let cmp_v = (a, b) => {
    let (_, a_n) = a;
    let (_, b_n) = b;
    if(a_n >= b_n) {
        a
    } else {
        b
    }
};

let tally = (s, s1, s2) => {
    let totals = combine_triples(s, s1, s2);
    // XXX *gosh* Ok, so I forgot that we will return state for the last item *and it will not be processed*
    // so here, when the *last* item of the group was the mode, we wouldn't find it, because it was 
    // stored not in the alist but in the state for the reducer
    let (last_count, last_char, counts) = List.fold_left(counter, (0, ' ', [('$', 0)]), totals);
    let max_v = List.fold_left(cmp_v, (' ', 0), List.cons((last_char, last_count), counts));
    let (c, n) = max_v;
    print_string("actual character for triples [" ++ s ++ "," ++ s1 ++ "," ++ s2 ++ "] is: ");
    print_char(c);
    print_endline("\nwith count: " ++ string_of_int(n));
    score(c);
};

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let fh = open_in(Array.get(Sys.argv, 1))
let sum = ref(0);
let lines = ref([]);
let cnt = ref(0);
Stream.iter((x) => {
    lines := List.cons(x, lines^);
    print_endline("adding rucksack: " ++ x);
    cnt := cnt^ + 1;
    if(List.length(lines^) >= 3) {
        let score = tally(List.nth(lines^, 0), List.nth(lines^, 1), List.nth(lines^, 2));
        print_endline("Score for current triple set: " ++ string_of_int(score));
        sum := sum^ + score;
        lines := [];
    }
}, iter_channel(fh));
print_endline("Total: " ++ string_of_int(sum^));
print_endline("Counted " ++ string_of_int(cnt^) ++ " rucksacks");

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
    let (a_c, a_n) = a;
    let (b_c, b_n) = b;
    if(a_n > b_n) {
        a
    } else {
        b
    }
};

let tally = (s, s1, s2) => {
    let totals = combine_triples(s, s1, s2);
    let (_, _, counts) = List.fold_left(counter, (0, ' ', [('$', 0)]), totals);
    let max_v = List.fold_left(cmp_v, (' ', 0), counts);
    let (c, _) = max_v;
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
Stream.iter((x) => {
    let left = String.sub(x, 0, String.length(x) / 2);
    let right = String.sub(x, String.length(x) / 2, String.length(x) / 2);
    let score = search_score(left, right, 0);
    sum := sum^ + score;
    print_endline("Score: " ++ string_of_int(score));
}, iter_channel(fh));
print_endline("total score: " ++ string_of_int(sum^));

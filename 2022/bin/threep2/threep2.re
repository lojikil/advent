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

let rec uniq = (a:array(char), idx:int, accum:list(char)):list(char) => {
    switch(Array.get(a, idx)) {
        | c when List.length(accum) > 0 && List.hd(accum) == c => uniq(a, idx + 1, accum);
        | c => uniq(a, idx + 1, List.cons(c, accum));
        | exception Invalid_argument(_) => accum;
    }
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

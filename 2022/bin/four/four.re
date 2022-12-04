let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let print_state = (fr0:int, fr1:int, sr0:int, sr1:int, cmp:(int, int)):unit => {
    let (bind0, bind1) = cmp;
    print_string("comparing: ")
    print_int(fr0);
    print_string("-");
    print_int(fr1);
    print_string(" to ");
    print_int(sr0);
    print_string("-");
    print_int(sr1);
    print_string(": ");
    print_int(bind0);
    print_string(" ");
    print_int(bind1);
    switch((bind0, bind1)) {
        | (0, 0) => print_string(" => not contained\n");
        | (-1, 1) => print_string(" => contained\n");
        | (1, -1) => print_string(" => contained\n");
        | (1, 0) => print_string(" => contained\n");
        | (-1, 0) => print_string(" => contained\n");
        | (0, 1) => print_string(" => contained\n");
        | (0, -1) => print_string(" => contained\n");
        | (_, _) => print_string(" => not contained\n");
    };
}

let overlap = (fr0:int, fr1:int, sr0:int, sr1:int):bool => {
    switch((compare(fr0, sr0), compare(fr1, sr1))) {
        | (0, 0) => true;
        | (-1, 1) => true;
        | (1, -1) => true;
        | (1, 0) => true;
        | (-1, 0) => true;
        | (0, 1) => true;
        | (0, -1) => true;
        | (_, _) => false;
    };
}

let partial_overlap = (fr0:int, fr1:int, sr0:int, sr1:int):bool => {
    ((fr0 <= sr0 && sr0 <= fr1) ||
     (fr0 <= sr1 && sr1 <= fr1) ||
     (sr0 <= fr0 && fr0 <= sr1) ||
     (sr0 <= fr1 && fr1 <= sr1))
}

let fh = open_in(Array.get(Sys.argv, 1));
let cnt = ref(0);
let pnt = ref(0);
Stream.iter((x) => {
    let pairs = String.split_on_char(',', x);
    let first_range = List.map(int_of_string, String.split_on_char('-', List.nth(pairs, 0)));
    let secnd_range = List.map(int_of_string, String.split_on_char('-', List.nth(pairs, 1)));
    // I initially tried `let [fr0, fr1] = first_range` but couldn't figure out how to
    // get around an exhaustiveness check, so :shrug:
    let fr0 = List.nth(first_range, 0);
    let fr1 = List.nth(first_range, 1);
    let sr0 = List.nth(secnd_range, 0);
    let sr1 = List.nth(secnd_range, 1);
    let cmp = (compare(fr0, sr0), compare(fr1, sr1));
    print_state(fr0, fr1, sr0, sr1, cmp);
    if(overlap(fr0, fr1, sr0, sr1)) {
        cnt := cnt^ + 1;
    } else if(partial_overlap(fr0, fr1, sr0, sr1)) {
        pnt := pnt^ + 1;
    }
}, iter_channel(fh));
print_endline("Final count: " ++ string_of_int(cnt^));
print_endline("Partial count: " ++ string_of_int(pnt^));
print_endline("Total: " ++ string_of_int(cnt^ + pnt^));

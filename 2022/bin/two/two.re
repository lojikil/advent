let play = (round:list(string)) => {
    switch(round) {
        | ["A", "X"] => 4 
        | ["A", "Y"] => 8
        | ["A", "Z"] => 3
        | ["B", "X"] => 1
        | ["B", "Y"] => 5
        | ["B", "Z"] => 9
        | ["C", "X"] => 7
        | ["C", "Y"] => 2
        | ["C", "Z"] => 6
        | _ => 0
    }
};

let wopr = (round:list(string)) => {
    // I thought about doing this in such
    // a way that I just had a `lose`, `draw`,
    // `win` set of lambdas, but it's easier
    // to just linearly lay out here...
    switch(round) {
        | ["A", "X"] => play(["A", "Z"]) 
        | ["A", "Y"] => play(["A", "X"]) 
        | ["A", "Z"] => play(["A", "Y"])
        | ["B", "X"] => play(["B", "X"])
        | ["B", "Y"] => play(["B", "Y"])
        | ["B", "Z"] => play(["B", "Z"])
        | ["C", "X"] => play(["C", "Y"])
        | ["C", "Y"] => play(["C", "Z"])
        | ["C", "Z"] => play(["C", "X"])
        | _ => 0
    }
}

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f |> String.split_on_char(' ', _))
            | exception End_of_file => None
        }
    })
}

let fh = open_in(Array.get(Sys.argv, 1))
let accum = ref(0)
let strat = ref(0)
Stream.iter((x) => {
    let rnd = play(x);
    let wnd = wopr(x);
    print_endline("Round: " ++ String.concat(" ", x) ++ " " ++ string_of_int(rnd) ++ ", " ++ string_of_int(wnd));
    accum := accum^ + rnd;
    strat := strat^ + wnd;
}, iter_channel(fh));

print_endline("Total: " ++ string_of_int(accum^));
print_endline("Strategic Total: " ++ string_of_int(strat^));

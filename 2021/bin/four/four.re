let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let fh = open_in(Array.get(Sys.argv, 1))
let bing_stream = iter_channel(fh)
let bingo_numbers = Stream.next(bing_stream)
Stream.iter((x) => {
    print_endline(x)
}, bing_stream)
print_endline("game stream is: " ++ bingo_numbers)

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f |> int_of_string)
            | exception End_of_file => None
        }
    })
}

let increase_p = (current:int, prev:int):bool => {
    prev != -1 && current > prev
}

let fh = open_in(Array.get(Sys.argv, 1))
let total_change = ref(0)
let previous_value = ref(-1)
Stream.iter((x) => {
    if(increase_p(x, previous_value^)) {
            total_change := total_change^ + 1
            previous_value := x
    } else {
        previous_value := x
    }
}, iter_channel(fh))

total_change^ |> string_of_int |> print_endline


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
let previous_values = [|-1, -1, -1|];
let idx = ref(0)
let sum = ref(0)
Stream.iter((x) => {
    switch(idx^) {
        | 0 => {
            Array.set(previous_values, idx^, x)
        }
        | 1 => {
            Array.set(previous_values, idx^, x)
        }
        | _ => {
            idx := 2
            Array.set(previous_values, idx^, x)
            sum := Array.fold_left((x, y) => { x + y}, 0, previous_values)
            if(increase_p(sum^, previous_value^)) {
                total_change := total_change^ + 1
                previous_value := sum^
            } else {
                previous_value := sum^
            }
            Array.set(previous_values, 0, Array.get(previous_values, 1))
            Array.set(previous_values, 1, Array.get(previous_values, 2))
        }
    }
    idx := idx^ + 1
}, iter_channel(fh))

total_change^ |> string_of_int |> print_endline


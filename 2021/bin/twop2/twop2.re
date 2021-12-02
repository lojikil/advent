let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let increase_p = (current:int, prev:int):bool => {
    prev != -1 && current > prev
}

let fh = open_in(Array.get(Sys.argv, 1))
let depth = ref(0)
let horizontal = ref(0)
let aim = ref(0)
Stream.iter((x) => {
    let off = String.index(x, ' ')
    let cmd = String.sub(x, 0, off)
    let value = String.sub(x, off + 1, String.length(x) - off - 1)
    print_endline("command: " ++ cmd ++ " value: " ++ value)
    switch(cmd) {
        | "forward" => {
            horizontal := horizontal^ + int_of_string(value)
            depth := depth^ + (aim^ * int_of_string(value))
        }
        | "down" => {
            aim := aim^ + int_of_string(value)
        }
        | "up" => {
            aim := aim^ - int_of_string(value)
        }
        | _ => { 
            print_endline("invalid command: " ++ cmd)
        }
    }
}, iter_channel(fh))

print_endline("depth: " ++ string_of_int(depth^) ++ " horizontal: " ++ string_of_int(horizontal^))
print_endline("combined: " ++ string_of_int(depth^ * horizontal^))

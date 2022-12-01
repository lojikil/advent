let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | "" => Some(-1)
            | f => Some(f |> int_of_string)
            | exception End_of_file => None
        }
    })
}

let fh = open_in(Array.get(Sys.argv, 1))
let max = ref(0)
let accum = ref(0)
Stream.iter((x) => {
    switch(x) {
        | -1 => {
            // new elf
            if(max^ < accum^) {
                max := accum^
            }
            accum := 0
        }
        | _ => {
            accum := accum^ + x
        }
    }
}, iter_channel(fh))

print_endline("")
print_int(max^)
print_endline("")

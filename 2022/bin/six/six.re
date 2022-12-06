let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}
let find_window = (~start_message:bool=false, packet:string):int => {
    let rec check_window = (accum:list((char, int)), cur:int, cnt:int):bool => {
        switch(String.get(packet, cur)) {
            | _ when cnt == 0 => true
            | c when List.mem_assoc(c, accum) => false
            | c => check_window(List.cons((c, 1), accum), cur + 1, cnt - 1)
        }
    };
    let rec inner_find = (start:int, cnt:int): int => {
        switch(check_window([(' ', 0)], start, cnt)) {
            | true => start
            | false when (start + cnt) >= String.length(packet) => -1
            | false => inner_find(start + 1, cnt)
        }
    };
    switch(start_message) {
        | false => inner_find(0, 4);
        | true => inner_find(0, 14);
    };
}
let fh = open_in(Array.get(Sys.argv, 1));
Stream.iter((x) => {
    let res = find_window(x);
    let msg = find_window(~start_message=true, x);
    print_endline("window for " ++ x ++ " is " ++ string_of_int(res) ++ " to " ++ string_of_int(res + 4));
    print_endline("and the message is " ++ string_of_int(msg) ++ " to " ++ string_of_int(msg + 14));
}, iter_channel(fh));

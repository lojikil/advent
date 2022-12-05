type elfstacksops = 
    | Stacks(int, list(string))
    | Move(int, int, int)
    | Nop

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let rec drop_until = (u:int, src:list(string)) => {
    switch(u) {
        | 0 => src
        | n when n > 0 => drop_until(u - 1, List.tl(src))
        | _ => src
    }
};

let rec take_while = (~accum:list(string)=[], w:int, src:list(string)) => {
    switch(w) { 
        | 0 => accum
        | n when n > 0 => take_while(~accum=List.cons(List.hd(src), accum), w - 1, List.tl(src))
        | _ => accum
    }
};

let parse = (src:string) => {
    let len = String.length(src);
    let parts = String.split_on_char(' ', src);
    switch(len) {
        | _ when String.starts_with(~prefix="stack", src) => {
            let stack_num = int_of_string(List.nth(parts, 1));
            let init_stack = List.rev(drop_until(2, parts));
            Stacks(stack_num, init_stack)
        }
        | _ when String.starts_with(~prefix="move", src) => {
            // I would just unpack, but the exhaustiveness checker
            // will get me again...
            let n = int_of_string(List.nth(parts, 1))
            let m = int_of_string(List.nth(parts, 3))
            let p = int_of_string(List.nth(parts, 5))
            Move(n, m, p)
        }
        | _ => {
            Nop
        }
    }
};

let show = (eso:elfstacksops):string => {
    switch(eso) {
        | Stacks(n, s) => "Stack " ++ string_of_int(n) ++ " " ++ String.concat(" ", s);
        | Move(n, m, p) when m == p => "SuperMove" ++ string_of_int(n) ++ ", " ++ string_of_int(m);
        | Move(n, m, p) => "Move " ++ string_of_int(n) ++ ", " ++ string_of_int(m) ++ ", " ++ string_of_int(p)
        | Nop => "Nop"
    }
};

let microx = (stacks:array(list(string)), eso:elfstacksops):unit => {
    switch(eso) {
        | Stacks(n, s) => {
            Array.set(stacks, n, s);
        }
        | Move(n, m, p) => {
            let stack_m = Array.get(stacks, m);
            let stack_p = Array.get(stacks, p);
            Array.set(stacks,
                      p,
                      List.append(take_while(n, stack_m), stack_p));
            Array.set(stacks,
                      m,
                      drop_until(n, stack_m));
        }
        | Nop => ()
    }
};

let fh = open_in(Array.get(Sys.argv, 1));
let stacks = Array.make(10, [""]);
Stream.iter((x) => {
    let eso = parse(x);
    print_endline(show(eso));
    microx(stacks, eso)
}, iter_channel(fh));
Array.iteri((idx, l) => {
    print_string(string_of_int(idx) ++ ": ")
    print_endline(String.concat(" ", l)) }, stacks);
let r = Array.to_list(Array.map((x) => { if (List.length(x) > 0) { List.hd(x) } else { "" } }, stacks));
print_endline("Result: " ++ String.concat("", r));

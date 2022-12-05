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
            let init_stack = drop_until(2, parts);
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

let rec find_top = (~idx:int=0, src:array(string)):int => {
    switch(Array.get(src, idx)) {
        | "" => idx
        | _ => find_top(~idx=idx + 1, src)
        | exception Invalid_argument(_) => -1
    }
};

let clamp = (i:int):int => {
    if(i < 0) {
        0
    } else if(i > 250) {
        250
    } else {
        i
    }
}

let cranelift = (src:array(string), dst: array(string), cnt:int):unit => {
    let src_top = find_top(src);
    let dst_top = find_top(dst);
    // technically, I think this is what the `Array.blit` method if for...
    print_endline("here on 76 with: " ++ string_of_int(cnt));
    let rec inner_lift = (src_idx:int, dst_idx, tnc) => {
        print_endline("here on 78 with " ++ string_of_int(src_idx) ++ ", " ++ string_of_int(dst_idx) ++ ", " ++ string_of_int(tnc));
        switch((src_idx, dst_idx, tnc)) {
            | (_, _, 0) => ()
            | (_, _, n) when n <= 0 => ()
            | (a, b, c) when (a >= 0 && b >= 0 && b <= 200) => {
                let t = Array.get(src, a);
                Array.set(src, a, "");
                Array.set(dst, b, t);
                inner_lift(src_idx - 1, dst_idx + 1, c - 1);
            }
            | (_, _, _) => ()
        }
    };
    inner_lift(clamp(src_top - 1), clamp(dst_top - 1), cnt);
}

let microx = (stacks:array(array(string)), eso:elfstacksops):unit => {
    switch(eso) {
        | Stacks(n, s) => {
            let t = Array.of_list(s);
            let dst = Array.get(stacks, n);
            Array.iteri((i, v) => {
                print_endline("adding " ++ v);
                Array.set(dst, i, v);
            }, t);
        }
        | Move(n, m, p) => {
            let stack_m = Array.get(stacks, m);
            let stack_p = Array.get(stacks, p);
            cranelift(stack_m, stack_p, n);
        }
        | Nop => ()
    }
};

let fh = open_in(Array.get(Sys.argv, 1));
//Alet stacks = Array.make(10, [""]);
let stacks = Array.make_matrix(10, 250, "");
Stream.iter((x) => {
    let eso = parse(x);
    print_endline(show(eso));
    microx(stacks, eso)
}, iter_channel(fh));
Array.iteri((idx, l) => {
    print_string(string_of_int(idx) ++ ": ")
    print_endline(String.concat(" ", List.of_seq(Array.to_seq(l)))) }, stacks);

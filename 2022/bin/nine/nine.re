let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}
let delta = (cur_h:(int, int), cur_t:(int, int)):(int, int) => {
    let (h_x, h_y) = cur_h;
    let (t_x, t_y) = cur_t;
    (h_x - t_x, h_y - t_y)
};
let print_pos = (p:(int, int)):unit => {
    let (n, m) = p;
    print_string("(" ++ string_of_int(n) ++ "," ++ string_of_int(m) ++ ")");
};
let string_of_pos = (p:(int, int)) => {
    let (n, m) = p;
    "(" ++ string_of_int(n) ++ "," ++ string_of_int(m) ++ ")";
};
let step = (direction:string, amt:int, cur_h:(int, int), cur_t:(int, int)):((int, int), (int, int)) => {
    let one_step = (dir:string, c_h:(int, int)):(int, int) => {
        let (c_h_x, c_h_y) = c_h;
        switch(dir) {
            | "R" => (c_h_x + 1, c_h_y)
            | "L" => (c_h_x - 1, c_h_y)
            | "U" => (c_h_x, c_h_y + 1)
            | "D" => (c_h_x, c_h_y - 1)
            | _ => c_h;
        };
    };
    let rec steps = (amt_rem:int, off_h:(int, int), off_t:(int, int)):((int, int), (int, int)) => {
        print_string("current " ++ direction ++ " amount: " ++ string_of_int(amt_rem) ++ " ");
        print_pos(off_h);
        print_string(" ");
        print_pos(off_t);
        print_endline("");
        if(amt_rem == 0) {
            (off_h, off_t)
        } else {
            let n_h = one_step(direction, off_h);
            let (t_x, t_y) = off_t;
            let d = delta(n_h, off_t);
            let (d_x, d_y) = d;
            print_endline("delta: " ++ string_of_pos(d));
            switch(d) {
                | (2, _) => steps(amt_rem - 1, n_h, (t_x + 1, t_y + d_y));
                | (-2, _) => steps(amt_rem - 1, n_h, (t_x - 1, t_y + d_y));
                | (_, 2) => steps(amt_rem - 1, n_h, (t_x + d_x, t_y + 1));
                | (_, -2) => steps(amt_rem - 1, n_h, (t_x + d_x, t_y - 1));
                | (_, _) => steps(amt_rem - 1, n_h, (t_x, t_y));
            }
        }
    };
    steps(amt, cur_h, cur_t);
};
let tpos = ref((0, 0));
let hpos = ref((0, 0));
let seen = ref([]);
let fh = open_in(Array.get(Sys.argv, 1));
Stream.iter((x) => {
    let parts = String.split_on_char(' ', x);
    let d = List.nth(parts, 0);
    let a = int_of_string(List.nth(parts, 1));
    let t = step(d, a, hpos^, tpos^);
    let (new_h, new_t) = t;
    seen := List.cons(tpos^, seen^);
    tpos := new_t;
    hpos := new_h;
}, iter_channel(fh));
List.iter((position) => {
    print_string("have seen: ");
    print_pos(position);
    print_endline("");
}, seen^);

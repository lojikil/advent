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
let hotstepper = (direction:string, amt:int, h_cur:(int, int), tail:list((int, int))):((int, int), list((int, int))) => {
    let rec delta_steps = (accum:list((int, int)), t:(int, int), rope:list((int, int))) => {
        switch(rope) {
            | [] => List.rev(accum);
            | [x, ... xs] => {
                let (d_x, d_y) = delta(t, x);
                let (t_x, t_y) = x;
                switch((d_x, d_y)) {
                    | (2, _) => delta_steps(List.cons(t, accum), (t_x + 1, t_y + d_y), xs);
                    | (-2, _) => delta_steps(List.cons(t, accum), (t_x - 1, t_y + d_y), xs);
                    | (_, 2) =>  delta_steps(List.cons(t, accum), (t_x + d_x, t_y + 1), xs);
                    | (_, -2) => delta_steps(List.cons(t, accum), (t_x + d_x, t_y - 1), xs);
                    | (_, _) => delta_steps(List.cons(t, accum), (t_x, t_y), xs);
                }
            }
        }
    }
    let rec hotsteps = (cur_amt, cur_h, cur_tail) => {
        print_endline("hotsteps length: " ++ string_of_int(List.length(cur_tail)));
        print_endline("hotsteps tailcheck: " ++ string_of_pos(List.nth(cur_tail, 8)));
        switch(cur_amt) {
            | 0 => (cur_h, cur_tail)
            | n => {
                let (new_h, new_t) = step(direction, 1, cur_h, List.hd(cur_tail));
                let offset_tail = delta_steps([], new_t, List.tl(cur_tail));
                hotsteps(n - 1, new_h, List.cons(new_t, offset_tail));
            }
        }
    }
    hotsteps(amt, h_cur, tail);
};
let tpos = ref((0, 0));
let hpos = ref((0, 0));
let p2hpos = ref((0, 0));
let p2tail= ref([(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0)]); 
let seen = ref([]);
let fh = open_in(Array.get(Sys.argv, 1));
Stream.iter((x) => {
    let parts = String.split_on_char(' ', x);
    let d = List.nth(parts, 0);
    let a = int_of_string(List.nth(parts, 1));
    let t = step(d, a, hpos^, tpos^);
    let (new_h, new_t) = t;
    let p2t = hotstepper(d, a, p2hpos^, p2tail^);
    let (new_p2h, new_p2tail) = p2t;
    seen := List.cons(tpos^, seen^);
    tpos := new_t;
    hpos := new_h;
    p2hpos := new_p2h;
    p2tail := new_p2tail;
}, iter_channel(fh));
List.iter((position) => {
    print_string("have seen: ");
    print_pos(position);
    print_endline("");
}, seen^);

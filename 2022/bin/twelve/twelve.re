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

let climable_p = (cur:char, next:char):bool => {
    let clamp_cur = if(cur == 'S') { 
        'a';
    } else if(cur == 'E') {
        'z';
    } else {
        cur;
    };
    let clamp_next = if(next == 'E') {
        'z'
    } else if(next == 'S') {
        'a'
    } else {
        next
    };
    let test = Char.compare(clamp_cur, clamp_next);
    print_endline("Climbing? " ++ Char.escaped(clamp_cur) ++ "<=>" ++ Char.escaped(clamp_next) ++ ": " ++ string_of_int(test));
    test >= -1 && test <= 1;
}

let make_map = (src:list(string)):array(array(char)) => {
    let w = String.length(List.nth(src, 0));
    let h = List.length(src);
    print_endline("died after 17");
    // each row of the map is `w` wide
    // and the map itself is `h` high
    let ret = Array.make_matrix(h, w, ' ');
    Array.iteri((idx, currow) => {
        print_endline("dying after: " ++ string_of_int(idx));
        let row = Array.of_seq(String.to_seq(List.nth(src, idx)));
        Array.blit(row, 0, currow, 0, w);
    }, ret);
    ret
};

let map_get = (m:array(array(char)), x, y) => {
    Array.get(Array.get(m, y), x); 
}

let map_get_pos = (m:array(array(char)), p:(int, int)) => {
    let (x, y) = p;
    Array.get(Array.get(m, y), x);
}

let print_list = (l:list(int)):unit => {
    print_endline("list: " ++ String.concat(", ", List.map(string_of_int, l)))
}

// pathfinder? more like *patch* finder amirite?
let pathfinder = (m:array(array(char)), spos:(int, int), epos:(int, int)):list(int) => {
    let max_x = Array.length(Array.get(m, 0));
    let max_y = Array.length(m);
    let up = (curpos):option((int, int))=> {
        switch(curpos) {
            | (_, n) when (n - 1) < 0 => None;
            | (l, n) => {
                print_endline("in new_pos test up " ++ string_of_pos((l, n - 1)));
                let up_range = map_get(m, l, n - 1);
                let cur_range = map_get(m, l, n);
                if(climable_p(cur_range, up_range)) {
                    print_endline("new_test up is actually climable");
                    Some((l, n - 1));
                } else {
                    None
                }
            }
        };
    };
    let down = (curpos):option((int, int))=> {
        switch(curpos) {
            | (_, n) when (n + 1) >= max_y => None;
            | (l, n) => {
                print_endline("in new_pos test down");
                let dw_range = map_get(m, l, n + 1);
                let cur_range = map_get(m, l, n);
                if(climable_p(cur_range, dw_range)) {
                    print_endline("new down is climable" ++ string_of_pos((l, n + 1)));
                    Some((l, n + 1));
                } else {
                    None
                }
            }
        };
    };
    let left = (curpos):option((int, int))=> {
        print_endline("in left: " ++ string_of_pos(curpos));
        switch(curpos) {
            | (z, _) when (z - 1) < 0 => None;
            | (l, n) => {
                print_endline("in new_test left:" ++ string_of_pos(curpos) ++ "=>" ++ string_of_pos((l - 1, n)));
                let lf_range = map_get(m, l - 1, n);
                let cur_range = map_get(m, l, n);
                if(climable_p(cur_range, lf_range)) {
                    print_endline("new_test left is climable");
                    Some((l - 1, n));
                } else {
                    print_endline("new test left climable returned false for: " ++ string_of_pos(curpos));
                    None
                }
            }
        };
    };
    let right = (curpos):option((int, int))=> {
        switch(curpos) {
            | (n, _) when (n + 1) >= max_x => None;
            | (l, n) => {
                print_endline("in new_test right");
                let rt_range = map_get(m, l + 1, n);
                let cur_range = map_get(m, l, n);
                if(climable_p(cur_range, rt_range)) {
                    print_endline("new_test right is actually climable" ++ string_of_pos((l + 1, n)));
                    Some((l + 1, n));
                } else {
                    None
                }
            }
        };
    };

    switch(left((0, 1))) {
        | Some(_) => print_endline("as a test: left of (0,1) is climable");
        | None => print_endline("as a test: left of (0,1) is unclimable");
    }
    switch(left((1, 0))) {
        | Some(_) => print_endline("as a test: left of (1,0) is climable");
        | None => print_endline("as a test: left of (1,0) is unclimable");
    }
    let seen = Hashtbl.create(~random=false, 10000);
    Hashtbl.add(seen, epos, true);
    // man... so really I need to convert these into a walker & a
    // consumer, but not tonight...
    let rec walk = (curdistance:int, curpos:(int, int)) => {
        //let lpaths = [up(curpos), down(curpos), left(curpos), right(curpos)];
        let next_walks = ref([])
        print_endline("curpos: " ++ string_of_pos(curpos));
        if(curpos == spos) {
            print_endline("super distance: " ++ string_of_int(curdistance));
        }
        switch(up(curpos)) {
            | Some(p) => {
                if(!Hashtbl.mem(seen, p)) {
                    Hashtbl.add(seen, p, true);
                    if(p == spos) {
                        print_endline("super distance first if: " ++ string_of_int(curdistance + 1));
                    }
                    next_walks := List.cons(p, next_walks^);
                } else if(p == spos) {
                    print_endline("super distance if: " ++ string_of_int(curdistance + 1));
                }
            }
            | None => ()
        }

        switch(down(curpos)) {
            | Some(p) => {
                if(!Hashtbl.mem(seen, p)) {
                    Hashtbl.add(seen, p, true);
                    if(p == spos) {
                        print_endline("super distance first if: " ++ string_of_int(curdistance + 1));
                    }
                    next_walks := List.cons(p, next_walks^);
                } else if(p == spos) {
                    print_endline("super distance if: " ++ string_of_int(curdistance + 1));
                }
            }
            | None => ()
        }

        switch(left(curpos)) {
            | Some(p) => {
                if(!Hashtbl.mem(seen, p)) {
                    Hashtbl.add(seen, p, true);
                    if(p == spos) {
                        print_endline("super distance first if: " ++ string_of_int(curdistance + 1));
                    }
                    next_walks := List.cons(p, next_walks^);
                } else if(p == spos) {
                    print_endline("super distance if: " ++ string_of_int(curdistance + 1));
                }
            }
            | None => ()
        }

        switch(right(curpos)) {
            | Some(p) => {
                if(!Hashtbl.mem(seen, p)) {
                    Hashtbl.add(seen, p, true);
                    if(p == spos) {
                        print_endline("super distance first if: " ++ string_of_int(curdistance + 1));
                    }
                    next_walks := List.cons(p, next_walks^);
                } else if(p == spos) {
                    print_endline("super distance if: " ++ string_of_int(curdistance + 1));
                }
            }
            | None => ()
        }
        List.iter((next_walk) => {
            walk(curdistance + 1, next_walk);
        }, next_walks^)
        /*print_endline("distance: " ++ string_of_int(curdistance));
        if(curpos == spos) {
            print_endline("  super distance: " ++ string_of_int(curdistance));
        }
        print_endline("from " ++ string_of_pos(curpos) ++ " the following are walkable: ");
        List.iter((x) => {
            switch(x) {
                | Some(p) => {
                    print_endline("  " ++ string_of_pos(p));
                    if(!Hashtbl.mem(seen, p)) {
                        Hashtbl.add(seen, p, true);
                        walk(curdistance + 1, p);
                    }
                }
                | None => ();
            }
        }, lpaths);*/
    }
    walk(0, epos);
    [0];
}

let print_map = (m:array(array(char))):unit => {
    Array.iter((r) => {
        print_endline(String.of_seq(Array.to_seq(r)));
    }, m);
}

let fh = open_in(Array.get(Sys.argv, 1));
let lines = ref([]);
let s_pos = ref((0, 0));
let e_pos = ref((0, 0));
let line = ref(0);
Stream.iter((x) => {
    lines := List.cons(x, lines^);
    // thoughts for my blog: `index` has the
    // opposite order of `split_on_char`
    switch(String.index(x, 'S')) {
        | n => {
            s_pos := (n, line^);
        }
        | exception Not_found => ();
    };
    // thoughts for my blog: this effectively
    // becomes a Result type, because Reason
    // makes exception handling nicer, but
    // would it be better to make this an
    // explicit result?
    switch(String.index(x, 'E')) {
        | n => {
            e_pos := (n, line^);
        }
        | exception Not_found => ();
    };
    line := line^ + 1;
}, iter_channel(fh));
let map = make_map(List.rev(lines^));
print_map(map);
print_endline("E: " ++ string_of_pos(e_pos^));
print_endline("S: " ++ string_of_pos(s_pos^));
let paths = pathfinder(map, s_pos^, e_pos^);
let sorted_paths = List.sort(compare, paths);
if(List.length(sorted_paths) > 0) {
    print_endline("shortest: " ++ string_of_int(List.nth(sorted_paths,0)));
    List.iter((path) => {
        print_endline("path cost: " ++ string_of_int(path));
    }, sorted_paths);
} else {
    print_endline("returned 0 paths?");
    print_endline(string_of_int(List.length(paths)));
}

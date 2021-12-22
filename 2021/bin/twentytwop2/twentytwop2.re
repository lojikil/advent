let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

// I really feel like I shouldn't have had to write this function...
let array_every = (fn: ('a => bool), a:array('a)):bool => {
    let rec inner = (idx:int):bool => {
        if(idx >= Array.length(a)) {
            true
        } else if(fn(Array.get(a, idx))) {
            inner(idx + 1)
        } else {
            false
        }
    }
    inner(0)
}

let mark_point = (b:array(array(int)), point: (int, int)) => {
    print_endline("in mark_point")
    let (x, y) = point
    let inner = Array.get(b, y)
    Array.set(inner, x, Array.get(inner, x) + 1)
}

let order_points = (s:int, t:int): (int, int) => {
    if(s < t) {
        (s, t)
    } else {
        (t, s)
    }
}

let calculate_line = (x0:int, y0:int, x1:int, y1:int):array((int, int)) => {
    let res = ref([])
    if(x0 == x1) {
        let (starty, endy) = order_points(y0, y1)
        for(dy in starty to endy) {
            res := List.append([(x0, dy)], res^)
        }
    } else if(y0 == y1) {
        let (startx, endx) = order_points(x0, x1)
        for(dx in startx to endx) {
            res := List.append([(dx, y0)], res^)
        }
    } else {
        ()
    }
    Array.of_list(res^)
}

let draw_line = (b:array(array(int)), x0:int, y0:int, x1:int, y1:int) => {
    let line_points = calculate_line(x0, y0, x1, y1)
    Array.iter((p) => {
        mark_point(b, p)
    }, line_points)
}

let show_board = (b:array(array(int))) => {
    for(i in 0 to Array.length(b) - 1) {
        let inner_board = Array.get(b, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            if(Array.get(inner_board, j) == 0) {
                print_string(".")
            } else {
                print_string(string_of_int(Array.get(inner_board, j)))
            }
        }
        print_newline()
    }
}

let count_board = (b:array(array(int))) => {
    let res = ref(0)
    // What is this, amateur hour? fold_left that stuff...
    // ... it is definitely amateur hour
    for(i in 0 to Array.length(b) - 1) {
        let inner_board = Array.get(b, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            if(Array.get(inner_board, j) >= 2) {
                res := res^ + 1
            } else {
                ()
            }
        }
    }
    print_endline("overlapping points: " ++ string_of_int(res^))
}

let calc_pos = (midpoint:int, x:int, y:int, z:int) => {
    midpoint + (x * 100) +  (y * 10) + z
}

let make_coords = (xs:string, xe:string, ys:string, ye: string, zs:string, ze:string):(int,int,int,int,int,int) => {
    (int_of_string(xs), int_of_string(xe),
     int_of_string(ys), int_of_string(ye),
     int_of_string(zs), int_of_string(ze))
}

let check_coords = (xs, xe, ys, ye, zs, ze) => {
    (xs <= 50 && xs >= -50 &&
     xe <= 50 && xe >= -50 &&
     ys <= 50 && ys >= -50 &&
     ye <= 50 && ye >= -50 &&
     zs <= 50 && zs >= -50 &&
     ze <= 50 && ze >= -50)
}

// what we actually need to do is just treat this as a linear array
// and calculate offsets for values therein
let board_size = 102 * 102 * 102
let midpoint = board_size / 2
let board = Array.make(board_size, 0)
let hboard = Hashtbl.create(~random=false, board_size)
let fh = open_in(Array.get(Sys.argv, 1))
let linestream = iter_channel(fh)
let line_re = Str.regexp("\\(on\\|off\\) x=\\(-?[0-9]+\\)..\\(-?[0-9]+\\),y=\\(-?[0-9]+\\)..\\(-?[0-9]+\\),z=\\(-?[0-9]+\\)..\\(-?[0-9]+\\)")
Stream.iter((x) => {
    if(Str.string_match(line_re, x, 0)) {
        let cmd = Str.matched_group(1, x)
        let startx = Str.matched_group(2, x)
        let endx = Str.matched_group(3, x)
        let starty = Str.matched_group(4, x)
        let endy = Str.matched_group(5, x)
        let startz = Str.matched_group(6, x)
        let endz = Str.matched_group(7, x)
        let (ixs, ixe, iys, iye, izs, ize) = make_coords(startx, endx, starty, endy, startz, endz)
        // thinking about this:
        // could store all the ranges (off, on)
        // then bound the "on" ranges by what is turned off
        // that way, we're only turning on things that actually
        // matter...
        for(x in ixs to ixe) {
            for(y in iys to iye) {
                for(z in izs to ize) {
                    if(cmd == "on") {
                        Hashtbl.replace(hboard, (x, y, z), 1)
                    } else {
                        Hashtbl.replace(hboard, (x, y, z), 0)
                    }
                }
            }
        }
    } else {
        ()
    }
}, linestream)

let sum = ref(0)
Hashtbl.iter((_, y) => {
    //let (ix, iy, iz) = x
    if(y == 1) {
        sum := sum^ + 1
        //print_endline("on at: " ++ string_of_int(ix) ++ "," ++ string_of_int(iy) ++ "," ++ string_of_int(iz))
    } else {
        ()
    }
}, hboard)
print_endline("hboard: " ++ string_of_int(Hashtbl.length(hboard)))
print_endline("sum: " ++ string_of_int(sum^))

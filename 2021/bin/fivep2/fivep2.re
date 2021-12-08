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
    let lim = Array.length(b)
    let (x, y) = point
    if(x < 0 || y < 0 || x >= lim || y >= lim ) {
        ()
    } else {
        let inner = Array.get(b, y)
        Array.set(inner, x, Array.get(inner, x) + 1)
    }
}

let order_points = (s:int, t:int): (int, int) => {
    if(s < t) {
        (s, t)
    } else {
        (t, s)
    }
}

let islope = (x0:int, y0:int, x1:int, y1:int):int => {
    (y1 - y0) / (x1 - x0)
}

let idistance = (x0:int, y0:int, x1:int, y1:int):int => {
    let fx0 = float_of_int(x0)
    let fy0 = float_of_int(y0)
    let fx1 = float_of_int(x1)
    let fy1 = float_of_int(y1)
    let distance = Float.sqrt(Float.pow(fx1 -. fx0, 2.) +. Float.pow(fy1 -. fy0, 2.))
    int_of_float(Float.floor(distance))
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
        let slope = islope(x0, y0, x1, y1)
        let distance = idistance(x0, y0, x1, y1)
        let dx = ref(x0)
        let dy = ref(y0)
        if(x1 < x0 && y1 < y0) {
            print_endline("we swapped points")
            dx := x1
            dy := y1
        } else {
            ()
        }
        // use the slope to modify dx, and distance to know how many points
        print_endline("slope: " ++ string_of_int(slope))
        for(_ in 0 to distance) {
            print_endline("sloping: " ++ string_of_int(dx^) ++ "," ++ string_of_int(dy^))
            if(dx^ > x1 && dy^ > y1 && slope < 0) {
                ()
            } else {
                res := List.append([(dx^, dy^)], res^)
                dx := dx^ + slope
                dy := dy^ + 1
            }
        }
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

let board_size = int_of_string(Array.get(Sys.argv, 2))
let board = Array.make_matrix(board_size, board_size, 0)
let fh = open_in(Array.get(Sys.argv, 1))
let linestream = iter_channel(fh)
let line_re = Str.regexp("\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)")
Stream.iter((x) => {
    if(Str.string_match(line_re, x, 0)) {
        let x0 = Str.matched_group(1, x)
        let y0 = Str.matched_group(2, x)
        let x1 = Str.matched_group(3, x)
        let y1 = Str.matched_group(4, x)
        print_string("Drawing line from (" ++ x0 ++ "," ++ y0 ++ ")")
        print_endline(" to (" ++ x1 ++ "," ++ y1 ++ ")")
        draw_line(board, int_of_string(x0), int_of_string(y0), int_of_string(x1), int_of_string(y1))
    } else {
        ()
    }
}, linestream)

show_board(board)
count_board(board)

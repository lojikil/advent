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

let board_size = 100 * 100 * 100
let board = Array.make_array(board_size, board_size, 0)
let fh = open_in(Array.get(Sys.argv, 1))
let linestream = iter_channel(fh)
let line_re = Str.regexp("\\(on\\|off\\) x=\\([0-9]+\\)..\\([0-9]+\\),y=\\([0-9]+\\)..\\([0-9]+\\),z=\\([0-9]+\\)..\\([0-9]+\\)")
Stream.iter((x) => {
    if(Str.string_match(line_re, x, 0)) {
        let cmd = Str.matched_group(1, x)
        let startx = Str.matched_group(2, x)
        let endx = Str.matched_group(3, x)
        let starty = Str.matched_group(4, x)
        let endy = Str.matched_group(5, x)
        let startz = Str.matched_group(6, x)
        let endz = Str.matched_group(7, x)
        print_string("turning " ++ startx ++ "..." ++ endx ++ "," ++ starty ++ "..." ++ endy)
        print_endline("," ++ startz ++ "..." ++ endz ++ " " ++ cmd)
    } else {
        ()
    }
}, linestream)


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

let calculate_line = (x0:int, y0:int, x1:int, y1:int):array((int, int)) => {
    [|(x0, y0), (x1, y1)|]
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

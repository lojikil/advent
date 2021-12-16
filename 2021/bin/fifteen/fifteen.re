let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

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

let matrix_get = (b:array(array(int)), x:int, y:int):option(int) => {
    if(x >= 0 && x < Array.length(b)) {
        let inner_array = Array.get(b, x)
        if(y >= 0 && y < Array.length(inner_array)) {
            Some(Array.get(inner_array, y))
        } else {
            None
        }
    } else {
        None
    }
}

let matrix_set = (b:array(array(int)), x:int, y:int, v:int):unit => {
    if(x >= 0 && x < Array.length(b)) {
        let inner_array = Array.get(b, x)
        if(y >= 0 && y < Array.length(inner_array)) {
            Array.set(inner_array, y, v)
        } else {
            ()
        }
    } else {
        ()
    }
}

let clamped_points = (b:array(array(int)), x:int, y:int) => {
    if(x >= 0 && x < Array.length(b)) {
        let inner_array = Array.get(b, x)
        if(y >= 0 && y < Array.length(inner_array)) {
            Some((x, y))
        } else {
            None
        }
    } else {
        None
    }
}

let walk_nodes = (board:array(array(int))) => {
    let max_x = Array.length(board) - 1
    let max_y = Array.length(Array.get(board, 0)) - 1
    print_endline("max x,y: " ++ string_of_int(max_x) ++ "," ++ string_of_int(max_y))
    let rec inner_walk = (x, y, risk) => {
        let new_nodes = [clamped_points(board, x + 1, y), clamped_points(board, x, y + 1)]
        print_endline("in inner_walk, have: " ++ string_of_int(List.length(new_nodes)) ++ " nodes to walk")
        List.iter((cur_coords) => {
            switch(cur_coords) {
                | Some((ex, ey)) when (ex == max_x && ey == max_y) => {
                    print_endline("risk for this path: " ++ string_of_int(risk))
                }
                | Some((dx, dy)) => {
                    let value = Option.value(matrix_get(board, dx, dy), ~default=0)
                    print_endline("walking to: " ++ string_of_int(dx) ++ "," ++ string_of_int(dy))
                    inner_walk(dx, dy, risk + value )
                }
                | None => {
                    ()
                }
            }
        }, new_nodes)
    }
    inner_walk(0, 0, 0)
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

let matrix_get = (b:array(array(int)), x:int, y:int):option(int) => {
    if(x >= 0 && x < Array.length(b)) {
        let inner_array = Array.get(b, x)
        if(y >= 0 && y < Array.length(inner_array)) {
            Some(Array.get(inner_array, y))
        } else {
            None
        }
    } else {
        None
    }
}

let adjacent_points = (b:array(array(int)), x:int, y:int) => {
    let points = [matrix_get(b, x - 1, y), matrix_get(b, x, y - 1), matrix_get(b, x + 1, y), matrix_get(b, x, y + 1)]
    let step1 = List.filter((x) => {
        switch(x) {
            | Some(_) => true
            | None => false
        }
    }, points)
    Array.of_list(List.map((x) => { Option.value(x, ~default=0) }, step1))
}

let risky = (b:array(array(int)), x:int, y:int) => {
    let c = matrix_get(b, x, y)
    switch(c) {
        | Some(cur) => {
            let adjacent = adjacent_points(b, x, y)
            // we are only risk iff all our adjacent points
            // are greater than we are
            array_every((x) => { x > cur }, adjacent)
        }
        | None => false
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)
let initboard = ref([])

Stream.iter((x) => {
    let curline = Array.make(String.length(x), 0)
    String.iteri((idx, curval) => {
        Array.set(curline, idx, Int.abs(Char.compare(curval, '0')))
    }, x)
    initboard := List.append(initboard^, [curline])
}, stream)

let finalboard = Array.of_list(initboard^)
show_board(finalboard)
walk_nodes(finalboard)

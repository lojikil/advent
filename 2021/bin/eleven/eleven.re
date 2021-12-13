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

let show_board = (b:array(array(int))) => {
    for(i in 0 to Array.length(b) - 1) {
        let inner_board = Array.get(b, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            if(Array.get(inner_board, j) < 10) {
                print_string("  " ++ string_of_int(Array.get(inner_board, j)))
            } else {
                print_string(" " ++ string_of_int(Array.get(inner_board, j)))
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
    print_string("clamping: " ++ string_of_int(x) ++ "," ++ string_of_int(y))
    if(x >= 0 && x < Array.length(b)) {
        let inner_array = Array.get(b, x)
        if(y >= 0 && y < Array.length(inner_array)) {
            print_endline("... clamped")
            Some((x, y))
        } else {
            print_endline("... not clamped")
            None
        }
    } else {
        print_endline("... not clamped 1")
        None
    }
}

let adjacent_points = (b:array(array(int)), x:int, y:int) => {
    let points0 = [matrix_get(b, x - 1, y), matrix_get(b, x, y - 1), matrix_get(b, x + 1, y), matrix_get(b, x, y + 1)]
    let points1 = [matrix_get(b, x - 1, y -1), matrix_get(b, x - 1, y + 1), matrix_get(b, x + 1, y - 1), matrix_get(b, x + 1, y + 1)]
    let points = List.append(points0, points1)
    let step1 = List.filter((x) => {
        switch(x) {
            | Some(_) => true
            | None => false
        }
    }, points)
    Array.of_list(List.map((x) => { Option.value(x, ~default=0) }, step1))
}

let flashes = ref(0)

let rec flash = (b:array(array(int)), x:int, y:int) => {
    let points0 = [clamped_points(b, x - 1, y), clamped_points(b, x, y - 1), clamped_points(b, x + 1, y), clamped_points(b, x, y + 1)]
    let points1 = [clamped_points(b, x - 1, y - 1), clamped_points(b, x - 1, y + 1), clamped_points(b, x + 1, y - 1), clamped_points(b, x + 1, y + 1)]
    let points = List.filter((x) => {
        switch(x) {
            | Some(_) => true
            | None => false
        }
    }, List.append(points0, points1))
    //Array.set(Array.get(b, x), y, 0)
    List.iter((x) => {
        let (ix, iy) = Option.value(x, ~default=(0, 0))
        let v = Option.value(matrix_get(b, ix, iy), ~default=0)
        //matrix_set(b, ix, iy, v + 1) 
        if(v == 0) {
            ()
        } else {
            clock(b, ix, iy)
        }
    }, points)
} and clock = (b:array(array(int)), x:int, y:int) => {
    let v = Array.get(Array.get(b, x), y) 
    switch(v) {
        | m when m >= 9 => {
            //print_endline("here on 111: " ++ string_of_int(x) ++ "," ++ string_of_int(y))
            matrix_set(b, x, y, 0)
            flashes := flashes^ + 1;
            flash(b, x, y) 
        }
        | n  => {
            Array.set(Array.get(b, x), y, n + 1)
        }
    }
}

let clear_flash = (b:array(array(int)), x:int, y:int):unit => {
    let v = Array.get(Array.get(b, x), y)
    switch(v) {
        | m when m > 9 => {
            //print_endline("here on 126")
            matrix_set(b, x, y, 0)
        }
        | _ => {
            ()
        }
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let days = int_of_string(Array.get(Sys.argv, 2))
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
print_endline("before any steps: ")
show_board(finalboard)
for(day in 1 to days) {
    for(dx in 0 to Array.length(finalboard) - 1) {
        let innerboard = Array.get(finalboard, dx)
        for(dy in 0 to Array.length(innerboard) - 1) {
            clock(finalboard, dx, dy) 
        }
    }
    /*for(dx in 0 to Array.length(finalboard) - 1) {
        let innerboard = Array.get(finalboard, dx)
        for(dy in 0 to Array.length(innerboard) - 1) {
            clear_flash(finalboard, dx, dy)
        }
    }*/
    print_endline("after step " ++ string_of_int(day) ++ ":")
    show_board(finalboard)
}
print_endline("total flashes: " ++ string_of_int(flashes^))

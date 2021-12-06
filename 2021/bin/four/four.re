let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

/*
 * This is super imperative, but it works; a functional version could
 * just pass along current values as parameters. Additionally, we could
 * construct the board as we go along with the monadic-style below, and
 * just include currently read board and construct it as we move along
 * the string. Can then use a monad to capture current state of the 
 * world... but this is fine really hahaha
 */
let find_slot = (board:ref(array(array(string))), value:string) => {
    let cont = ref(true)
    for(i in 0 to Array.length(board^) - 1) {
        let inner_board = Array.get(board^, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            // obviously we will iterate through the whole
            // board regardless here, but it's not that big
            // of a deal when you consider that boards are
            // 5x5 grids
            if(Array.get(inner_board, j) == "" && cont^) {
                Array.set(inner_board, j, value)
                cont := false
            } else {
                ()
            }
        }
    }
}

let is_whitespace = (c:char):bool => {
    Char.compare(c, ' ') == 0 || Char.compare(c, '\t') == 0 || Char.compare(c, '\n') == 0 || Char.compare(c, '\r') == 0
}

let is_numeric = (c:char):bool => {
    let r = Char.compare('0', c);
    r >= -9 && r <= 0
}

let rec take_white = (src:string, offset:int):int => {
    print_endline("t/w: " ++ string_of_int(offset))
    switch(String.get(src, offset)) {
        | n when is_whitespace(n) => take_white(src, offset + 1)
        | _ => offset
    }
}

let take_number = (b:ref(array(array(string))), src:string, start:int):int => {
    print_endline("t/n: " ++ string_of_int(start))
    let rec inner_numeric = (offset:int):int => {
        print_endline("i/n: " ++ string_of_int(offset))
        switch(String.get(src, offset)) {
            | n when is_numeric(n) => inner_numeric(offset + 1)
            | _ => {
                print_endline("i/n(fin): " ++ string_of_int(offset))
                let num = String.sub(src, start, offset - start)
                find_slot(b, num)
                offset
            }
            | exception Invalid_argument(_) => {
                start
            }
        }
    }
    if(start < String.length(src)) {
        inner_numeric(start)
    } else {
        start
    }
}

let show_board = (b:array(array(string)), _:int) => {
    print_endline("in show_board")
    for(i in 0 to Array.length(b) - 1) {
        let inner_board = Array.get(b, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            if(String.length(Array.get(inner_board, j)) == 2) {
                print_string(Array.get(inner_board, j) ++ " ")
            } else {
                print_string(" " ++ Array.get(inner_board, j) ++ " ")
            }
        }
        print_newline()
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let bing_stream = iter_channel(fh)
let bingo_numbers = Stream.next(bing_stream)
let numbers_split_re = Str.regexp(",")
let initboards = ref([])
let curboard = ref([||])
print_endline("here?")
Stream.iter((x) => {
    print_endline("89: " ++ x)
    switch(x) {
        | "" => {
            initboards := List.append(initboards^, [curboard^])
            curboard := Array.make_matrix(5, 5, "")
        }
        | _ => {
            take_white(x, 0) |> take_number(curboard, x, _) |>
            take_white(x, _) |> take_number(curboard, x, _) |>
            take_white(x, _) |> take_number(curboard, x, _) |>
            take_white(x, _) |> take_number(curboard, x, _) |>
            take_white(x, _) |> take_number(curboard, x, _) |>
            show_board(curboard^, _)
        }
    }
}, bing_stream)
initboards := List.append(initboards^, [curboard^])
print_endline("game stream is: " ++ bingo_numbers)
let initnumbers = Str.split(numbers_split_re, bingo_numbers)
let numbers = Array.of_list(initnumbers)
Array.iter((x) => { print_endline("number is: " ++ x) }, numbers)
let boards = Array.of_list(initboards^)

Array.iter((x) => {
    show_board(x, 0)
}, boards)

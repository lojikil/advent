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
    switch(String.get(src, offset)) {
        | n when is_whitespace(n) => take_white(src, offset + 1)
        | _ => offset
    }
}

let take_number = (b:ref(array(array(string))), src:string, start:int):int => {
    let rec inner_numeric = (offset:int):int => {
        switch(String.get(src, offset)) {
            | n when is_numeric(n) => inner_numeric(offset + 1)
            | _ => {
                let num = String.sub(src, start, offset - start)
                find_slot(b, num)
                offset
            }
            | exception Invalid_argument(_) => {
                let num = String.sub(src, start, offset - start)
                find_slot(b, num)
                offset
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


let calc_score = (b:array(array(string))):int => {
    let sum = ref(0)
    for(i in 0 to Array.length(b) - 1) {
        let inner_board = Array.get(b, i)
        for(j in 0 to Array.length(inner_board) - 1) {
            if(String.get(Array.get(inner_board, j), 0) != '+') {
                sum := sum^ + int_of_string(Array.get(inner_board, j))
            } else {
                ()
            }
        }
    }
    sum^
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

let vertical_win_p = (board:array(array(string)), col:int):bool => {
    array_every((x) => { Char.compare(String.get(Array.get(x, col), 0), '+') == 0 }, board)
}

let horizontal_win_p = (board_row:array(string)):bool => {
    array_every((x) => { Char.compare(String.get(x, 0), '+') == 0 }, board_row)
}

let play = (playcount:int, bingo_number:string, b:array(array(array(string)))) => {
    Array.iteri((idx, board) => {
        for(i in 0 to Array.length(board) - 1) {
            let inner_board = Array.get(board, i)
            for(j in 0 to Array.length(inner_board) - 1) {
                if(Array.get(inner_board, j) == bingo_number) {
                    Array.set(inner_board, j, "+" ++ bingo_number)
                } else {
                    ()
                }
            }
            if(horizontal_win_p(inner_board)) {
                let final_play = int_of_string(bingo_number)
                let score = calc_score(board)
                print_string("board: " ++ string_of_int(idx) ++ " won on play: " ++ string_of_int(playcount) ++ " horizontally; ")
                print_endline("final score is: " ++ string_of_int(final_play * score))
            } else {
                ()
            }
            if(vertical_win_p(board, i)) {
                let final_play = int_of_string(bingo_number)
                let score = calc_score(board)
                print_string("board: " ++ string_of_int(idx) ++ " won on play: " ++ string_of_int(playcount) ++ " vertically; ")
                print_endline("final score is: " ++ string_of_int(final_play * score))
            } else {
                ()
            }
        }
    }, b)
}

let fh = open_in(Array.get(Sys.argv, 1))
let bing_stream = iter_channel(fh)
let bingo_numbers = Stream.next(bing_stream)
let numbers_split_re = Str.regexp(",")
let initboards = ref([])
let curboard = ref([||])
Stream.iter((x) => {
    print_endline("152: " ++ x)
    switch(x) {
        | "" => {
            if(Array.length(curboard^) != 0) {
                initboards := List.append(initboards^, [curboard^])
            } else {
                ()
            }
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

print_endline("Let's play a game:")
Array.iteri((pc, x) => {
    play(pc, x, boards)
}, numbers)

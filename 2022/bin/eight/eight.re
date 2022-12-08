let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
};

let rec print_board = (~row:int=0, ~col:int=0, b:array(array(int))):unit => {
    switch((row, col)) {
        | (n, _) when (n >= 100) => ()
        | (_, m) when (m >= 100) => {
            print_endline("");
            print_board(~row=row + 1, ~col=0, b);
        }
        | (k, 0) when Array.get(Array.get(b, k), 0) == -1 => ()
        | (i, j) when Array.get(Array.get(b, i), j) == -1 => {
            print_endline("");
            print_board(~row=row + 1, ~col=0, b);
        }
        | (r, c) => {
            print_int(Array.get(Array.get(b, r), c));
            print_board(~row=row, ~col=col + 1, b);
        }
    }
}

let make_ints = (src:char):int => {
    Char.code(src) - 48;
};

let get_row_length = (row:array(int)):int => {
    let l = Array.length(row);
    let rec inner_g_r_l = (idx:int):int => {
        if (idx >= l) {
            l
        } else if (Array.get(row, idx) == -1) {
            idx
        } else {
            inner_g_r_l(idx + 1);
        }
    };
    inner_g_r_l(0);
};

let get_col_length = (board:array(array(int))):int => {
    let l = Array.length(board); // this is the max number of cols...
    // so here, we just iterate over every row
    // until we hit either:
    //
    // . a row whose first cell is -1
    // . the max length of the board
    let rec inner_g_c_l = (idx:int):int => {
        if(idx >= l) {
            l
        } else if (Array.get(Array.get(board, idx), 0) == -1) {
            idx
        } else {
            inner_g_c_l(idx + 1);
        }
    };
    inner_g_c_l(0);
};

// an interesting thought: because we iterate through this board to calculate 
// visibility, we *could* actually memoize if a specific position is visible or not
// and then we don't actually have to check it again there after... basically,
// create a visibility matrix in the first path, and we can easily count which
// members of the matrix are visible or not
let check_visible = (board:array(array(int)), row:int, col:int, max_row:int, max_col:int) => {
    let rec check_rows_above = (v:int, r:int, c:int):bool => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            print_endline("hit false on " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            false;
        } else if (r == 0) {
            print_endline("hit an edge without an intervening value: " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            true;
        } else {
            check_rows_above(v, r - 1, c);
        }
    };
    let rec check_rows_below = (v:int, r:int, c:int):bool => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            print_endline("hit false on " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            false;
        } else if (r == max_row) {
            print_endline("hit an edge without an intervening value: " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            true;
        } else {
            check_rows_below(v, r + 1, c);
        }
    };
    let rec check_cols_left = (v:int, r:int, c:int):bool => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            print_endline("hit false on " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            false;
        } else if (c == 0) {
            print_endline("hit an edge without an intervening value: " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            true;
        } else {
            check_cols_left(v, r, c - 1);
        }
    };
    let rec check_cols_right = (v:int, r:int, c:int):bool => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            print_endline("hit false on " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            false;
        } else if (c == max_col) {
            print_endline("hit an edge without an intervening value: " ++ string_of_int(r) ++ "," ++ string_of_int(c));
            true;
        } else {
            check_cols_right(v, r, c + 1);
        }
    }
    // weirdly, this entire switch is marked as unused...
    /*
    switch((row, col)) {
        // the edges are always visible
        | (0, _) => true
        | (max_row, _) => true
        | (_, 0) => true
        | (_, max_col) => true
        | (r, c) => { // probably because of this? maybe it should have been (_, _)?
            let tmp_v = Array.get(Array.get(board, r), c);
            check_row_major(tmp_v, r, c) || check_col_major(tmp_v, r, c);
        }
    }
    */
    if(row == 0 || row == max_row) {
        true
    } else if (col == 0 || col == max_col) {
        true
    } else {
        let tmp_v = Array.get(Array.get(board, row), col);
        check_rows_above(tmp_v, row - 1, col) || 
        check_rows_below(tmp_v, row + 1, col) ||
        check_cols_left(tmp_v, row, col - 1) ||
        check_cols_right(tmp_v, row, col + 1);
    }
};

let count_board = (board:array(array(int)), max_row:int, max_col:int):int => {
    // edges are always visible
    let init_count = (max_col * 2) + ((max_row - 2) * 2);
    print_endline("initial count: " ++ string_of_int(init_count));
    // we could do this is a p functional way: pass in an accumulator and 
    // increment it when a cell is visible
    let rec inner_count = (accum, row, col): int => {
        // again, the visibility checker for switch is killing me
        /*
        switch((row, col)) {
            | (max_row, _) => accum
            | (_, max_col) => {
                inner_count(accum, row + 1, 0);
            }
            | (r, c) when (r != max_row && c != max_col) => {
                if(check_visible(board, row, col, max_row, max_col)) { 
                    inner_count(accum + 1, row, col + 1);
                } else {
                    inner_count(accum, row, col + 1);
                }
            }
        }
        */
        if(row == max_row) {
            accum
        } else if(col == (max_col - 1)) {
            // we know the edges are visible
            inner_count(accum, row + 1, 1);
        } else if(check_visible(board, row, col, max_row, max_col)) {
            print_endline("count: " ++ string_of_int(row) ++ "," ++ string_of_int(col) ++ " is visible?");
            inner_count(accum, row, col + 1);
        } else {
            print_endline("count: " ++ string_of_int(row) ++ "," ++ string_of_int(col) ++ " is not visible?");
            inner_count(accum + 1, row, col + 1);
        }
    };
    inner_count(0, 1, 1);
};

let count_visible = (board:array(array(int)), row:int, col:int, max_row:int, max_col:int) => {
    let rec count_rows_above = (accum:int, v:int, r:int, c:int):int => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            accum;
        } else if (r == 0) {
            accum;
        } else {
            count_rows_above(accum + 1, v, r - 1, c);
        }
    };
    let rec count_rows_below = (accum:int, v:int, r:int, c:int):int => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            accum;
        } else if (r == max_row) {
            accum;
        } else {
            count_rows_below(accum + 1, v, r + 1, c);
        }
    };
    let rec count_cols_left = (accum:int, v:int, r:int, c:int):int => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            accum;
        } else if (c == 0) {
            accum;
        } else {
            count_cols_left(accum + 1, v, r, c - 1);
        }
    };
    let rec count_cols_right = (accum: int, v:int, r:int, c:int):int => {
        let check_v = Array.get(Array.get(board, r), c);
        if (check_v >= v) {
            accum;
        } else if (c == max_col) {
            accum;
        } else {
            count_cols_right(accum + 1, v, r, c + 1);
        }
    }

    let tmp_v = Array.get(Array.get(board, row), col);
    count_rows_above(0, tmp_v, row - 1, col) *
    count_rows_below(0, tmp_v, row + 1, col) *
    count_cols_left(0, tmp_v, row, col - 1) *
    count_cols_right(0, tmp_v, row, col + 1);
};

let scenic_board = (board:array(array(int)), max_row:int, max_col:int):int => {
    let rec find_max_scenery = (cur:int, r:int, c:int):int => {
        let check_v = Array.get(Array.get(board, r), c);
        if (r == max_row) {
            cur;
        } else if (c == max_col) {
            find_max_scenery(cur, r + 1, 1);
        } else if (check_v > cur) {
            find_max_scenery(check_v, r, c + 1);
        } else {
            find_max_scenery(cur, r, c + 1);
        }
    }
    find_max_scenery(0, 1, 1);
};

let board = Array.make_matrix(100, 100, -1);
let fh = open_in(Array.get(Sys.argv, 1));
let row = ref(0);
Stream.iter((x) => {
    let tmp = Array.of_seq(Seq.map(make_ints, String.to_seq(x)));
    Array.blit(tmp, 0, Array.get(board, row^), 0, Array.length(tmp));
    row := row^ + 1;
}, iter_channel(fh));
print_board(board);
let max_row = get_row_length(Array.get(board, 0));
let max_col = get_col_length(board);
print_endline("board is " ++ string_of_int(max_row) ++ "x" ++ string_of_int(max_col));
print_string("is board[1,1] visible? ");
if(check_visible(board, 1, 1, max_row, max_col)) {
    print_endline("yes");
} else {
    print_endline("no");
}
print_string("is board[1,3] visible? ");
if(check_visible(board, 1, 3, max_row, max_col)) {
    print_endline("yes");
} else {
    print_endline("no");
}
print_string("is board[2,2] visible? ");
if(check_visible(board, 2, 2, max_row, max_col)) {
    print_endline("yes");
} else {
    print_endline("no");
}
print_string("is board[2,3] visible? ");
if(check_visible(board, 2, 3, max_row, max_col)) {
    print_endline("yes");
} else {
    print_endline("no");
}
let total_size = max_row * max_col;
let notviz_size = count_board(board, max_row, max_col);
print_endline("board count: " ++ string_of_int(total_size) ++ " visible: " ++ string_of_int(notviz_size));
print_endline("visible trees count: " ++ string_of_int(total_size - notviz_size));
let scenary = scenic_board(board, max_row, max_col);
print_endline("max scenary: " ++ string_of_int(scenary));

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

let rec calc_pos = (start:int, roll:int):int => {
    switch(start) {
        | _ when roll == 0 => {
            start
        }
        | 10 => {
            calc_pos(1, roll - 1)
        }
        | _ => {
            calc_pos(start + 1, roll - 1)
        }
    }
}

let rec diracs_game = (onepos:int, onescore:int, twopos:int, twoscore:int, curdie:int, whose:bool, onewins:int, twowins:int):(int, int) {
}

let onestart = int_of_string(Array.get(Sys.argv, 1))
let twostart = int_of_string(Array.get(Sys.argv, 2))

// none of this needs to be stateful,
// a simple tail called function would be easy enough
let onepos = ref(onestart)
let twopos = ref(twostart)
let onescore = ref(0)
let twoscore = ref(0)
let die = ref(1)
let dierolls = ref(0)

while(onescore^ < 1000 && twoscore^ < 1000) {
    let roll = (die^ + (die^ + 1) + (die^ + 2))
    die := die^ + 3
    onepos := calc_pos(onepos^, roll)
    onescore := onescore^ + onepos^
    print_endline("Player 1 rolls " ++ string_of_int(roll) ++ " and moves to space " ++ string_of_int(onepos^) ++ " for a total score of " ++ string_of_int(onescore^))
    let roll2 = (die^ + (die^ + 1) + (die^ + 2))
    die := die^ + 3
    twopos := calc_pos(twopos^, roll2)
    twoscore := twoscore^ + twopos^
    print_endline("Player 2 rolls " ++ string_of_int(roll2) ++ " and moves to space " ++ string_of_int(twopos^) ++ " for a total score of " ++ string_of_int(twoscore^))
    dierolls := dierolls^ + 6
}
print_endline("die rolls: " ++ string_of_int(dierolls^))
print_endline("scores: " ++ string_of_int(onescore^) ++ "/" ++ string_of_int(twoscore^))
if(onescore^ < twoscore^) {
    print_endline(string_of_int(onescore^ * dierolls^))
} else {
    print_endline(string_of_int(twoscore^ * dierolls^))
}

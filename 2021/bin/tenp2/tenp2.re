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

type either = 
    | Left(string)
    | Right(int);

let score = (c:char):int => {
    switch(c) {
        | ')' => 3
        | ']' => 57
        | '}' => 1197
        | '>' => 25137
        | _ => 0
    }
}

let pushdown_navigation = (line:string) => {
    let stack = ref([' '])
    let mismatch = ref(' ')
    print_endline("line: " ++ line)
    String.iter((x) => {
        switch(x) {
            | '('  
            | '{' 
            | '[' 
            | '<' when Char.equal(mismatch^, ' ') => {
                    stack := List.append([x], stack^)
            }
            | ')'
            | '}'
            | ']'
            | '>' when Char.equal(mismatch^, ' ') => {
                let cur = List.hd(stack^)
                stack := List.tl(stack^)
                switch((cur, x)) {
                    | ('(', ')') 
                    | ('{', '}')
                    | ('[', ']')
                    | ('<', '>') => ()
                    | _ => {
                        print_endline("mismatch: " ++ Char.escaped(cur) ++ ":" ++ Char.escaped(x))
                        if(Char.equal(x, ' ')) {
                            ()
                        } else {
                            mismatch := x
                        }
                    }
                }
            }
            | _ => ()
        }
    }, line)
    if(Char.equal(mismatch^, ' ')) {
        Left(line)
    } else {
        Right(score(mismatch^))
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)
let sum = ref(0)

Stream.iter((x) => {
    let res = pushdown_navigation(x)
    switch(res) {
        | Left(_) => print_endline("incomplete string")
        | Right(score) => {
            sum := sum^ + score
            print_endline("corrupted string: " ++ string_of_int(score))
        }
    }
}, stream)

print_endline("total: " ++ string_of_int(sum^))

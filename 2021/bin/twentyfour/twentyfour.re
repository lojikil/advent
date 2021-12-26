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

type instruction = 
    | Inp(string)
    | Add(string, string)
    | Mul(string, string)
    | Div(string, string)
    | Mod(string, string)
    | Eql(string, string)
    | Nop

// haha not sleeping with the baby killed my forward momentum here... 
let parse_instruction = (src:string):instruction => {
    let op = String.sub(src, 0, 3)
    let op0 = String.sub(src, 4, 1)
    switch(op) {
        | "inp" => {
            Inp(op0)    
        }
        | "add" => {
            let op1 = String.sub(src, 6, String.length(src) - 6)
            Add(op0, op1)
        }
        | "mul" => {
            let op1 = String.sub(src, 6, String.length(src) - 6)
            Mul(op0, op1)
        }
        | "div" => {
            let op1 = String.sub(src, 6, String.length(src) - 6)
            Div(op0, op1)
        }
        | "mod" => {
            let op1 = String.sub(src, 6, String.length(src) - 6)
            Mod(op0, op1)
        }
        | "eql" => {
            let op1 = String.sub(src, 6, String.length(src) - 6)
            Eql(op0, op1)
        }
        | _ => {
            Nop
        }
    }
}

let dump_instruction = (src:instruction):unit => {
    switch(src) {
        | Inp(i) => print_endline("Input value to: " ++ i)
        | Add(a0, a1) => print_endline("Add value " ++ a1 ++ " to value stored in " ++ a0)
        | Mul(m0, m1) => print_endline("Mull value " ++ m1 ++ " to value stored in " ++ m0)
        | Div(d0, d1) => print_endline("Div value " ++ d1 ++ " to value stored in " ++ d0)
        | Mod(o0, o1) => print_endline("Mod value " ++ o1 ++ " to value stored in " ++ o0)
        | Eql(e0, e1) => print_endline("Eql value " ++ e1 ++ " to value stored in " ++ e0)
        | Nop => print_endline("Nope")
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)

Stream.iter((x) => {
    dump_instruction(parse_instruction(x))
}, stream)

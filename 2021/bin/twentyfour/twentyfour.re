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

type alu_state = {
    input_values:array(int),
    mutable iv_offset:int,
    mutable x:int,
    mutable y:int,
    mutable z:int,
    mutable w:int,
    mutable ip:int
}

let register_or_integer = (state:alu_state, src:string):int => {
    switch(src) {
        | "w" => state.w
        | "x" => state.x
        | "y" => state.y
        | "z" => state.z
        | _ => int_of_string(src)
    }
}

/*
 * it could also make a lot of sense to return a new ALU state
 * for each execution
 */
let micro_execute = (inst:instruction, state:alu_state):unit => {
    switch(inst) {
        /* it almost would have been easier to just use a
         * register file here...
         */
        | Add(a0, a1) => {
            let op1 = register_or_integer(state, a1)
            let op0 = register_or_integer(state, a0)
            switch(a0) {
                | "w" => {
                    state.w = op1 + op0
                }
                | "x" => {
                    state.x = op1 + op0
                }
                | "y" => {
                    state.y = op1 + op0
                }
                | "z" => {
                    state.z = op1 + op0
                }
                | _ => ()
            }
        }
        | Mul(m0, m1) => {
            let op1 = register_or_integer(state, m1)
            let op0 = register_or_integer(state, m0)
            switch(m0) {
                | "w" => {
                    state.w = op1 * op0
                }
                | "x" => {
                    state.x = op1 * op0
                }
                | "y" => {
                    state.y = op1 * op0
                }
                | "z" => {
                    state.z = op1 * op0
                }
                | _ => ()
            }
        }
        | Div(d0, d1) => {
            let op1 = register_or_integer(state, d1)
            let op0 = register_or_integer(state, d0)
            switch(d0) {
                | "w" when op1 > 0 => {
                    state.w = op0 / op1
                }
                | "x" when op1 > 0 => {
                    state.x = op0 / op1
                }
                | "y" when op1 > 0 => {
                    state.y = op0 / op1
                }
                | "z" when op1 > 0 => {
                    state.z = op0 / op1
                }
                | _ => ()
            }
        }
        | Mod(o0, o1) => {
            let op1 = register_or_integer(state, o1)
            let op0 = register_or_integer(state, o0)
            let res = if(op0 >= 0 && op1 > 0) {
                Some(op0 mod op1)
            } else {
                None
            }
            switch((o0, res)) {
                | ("w", Some(r)) => {
                    state.w = r
                }
                | ("x", Some(r)) => {
                    state.x = r
                }
                | ("y", Some(r)) => {
                    state.y = r
                }
                | ("z", Some(r)) => {
                    state.z = r 
                }
                | _ => ()
            }
        }
        | Eql(m0, m1) => {
            let op1 = register_or_integer(state, m1)
            let op0 = register_or_integer(state, m0)
            let res = if(op1 == op0) { 1 } else { 0 }
            switch(m0) {
                | "w" => {
                    state.w = res
                }
                | "x" => {
                    state.x = res
                }
                | "y" => {
                    state.y = res
                }
                | "z" => {
                    state.z = res
                }
                | _ => ()
            }
        }
        | Inp(i0) => {
            switch(i0) {
                | "w" when (state.iv_offset < Array.length(state.input_values))  => {
                    state.w = Array.get(state.input_values, state.iv_offset)
                    state.iv_offset = state.iv_offset + 1
                }
                | "x" when (state.iv_offset < Array.length(state.input_values))  => {
                    state.x = Array.get(state.input_values, state.iv_offset)
                    state.iv_offset = state.iv_offset + 1
                }
                | "y" when (state.iv_offset < Array.length(state.input_values))  => {
                    state.y = Array.get(state.input_values, state.iv_offset)
                    state.iv_offset = state.iv_offset + 1
                }
                | "z" when (state.iv_offset < Array.length(state.input_values))  => {
                    state.z = Array.get(state.input_values, state.iv_offset)
                    state.iv_offset = state.iv_offset + 1
                }
                | _ => ()
            }
        }
        | _ => ()
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

let dump_state = (state:alu_state):unit => {
    print_endline("w: " ++ string_of_int(state.w))
    print_endline("x: " ++ string_of_int(state.x))
    print_endline("y: " ++ string_of_int(state.y))
    print_endline("z: " ++ string_of_int(state.z))
    print_endline("v: " ++ string_of_int(state.iv_offset))
}

let increment_iv = (iv:array(int)):bool => {
    // we always want to increment at least one
    let carry = ref(1)
    // we really could do this "while carry == 1"
    for(idx in 13 downto 0) {
        switch(carry^) {
            | 1 when Array.get(iv, idx) == 9 => {
                Array.set(iv, idx, 1)
            }
            | 1 => {
                Array.set(iv, idx, Array.get(iv, idx) + 1)
                carry := 0
            }
            | _ => ()
        }
    };
    !(carry^ == 1)
}

let fuzz_monad = (src:list(instruction)):unit => {
    let iv = [|3,3,3,3,3,3,3,3,3,3,3,3,3,3|]
    let cont = ref(true)
    while(cont^) {
        let state = {
            input_values:iv,
            iv_offset:0,
            w:0,
            x:0,
            y:0,
            z:0,
            ip:0
        }
        List.iter((inst) => {
            micro_execute(inst, state)
        }, src)
        if(state.z == 0) {
            print_endline("found a match: ")
            Array.iter((x) => {print_string(string_of_int(x) ++ " ") }, iv)
            print_newline()
        } else {
            ()
        }
        cont := increment_iv(iv)
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)
let init_state = {
    input_values: [|10, 30|],
    iv_offset: 0,
    w: 0,
    x: 0,
    y: 0,
    z: 0,
    ip: 0
}
let instructions = ref([])

Stream.iter((x) => {
    let inst = parse_instruction(x)
    instructions := List.append(instructions^, [inst])
    dump_instruction(inst)
    micro_execute(inst, init_state)
    dump_state(init_state)
}, stream)

if(Array.length(Sys.argv) >= 2) {
    fuzz_monad(instructions^)
} else {
    ()
}

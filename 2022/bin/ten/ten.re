type instruction =
    | AddX
    | LoadPreX(int)
    | Noop

let string_of_instruction = (inst:instruction):string => {
    switch(inst) {
        | AddX => "muAddX";
        | LoadPreX(x) => "muLoadPreX " ++ string_of_int(x);
        | Noop => "muNoop";
    }
};

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let assemble = (line:string):list(instruction) => {
    let parts = String.split_on_char(' ', line);
    switch(List.nth(parts, 0)) {
        | "addx" => {
            [LoadPreX(int_of_string(List.nth(parts, 1))), AddX];
        }
        | _ => [Noop]
    }
};

let execute = (icount:int, prg:array(instruction)):array(int) => {
    let states = Array.make(icount, 0);
    // not quite a micro-executor, but not a full one either
    let rec minix = (x:int, pre_x:option(int), pc:int):array(int) => {
        if(pc >= icount) {
            states
        } else {
            Array.set(states, pc, x);
            switch(Array.get(prg, pc)) {
                | LoadPreX(pre_x) => minix(x, Some(pre_x), pc + 1);
                | AddX => minix(x + Option.value(pre_x, ~default=0), None, pc + 1);
                | Noop => minix(x, pre_x, pc + 1); 
            }
        }
    }
    minix(1, None, 0);
}

let draw_screen = (screen:array(array(char)), states:array(int)):unit => {
    print_endline("in draw_screen: ");
    let rec inner_draw = (cycle:int, screen_x:int, screen_y:int) => {
        print_endline("drawing cycle: " ++ string_of_int(cycle) ++ " @ " ++ string_of_int(screen_x) ++ "," ++ string_of_int(screen_y));
        if(cycle >= 240) {
            ()
        } else if(Array.get(states, cycle) >= (screen_x - 1) && Array.get(states, cycle) <= (screen_x + 1)) {
            print_endline("dying before or after 57? cycle:" ++ string_of_int(cycle));
            print_endline("dying around " ++ string_of_int(screen_x) ++ "," ++ string_of_int(screen_y));
            let row = Array.get(screen, screen_y);
            print_endline("length of row: " ++ string_of_int(Array.length(row)));
            Array.set(Array.get(screen, screen_y), screen_x, 'X');
            print_endline("dying before or after 59?");
            if(screen_x >= 40) {
                inner_draw(cycle + 1, 0, screen_y + 1);
            } else {
                inner_draw(cycle + 1, screen_x + 1, screen_y);
            }
        } else {
            if(screen_x >= 40) {
                inner_draw(cycle, 0, screen_y + 1);
            } else {
                inner_draw(cycle + 1, screen_x + 1, screen_y);
            }
        }
    }
    inner_draw(0, 0, 0);
}

let display_screen = (screen:array(array(char))):unit => {
    print_endline("in display_screen: ");
    let rec inner_d_s = (screen_x:int, screen_y:int):unit => {
        if(screen_x >= 40) {
            print_endline("");
            inner_d_s(0, screen_y + 1)
        } else if (screen_y >= 6) {
            ()
        } else {
            print_char(Array.get(Array.get(screen, screen_y), screen_x));
            inner_d_s(screen_x + 1, screen_y);
        }
    }
    inner_d_s(0, 0);
}

let fh = open_in(Array.get(Sys.argv, 1));
let prg = Array.make(512, Noop);
let pc = ref(0);
Stream.iter((x) => {
    List.iter((i) => {
        Array.set(prg, pc^, i);
        pc := pc^ + 1;
    }, assemble(x));
}, iter_channel(fh));
let total_instructions = pc^;
pc := 0;
print_endline("total instruction count: " ++ string_of_int(total_instructions));
Array.iter((p) => {
    if(pc^ < total_instructions) {
        print_endline("instruction(" ++ string_of_int(pc^) ++ "): " ++ string_of_instruction(p));
        pc := pc^ + 1;
    }
}, prg);

let states = execute(total_instructions, prg);
let screen = Array.make_matrix(6, 40, '.');

if(Array.length(states) > 20) {
    let sum = ((Array.get(states, 19) * 20) +
               (Array.get(states, 59) * 60) +
               (Array.get(states, 99) * 100) +
               (Array.get(states, 139) * 140) +
               (Array.get(states, 179) * 180) +
               (Array.get(states, 219) * 220));
    print_endline("Sum: " ++ string_of_int(sum));
    draw_screen(screen, states);
    display_screen(screen);
} else {
    print_endline(string_of_int(Array.get(states, 1)));
    print_endline(string_of_int(Array.get(states, 2)));
    print_endline(string_of_int(Array.get(states, 3)));
}

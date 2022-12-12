type monkey = {
    monkey_no:int,
    items:array(int),
    operation:string,
    test:int,
    if_true:int,
    if_false:int,
};

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(String.trim(f))
            | exception End_of_file => None
        }
    })
}

let string_of_items = (src:array(int)):string => {
    String.concat(", ", List.of_seq(Seq.map(string_of_int, Array.to_seq(src))));
}

let items_of_string = (src:string):array(int) => {
    Array.of_seq(Seq.map((x) => { int_of_string(String.trim(x)) }, List.to_seq(String.split_on_char(',', src))));
}

let string_of_monkey = (~short:bool=false, m:monkey):string => {
    if(short) {
        "Monkey(" ++ string_of_int(m.monkey_no) ++ ")";
    } else {
        let ret = ["Monkey " ++ string_of_int(m.monkey_no) ++ ":",
                   "    Starting Items: " ++ string_of_items(m.items),
                   "    Operation: " ++ m.operation,
                   "    Test: divisible by " ++ string_of_int(m.test),
                   "        If true: throw to monkey " ++ string_of_int(m.if_true),
                   "        If false: throw to monkey " ++ string_of_int(m.if_false)];
        String.concat("\n", ret);
    }
};

let monkey_maker = (number, items, operation, test, if_t, if_f):monkey => {
    print_endline("dying here?");
    print_endline(number);
    print_endline(items);
    print_endline(operation);
    print_endline(test);
    print_endline(if_t);
    print_endline(if_f);
    {
        monkey_no: int_of_string(number),
        items: items_of_string(items),
        operation: String.trim(operation),
        test: int_of_string(test),
        if_true: int_of_string(if_t),
        if_false: int_of_string(if_f),
    };
};

let op_eval = (src:string, old:int):int => {
    let decode_operand = (operand:string):int => {
        switch(operand) {
            | "old" => old;
            | _ => int_of_string(operand);
        }
    };
    let parts = String.split_on_char(' ', src);
    let op0 = decode_operand(List.nth(parts, 2))
    let op = List.nth(parts, 3);
    let op1 = decode_operand(List.nth(parts, 4));
    switch(op) {
        | "+" => op0 + op1;
        | "*" => op0 * op1;
        | "-" => op0 - op1;
        | "/" => op0 / op1;
        | _ => old;
    }
}

let array_inserter = (dst:array(int), item:int):unit => {
    let rec find_dst_idx = (idx:int):int => {
        if(idx < Array.length(dst)) {
            let v = Array.get(dst, idx);
            if (v == -1) {
                idx;
            } else {
                find_dst_idx(idx + 1);
            }
        } else {
            -1;
        }
    };
    let dst_idx = find_dst_idx(0);
    if (dst_idx > -1) {
        Array.set(dst, dst_idx, item);
    }
};

let monkey_watch = (rnds:int, monkeys:array(monkey), part_two:bool):unit => {
    // it seems like actually attempting to update the
    // records for each round would be a waste... but
    // tracking what each monkey should be holdin is
    // p easy...
    let monkey_items = Array.make_matrix(10, 50, -1);
    let monkey_inspections = Array.make(10, 0);
    let monkey_inspector = (curm:monkey):unit => {
        let m_current_items = Array.get(monkey_items, curm.monkey_no);
        print_endline("Monkey " ++ string_of_int(curm.monkey_no));
        Array.iteri((idx, item) => {
            if(item != -1) {
                let op_result = op_eval(curm.operation, item);
                let new_worry = if (part_two) {
                    op_result / 3;
                } else {
                    op_result;
                };
                Array.set(monkey_inspections, curm.monkey_no, Array.get(monkey_inspections, curm.monkey_no) + 1);
                Array.set(m_current_items, idx, -1);
                print_endline("  Monkey inspects an item with a worry level of " ++ string_of_int(item));
                print_endline("    Worry level after operation is " ++ string_of_int(op_result));
                print_endline("    Monkey gets bored with item. Worry level divided by 3 to " ++ string_of_int(new_worry));
                if((new_worry mod curm.test) == 0) {
                    print_endline("    Current worry level is divisible by " ++ string_of_int(curm.test));
                    print_endline("    Item with worry level " ++ string_of_int(new_worry) ++ " is thrown to monkey " ++ string_of_int(curm.if_true));
                    let dst_monkey = Array.get(monkey_items, curm.if_true);
                    array_inserter(dst_monkey, new_worry);
                } else {
                    print_endline("    Current worry level is not divisible by " ++ string_of_int(curm.test));
                    print_endline("    Item with worry level " ++ string_of_int(new_worry) ++ " is thrown to monkey " ++ string_of_int(curm.if_false));
                    let dst_monkey = Array.get(monkey_items, curm.if_false);
                    array_inserter(dst_monkey, new_worry);
                }
            }
        }, m_current_items);
    };
    let rec watcher = (cur_rnd:int):unit => {
        Array.iter((m) => {
            let m_i = Array.get(monkey_items, m.monkey_no);
            if(Array.get(m_i, 0) != -1) {
                monkey_inspector(m); 
            }
        }, monkeys);
        print_endline("monkey inspections:")
        Array.iteri((idx, watches) => {
            print_endline("Monkey " ++ string_of_int(idx) ++ ": " ++ string_of_int(watches)); 
        }, monkey_inspections);
        if(cur_rnd < rnds) {
            watcher(cur_rnd + 1);
        }
    };
    // preload the starting items
    Array.iter((m) => {
        let dst_items = Array.get(monkey_items, m.monkey_no);
        Array.blit(m.items, 0, dst_items, 0, Array.length(m.items));
    }, monkeys);
    watcher(0);
};

let fh = open_in(Array.get(Sys.argv, 1));
let monkeys = ref([]);
let curmonkey = ref("");
let curitems = ref("");
let curop = ref("");
let curtest = ref("");
let curtrue = ref("");
let curfalse = ref("");
Stream.iter((x) => {
    print_endline("x: " ++ x);
    let parts = String.split_on_char(' ', x);
    switch(List.nth(parts, 0)) {
        | "Monkey" => {
            let monkeynum = List.nth(parts, 1);
            curmonkey := String.sub(monkeynum, 0, String.index(monkeynum, ':'))
        }
        | "Starting" => {
            let new_parts = String.split_on_char(':', x);
            curitems := List.nth(new_parts, 1);
        }
        | "Operation:" => {
            let new_parts = String.split_on_char(':', x);
            curop := List.nth(new_parts, 1);
            print_endline("current operation: " ++ curop^);
        }
        | "Test:" => {
            curtest := List.nth(parts, 3);
        }
        | "If" when String.starts_with(~prefix="If true:", x) => {
            let endpos = String.rindex(x, ' ');
            let len = String.length(x) - endpos;
            curtrue := String.trim(String.sub(x, endpos, len));
        }
        | "If" => {
            let endpos = String.rindex(x, ' ');
            let len = String.length(x) - endpos;
            curfalse := String.trim(String.sub(x, endpos, len));
        }
        | "" => {
            // return to monke
            monkeys := List.cons(monkey_maker(curmonkey^, curitems^, curop^, curtest^, curtrue^, curfalse^), monkeys^);
        }
        | _ => ()
    }
}, iter_channel(fh));
monkeys := List.cons(monkey_maker(curmonkey^, curitems^, curop^, curtest^, curtrue^, curfalse^), monkeys^);
List.iter((x) => {
    print_endline(string_of_monkey(x));
}, monkeys^);
let eval_monkeys = Array.of_list(List.rev(monkeys^));
print_endline("part one:");
monkey_watch(19, eval_monkeys, false);
print_endline("part two:");
monkey_watch(9999, eval_monkeys, true);


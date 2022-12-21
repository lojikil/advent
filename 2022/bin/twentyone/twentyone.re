type value = 
    | Concrete(int)
    | Symbolic(value, string, value)
    | Symbol

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
};

let string_of_value = (src:value):string => {
    let rec walker = (start:value):string => {
        switch(start) {
            | Concrete(n) => {
                string_of_int(n);
            }
            | Symbolic(r, op, l) => {
                "(" ++ walker(r) ++ " " ++ op ++ " " ++ walker(l) ++ ")"
            }
            | Symbol => "$$$";
        }
    }
    walker(src);
}

// I was thinking of a few different strategies here
//
// . I could turn this into a Shunting Yard and use a pushdown automaton
// . I could turn this into a true compiler and just compile things as we get terminating terms
// . I could go DFS and then build up the value
//
// I'm going to try the naieve approach and just recurse down the tree of evaluations
// and see what I get; probably overflown stack, but still

let op_eval = (tbl, starting_node:string):int => {
    // cache results, because I suspect the actual
    // will have several things that are looked up over
    // and over...
    let cache = Hashtbl.create(~random=false, 10000);
    let rec runner = (name:string, src:string):int => {
        let parts = String.split_on_char(' ', src);
        if (Hashtbl.mem(cache, name)) {
            Hashtbl.find(cache, name);
        } else {
            switch(List.length(parts)) {
                | 1 => {
                    let v = int_of_string(src);
                    Hashtbl.add(cache, name, v);
                    v;
                }
                | n when n >= 3 => {
                    let rhs = List.nth(parts, 0);
                    let rhv = ref(0);
                    let lhs = List.nth(parts, 2);
                    let lhv = ref(0);
                    let op = List.nth(parts, 1);

                    // potential optimization: we could preload the
                    // cache with values that are integers...

                    if(Hashtbl.mem(cache, rhs)) {
                        rhv := Hashtbl.find(cache, rhs); 
                    } else {
                        rhv := runner(rhs, Hashtbl.find(tbl, rhs));
                        Hashtbl.add(cache, rhs, rhv^);
                    }

                    if(Hashtbl.mem(cache, lhs)) {
                        lhv := Hashtbl.find(cache, lhs); 
                    } else {
                        lhv := runner(lhs, Hashtbl.find(tbl, lhs));
                        Hashtbl.add(cache, lhs, lhv^);
                    }

                    switch(op) {
                        | "+" => {
                            let v = rhv^ + lhv^;
                            Hashtbl.add(cache, name, v);
                            v;
                        }
                        | "*" => {
                            let v = rhv^ * lhv^;
                            Hashtbl.add(cache, name, v);
                            v;
                        }
                        | "/" => {
                            // I really hope we don't have to worry about Divide By Zero...
                            let v = rhv^ / lhv^;
                            Hashtbl.add(cache, name, v);
                            v;
                        }
                        | "-" => {
                            let v = rhv^ - lhv^;
                            Hashtbl.add(cache, name, v);
                            v;
                        }
                        | _ => 0;
                    }
                }
                | _ => 0;
            }
        }
    };
    let start = Hashtbl.find(tbl, starting_node);
    runner(starting_node, start);
};

let symbolicate = (tbl) => {
    // cache results, because I suspect the actual
    // will have several things that are looked up over
    // and over...
    let scache = Hashtbl.create(~random=false, 10000);
    let rec runner = (name:string, src:string):value => {
        let parts = String.split_on_char(' ', src);
        if (name == "humn") {
            Symbol;
        } else if (Hashtbl.mem(scache, name)) {
            Hashtbl.find(scache, name);
        } else {
            switch(List.length(parts)) {
                | 1 => {
                    let v = Concrete(int_of_string(src));
                    Hashtbl.add(scache, name, v);
                    v;
                }
                | n when n >= 3 => {
                    let rhs = List.nth(parts, 0);
                    let rhv = ref(Concrete(0));
                    let lhs = List.nth(parts, 2);
                    let lhv = ref(Concrete(0));
                    let op = List.nth(parts, 1);

                    // potential optimization: we could preload the
                    // scache with values that are integers...

                    if(Hashtbl.mem(scache, rhs)) {
                        rhv := Hashtbl.find(scache, rhs); 
                    } else {
                        rhv := runner(rhs, Hashtbl.find(tbl, rhs));
                        Hashtbl.add(scache, rhs, rhv^);
                    }

                    if(Hashtbl.mem(scache, lhs)) {
                        lhv := Hashtbl.find(scache, lhs); 
                    } else {
                        lhv := runner(lhs, Hashtbl.find(tbl, lhs));
                        Hashtbl.add(scache, lhs, lhv^);
                    }

                    switch(op) {
                        | "+" => {
                            switch((rhv^, lhv^)) {
                                | (Concrete(r), Concrete(l)) => {
                                    let v = r + l;
                                    Hashtbl.add(scache, name, Concrete(v));
                                    Concrete(v);
                                }
                                | (_, _) => {
                                    let res = Symbolic(rhv^, "+", lhv^);
                                    Hashtbl.add(scache, name, res);
                                    res;
                                }
                            }
                        }
                        | "*" => {
                            switch((rhv^, lhv^)) {
                                | (Concrete(r), Concrete(l)) => {
                                    let v = r * l;
                                    Hashtbl.add(scache, name, Concrete(v));
                                    Concrete(v);
                                }
                                | (_, _) => {
                                    let res = Symbolic(rhv^, "*", lhv^);
                                    Hashtbl.add(scache, name, res);
                                    res;
                                }
                            }
                        }
                        | "/" => {
                            switch((rhv^, lhv^)) {
                                | (Concrete(r), Concrete(l)) => {
                                    let v = r / l;
                                    Hashtbl.add(scache, name, Concrete(v));
                                    Concrete(v);
                                }
                                | (_, _) => {
                                    let res = Symbolic(rhv^, "/", lhv^);
                                    Hashtbl.add(scache, name, res);
                                    res;
                                }
                            }
                        }
                        | "-" => {
                            switch((rhv^, lhv^)) {
                                | (Concrete(r), Concrete(l)) => {
                                    let v = r - l;
                                    Hashtbl.add(scache, name, Concrete(v));
                                    Concrete(v);
                                }
                                | (_, _) => {
                                    let res = Symbolic(rhv^, "-", lhv^);
                                    Hashtbl.add(scache, name, res);
                                    res;
                                }
                            }
                        }
                        | _ => Concrete(0);
                    }
                }
                | _ => Concrete(0);
            }
        }
    };
    let start = Hashtbl.find(tbl, "root");
    let parts = String.split_on_char(' ', start)
    let rroot = Hashtbl.find(tbl, List.nth(parts, 0));
    let lroot = Hashtbl.find(tbl, List.nth(parts, 2));
    let rhs = runner(List.nth(parts, 0), rroot);
    let lhs = runner(List.nth(parts, 2), lroot);
    Symbolic(rhs, "=", lhs);
}

let concretize = (v:value):int => {
    // should pass in the root value here
    let zulu = Concrete(0);
    let unmix = (r:value, l:value):(value, value) => {
        switch((r, l)) {
            | (Concrete(_), _) => (r, l);
            | (_, Concrete(_)) => (l, r);
            | (_, _) => (r, l);
        }
    }
    /*
    let rec cement = (v:value, start:int) => {
        switch(v) {
            | Symbol => start;
            | Symbolic(r, op, l) => {
                // this would be easy, save for that we have to support "*" and "/",
                // which means we need to know which direction the reverse operation
                // needs to be applied
                //
                // [source]
                // ----
                // ($$$ * 3) = 150
                // ($$$) = (150 / 3)
                //
                // ($$$ / 4) = 150
                // ($$$) = (150 * 4)
                //
                // (3 * $$$) = 150
                // ($$$) = (150 / 3)
                //
                // (4 / $$$)
                // ($$$) = (150 * 4)
                // ----
                //
                // oh actually maybe not?

                switch
            }
            | Concrete(c) => c;
        }
    };
    */
    let (r, op, l) = switch(v) {
        | Symbolic(rr, oo, ll) => (rr, oo, ll);
        | Concrete(rr) => (Concrete(rr), "", zulu);
        | Symbol => (zulu, "S", zulu);
    };
    switch(op) {
        | "=" => {
            print_endline("we have symbolic values: " ++ string_of_value(r) ++ " and " ++ string_of_value(l));
            switch((r, l)) {
                | (Concrete(ir), _) => {
                    // call cement
                    ir;
                }
                | (_, Concrete(il)) => {
                    il
                }
                | (_, _) => {
                    10;
                }
            }
        }
        | _ => {
            print_endline("whoops, did you pass in the root node?");
            10;
        }
    };
}

let fh = open_in(Array.get(Sys.argv, 1));
let tbl = Hashtbl.create(~random=false, 10000);

Stream.iter((x) => {
    let parts = String.split_on_char(':', x);
    if(List.length(parts) > 1) {
        let name = String.trim(List.nth(parts, 0));
        let value = String.trim(List.nth(parts, 1));
        Hashtbl.add(tbl, name, value);
    }
}, iter_channel(fh));
/*Hashtbl.iter((monkey_name, monkey_value) => {
    print_endline(monkey_name ++ " => " ++ monkey_value);
}, tbl);*/
print_endline("part one: value of root: " ++ string_of_int(op_eval(tbl, "root")));
// for part two, we could just evaluate the entire tree symbolically; if we 
// have concrete values on both sides, we make a concrete value. If we have
// a symbolic value anywhere, we return a trace... and then we just need to
// "solve" for that value
let symb = symbolicate(tbl);
let conc = concretize(symb);
print_endline("part two (symbolic): " ++ string_of_value(symb));
print_endline("let's concretize: " ++ string_of_int(conc));

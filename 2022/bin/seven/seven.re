let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let is_numeric = (c:char):bool => {
    Char.code(c) >= 48 && Char.code(c) <= 57;
}

let make_paths = (cwd:string) => {
    List.fold_left((x, y) => { List.cons(List.hd(x) ++ y ++ "/", x) }, [""], String.split_on_char('/', cwd));
}

let fh = open_in(Array.get(Sys.argv, 1));
let cwd = ref("");
let tbl = Hashtbl.create(~random=false, 10000);
let sum = ref(0);
let sizes = ref([]);

Stream.iter((x) => {
    switch(x) {
        | d when String.starts_with(~prefix="$ cd", x) => {
            let parts = String.split_on_char(' ', d);
            let path = List.nth(parts, 2);
            print_endline("cwd: " ++ cwd^ ++ " path: " ++ path);
            sum := 0
            switch(path) {
                | ".." => {
                    // this state tracking would have been easy in a
                    // tail recursive lambda, but also it's straight
                    // forward here...
                    cwd := String.sub(cwd^, 0, String.rindex(cwd^, '/'))
                }
                | "/" => ()
                | _ => {
                    cwd := String.concat("/", [cwd^, path]);
                }
            }
        }
        // we don't _really_ need to do anything when we list
        // the current directory...
        | _ when String.starts_with(~prefix="$ ls", x) => ()
        // same here, for now we don't need to do much...
        | _ when String.starts_with(~prefix="dir", x) => ()
        | file_size when is_numeric(String.get(x, 0)) => {
            let fparts = String.split_on_char(' ', file_size);
            let size = int_of_string(List.nth(fparts, 0));
            List.iter((x) => {
                print_endline("adding " ++ string_of_int(size) ++ " to " ++ x);
                if(Hashtbl.mem(tbl, x)) {
                    Hashtbl.replace(tbl, x, Hashtbl.find(tbl, x) + size);
                } else {
                    Hashtbl.add(tbl, x, size);
                }
            }, make_paths(cwd^));
        }
        | _ => ()
    }
}, iter_channel(fh));
Hashtbl.iter((path, size) => {
    if(size <= 100000) {
        sum := size + sum^;
        print_endline(path ++ " is correctly sized");
    } else {
        print_endline(path ++ " is wrong sized: " ++ string_of_int(size));
    }
}, tbl);
print_endline("part one final sum: " ++ string_of_int(sum^));

let used = 70000000 - Hashtbl.find(tbl, "/");
let reqd = 30000000;

print_endline("Currently used: " ++ string_of_int(used));
print_endline("Needed: " ++ string_of_int(reqd));
print_endline("need to exceed: " ++ string_of_int(reqd - used));

Hashtbl.iter((_, size) => {
    if((used + size) >= reqd ) {
        print_endline("Adding: " ++ string_of_int(size));
        sizes := List.cons(size, sizes^);
    }
}, tbl);
sizes := List.sort(compare, sizes^);
print_endline("part two: " ++ string_of_int(List.hd(sizes^)));

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

let fh = open_in(Array.get(Sys.argv, 1));
let cwd = ref("");
let sum = ref(0);
let total = ref(0);
let vfs = ref([("$", 0)]);

Stream.iter((x) => {
    switch(x) {
        | d when String.starts_with(~prefix="$ cd", x) => {
            let parts = String.split_on_char(' ', d);
            let path = List.nth(parts, 2);
            print_endline("cwd: " ++ cwd^ ++ " path: " ++ path);
            switch(path) {
                | ".." => {
                    // this state tracking would have been easy in a
                    // tail recursive lambda, but also it's straight
                    // forward here...
                    vfs := List.cons((cwd^, sum^), vfs^);
                    cwd := String.sub(cwd^, 0, String.rindex(cwd^, '/'))
                    let old_sum = List.assoc(cwd^, vfs^);
                    vfs := List.cons((cwd^, sum^ + old_sum), List.remove_assoc(cwd^, vfs^));
                    sum := 0
                }
                | "/" => ()
                | _ => {
                    print_endline("should be here... path: " ++ path ++ " base '" ++ cwd^ ++ "'");
                    vfs := List.cons((cwd^, sum^), vfs^);
                    cwd := String.concat("/", [cwd^, path]);
                    print_endline("sanity check: " ++ cwd^);
                    sum := 0
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
            sum := sum^ + size;
            total := total^ + size;
        }
        | _ => ()
    }
}, iter_channel(fh));
vfs := List.cons((cwd^, sum^), vfs^)
sum := 0
List.iter((alist_entry) => {
    let (name, size) = alist_entry;
    print_string(name ++ " " ++ string_of_int(size));
    if(size <= 100000) {
        sum := size + sum^;
        print_endline(" correctly sized");
    } else {
        print_endline(" wrong sized");
    }
}, vfs^);
print_endline("Total: " ++ string_of_int(sum^));

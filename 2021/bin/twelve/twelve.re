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

let rec walk_nodes = (graph, start) => {
    // walk the graph, starting with node "start"
    // iterate through the connected nodes, and stopping if
    // a "small node" (lower case) is in the already viewed
    // list
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)
let graph_re = Str.regexp("\\([a-zA-Z]+\\)-\\([a-zA-Z]+\\)");
let node_graph = Hashtbl.create(~random=false, 100)
Stream.iter((x) => {
    if(Str.string_match(graph_re, x, 0)) {
        let n0 = Str.matched_group(1, x)
        let n1 = Str.matched_group(2, x)
        // the connections seem to imply that connections can
        // work both ways, and we don't need to specify direction
        Hashtbl.add(node_graph, n0, n1)
        Hashtbl.add(node_graph, n1, n0)
    } else {
        ()
    }
}, stream)

List.iter((x) => { print_endline(x) }, Hashtbl.find_all(node_graph, "start"))

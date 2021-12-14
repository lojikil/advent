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

let smallnode_re = Str.regexp("[a-z]+")

// so for part 2, what we need to do is scan the graph,
// and see if there is a small node that is the sole entrance
// to another subgraph, and, if so, allow it to be traversed
// twice

let walk_nodes = (graph, start) => {
    // walk the graph, starting with node "start"
    // iterate through the connected nodes, and stopping if
    // a "small node" (lower case) is in the already viewed
    // list; use a spaghetti stack of viewed nodes
    //
    // oh that's a good point: we don't actually care about having
    // a full stack of nodes, just the "small nodes" we've seen, since
    // those are the only ones we care about not revisiting
    //
    // ja, but we *do* need to walk paths, so...
    let rec inner_walk = (graph, start, seen, path) => {
        let new_nodes = Hashtbl.find_all(graph, start)
        List.iter((x) => {
            switch(x) {
                | c when Str.string_match(smallnode_re, c, 0) && List.mem(c, seen) => {
                    // we've already seen the small node, don't traverse
                    ()
                }
                | s when String.equal(s, "start") => {
                    ()
                }
                | e when String.equal(e, "end") => {
                    print_endline(List.fold_right((x, y) => { x ++ ", " ++ y }, List.append([x], path), ""))
                } 
                | _ => {
                    inner_walk(graph, x, List.append([x], seen), List.append([x], path))
                }
            }
        }, new_nodes)
    }
    inner_walk(graph, start, [], [])
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

walk_nodes(node_graph, "start")

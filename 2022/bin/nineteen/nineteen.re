type blueprint = {
    bp_id:int,
    orebot_cost:int,
    claybot_cost:int,
    obsbot_ore_cost:int,
    obsbot_clay_cost:int,
    geobot_ore_cost:int,
    geobot_obs_cost:int,
};

let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let blueprinter = (bp_numbers:list(string)):blueprint => {
    let bp = List.nth(bp_numbers, 0);
    {
        bp_id:int_of_string(String.sub(bp, 0, String.index(bp, ':'))),
        orebot_cost:int_of_string(List.nth(bp_numbers, 1)),
        claybot_cost:int_of_string(List.nth(bp_numbers, 2)),
        obsbot_ore_cost:int_of_string(List.nth(bp_numbers, 3)),
        obsbot_clay_cost:int_of_string(List.nth(bp_numbers, 4)),
        geobot_ore_cost:int_of_string(List.nth(bp_numbers, 5)),
        geobot_obs_cost:int_of_string(List.nth(bp_numbers, 6)),
    }
}

let string_of_blueprint = (b:blueprint):string => {
    let bp_id = "Blueprint " ++ string_of_int(b.bp_id) ++ ":";
    let orebot = "Each ore robot costs " ++ string_of_int(b.orebot_cost) ++ " ore.";
    let claybot = "Each clay robot costs " ++ string_of_int(b.claybot_cost) ++ " ore.";
    let obsbot = "Each obsidian robot costs " ++ string_of_int(b.obsbot_ore_cost) ++ " ore and " ++ string_of_int(b.obsbot_clay_cost) ++ " clay.";
    let geobot = "Each geode robot costs " ++ string_of_int(b.geobot_ore_cost) ++ " ore and " ++ string_of_int(b.geobot_obs_cost) ++ " obsidian."
    String.concat(" ", [bp_id, orebot, claybot, obsbot, geobot]);
}

let buy_geobot_p = (resources:array(int), cost_ore:int, cost_obs:int) => {
    let amt_ore = Array.get(resources, 0);
    let amt_obs = Array.get(resources, 2);
    (amt_ore >= cost_ore) && (amt_obs >= cost_obs)
};

let half_geobot_p = (resources:array(int), bots:array(int), cost_obs:int) => {
    let amt_obs = Array.get(resources, 2);
    if (amt_obs >= ((cost_obs / 2) + 1)) {
        true
    } else if (Array.get(bots, 2) >= (cost_obs / 2)) {
        // we will have enough next round
        true
    } else {
        false
    }
};

let buy_obsbot_p = (resources:array(int), cost_ore:int, cost_clay:int) => {
    let amt_ore = Array.get(resources, 0);
    let amt_clay = Array.get(resources, 1);
    (amt_ore >= cost_ore) && (amt_clay >= cost_clay)
};

let half_obsbot_p = (resources:array(int), bots:array(int), cost_clay:int) => {
    let amt_clay = Array.get(resources, 1);
    if (amt_clay >= ((cost_clay / 2) + 1)) {
        true
    } else if(Array.get(bots, 1) >= (cost_clay / 2)) {
        // will have enough next round
        true
    } else {
        false
    }
};

let buy_bot_p = (resources:array(int), cost_ore:int) => {
    (cost_ore <= Array.get(resources, 0)) 
};

let print_bots = (bots:array(int)):unit => {
    print_string("ore robots: " ++ string_of_int(Array.get(bots, 0)));
    print_string(" clay robots: " ++ string_of_int(Array.get(bots, 1)));
    print_string(" obsidian robots: " ++ string_of_int(Array.get(bots, 2)));
    print_endline(" geodode robots: " ++ string_of_int(Array.get(bots, 3)));
};

let print_resources = (resources:array(int)):unit => {
    print_string("ores: " ++ string_of_int(Array.get(resources, 0)));
    print_string(" clay: " ++ string_of_int(Array.get(resources, 1)));
    print_string(" obsidian: " ++ string_of_int(Array.get(resources, 2)));
    print_endline(" geododes: " ++ string_of_int(Array.get(resources, 3)));
};

let rec simulate = (rnd:int, resources:array(int), bots:array(int), b:blueprint) => {
    if(rnd == 0) {
        Array.get(resources, 3);
    } else {
        // first we spend if we can then we do work 
        print_endline("== Minute " ++ string_of_int(25 - rnd) ++ " ==");
        print_endline("blueprint: " ++ string_of_int(b.bp_id));
        print_resources(resources);
        print_bots(bots);
        let new_geo = if(buy_geobot_p(resources, b.geobot_ore_cost, b.geobot_obs_cost)) {
            print_endline("\tbuying geobot for " ++ string_of_int(b.geobot_ore_cost) ++ " " ++ string_of_int(b.geobot_obs_cost));
            Array.set(resources, 0, Array.get(resources, 0) - b.geobot_ore_cost);
            Array.set(resources, 2, Array.get(resources, 2) - b.geobot_obs_cost);
            Array.get(bots, 3) + 1;
        } else {
            Array.get(bots, 3);
        };
        // it's this relationship here that's causing my failure; there's basically
        // a check if we're close enough to a geo bot that we optimize for that,
        // but this basic stack is correct up to 20 minutes...
        let new_obs = if(half_geobot_p(resources, bots, b.geobot_obs_cost)) {
            // if we are half way or greater to a geobot in one resource, delay
            // this purchase
            Array.get(bots, 2);
        } else  if(buy_obsbot_p(resources, b.obsbot_ore_cost, b.obsbot_clay_cost)) {
            print_endline("\tbuying obsbot for " ++ string_of_int(b.obsbot_ore_cost) ++ " " ++ string_of_int(b.obsbot_clay_cost));
            Array.set(resources, 0, Array.get(resources, 0) - b.obsbot_ore_cost);
            Array.set(resources, 1, Array.get(resources, 1) - b.obsbot_clay_cost);
            Array.get(bots, 2) + 1;
        } else {
            Array.get(bots, 2);
        };
        let new_clay = if(half_obsbot_p(resources, bots, b.obsbot_clay_cost)) {
            Array.get(bots, 1); 
        } else if(buy_bot_p(resources, b.claybot_cost)) {
            print_endline("\tbuying claybot for " ++ string_of_int(b.claybot_cost));
            Array.set(resources, 0, Array.get(resources, 0) - b.claybot_cost);
            Array.get(bots, 1) + 1; 
        } else {
            Array.get(bots, 1);
        };
        let new_ore = if(buy_bot_p(resources, b.orebot_cost)) {
            print_endline("\tbuying an orebot for " ++ string_of_int(b.orebot_cost));
            Array.set(resources, 0, Array.get(resources, 0) - b.orebot_cost);
            Array.get(bots, 0) + 1;
        } else {
            Array.get(bots, 0);
        };
        Array.iteri((botid, botcount) => {
            let cur = Array.get(resources, botid);
            print_endline("\t\t\tupdating " ++ string_of_int(botid) ++ " to " ++ string_of_int(cur + botcount));
            Array.set(resources, botid, cur + botcount);
        }, bots);
        simulate(rnd - 1, resources, [|new_ore, new_clay, new_obs, new_geo|], b);
    }
};

let fh = open_in(Array.get(Sys.argv, 1));
let blueprints = ref([]);
Stream.iter((x) => {
    let parts = String.split_on_char(' ', x);
    let costs = [List.nth(parts, 1), List.nth(parts, 6), List.nth(parts, 12),
                 List.nth(parts, 18), List.nth(parts, 21), List.nth(parts, 27),
                 List.nth(parts, 30)];
    print_endline("Number pulls: " ++ String.concat(" ", costs));
    let bp = blueprinter(costs)
    print_endline("Testing round trip");
    print_endline(string_of_blueprint(bp));
    blueprints := List.cons(bp, blueprints^);
}, iter_channel(fh));

List.iter((b) => {
    let res = simulate(24, [|0, 0, 0, 0|], [|1, 0, 0, 0|], b);
    print_endline("blueprint resulted in: " ++ string_of_int(res) ++ " with quality ID: " ++ string_of_int(res * b.bp_id));
}, blueprints^);

import sys
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(1)

with open(sys.argv[1]) as fh:
    # pull the list of seeds first
    raw_seeds = fh.readline().strip()
    seeds = [int(x) for x in raw_seeds[7:].split(' ')]
    print("seeds: ", seeds)

    # ok, next we will have blanks to separate sections, then
    # section declarations, then maps
    section = ""
    mappings = {}
    for line in fh:
        line = line.strip()
        if line == "":
            continue
        elif "map" in line:
            section = line.split(' ')[0]
            mappings[section] = {}
            mappings[section]["source"] = []
            mappings[section]["source-end"] = []
            mappings[section]["destination"] = []
            mappings[section]["destination-end"] = []
            mappings[section]["run-length"] = []
        else:
            destination, source, rl = [int(x) for x in line.split(' ')]
            mappings[section]["source"].append(source)
            mappings[section]["source-end"].append(source + rl)
            mappings[section]["destination"].append(destination)
            mappings[section]["destination-end"].append(destination + rl)
            mappings[section]["run-length"].append(rl)

    print(mappings)

    sections = [ "seed-to-soil", "soil-to-fertilizer",
        "fertilizer-to-water", "water-to-light",
        "light-to-temperature", "temperature-to-humidity",
        "humidity-to-location"]
    new_seeds = []
    # we honestly could have done these in order above...
    for seed in seeds:
        # we need to transform the seed through these sections:
        # seed-to-soil map:
        # soil-to-fertilizer map:
        # fertilizer-to-water map:
        # water-to-light map:
        # light-to-temperature map:
        # temperature-to-humidity map:
        # humidity-to-location map:
        print("starting seed:", seed)
        for section in sections:
            print("\tseed: ", seed)
            print("\tmappings:", section,":", mappings[section])
            for source_idx in range(0, len(mappings[section]["source"])):
                source = mappings[section]["source"][source_idx]
                source_end = mappings[section]["source-end"][source_idx]
                if seed >= source and seed < source_end:
                    delta = seed - source
                    nseed = mappings[section]["destination"][source_idx] + delta
                    print("\t\tmapping:", seed, delta, nseed)
                    seed = nseed
                    break
        new_seeds.append(seed)
    print(new_seeds)
    print(min(new_seeds))

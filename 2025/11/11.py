import sys
import itertools
import re
import copy

if len(sys.argv) != 2:
    print("usage: 11.py [file]")
    sys.exit(0)

def part_one(t):
    workq = ["you"]
    out = 0

    if "you" not in t:
        return 0

    # really interesting optimization:
    # instead of a `seen` set, we could have a
    # `seen_out` set, so that we don't even add it to
    # the queue again if we know it has an "out"
    while len(workq) > 0:
        n = workq.pop()
        tmp = t[n]
        for j in tmp:
            if j == "out":
                out += 1
            else:
                workq.append(j)
    return out

def part_two(rack):
    workq = [("svr", ["svr"])]
    out = 0
    seen = set()

    if "svr" not in rack:
        return 0

    # really interesting optimization:
    # instead of a `seen` set, we could have a
    # `seen_out` set, so that we don't even add it to
    # the queue again if we know it has an "out"
    while len(workq) > 0:
        name, path = workq[0]
        workq = workq[1:]
        nodes = rack[name]
        seen.add(name)
        #print(f"p2: at {name}")
        for j in nodes:
            #print(f"in nodes, {j}")
            if j == "out":
                #print(f"made it to out from {name}")
                #print(path)
                if "dac" in path and "fft" in path:
                    print("p2: made it to out")
                    out += 1
            elif j not in seen:
                path.append(j)
                workq.append((j, copy.copy(path)))
    return out

def invert_rack(rack):
    # I think for part 2 we actually need to invert the server
    # tree and go from "out" back to "svr" via "fft" and "dac"
    #
    # it also could be interesting to change how paths work, so
    # we can amortize walking down a path once and then know if it
    # passes through those two
    return None

def part_two_inverse(rack, inverse_rack):
    workq = ["out"]
    return 0

with open(sys.argv[1]) as fh:
    tree = {}
    inverse_rack = {}
    for line in fh:
        leaf = line.strip().split(' ')
        tree[leaf[0][0:-1]] = leaf[1:]
        for l in leaf[1:]:
            if l in inverse_rack:
                inverse_rack[l].append(leaf[0][0:-1])
            else:
                inverse_rack[l] = [leaf[0][0:-1]]

    print("p1:", part_one(tree))
    #print("p2:", part_two(tree))
    print("p2:", part_two_inverse(tree, inverse_rack))

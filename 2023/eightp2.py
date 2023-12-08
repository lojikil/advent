import sys
import collections
import functools
import math

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

with open(sys.argv[1]) as fh:
    instructions = fh.readline().strip()
    nodes = {}
    starting_nodes = []
    for line in fh:
        line = line.strip()
        if line == "" or len(line) == 1:
            continue
        print("line: ", line, len(line))
        node, rchildren = line.split(' = ')
        if node.endswith("A"):
            starting_nodes.append(node)
        left, right = rchildren.replace('(','').replace(')', '').split(', ')
        nodes[node] = (left, right)
    node = "AAA"
    nodep = 0
    count = 0
    p_nodes = starting_nodes.copy()
    counts = []
    print("starting nodes:", starting_nodes)
    while True:
        n_nodes = []
        cnt_z = 0
        if nodep >= len(instructions):
            nodep = 0
        for n in p_nodes:
            tree = nodes[n]
            if instructions[nodep] == "L":
                node = tree[0]
            else:
                node = tree[1]
            print("node:", node, "z?", node.endswith("Z"))
            if node.endswith("Z"):
                counts.append(count + 1)
                cnt_z += 1
                print(cnt_z, len(starting_nodes))
            else:
                n_nodes.append(node)

        p_nodes = n_nodes
        print(count, n_nodes)
        count += 1
        nodep += 1
        if cnt_z == len(starting_nodes) or len(p_nodes) == 0:
            break
    print(count)
    print(counts)
    print(math.lcm(*counts))

import sys
import collections
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

with open(sys.argv[1]) as fh:
    instructions = fh.readline().strip()
    nodes = {}
    for line in fh:
        line = line.strip()
        if line == "" or len(line) == 1:
            continue
        print("line: ", line, len(line))
        node, rchildren = line.split(' = ')
        left, right = rchildren.replace('(','').replace(')', '').split(', ')
        nodes[node] = (left, right)
    node = "AAA"
    nodep = 0
    count = 0
    while node != "ZZZ":
        tree = nodes[node]
        if nodep >= len(instructions):
            nodep = 0
        if instructions[nodep] == "L":
            node = tree[0]
        else:
            node = tree[1]
        count += 1
        nodep += 1
    print(count)

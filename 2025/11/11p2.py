import networkx as nx
import sys

if len(sys.argv) != 2:
    print("usage: 11.py [file]")
    sys.exit(0)

with open(sys.argv[1]) as fh:
    rack = nx.DiGraph()
    for line in fh:
        leaves = line.strip().partition(': ')
        for l in leaves[2].split(' '):
            rack.add_edge(leaves[0], l)

    paths = list(nx.all_simple_paths(rack, "svr", "out"))
    total = 0
    for path in paths:
        if "dac" in path and "fft" in path:
            total += 1
    print("p2:", total)

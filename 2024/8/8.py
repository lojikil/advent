import sys
import collections
import itertools

keygrp = lambda x: x[2]

def find_antennae(amap):
    ret = []
    max_y = len(amap)
    max_x = len(amap[0])
    for y in range(0, max_y):
        for x in range(0, max_x):
            if amap[y][x] != '.':
                print(f"{x}, {y}, {amap[y][x]}")
                ret.append((x, y, amap[y][x]))

    return [(x[0], list(x[1])) for x in itertools.groupby(sorted(ret, key=keygrp), key=keygrp)]

def clamped_add(p, max_x, max_y):
    x, y = p

    if x < 0 or x >= max_x:
        return None
    elif y < 0 or y >= max_y:
        return None
    else:
        return p

def part1(amap, ants):

    ret = set()

    max_x = len(amap[0])
    max_y = len(amap)

    for antgrp in ants:
        grp, locs = antgrp
        print(f"group {grp}")
        print("location pairs:", list(itertools.combinations(locs, r=2)))
        for pair in itertools.combinations(locs, r=2):
            print("pair:", pair)
            p0, p1 = pair
            dx, dy = p1[0] - p0[0], p1[1] - p0[1]
            tp0 = clamped_add((p0[0] - dx, p0[1] - dy), max_x, max_y)
            tp1 = clamped_add((p1[0] + dx, p1[1] + dy), max_x, max_y)

            if tp0 is not None:
                ret.add(tp0)
            if tp1 is not None:
                ret.add(tp1)

    print(ret)

    return len(ret)

def part2(amap, ants):
    ret = set()

    max_x = len(amap[0])
    max_y = len(amap)

    for antgrp in ants:
        grp, locs = antgrp
        print(f"group {grp}")
        print("location pairs:", list(itertools.combinations(locs, r=2)))
        for pair in itertools.combinations(locs, r=2):
            print("pair:", pair)
            p0, p1 = pair
            dx, dy = p1[0] - p0[0], p1[1] - p0[1]

            ret.add(p0)
            ret.add(p1)

            tp0 = clamped_add((p0[0] - dx, p0[1] - dy), max_x, max_y)
            while tp0 is not None:
                ret.add(tp0)
                tp0 = clamped_add((tp0[0] - dx, tp0[1] - dy), max_x, max_y)

            tp1 = clamped_add((p1[0] + dx, p1[1] + dy), max_x, max_y)
            while tp1 is not None:
                ret.add(tp1)
                tp1 = clamped_add((tp1[0] + dx, tp1[1] + dy), max_x, max_y)

    print(ret)

    return len(ret)

with open(sys.argv[1]) as fh:
    antmap = []

    for line in fh:
        antmap.append(line.strip())

    antennae = find_antennae(antmap)

    print(antennae)

    ret1 = part1(antmap, antennae)
    ret2 = part2(antmap, antennae)

    print("part1:", ret1)
    print("part2:", ret2)

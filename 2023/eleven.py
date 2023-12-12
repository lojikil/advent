import sys
import collections
import functools
from dataclasses import dataclass

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

def transpose(mat):
    newmat = []
    for idx in range(0, len(mat[0])):
        new_row = []
        for yidx in range(0, len(mat)):
            new_row.append(mat[yidx][idx])
        newmat.append(new_row)
    return newmat

def print_universe(u):
    for row in u:
        for r in row:
            print(r, end='')
        print("")

def clamped_probe(y, x, u):
    if y < 0:
        y = 0
    elif y >= len(u):
        y = len(u) - 1
    if x < 0:
        x = 0
    elif x >= len(u[0]):
        x = len(u[0]) - 1
    return (y, x)

def pathfinder(universe, glx, tglx):
    # we're biased towards target galaxies that
    # are "below" the original one, so I'm only
    # going to focus on left, down, right
    def yeqmxplb(start, end):
        y1, x1 = start
        y2, x2 = end
        irise = y2 - y1
        irun = x2 - x1
        if irun == 0:
            islope = irise
        else:
            islope = irise / irun
        return (islope, irise, irun)

    probe = glx
    (slope, rise, run) = yeqmxplb(probe, tglx)
    #print("slope of", glx, "=>", tglx, ":", slope)
    cnt = 0
    while True:
        #print(probe, tglx, slope, run, rise)
        if slope > 0.0:
            np = clamped_probe(probe[0] + 1, probe[1], universe)
        elif slope < 0.0:
            np = clamped_probe( probe[0] + 1, probe[1], universe)
        elif run == 0 and rise < 0:
            # x1 == x2
            np = clamped_probe(probe[0] - 1, probe[1], universe)
        elif run == 0 and rise > 0:
            np = clamped_probe(probe[0] + 1, probe[1], universe)
        elif rise == 0 and run < 0:
            np = clamped_probe(probe[0], probe[1] - 1, universe)
        elif rise == 0 and run > 0:
            np = clamped_probe(probe[0], probe[1] + 1, universe)
        probe = np
        cnt += 1
        (slope, rise, run) = yeqmxplb(probe, tglx)
        if probe == tglx:
            #print("probe == tglx", cnt)
            break
    return cnt

def part2(exs, gs):
    newgs = []
    for g in gs:
        y, x = g
        newy, newx = g
        for ex in exs:
            if type(ex) is RowExpand and y >= ex.expansion:
                print("exanding:", ex, "for:", g)
                #newy += (1_000_000 - 1)
                newy += 9
            elif type(ex) is ColumnExpand and x >= ex.expansion:
                print("exanding:", ex, "for:", g)
                #newx += (1_000_000 - 1)
                newx += 9
        print("expanded", g, "to", (newy, newx))
        newgs.append((newy, newx))

    distances = []
    paths = {}
    for g in newgs:
        for tg in newgs:
            if g == tg:
                continue
            elif (g, tg) in paths:
                continue
            elif (tg, g) in paths:
                continue
            y1, x1 = g
            y2, x2 = tg
            print("path for", g, "to", tg)
            d = abs(x2 - x1) + abs(y2 - y1)
            paths[(g, tg)] = d
            distances.append(d)
    print(len(distances))
    return sum(distances)

@dataclass
class Expand:
    expansion:int

@dataclass
class RowExpand(Expand):
    pass

@dataclass
class ColumnExpand(Expand):
    pass

universe = []
galaxies = []
galactic_paths = {}
expansions = []
with open(sys.argv[1]) as fh:
    cnt = 0
    for line in fh:
        line = line.strip()
        universe.append(line)
        if '#' not in line:
            expansions.append(RowExpand(cnt))
            universe.append(line)
        cnt += 1
    # columar expansion is going to be messy
    tempu = transpose(universe)
    lastu = []
    cnt = 0
    for row in tempu:
        lastu.append(row)
        if '#' not in row:
            expansions.append(ColumnExpand(cnt))
            lastu.append(row)
        cnt += 1
    universe = transpose(lastu)
    #print_universe(universe)
    cnt = 0
    for line in universe:
        if '#' in line:
            i = 0
            while True:
                try:
                    i = line.index('#', i)
                    galaxies.append((cnt, i))
                    i += 1
                except:
                    break
        cnt += 1
    #print(galaxies, len(galaxies))
    # we could pop galaxies off the list here
    # (pop off, galaxy)
    # but I'm just choosing to map all of them
    # and then test if we have already
    for galaxy in galaxies:
        for target_galaxy in galaxies:
            if galaxy == target_galaxy:
                continue
            elif (galaxy, target_galaxy) in galactic_paths:
                continue
            elif (target_galaxy, galaxy) in galactic_paths:
                continue
            p = pathfinder(universe, galaxy, target_galaxy)
            #print("path:", galaxy, target_galaxy, p)
            galactic_paths[(galaxy, target_galaxy)] = p
    isum = 0
    cnt = 0
    for path in galactic_paths.values():
        isum += path
        cnt += 1
    #print(isum, cnt)
    print(expansions)
    print(part2([], galaxies))
    # part 2
    print(part2(expansions, galaxies))

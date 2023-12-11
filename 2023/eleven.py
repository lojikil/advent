import sys
import collections
import functools

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
    print("slope of", glx, "=>", tglx, ":", slope)
    cnt = 0
    while True:
        print(probe, tglx, slope, run, rise)
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
            print("probe == tglx", cnt)
            break
    return cnt

universe = []
galaxies = []
galactic_paths = {}
with open(sys.argv[1]) as fh:
    for line in fh:
        line = line.strip()
        universe.append(line)
        if '#' not in line:
            universe.append(line)
    # columar expansion is going to be messy
    tempu = transpose(universe)
    lastu = []
    for row in tempu:
        lastu.append(row)
        if '#' not in row:
            lastu.append(row)
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
            print("path:", galaxy, target_galaxy, p)
            galactic_paths[(galaxy, target_galaxy)] = p
    sum = 0
    cnt = 0
    for path in galactic_paths.values():
        sum += path
        cnt += 1
    print(sum, cnt)

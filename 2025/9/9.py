import sys
import itertools
import functools
import copy
import shapely

def getheight(p, q):
    py = p[1]
    qy = q[1]
    return abs(py - qy) + 1

def getwidth(p, q):
    px = p[0]
    qx = q[0]
    return abs(px - qx) + 1

def getadjacents(d):
    # it's interesting, so we basically
    # need to build and adjacency map
    # and then return the pairs that are:
    #
    # . directly connected with you
    # . directly conneccted to your next neighbors
    am = {}
    l = len(d)
    for i in range(0, len(d)):
        p = d[i]
        am[p] = [d[i - 1], d[(i + 1) % l]]

    res = []
    for p in d:
        r = am[p]
        n = [r[0], r[1]]
        n.extend(am[n[0]])
        n.extend(am[n[1]])
        for m in n:
            res.append((p, m))

    return (res, am)

def find_hlines(point, polygon):
    # NOTE we need to make sure the lines "point towards"
    # the polygon...
    # oh, or we can just cast 4 rays...
    return None

def winding_number(points, polygon):
    # points is the two selected edges of our rectangle
    # polygon is the full original polygon...
    polypoints = list(polygon.keys())

    # ok so:
    # . find the lines that the ray will intersect (v/h)
    # . if any of the rays are 0, stop, it's outside
    for point in points:
        hlines = find_hlines(point, polygon)
        vlines = find_vlines(point, polygon)
        ray = copy.copy(point)

def explode_rect(comb):
    x0, y0 = comb[0]
    x1, y1 = comb[1]
    return shapely.Polygon([(x0, y0),
                            (x1, y0),
                            (x1, y1),
                            (x0, y1)])

def part1(d):
    maxa = 0
    maxp = None
    combs = list(itertools.combinations(d, 2))
    print(f"p1: number of tests: {len(combs)}")
    for comb in combs:
        h = getheight(comb[0], comb[1])
        w = getwidth(comb[0], comb[1])
        a = h * w
        if a > maxa:
            maxa = a
            maxp = comb
    print(f"p1: returning: area = {maxa}, pair = {maxp}")
    return (maxa, maxp)

def part2(d):
    maxa = 0
    maxp = None
    # ok, for part two, it's not arbitrary combinations,
    # but rather adjacent corners. so we need to pick
    # a counter party that is a neighbor of a neighbor
    # to work with
    # that's actually wrong, lol. What we can do is check
    # if all four corners of the rectangle are in the polygon
    #
    # there's an interesting idea: we can do the ray casting
    # method, but keep it simple: does the line any parts of
    # our proposed rectangle cross another point? Oh wait no,
    # it won't work as a cheap test, but we can still do the
    # same for ray casting:
    #
    # . take the two generated points of a proposed rectangle
    # . cast two rays from each:
    # .. if they cross an odd number of red tiles, it's outside
    # .. if they cross an even number of red tiles, it's inside
    # . can't be done cheaply, because of how the polygon is shaped ;____;
    # . which means we need to construct the full polygon and check it
    #
    # ... or just use a library
    poly = shapely.Polygon(d)
    combs = list(itertools.combinations(d, 2))
    print(f"p2: number of tests: {len(combs)}")
    for comb in combs:
        h = getheight(comb[0], comb[1])
        w = getwidth(comb[0], comb[1])
        a = h * w

        if a > maxa and poly.covers(explode_rect(comb)):
            maxa = a
            maxp = comb
    print(f"p2: returning: area = {maxa}, pair = {maxp}")
    return (maxa, maxp)

if len(sys.argv) != 2:
    print("usage: 9.py [input]")
    sys.exit(1)

with open(sys.argv[1]) as fh:
    data = []
    for line in fh:
        data.append(tuple([int(x) for x in line.strip().split(',')]))

    print(part1(data))
    print(part2(data))

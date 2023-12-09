import sys
import collections
import functools

def delta_line(l):
    zc = 0
    ret = []
    for i in range(0, len(l) - 1):
        v = l[i + 1] - l[i]
        ret.append(v)
        if v == 0:
            zc += 1
    if zc == len(l) - 1:
        return ret
    else:
        return (ret, delta_line(ret))

def flatten(l):
    tmp = l
    ret = []
    while True:
        if type(tmp) == tuple:
            ret.append(tmp[0])
            tmp = tmp[1]
        else:
            ret.append(tmp)
            break
    return ret

def reducer(x, y):
    return x + y[-1]

def deducer(x, y):
    return y[0] - x

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

with open(sys.argv[1]) as fh:
    total = 0
    histt = 0
    for line in fh:
        vals = [int(x) for x in line.strip().split(' ')]
        triangles = flatten((vals, delta_line(vals)))
        rtriangle = triangles.copy()
        rtriangle.reverse()
        total += functools.reduce(reducer, triangles, 0)
        histt += functools.reduce(deducer, rtriangle, 0)
    print(total)
    print(histt)

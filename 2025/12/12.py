import sys
import functools
import collections

def mul(a):
    return functools.reduce(lambda x, y: x * y, a)

if len(sys.argv) != 2:
    print("usage: 12.py [file]")
    sys.exit(1)

with open(sys.argv[1]) as fh:
    raw = fh.read()
    parts = raw.split("\n\n")
    shapes = {}
    regions = []
    raw_region = ""
    for part in parts:
        if 'x' in part:
            raw_region = part
        else:
            tmp = part.partition(":\n")
            shapes[int(tmp[0])] = dict(raw_image=tmp[2],
                                       area=collections.Counter(tmp[2]))

    regions = raw_region.split('\n')
    accum = 0
    for region in regions:
        if len(region) == 0:
            continue
        raw_size, _, raw_makeup = region.partition(": ")
        size = mul([int(x) for x in raw_size.split('x')])
        makeup = [int(x) for x in raw_makeup.split(' ')]
        mas = sum([makeup[i] * shapes[i]["area"]['#'] for i in range(0, len(makeup))])
        ras = [makeup[i] * shapes[i]["area"]['#'] for i in range(0, len(makeup))]
        #print(f"p1: raw areas for the regon: {ras}")
        #print(f"p1: size is {size} and mas is {mas}")
        if mas < size:
            accum += 1
        #print(f"looking at region of size {size} and makeup {makeup}")
    print(f"p1: {accum}")

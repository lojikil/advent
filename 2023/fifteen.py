import sys
import collections
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

def hashline(l):
    cv = 0
    for x in l:
        cv += ord(x)
        #print("CV:", cv)
        cv *= 17
        #print("CV:", cv)
        cv %= 256
        #print("CV:", cv)
        #print("----")
    return cv

def part1(d):
    return sum([hashline(x) for x in d])

def part2(data):
    boxes = {}
    for d in data:
        print(d)
        if '=' in d:
            ln = d[:d.index('=')]
            op = d[d.index('=') + 1:]
        elif '-' in d:
            ln = d[:-1]
            op = '-'
        k = hashline(ln)
        print(ln, op, k)
        if k not in boxes:
            boxes[k] = collections.OrderedDict()

        if ln in boxes[k] and op == '-':
            del(boxes[k][ln])
        elif op != '-':
            boxes[k][ln] = int(op)
    powers = []
    print(boxes)
    for k, v in boxes.items():
        idx = 1
        for kk, f in v.items():
            print(kk, ":", (k + 1), "*", idx, "*", f)
            powers.append((k + 1) * idx * f)
            idx += 1
    print(powers)
    print(sum(powers))
    return sum(powers)
with open(sys.argv[1]) as fh:
    for line in fh:
        print(part1(line.strip().split(',')))
        print(part2(line.strip().split(',')))

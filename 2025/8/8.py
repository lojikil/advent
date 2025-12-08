import math
import queue
import itertools
import functools

def mul(c):
    return functools.reduce(lambda x, y: x * y, c)

QLEN = 1000

class CircuitPriority:
    def __init__(self, points):
        self.priority = math.dist(points[0], points[1])
        self.item = points

    def __lt__(self, o):
        return self.priority < o.priority

    def __le__(self, o):
        return self.priority <= o.priority

    def __str__(self):
        return f"{self.points[0]} <=> {self.points[1]} ({self.priority})"

    def __repr__(self):
        return f"CircuitPriority({self.item[0]} <=> {self.item[1]} ({self.priority}))"

class MyPQ:
    def __init__(self):
        self.queue = {}
        self.keys = []

    def put(self, i):
        if i.priority in self.queue:
            self.queue.append(i)
        else:
            self.keys.append(i.priority)
            self.queue[i.priority] = [i]

    def nsmallest(self, n):
        sk = sorted(self.keys)
        ret = []
        for i in sk:
            if len(self.queue[i]) > 1:
                ret.extend(self.queue[i])
            else:
                ret.append(self.queue[i][0])
        return ret[0:n]


with open('input-test.txt') as fh:
    all_points = []
    for line in fh:
        points = [int(x) for x in line.strip().split(',')]
        all_points.append(points)

    combs = itertools.combinations(all_points, 2)
    pq = MyPQ()

    for c in combs:
        pq.put(CircuitPriority(c))

    #shorts = pq.nsmallest(12)
    shorts = pq.nsmallest(11)

    buckets = []
    for conn in shorts:
        c = [tuple(x) for x in conn.item]
        print(c)
        if len(buckets) == 0:
            buckets.append(set(c))
        else:
            found = False
            for bucket in buckets:
                if c[0] in bucket:
                    found = True
                    bucket.add(c[1])
                    break
                elif c[1] in bucket:
                    found = True
                    bucket.add(c[0])
                    break
            if not found:
                buckets.append(set(c))

    sb = []
    accum = 1
    for bucket in buckets:
        sb.append(len(bucket))

    sb = sorted(sb, reverse=True)
    print(sb)
    accum = mul(sb[0:3])

    print(f"p1:{accum}")

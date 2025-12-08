import itertools
import math

with open('input-test.txt') as fh:
    all_points = []
    for line in fh:
        points = tuple([int(x) for x in line.strip().split(',')])
        all_points.append(points)

results = {}

combs = itertools.product(all_points, all_points)

for a, b in combs:
    if a == b:
        continue

    dist = math.dist(a, b)

    if a in results:
        if dist < results[a][0]:
            results[a] = (dist, b)
    else:
        results[a] = (dist, b)

print(results)

accum = 1



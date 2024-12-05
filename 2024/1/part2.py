import sys
import collections

with open(sys.argv[1]) as fh:
    left = []
    right = []
    for line in fh:
        l, r = line.split('   ')
        left.append(int(l))
        right.append(int(r))
    diff = 0
    counts = collections.Counter(right)
    for v in left:
        if v in counts:
            j = counts[v]
            diff += (v * j)
    print(diff)


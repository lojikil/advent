import sys

with open(sys.argv[1]) as fh:
    left = []
    right = []
    for line in fh:
        l, r = line.split('   ')
        left.append(int(l))
        right.append(int(r))
    diff = 0
    left = sorted(left)
    right = sorted(right)
    for idx in range(0, len(left)):
        if left[idx] > right[idx]:
            diff += (left[idx] - right[idx])
        else:
            diff += (right[idx] - left[idx])
    print(diff)


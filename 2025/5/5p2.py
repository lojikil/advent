import intervals

bag = intervals.empty()

with open('input-full.txt') as fh:
    for line in fh.readlines():
        if '-' in line:
            s, e = [int(x) for x in line.split('-')]
            bag |= intervals.closed(s, e)

print("done, dumping intervals:")
#print(len(list(intervals.iterate(bag, incr=1))))
accum = 0
for i in bag._intervals:
    s = i._lower
    e = i._upper
    print(s, e)
    accum += ((e - s) + 1)

print("p2:", accum)

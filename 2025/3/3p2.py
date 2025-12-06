SLICE_LEN = 12

with open('input-test.txt') as fh:
    accum = 0
    for line in fh:
        line = line.strip()
        """
        lmax = 0
        for i in range(0, len(line)):
            f = line[i]
            subline = sorted(line[i+1:], reverse=True)
            if len(subline) < SLICE_LEN:
                break
            t = int(f + ''.join(subline[0:SLICE_LEN]), base=10)
            if t > lmax:
                lmax = t
        """
        subline = sorted(line, reverse=True)
        t = int(''.join(subline[0:SLICE_LEN]), base=10)
        #accum += lmax
        print(t)
        accum += t
    print(accum)

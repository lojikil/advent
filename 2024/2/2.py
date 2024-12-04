import sys

def safe_report(l):
    if type(l) is str:
        v = [int(a) for a in l.split(' ')]
    else:
        v = l

    d = False

    if v[0] > v[1]:
        d = "INC"
    else:
        d = "DEC"

    last = v[0]
    for h in v[1:]:
        delta = abs(last - h)
        if delta < 1 or delta > 3:
            return False
        if d == "DEC" and (last > h):
            return False
        elif d == "INC" and (last < h):
            return False
        last = h
    return True

def safe_report2(l):
    def int_report(o):
        print("testing:", o)
        d = False

        if o[0] > o[1]:
            d = "INC"
        else:
            d = "DEC"

        last = o[0]
        for idx in range(1, len(o)):
            h = o[idx]
            delta = abs(last - h)
            if delta < 1 or delta > 3:
                return False, idx
            if d == "DEC" and (last > h):
                return False, idx
            elif d == "INC" and (last < h):
                return False, idx
            last = h
        return True, idx

    v = [int(a) for a in l.split(' ')]

    safecnt = 0

    while safecnt <= 1:
        print("running:", v)
        ret, fail_idx = int_report(v)
        if ret:
            return True
        elif fail_idx == 1 and safecnt == 0:
            new_v = v.copy()
            del(new_v[0])
            vr, fi = int_report(new_v)
            if vr:
                return True
            safecnt += 1
        else:
            print(f"line {v} failed on {fail_idx}")
            safecnt += 1
        del(v[fail_idx])
        print("=====")

    return False

with open(sys.argv[1]) as fh:
    cnt = 0
    lines = []
    for line in fh:
        lines.append(line.strip())
        if safe_report(line.strip()):
            cnt += 1
    print("final count:", cnt)
    cnt = 0
    for line in lines:
        if safe_report2(line):
            cnt += 1
    print("part 2:", cnt)

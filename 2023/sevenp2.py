import sys
import collections
import functools

strength = "J23456789TQKA"
keyf = lambda x: strength.index(x)

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

hands = []
bids = []
data = {}
ordered_hands = []

def jokers_wild(clazz, counts):
    nojokers = counts["J"]
    print("jw: clazz:{0}, counts:{1}".format(clazz, counts))
    if clazz == 7:
        # it doesn't matter here, we have a five of a kind
        return clazz
    elif nojokers == 0:
        return clazz
    elif clazz == 6:
        return 7
    elif clazz == 5:
        return 7
    elif clazz == 4:
        return 6
    elif clazz == 3:
        if nojokers == 1:
            return 5
        return 6
    elif clazz == 2:
        return 4
    else:
        return 2

def classify(v):
    vals = [x[1] for x in v.most_common()]
    ret = 1
    if vals[0] == 5:
       ret = 7
    elif vals[0] == 4:
       ret = 6
    elif vals[0] == 3 and vals[1] == 2:
       ret = 5
    elif vals[0] == 3:
       ret = 4
    elif vals[0] == 2 and vals[1] == 2:
       ret = 3
    elif vals[0] == 2:
       ret = 2
    else:
       ret = 1
    print("ret", ret, "for v", v)
    return jokers_wild(ret, v)

def order(a, b):
    ca = classify(a)
    cb = classify(b)

    if ca > cb:
        return (a, b)
    elif ca < cb:
        return (b, a)
    else:
        elemsa = sorted([x[0] for x in ca.most_common()], key=keyf, reverse=True)
        elemsb = sorted([x[0] for x in cb.most_common()], key=keyf, reverse=True)
        for idx in range(0, len(elemsa)):
            sa = strength.index(elemsa[idx])
            sb = strength.index(elemsb[idx])
            print(sa, "<=>", sb)
            if sa > sb:
                return (a, b)
            elif sa < sb:
                return (b, a)
        return (a, b)

# spaceships for cards
def compare(oa, ob):
    a = collections.Counter(oa)
    b = collections.Counter(ob)
    ca = classify(a)
    cb = classify(b)

    print("compare", oa, ca, ob, cb)

    if ca > cb:
        return 1
    elif ca < cb:
        return -1
    else:
        print("in compare first else", oa, ob)
        #elemsa = [x[0] for x in a.most_common()]
        #elemsb = [x[0] for x in b.most_common()]
        elemsa = oa
        elemsb = ob

        #if ca == 1:
            # we have Highest Card, so we need to actually sort these first
            #elemsa = sorted(elemsa, key=keyf, reverse=True)
            #elemsb = sorted(elemsb, key=keyf, reverse=True)

        for idx in range(0, len(elemsa)):
            sa = strength.index(elemsa[idx])
            sb = strength.index(elemsb[idx])
            if sa > sb:
                return 1
            elif sa < sb:
                return -1
        return 0

def insert_order(l, e):
    if len(l) == 0:
        return [e]
    ret = []
    inserted = False
    for i in l:
        if inserted:
            ret.append(i)
            continue
        cmp = compare(i, e)
        print("{0} <=> {1} ... {2}".format(i, e, cmp))
        if cmp == -1:
            ret.append(e)
            ret.append(i)
            inserted = True
        else:
            ret.append(i)

    if not inserted:
        ret.append(e)
    return ret

with open(sys.argv[1]) as fh:
    for line in fh:
        rawhand, bid = line.strip().split(' ')
        cnt = collections.Counter(rawhand)
        hands.append(cnt)
        bids.append(bid)
        data[rawhand] = {}
        data[rawhand]["bid"] = bid
        data[rawhand]["counts"] = cnt
        data[rawhand]["class"] = classify(cnt)
        ordered_hands = insert_order(ordered_hands, rawhand)
    print(data)
    print(ordered_hands)
    ordered_hands.reverse()
    total = 0
    for idx in range(0, len(ordered_hands)):
        total += (int(data[ordered_hands[idx]]["bid"]) * (idx + 1))
    print(total)

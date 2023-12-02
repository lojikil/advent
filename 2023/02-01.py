import sys
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(1)

m = lambda x: functools.reduce(max, x, 0)

with open(sys.argv[1]) as fh:
    sum = 0
    power = 0
    for line in fh:
        game = line.split(':')
        plays = game[1]
        gameid = game[0][5:]
        print("gameid: ", gameid)
        cubes = plays.split(';')
        possible = True
        reds = []
        greens = []
        blues = []
        for rnd in cubes:
            r,g,b = 0,0,0
            pulls = rnd.split(',')
            for p in pulls:
                p = p.strip()
                print(p)
                if p.endswith("red"):
                    r = int(p.split(' ')[0])
                elif p.endswith("blue"):
                    b = int(p.split(' ')[0])
                elif p.endswith("green"):
                    g = int(p.split(' ')[0])
            print("\t round({0}): {1} {2} {3}".format(gameid, r, g, b))
            reds.append(r)
            greens.append(g)
            blues.append(b)
            if r <= 12 and g <= 13 and b <= 14 and possible:
                possible = True
            else:
                possible = False
        if not possible:
            print("round {0} is impossible".format(gameid))
        else:
            sum += int(gameid)
        power += (m(reds) * m(greens) * m(blues))
    print("total: {0}".format(sum))
    print("power: {0}".format(power))

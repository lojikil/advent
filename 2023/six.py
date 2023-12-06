import sys
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".fromat(sys.argv[0]))
    sys.exit(1)

with open(sys.argv[1]) as fh:
    race_time = [int(x) for x in fh.readline()[5:].strip().split(' ') if x != '']
    race_dist = [int(x) for x in fh.readline()[9:].strip().split(' ') if x != '']
    print(race_time,"\n", race_dist)
    wins = []
    for idx in range(0, len(race_time)):
        ct = race_time[idx]
        cd = race_dist[idx]
        lw = []
        for tt in range(0, ct + 1):
            if ((ct - tt) * tt) > cd:
                print(tt, " vs ", cd)
                lw.append(tt)
        wins.append(len(lw))
    print(wins)
    print(functools.reduce(lambda x, y: x * y, wins, 1))

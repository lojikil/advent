import sys

with open(sys.argv[1]) as fh:
    sum = 0
    for line in fh.readlines():
        line = line.strip()
        start = line.index(':')
        end = line.index('|')
        wins = set(line[start:end].split(' '))
        have = set(line[end:].split(' '))
        items = list(have & wins)
        cnt = 0
        for i in items:
            if i != '':
                cnt *= 2
                if cnt == 0:
                    cnt = 1
        sum += cnt
    print("total:", sum)

import sys

def cardwinp(card):
    start = card.index(':')
    end = card.index('|')
    wins = set(card[start:end].split(' '))
    have = set(card[end:].split(' '))
    items = list(have & wins)
    fitems = [x for x in items if x != '']
    return len(fitems)

def cardnumber(card):
    end = card.index(':')
    return int(card[4:end].strip())

with open(sys.argv[1]) as fh:
    lines = []
    data = {}
    for line in fh.readlines():
        line = line.strip()
        cardno = cardnumber(line)
        data[cardno] = 1
        lines.append(line)

    for line in lines:
        cardno = cardnumber(line)
        wins = cardwinp(line)
        for idx in range(0, wins):
            data[idx + cardno + 1] += data[cardno]
    print(data)
    print(sum(list(data.values())))

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
    for line in fh.readlines():
        line = line.strip()
        lines.append(line)

    workq = lines.copy()
    total = 0
    while len(workq) > 0:
        head = workq[0]
        workq = workq[1:]
        cardno = cardnumber(head)
        wins = cardwinp(head)
        total += wins
        for i in range(cardno + 1, cardno + 1 + wins):
            workq.append(lines[i - 1])
    print(total + len(lines))

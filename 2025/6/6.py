import re
import functools

white = re.compile(' +')

def mul(c):
    return functools.reduce(lambda x, y: x * y, c)

with open('input-full.txt') as fh:
    rows = []
    for line in fh:
        rows.append(white.sub(' ', line.strip()).split(' '))

    ops = rows[-1]
    rows = rows[0:-1]
    print(f"operations: {ops}\n{rows}")
    accum = 0

    for i in range(0, len(ops)):
        tmp = []
        for row in rows:
            tmp.append(int(row[i]))
        if ops[i] == '*':
            accum += mul(tmp)
        else:
            accum += sum(tmp)

    print(accum)

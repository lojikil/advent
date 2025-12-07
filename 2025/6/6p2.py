import re
import functools


# ok, here, we need to actually slice up the lines:
#
# . if a column is all spaces, then it's a delimiter
# . a column has one or more numbers in it, it's a number itself
# .. fill in all other spaces with zeros, process it into an int
white = re.compile(' +')
allwhite = re.compile('^ +$')

def mul(c):
    return functools.reduce(lambda x, y: x * y, c)

# rotate the matrix 90degrees to the left
def transpose(m):
    res = []
    l = len(m[0]) - 1
    for i in range(l, -1, -1):
        tmp = []
        for row in m:
            tmp.append(row[i])
        res.append(''.join(tmp))
    return res

def group_slices(m):
    groups = []
    tmp = []
    for n in m:
        if allwhite.match(n):
            groups.append(tmp)
            tmp = []
        else:
            tmp.append(int(n, base=10))
    groups.append(tmp)
    return groups

with open('input-full.txt') as fh:
    rows = []
    for line in fh:
        rows.append(line[0:-1])

    ops = list(reversed(white.sub(' ', rows[-1]).strip().split(' ')))
    rows = rows[0:-1]
    accum = 0
    nm = transpose(rows)
    operands = group_slices(nm)
    for i in range(0, len(ops)):
        op = ops[i]
        if op == '+':
            operand = operands[i]
            accum += sum(operand)
        elif op == '*':
            operand = operands[i]
            accum += mul(operand)
    print("p2:", accum)

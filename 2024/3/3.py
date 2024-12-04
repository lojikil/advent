import re
import sys

mulre = re.compile(r'mul\(\d{1,3},\d{1,3}\)')
p2re = re.compile(r"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)")

def santa_eval(e):
    fe = e.replace('mul(', '').replace(')', '').split(',')
    return int(fe[0]) * int(fe[1])

with open(sys.argv[1]) as fh:
    cnt = 0
    lines = []
    prg = []
    for line in fh:
        line = line.strip()
        exprs = mulre.findall(line)
        prg.extend(p2re.findall(line))
        for expr in exprs:
            cnt += santa_eval(expr)
    print("part1:", cnt)
    cnt = 0
    enabled = True
    for expr in prg:
        if expr.startswith('mul('):
            if enabled:
                cnt += santa_eval(expr)
        elif expr == "don't()":
            enabled = False
        elif expr == "do()":
            enabled = True
    print('part2:', cnt)

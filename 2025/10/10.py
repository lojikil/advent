import sys
import itertools
import re
import copy

line_parse = re.compile(r"(\[.*\]) (\(.*\)) (\{.*\})")

if len(sys.argv) != 2:
    print("usage: 10.py [file]")
    sys.exit(0)

def light2vec(light):
    res = []
    for i in light[1:-1]:
        if i == '.':
            res.append(0)
        elif i == '#':
            res.append(1)
    return res

def push_buttons(cur, comb):
    newp = copy.copy(cur)
    for c in comb:
        pushes = [int(x) for x in c[1:-1].split(',')]
        for push in pushes:
            newp[push] ^= 1
    return newp

def simulate_buttons(end_state, buttons):
    ves = light2vec(end_state)
    off = [0] * len(ves)
    c = itertools.count(1)
    minp = 10000
    for r in c:
        for comb in itertools.combinations(buttons, r=r):
            res = push_buttons(off, comb)
            if res == ves and len(comb) < minp:
                minp = len(comb)
                return minp
    return minp

def power_buttons(off, pushes):
    res = copy.copy(off)
    for push in pushes:
        buttons = [int(x) for x in push[1:-1].split(',')]
        for button in buttons:
            res[button] += 1
    return res

def simulate_power(joltages, buttons):
    end_state = [int(x) for x in joltages[1:-1].split(',')]
    off = [0] * len(end_state)
    minp = 10000
    for r in itertools.count(1):
        for comb in itertools.combinations_with_replacement(buttons, r):
            #print(f"p2: trying: {comb}")
            res = power_buttons(off, comb)
            if res == end_state and len(comb) < minp:
                print("p2: got one:", len(comb))
                return len(comb)

with open(sys.argv[1]) as fh:
    accum = 0
    data = []
    for line in fh:
        data.append(line.strip())

    for line in data:
        parts = line.split(' ')
        buttons = parts[0]
        joltages = parts[-1]
        presses = parts[1:-1]
        accum += simulate_buttons(buttons, presses)

    print("p1:", accum)
    accum = 0
    for line in data:
        parts = line.split(' ')
        buttons = parts[0]
        joltages = parts[-1]
        presses = parts[1:-1]
        accum += simulate_power(joltages, presses)

    print("p2:", accum)


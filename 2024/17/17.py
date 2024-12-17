import sys
from itertools import islice

# I'm using python 3.9.6 which doesn't have
# bathced in itertools
def batched(iterable, n, *, strict=False):
    # batched('ABCDEFG', 3) → ABC DEF G
    if n < 1:
        raise ValueError('n must be at least one')
    iterator = iter(iterable)
    while batch := tuple(islice(iterator, n)):
        if strict and len(batch) != n:
            raise ValueError('batched(): incomplete batch')
        yield batch

def decode_combo(registers, combo):
    if combo >= 0 and combo <= 3:
        return combo
    elif combo == 4:
        return registers['A']
    elif combo == 5:
        return registers['B']
    elif combo == 6:
        return registers['C']

def part1(registers, instructions):
    ip = 0
    max_ip = len(instructions)
    out = []
    while ip < max_ip:
        op, oper = instructions[ip]
        if op == 0:
            v = decode_combo(registers, oper)
            tmp = registers['A'] // (2 ** v)
            registers['A'] = tmp
            ip += 1
        elif op == 1:
            b = registers['B']
            registers['B'] = b ^ oper
            ip += 1
        elif op == 2:
            combo = decode_combo(registers, oper)
            registers['B'] = combo % 8
            ip += 1
        elif op == 3:
            a = registers['A']
            if a != 0:
                ip = oper
            else:
                ip += 1
        elif op == 4:
            b = registers['B']
            c = registers['C']
            registers['B'] = b ^ c
            ip += 1
        elif op == 5:
            combo = decode_combo(registers, oper)
            out.append(str(combo % 8))
            ip += 1
        elif op == 6:
            v = decode_combo(registers, oper)
            tmp = registers['A'] // (2 ** v)
            registers['B'] = tmp
            ip += 1
        elif op == 7:
            v = decode_combo(registers, oper)
            tmp = registers['A'] // (2 ** v)
            registers['C'] = tmp
            ip += 1
    return ",".join(out)


def part2(registers, instructions, program):
    idx = 0

    while True:
        registers['A'] = idx
        res = part1(registers, instructions)
        if res == program:
            return idx

        idx += 1

with open(sys.argv[1]) as fh:
    regs = {"A":0, "B":0, "C":0}
    instructions = []
    program = ""
    for line in fh:
        if line.startswith("Register"):
            parts = line.split(' ')
            regs[parts[1][0]] = int(parts[2])
        elif line.startswith("Program:"):
            program = line[line.index(': ')+2:].strip()
            instructions.append(line[line.index(': ') + 2:].strip())

    pmem = list(batched([int(x) for x in instructions[0].split(',')], 2))
    ret1 = part1(regs, pmem)
    ret2 = part2(regs, pmem, program)
    print("part 1:", ret1)
    print("part 2:", ret2)

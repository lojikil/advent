import sys
import collections
import functools
import typing
import random
from dataclasses import dataclass

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

class Instruction:
    pass

@dataclass
class GtOp(Instruction):
    op0:str
    op1:int

@dataclass
class LtOp(Instruction):
    op0:str
    op1:int

@dataclass
class Accept(Instruction):
    pass

@dataclass
class Reject(Instruction):
    pass

@dataclass
class JumpNT(Instruction):
    pass

@dataclass
class Call(Instruction):
    callee:str

class Symbolic:
    pass

# we really need a constraint class
@dataclass
class SymValue(Symbolic):
    x:str
    m:str
    a:str
    s:str
    trace:str
    val:str

@dataclass
class SymPath(Symbolic):
    trace:typing.Any
    cnd:typing.Any
    val:int
    valleft:SymValue
    valright:SymValue

def dict_of_part(rawp):
    ret = {}
    parts = rawp[1:-1].split(',')
    for part in parts:
        name,val = part.split('=')
        ret[name] = int(val)
    return ret

def compile_rule(ruletxt):
    rules = ruletxt[1:-1].split(',')
    ret = []
    for rule in rules:
        if ':' in rule:
            cnd,then = rule.split(':')
            if '<' in cnd:
                operands = cnd.split('<')
                ret.append(LtOp(operands[0], int(operands[1])))
                ret.append(JumpNT())
            elif '>' in cnd:
                operands = cnd.split('>')
                ret.append(GtOp(operands[0], int(operands[1])))
                ret.append(JumpNT())
            if then == "A":
                ret.append(Accept())
            elif then == "R":
                ret.append(Reject())
            else:
                ret.append(Call(then))
        elif rule == "A":
            ret.append(Accept())
        elif rule == "R":
            ret.append(Reject())
        else:
            ret.append(Call(rule))
    return ret

def print_program(rules):
    for k in rules.keys():
        print("{0}:".format(k))
        for i in rules[k]:
            print("\t", i)

def part1(rules, parts):
    accepted = []
    currule = "in"
    for part in parts:
        currule = "in"
        ar = False
        tv = False
        pc = 0
        p = rules[currule]
        print("debugging: curpart = ", part)
        while not ar:
            inst = p[pc]
            print("debugging: ", inst, pc, tv)
            if type(inst) == LtOp:
                print(inst.op0, "(", part[inst.op0], ") <", inst.op1, ":", part[inst.op0] < inst.op1)
                tv = part[inst.op0] < inst.op1
            elif type(inst) == GtOp:
                print(inst.op0, "(", part[inst.op0], ") >", inst.op1, ":", part[inst.op0] > inst.op1)
                tv = part[inst.op0] > inst.op1
            elif type(inst) == JumpNT:
                if not tv:
                    pc += 1
                tv = False
            elif type(inst) == Accept:
                accepted.append(part)
                ar = True
            elif type(inst) == Reject:
                ar = True
            elif type(inst) == Call:
                currule = inst.callee
                p = rules[currule]
                pc = -1
                print("debug calling:", currule, p)
            pc += 1
    total = 0
    for a in accepted:
        total += a["x"]
        total += a["m"]
        total += a["a"]
        total += a["s"]
    return total

def part2(workflows):
    accepted = []
    currule = "in"
    workq = [(SymVal(0), 0, "in", [])]
    currule = "in"
    pc = 0
    p = rules[currule]
    curpath = []
    print("debugging: curpart = ", part)
    while len(workq) > 0:
        cv, pc, currule, curpath = workq[0]
        workq = workq[1:]
        p = rules[currule]
        inst = p[pc]
        print("debugging: ", inst, pc, tv)
        if type(inst) == LtOp:
            print(inst.op0, "(", part[inst.op0], ") <", inst.op1, ":", part[inst.op0] < inst.op1)
            curpath.append(SymPath(curpath, "{0} < {1}".format(inst.op0, inst.op1)))
            tv = part[inst.op0] < inst.op1
        elif type(inst) == GtOp:
            print(inst.op0, "(", part[inst.op0], ") >", inst.op1, ":", part[inst.op0] > inst.op1)
            tv = part[inst.op0] > inst.op1
        elif type(inst) == JumpNT:
            if not tv:
                pc += 1
            tv = False
        elif type(inst) == Accept:
            accepted.append(part)
            ar = True
        elif type(inst) == Reject:
            ar = True
        elif type(inst) == Call:
            currule = inst.callee
            p = rules[currule]
            pc = -1
            print("debug calling:", currule, p)
        pc += 1
    total = 0
    for a in accepted:
        total += a["x"]
        total += a["m"]
        total += a["a"]
        total += a["s"]
    return total

with open(sys.argv[1]) as fh:
    workflows = {}
    data = []
    for line in fh:
        line = line.strip()
        if line == "":
            continue
        elif line[0].isidentifier():
            name = line[:line.index('{')]
            rule = line[line.index('{'):]
            workflows[name] = compile_rule(rule)
        elif line[0] == "{":
            data.append(dict_of_part(line))
        else:
            continue
    print_program(workflows)
    print("parts:", data)
    print(part1(workflows, data))

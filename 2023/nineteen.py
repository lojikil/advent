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
    val:str

@dataclass
class SymPath(Symbolic):
    trace:typing.Any
    cnd:typing.Any
    val:int

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

def part2(rules):
    accepted = []
    currule = "in"
    workq = [(SymValue(0), 0, "in", [], None)]
    currule = "in"
    pc = 0
    p = rules[currule]
    curpath = []
    currel = None
    while len(workq) > 0:
        cv, pc, currule, curpath, currel = workq[0]
        workq = workq[1:]
        p = rules[currule]
        inst = p[pc]
        if len(curpath) > 1:
            lastcnd = curpath[-2].cnd
        else:
            lastcnd = ""
        if type(inst) == LtOp:
            currel = (inst.op0, "LT", inst.op1)
            curpath.append(SymPath(curpath, lastcnd + "&& ({0} < {1})".format(inst.op0, inst.op1), pc))
            workq.append((cv, pc + 1, currule, curpath, currel))
        elif type(inst) == GtOp:
            currel = (inst.op0, "GT", inst.op1)
            curpath.append(SymPath(curpath, lastcnd + "&& ({0} > {1})".format(inst.op0, inst.op1), pc))
            workq.append((cv, pc + 1, currule, curpath, currel))
        elif type(inst) == JumpNT:
            workq.append((cv, pc + 1, currule, curpath, currel))
            newpath = curpath[:-1]
            if currel[1] == "LT":
                newpath.append(SymPath(newpath, lastcnd + " && ({0} >= {1})".format(currel[0], currel[2]), pc))
            else:
                newpath.append(SymPath(newpath, lastcnd + " && ({0} <= {1})".format(currel[0], currel[2]), pc))
            workq.append((cv, pc + 2, currule, newpath, currel))
        elif type(inst) == Accept:
            accepted.append(curpath)
            #print("this path was accepted:", curpath[-1])
            p = curpath[-1]
            #print("trace:", p.trace)
            print("accepted condition:", p.cnd)
            continue
        elif type(inst) == Reject:
            continue
        elif type(inst) == Call:
            print("debug calling:", inst.callee)
            workq.append((cv, 0, inst.callee, curpath, currel))
    total = 0
    #print("Accepted paths:", accepted)
    print("Accepted paths:", len(accepted))
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
    print("--------\npart 2")
    print(part2(workflows))

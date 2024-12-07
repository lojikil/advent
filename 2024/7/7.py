import sys
import itertools

def compute(nums, ops):
    res = nums[0]
    nums = nums[1:]
    idx = 0
    for op in ops:
        if op == '+':
            res += nums[idx]
        elif op == '*':
            res *= nums[idx]
        idx += 1
    return res

def compute2(nums, ops):
    res = nums[0]
    nums = nums[1:]
    idx = 0
    for op in ops:
        if op == '+':
            res += nums[idx]
        elif op == '*':
            res *= nums[idx]
        elif op == '||':
            t = str(res) + str(nums[idx])
            res = int(t)
        idx += 1
    return res

def part1(eqn):
    res = int(eqn[0:eqn.index(':')])
    vals = [int(x) for x in eqn[eqn.index(':') + 2:].split(' ')]
    rvec = [['+', '*']] * (len(vals) - 1)
    bvec = list(itertools.product(*rvec))
    for ops in bvec:
        #print("trying:", vals, ops)
        ret = compute(vals, ops)
        if ret == res:
            return res
    return 0

def part2(eqn):
    res = int(eqn[0:eqn.index(':')])
    vals = [int(x) for x in eqn[eqn.index(':') + 2:].split(' ')]
    rvec = [['+', '*', '||']] * (len(vals) - 1)
    bvec = list(itertools.product(*rvec))
    for ops in bvec:
        #print("trying:", vals, ops)
        ret = compute2(vals, ops)
        if ret == res:
            return res
    return 0

with open(sys.argv[1]) as fh:
    res1 = 0
    lines2 = []
    for line in fh:
        ret = part1(line.strip())
        if ret == 0:
            lines2.append(line)
        res1 += ret

    res2 = 0
    for line in lines2:
        res2 += part2(line)

    print("part 1:", res1)
    print("part 2:", res1 + res2)

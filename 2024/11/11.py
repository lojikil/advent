import sys

def part1(stones):
    blink = 0
    curstones = stones.copy()
    for blink in range(0, 25):
        newstones = []
        for stone in curstones:
            if stone == "0":
                newstones.append("1")
            elif (len(stone) % 2) == 0:
                l = len(stone)
                newstones.append(stone[0:l//2])
                newstones.append(str(int(stone[l//2:])))
            else:
                newstones.append(str(int(stone) * 2024))

        curstones = newstones
    return len(curstones)

def part2(stones):
    blink = 0
    curstones = stones.copy()
    for blink in range(0, 75):
        print("in blink:", blink)
        stoneidx = 0
        while stoneidx < len(curstones):
            stone = curstones[stoneidx]
            if stone == "0":
                curstones[stoneidx] = "1"
            elif (len(stone) % 2) == 0:
                l = len(stone)
                curstones[stoneidx] = stone[0:l//2]
                curstones.insert(stoneidx + 1, str(int(stone[l//2:])))
                stoneidx += 1
            else:
                curstones[stoneidx] = str(int(stone) * 2024)
            stoneidx += 1

    return len(curstones)

with open(sys.argv[1]) as fh:
    stones = fh.readline().strip().split(' ')

    ret1 = part1(stones)
    ret2 = part2(stones)

    print("part1:", ret1)
    print("part2:", ret2)

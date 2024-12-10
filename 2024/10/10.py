import sys

def north(x, y, topomap):
    max_y = len(topomap)
    max_x = len(topomap[0])
    new_x, new_y = x, y - 1
    if new_y >= 0 and new_y < max_y:
        return topomap[new_y][new_x], (new_x, new_y)

def south(x, y, topomap):
    max_y = len(topomap)
    max_x = len(topomap[0])
    new_x, new_y = x, y + 1
    if new_y >= 0 and new_y < max_y:
        return topomap[new_y][new_x], (new_x, new_y)

def east(x, y, topomap):
    max_y = len(topomap)
    max_x = len(topomap[0])
    new_x, new_y = x + 1, y
    if new_x >= 0 and new_x < max_x:
        return topomap[new_y][new_x], (new_x, new_y)

def west(x, y, topomap):
    max_y = len(topomap)
    max_x = len(topomap[0])
    new_x, new_y = x - 1, y
    if new_x >= 0 and new_x < max_x:
        return topomap[new_y][new_x], (new_x, new_y)

def part1(hike):
    starts = []
    max_y = len(topomap)
    max_x = len(topomap[0])
    counts = {}

    for y in range(0, max_y):
        for x in range(0, max_x):
            cell = hike[y][x]
            if cell == 0:
                starts.append((x, y))

    alt_counts = 0
    while len(starts) > 0:
        cur = starts.pop()
        count_nines = 0
        workq = [cur]
        seen = set()
        while len(workq) > 0:
            x, y = workq.pop()
            cell = hike[y][x]

            if cell == 9:
                if (x, y) not in seen:
                    seen.add((x,y))
                    count_nines += 1
                    continue

            if (v := north(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := south(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := east(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := west(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])

        counts[cur] = count_nines
        alt_counts += len(seen)
        print("seen:", seen)

    print(counts)

    ret = 0
    for k, v in counts.items():
        ret += v


    return ret

def part2(hike):
    starts = []
    max_y = len(topomap)
    max_x = len(topomap[0])
    counts = {}

    for y in range(0, max_y):
        for x in range(0, max_x):
            cell = hike[y][x]
            if cell == 0:
                starts.append((x, y))

    alt_counts = 0
    while len(starts) > 0:
        cur = starts.pop()
        count_nines = 0
        workq = [cur]
        seen = set()
        while len(workq) > 0:
            x, y = workq.pop()
            cell = hike[y][x]

            if cell == 9:
                seen.add((x,y))
                count_nines += 1
                continue

            if (v := north(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := south(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := east(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])
            if (v := west(x, y, hike)) is not None and v[0] == (cell + 1):
                workq.append(v[1])

        counts[cur] = count_nines
        alt_counts += len(seen)
        print("seen:", seen)

    print(counts)

    ret = 0
    for k, v in counts.items():
        ret += v


    return ret

with open(sys.argv[1]) as fh:
    topomap = []
    for line in fh:
        newline = [int(x) for x in line.strip()]
        topomap.append(newline)

    ret1 = part1(topomap)
    ret2 = part2(topomap)
    print("part1:", ret1)
    print("part2:", ret2)

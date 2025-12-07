import copy

def tachyon(m, x, y, d):
    nx, ny = 0, 0
    lx = len(m[0])
    ly = len(m)
    match d:
        case 'n':
            nx, ny = x, (y - 1)
        case 's':
            nx, ny = x, (y + 1)
        case 'w':
            nx, ny = (x - 1), y
        case 'e':
            nx, ny = (x + 1), y

    if nx < 0 or nx >= lx:
        return False
    if ny < 0 or ny >= ly:
        return False
    if m[ny][nx] == '|':
        return True
    return False

def print_map(m):
    for row in m:
        print("".join(row))

with open('input-test.txt') as fh:
    tachmap = [list(x.strip()) for x in fh.readlines()]
    lx = len(tachmap[0])
    ly = len(tachmap)
    maxworlds = 0
    split = 0
    workq = [(tachmap, 0, 0)]
    worked = False
    while True:
        if len(workq) > maxworlds:
            maxworlds = len(workq)
        if len(workq) == 0:
            break

        tm, sx, sy = workq.pop()
        for y in range(sy, ly):
            for x in range(sx, lx):
                print(f"{x=}, {y=}")
                p = tm[y][x]
                match p:
                    case '.':
                        if tachyon(tm, x, y, "n"):
                            tm[y][x] = '|'
                    case '^':
                        if tachyon(tm, x, y, "n"):
                            split += 1
                            if not tachyon(tm, x, y, "w"):
                                ntm = copy.deepcopy(tm)
                                ntm[y][x - 1] = '|'
                                workq.append((ntm, y, x - 1))
                            if not tachyon(tm, x, y, "e"):
                                ntm = copy.deepcopy(tm)
                                ntm[y][x + 1] = '|'
                                workq.append((ntm, y, x + 1))
                    case 'S':
                        tm[y + 1][x] = '|'
    #print_map(tachmap)
    print(f"p2: {maxworlds=}")

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

with open('input-full.txt') as fh:
    tachmap = [list(x.strip()) for x in fh.readlines()]
    lx = len(tachmap[0])
    ly = len(tachmap)
    split = 0
    for y in range(0, ly):
        for x in range(0, lx):
            p = tachmap[y][x]
            match p:
                case '.':
                    if tachyon(tachmap, x, y, "n"):
                        tachmap[y][x] = '|'
                case '^':
                    if tachyon(tachmap, x, y, "n"):
                        split += 1
                        if not tachyon(tachmap, x, y, "w"):
                            tachmap[y][x - 1] = '|'
                        if not tachyon(tachmap, x, y, "e"):
                            tachmap[y][x + 1] = '|'
                case 'S':
                    tachmap[y + 1][x] = '|'
    #print_map(tachmap)
    print(f"p1: {split=}")

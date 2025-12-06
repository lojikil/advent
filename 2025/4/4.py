import copy

def inaccessible(m, x, y):
    if x < 0 or x >= (len(m[0])):
        return True
    elif y < 0 or y >= len(m):
        return True
    return False

def accessible(m, d, x, y):
    nx, ny = 0, 0

    if m[y][x] != '@':
        return 0

    match d:
        case 'nw':
            nx, ny = (x - 1), (y - 1)
        case 'n':
            nx, ny = x, (y - 1)
        case 'ne':
            nx, ny = (x + 1), (y - 1)
        case 'e':
            nx, ny = (x + 1), y
        case 'se':
            nx, ny = (x + 1), (y + 1)
        case 's':
            nx, ny = x, (y + 1)
        case 'sw':
            nx, ny = (x - 1), (y + 1)
        case 'w':
            nx, ny = (x - 1), y

    #print(f"p1: checking: {x}, {y} {d}")

    if inaccessible(m, nx, ny):
        return 0
    elif m[ny][nx] == '.':
        return 0
    return 1

def p1(m):
    accum = 0
    for i in range(0, len(m)):
        for j in range(0, len(m[0])):
            if m[i][j] != '@':
                continue

            t = sum([accessible(m, "n", j, i),
                     accessible(m, "ne", j, i),
                     accessible(m, "e", j, i),
                     accessible(m, "se", j, i),
                     accessible(m, "s", j, i),
                     accessible(m, "sw", j, i),
                     accessible(m, "w", j, i),
                     accessible(m, "nw", j, i)])
            if t < 4:
                accum += 1
    print("p1:", accum)

def p2(mm):
    m = copy.deepcopy(mm)
    accum = 0
    accessed = False

    while True:
        for i in range(0, len(m)):
            for j in range(0, len(m[0])):
                if m[i][j] != '@':
                    continue

                t = sum([accessible(m, "n", j, i),
                         accessible(m, "ne", j, i),
                         accessible(m, "e", j, i),
                         accessible(m, "se", j, i),
                         accessible(m, "s", j, i),
                         accessible(m, "sw", j, i),
                         accessible(m, "w", j, i),
                         accessible(m, "nw", j, i)])
                if t < 4:
                    m[i][j] = '.'
                    accessed = True
                    accum += 1
        if accessed:
            accessed = False
        else:
            break
    print(f"p2: {accum}")

def main():
    with open('input-full.txt') as fh:
        m = [list(x.strip()) for x in fh.readlines()]
        p1(m)
        p2(m)

if __name__ == "__main__":
    main()

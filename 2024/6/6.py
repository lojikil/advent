import sys
import collections

def forward(gd, curx, cury):
    if gd == '^':
        return (curx, cury - 1)
    elif gd == '>':
        return (curx + 1, cury)
    elif gd == '<':
        return (curx - 1, cury)
    elif gd == 'v':
        return (curx, cury + 1)

def right(gd):
    if gd == '^':
        return '>'
    elif gd == '>':
        return 'v'
    elif gd == 'v':
        return '<'
    elif gd == '<':
        return '^'

def part1(labmap, gx, gy):
    curx = gx
    cury = gy
    maxx = len(labmap[0])
    maxy = len(labmap)
    seen = set([(curx, cury)])
    gd = '^'
    onscreen = True

    while onscreen:
        tx, ty = forward(gd, curx, cury)
        if tx < 0 or tx >= maxx:
            break
        elif ty < 0 or ty >= maxy:
            break
        elif labmap[ty][tx] == '#':
            gd = right(gd)
        else:
            curx, cury = tx, ty

        seen.add((curx, cury))

    return len(seen)

def part2(labmap):
    pass

with open(sys.argv[1]) as fh:
    labmap = []
    gy = 0
    gx = -1
    for line in fh:
        labmap.append(line.strip())
        if '^' in line:
            gx = line.index('^')

        if gx == -1:
            gy += 1

    res1 = part1(labmap, gx, gy)
    print("part1:", res1)

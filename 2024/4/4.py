import sys
import collections

def clamped_add(ws, state, x, y, d):
    states = "XMAS"
    if state > 3:
        return None
    elif x < 0 or x >= len(ws[0]):
        return None
    elif y < 0 or y >= len(ws):
        return None
    elif state == -1:
        return (0, x, y, d)

    cell = ws[y][x]
    if cell != states[state]:
        return None
    return (state, x, y, d)

def part1(ws):
    wq = []
    max_x = len(ws[0])
    max_y = len(ws)
    states = 'XMAS'
    finals = set()
    seen = set()
    cnt = 0

    for y in range(0, max_y):
        for x in range(0, max_x):
            cell = ws[y][x]
            if cell == "X":
                tmp = [clamped_add(ws, 1, x - 1, y - 1, "nw"),
                       clamped_add(ws, 1, x , y - 1, "n"),
                       clamped_add(ws, 1, x + 1, y - 1, "ne"),
                       clamped_add(ws, 1, x - 1, y, "w"),
                       clamped_add(ws, 1, x + 1, y, "e"),
                       clamped_add(ws, 1, x - 1, y + 1, "sw"),
                       clamped_add(ws, 1, x, y + 1, "s"),
                       clamped_add(ws, 1, x + 1, y + 1, "se")]
                #print("initial scan:", tmp)
                wq.extend(list(filter(lambda v: v is not None, tmp)))

    while len(wq) != 0:
        curstate = wq.pop()
        state = curstate[0]
        x = curstate[1]
        y = curstate[2]
        direction = curstate[3]

        if curstate in seen:
            continue

        #print(len(wq), curstate)

        cell = ws[y][x]
        cm = (cell == states[state])
        #print(f"cell at {x}, {y} is {cell}")
        if state == 3 and cell == states[3]:
            cnt += 1
            finals.add((x, y, direction))
        elif cm and direction == "nw" and (v := clamped_add(ws, state + 1, x - 1, y - 1, "nw")) is not None:
            wq.append(v)
        elif cm and direction == "n" and (v := clamped_add(ws, state + 1, x , y - 1, "n")) is not None:
            wq.append(v)
        elif cm and direction == "ne" and (v := clamped_add(ws, state + 1, x + 1, y - 1, "ne")) is not None:
            wq.append(v)
        elif cm and direction == "w" and (v := clamped_add(ws, state + 1, x - 1, y, "w")) is not None:
            wq.append(v)
        elif cm and direction == "e" and (v := clamped_add(ws, state + 1, x + 1, y, "e")) is not None:
            wq.append(v)
        elif cm and direction == "sw" and (v := clamped_add(ws, state + 1, x - 1, y + 1, "sw")) is not None:
            wq.append(v)
        elif cm and direction == "s" and (v := clamped_add(ws, state + 1, x, y + 1, "s")) is not None:
            wq.append(v)
        elif cm and direction == "se" and (v := clamped_add(ws, state + 1, x + 1, y + 1, "se")) is not None:
            wq.append(v)
        elif x >= max_x:
            wq.append((0, 0, y + 1, "s"))
        else:
            # XXX we're not currently scanning any direction but east correctly
            tmp = [clamped_add(ws, -1, x - 1, y - 1, "nw"),
                   clamped_add(ws, -1, x , y - 1, "n"),
                   clamped_add(ws, -1, x + 1, y - 1, "ne"),
                   clamped_add(ws, -1, x - 1, y, "w"),
                   clamped_add(ws, -1, x + 1, y, "e"),
                   clamped_add(ws, -1, x - 1, y + 1, "sw"),
                   clamped_add(ws, -1, x, y + 1, "s"),
                   clamped_add(ws, -1, x + 1, y + 1, "se")]
            #print(tmp)
            wq.extend(list(filter(lambda v: v is not None, tmp)))
        seen.add(curstate)
        if (v := clamped_add(ws, 0, x + 1, y, "e")) is not None:
            wq.append((0, x + 1, y, "e"))

    print(len(finals), cnt)
    return cnt

def part2(ws):
    wq = []
    max_x = len(ws[0])
    max_y = len(ws)
    states = 'XMAS'
    finals = set()
    seen = set()
    cnt = 0

    for y in range(0, max_y):
        for x in range(0, max_x):
            cell = ws[y][x]
            if cell == "M":
                print(f"M found at {x}, {y}")
                tmp = [clamped_add(ws, 2, x - 1, y - 1, "nw"),
                       clamped_add(ws, 2, x + 1, y - 1, "ne"),
                       clamped_add(ws, 2, x - 1, y + 1, "sw"),
                       clamped_add(ws, 2, x + 1, y + 1, "se")]
                print("initial scan:", tmp)
                wq.extend(list(filter(lambda v: v is not None, tmp)))

    print("initial work queue:", wq)
    print(len(wq))
    wq = list(set(wq))
    print(len(wq))

    while len(wq) != 0:
        curstate = wq.pop()
        state = curstate[0]
        x = curstate[1]
        y = curstate[2]
        direction = curstate[3]


        #print(len(wq), curstate)

        cell = ws[y][x]
        cm = (cell == states[state])
        #print(f"cell at {x}, {y} is {cell}")
        if state == 3 and cell == states[3]:
            if (x, y, direction) not in finals:
                cnt += 1
                finals.add((x, y, direction))
        elif cm and direction == "nw" and (v := clamped_add(ws, state + 1, x - 1, y - 1, "nw")) is not None:
            wq.append(v)
        elif cm and direction == "ne" and (v := clamped_add(ws, state + 1, x + 1, y - 1, "ne")) is not None:
            wq.append(v)
        elif cm and direction == "sw" and (v := clamped_add(ws, state + 1, x - 1, y + 1, "sw")) is not None:
            wq.append(v)
        elif cm and direction == "se" and (v := clamped_add(ws, state + 1, x + 1, y + 1, "se")) is not None:
            wq.append(v)
        seen.add(curstate)

    aoverlaps = collections.Counter()
    for s in finals:
        newx = s[0]
        newy = s[1]
        newd = s[2]
        print(f"s at {newx}, {newy}")
        if newd == "se" and (v := clamped_add(ws, 2, newx - 1, newy - 1, "nw")) is not None:
            ax = v[1]
            ay = v[2]
            print(f"found an a at: {v}")
            aoverlaps.update([(ax, ay)])
        if newd == "sw" and (v := clamped_add(ws, 2, newx + 1, newy - 1, "ne")) is not None:
            ax = v[1]
            ay = v[2]
            print(f"found an a at: {v}")
            aoverlaps.update([(ax, ay)])
        if newd == "ne" and (v := clamped_add(ws, 2, newx - 1, newy + 1, "sw")) is not None:
            ax = v[1]
            ay = v[2]
            print(f"found an a at: {v}")
            aoverlaps.update([(ax, ay)])
        if newd == "nw" and (v := clamped_add(ws, 2, newx + 1, newy + 1, "se")) is not None:
            ax = v[1]
            ay = v[2]
            print(f"found an a at: {v}")
            aoverlaps.update([(ax, ay)])
    cnt = 0
    print(len(aoverlaps))
    for idx in aoverlaps:
        print(idx, aoverlaps[idx])
        if aoverlaps[idx] >= 2:
            cnt += 1
    print(len(finals), cnt)
    return cnt

with open(sys.argv[1]) as fh:
    wsmap = []
    for line in fh:
        wsmap.append(line.strip())
    print("part1:", part1(wsmap))
    print("part2:", part2(wsmap))

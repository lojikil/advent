import sys

class Map12:
    def __init__(self, map_data):
        self.map_data = [x.strip() for x in map_data.split('\n')]
        self.start_pos = (0, 0)
        self.end_pos = (0, 0)

        for idx in range(0, len(self.map_data)):
            row = self.map_data[idx]
            if "S" in row:
                self.start_pos = (row.index("S"), idx)
            elif "E" in row:
                self.end_pos = (row.index("E"), idx)

    def __getitem__(self, key):
        return self.map_data[key]

    def max_y(self):
        return len(self.map_data) - 1

    def max_x(self):
        return len(self.map_data[0])

    def end(self):
        return self.end_pos

    def start(self):
        return self.start_pos

def climable_p(m, s, p):
    (xs, ys) = s
    (xp, yp) = p
    start_row = m[ys]
    start = start_row[xs]
    next_row = m[yp]
    nexts = next_row[xp]

    if start == 'S':
        start = 'a'
    elif start == 'E':
        start = 'z'

    if nexts == 'S':
        nexts = 'a'
    elif nexts == 'E':
        nexts = 'z'

    test = ord(start) - ord(nexts)
    print("climable?", start, nexts, s, p, test, test >= -1 and test <= 1)
    return test >= -1 and test <= 1

def up(m, curpos):
    (x, y) = curpos
    print("up: ", curpos)
    if y - 1 < 0:
        return None
    if climable_p(m, curpos, (x, y - 1)):
        print("climbing up:", (x, y - 1))
        return (x, y - 1)

def down(m, curpos):
    (x, y) = curpos
    print("down: ", curpos)
    if y + 1 >= m.max_y():
        return None
    if climable_p(m, curpos, (x, y + 1)):
        print("climbing down:", (x, y + 1))
        return (x, y + 1)

def left(m, curpos):
    (x, y) = curpos
    print("left:", curpos)
    if (x - 1) < 0:
        print("dying on value check? left")
        return None
    if climable_p(m, curpos, (x - 1, y)):
        print("climbing left:", (x - 1, y));
        return (x - 1, y)
    print("default death? left")

def right(m, curpos):
    (x, y) = curpos
    print("right:", curpos)
    print(m.max_x())
    if (x + 1) >= m.max_x():
        print("dying on val check? right for", curpos, (x + 1), m.max_x())
        return None
    if climable_p(m, curpos, (x + 1, y)):
        print("climbing right:", (x + 1, y))
        return (x + 1, y)
    print("default death? right")

tree = {}

def pathfinder(m, startingpos, depth=0, seen=None):
    paths = [up(m, startingpos), down(m, startingpos), left(m, startingpos), right(m, startingpos)]
    retpaths = []
    print("pathfinder:", startingpos, depth, "\n\tseen:", seen, "\n\tpaths:", paths)
    if seen is None:
        seen = []

    seen.append(startingpos)

    if(startingpos == m.end()):
        print("super hit end at depth: ", depth)
    elif(startingpos == m.start()):
        print("super hit start at depth:", depth)

    fpaths = filter(lambda x: x is not None, paths)
    if startingpos not in tree:
        tree[startingpos] = list(fpaths)

    for p in paths:
        if p is not None and p not in seen:
            v = pathfinder(m, p, depth=depth+1, seen=seen)
        elif p == m.start():
            print("super hit start depth in child at depth: ", depth + 1)
        elif p == m.end():
            print("super hit end depth in child at depth: ", depth + 2)

def patchfinder(m):
    for y in range(0, m.max_y()):
        for x in range(0, m.max_x()):
            startingpos = (x, y)
            fpaths = filter(lambda x: x is not None,
                            [up(m, startingpos),
                             down(m, startingpos),
                             left(m, startingpos),
                             right(m, startingpos)])
            if startingpos not in tree:
                tree[startingpos] = list(fpaths)

# this works by brute force for the sample, but
# not the actual; I thought I could be clever and
# avoid Dijkstra's algorithm, but I'm not actually
# that clever
def forestwalkers(m, depth, curpos, seen):
    seen.append(curpos)
    if curpos == m.end():
        print("super depth: ", depth)
    elif curpos == m.start():
        print("super depth start:", depth)

    neighbors = tree[curpos]
    rets = []
    for n in neighbors:
        if n not in seen:
            v = forestwalkers(m, depth+1, n, seen)
            rets.extend(v)
        if n == m.end():
            rets.append(depth + 1)
        if n == m.start():
            rets.append(depth + 1)
    return rets

INF = 999999999999

def setdepth(t, v, k):
    if k not in t:
        t[k] = v
    elif t[k] > v:
        t[k] = v

# really A*, but who's counting?
# ... me, I'm counting. Literally the point of the puzzle

def getgscore(g, k):
    if k in g:
        return g[k]
    else:
        return INF

def lowest_score(scores, unvisited):
    minscore = INF
    cur = None
    for u in unvisited:
        if scores[u] < minscore:
            cur = u
            minscore = scores[u]
    return cur

def dijkstra_walker(m):
    visited = set([])
    unvisited = set([m.start()])
    tvalues = {}
    tvalues[m.start] = 0
    depth = 0
    fscore = {}
    gscore = {}
    cur = m.start()
    gscore[cur] = 0
    fscore[cur] = 1
    all_gscore = {}
    all_fscore = {}
    t_gscore = 0
    while len(unvisited) > 0:
        print("cur: ", cur)
        if cur == m.end():
            print("super hit end with depth: ", t_gscore, depth)
        neighbors = tree[cur]
        for neighbor in neighbors:
            t_gscore = gscore[cur] + 1

            if t_gscore < getgscore(gscore, neighbor):
                if neighbor not in all_gscore:
                    all_gscore[neighbor] = [t_gscore]
                else:
                    all_gscore[neighbor].append(t_gscore)

                if neighbor not in all_fscore:
                    all_fscore[neighbor] = [t_gscore + 400]
                else:
                    all_fscore[neighbor].append(t_gscore + 400)
                gscore[neighbor] = t_gscore
                fscore[neighbor] = t_gscore + 400
                if neighbor not in unvisited:
                    unvisited.add(neighbor)

        unvisited.remove(cur)
        cur = lowest_score(fscore, unvisited)
    print("gscores:", gscore);
    print("fscores:", fscore);
    print("lowest: ", gscore[m.end()])
    print("end fscore", fscore[m.end()])
    print("all gscores for end:", all_gscore[m.end()])
    print("all fscores for end:", all_fscore[m.end()])

def rise_run(s, e):
    (xs, ys) = s
    (xe, ye) = e
    return (ye - ys) / (1 + (xe - xs))

def main():
    if len(sys.argv) != 2:
        print("usage: twelve.py [file]")
        sys.exit(1)

    with open(sys.argv[1], 'r') as fh:
        data = fh.read()

    puzzle_map = Map12(data)
    print("start:", puzzle_map.start())
    print("end:", puzzle_map.end())
    print("delta: ", rise_run(puzzle_map.start(), puzzle_map.end()));
    #paths = pathfinder(puzzle_map, puzzle_map.end())
    patchfinder(puzzle_map)
    paths = forestwalkers(puzzle_map, 0, puzzle_map.end(), [])
    print("tree:", tree)
    print(paths)
    dijkstra_walker(puzzle_map)
    #for p in paths:
    #    print(p)

if __name__ == "__main__":
    sys.setrecursionlimit(10 * sys.getrecursionlimit())
    main()

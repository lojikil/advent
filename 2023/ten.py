import sys
import collections
import functools

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

def clamped_access(dmap, pos):
    y, x = pos
    if y >= 0 and y < len(dmap):
        row = dmap[y]
        if x >= 0 and x < len(dmap):
            return row[x]
    return None

def east(pos):
    y, x = pos
    return (y, x + 1)

def west(pos):
    y, x = pos
    return (y, x - 1)

def north(pos):
    y, x = pos
    return (y - 1, x)

def south(pos):
    y, x = pos
    return (y + 1, x)

def analyze_pipe(dmap, pos):
    cur_pipe = clamped_access(dmap, pos)
    if cur_pipe == 'S':
        a_r = []
        n = clamped_access(dmap, north(pos))
        if n is not None and n in ['|', '7', 'F']:
            a_r.append(north(pos))
        s = clamped_access(dmap, south(pos))
        if s is not None and s in ['|', 'L', 'J']:
            a_r.append(south(pos))
        e = clamped_access(dmap, east(pos))
        if e is not None and e in ['-', 'J', '7']:
            a_r.append(east(pos))
        w = clamped_access(dmap, west(pos))
        if w is not None and w in ['-', 'L', 'F']:
            a_r.append(west(pos))
        return a_r
    elif cur_pipe == '|':
        return [north(pos), south(pos)]
    elif cur_pipe == '-':
        return [east(pos), west(pos)]
    elif cur_pipe == 'L':
        return [north(pos), east(pos)]
    elif cur_pipe == 'J':
        return [north(pos), west(pos)]
    elif cur_pipe == '7':
        return [south(pos), west(pos)]
    elif cur_pipe == 'F':
        return [south(pos), east(pos)]
    elif cur_pipe == '.':
        return [None, None]
    elif cur_pipe == None:
        return [None, None]

def dfs(dmap, pos):
    depth = 0
    seenq = []
    workq = [pos]
    tree = {}
    while len(workq) > 0:
        node = workq.pop()
        print(node)
        if node in tree or node in seenq:
            continue
        a_r = analyze_pipe(dmap, node)
        seenq.append(node)
        tree[node] = a_r
        workq.extend([x for x in a_r if x not in seenq])
        depth += 1
    return tree, depth

accessible = lambda x: x is not None and x != '.'

def start2pipe(dmap, pos):
    y, x = pos
    conns = [clamped_access(dmap, north(pos)),
             clamped_access(dmpa, south(pos)),
             clamped_access(dmap, west(pos)),
             clamped_access(dmap, east(pos))]
    if accessible(cons[0], cons[1]):
        return '|'
    elif accessible(cons[2], cons[3]):
        return '-'
    elif accessible(cons[0], cons[3]):
        return 'L'
    elif accessible(cons[0], cons[2]):
        return 'J'
    elif accessible(cons[1], cons[2]):
        return '7'
    elif accessible(cons[1], cons[3]):
        return 'F'

def explode_map(dmap, start_pos):
    tiles = {}
    tiles['|'] = [[0x00, 0xFF, 0x00],
                  [0x00, 0xFF, 0x00],
                  [0x00, 0xFF, 0x00]]
    tiles['-'] = [[0x00, 0x00, 0x00],
                  [0xFF, 0xFF, 0xFF],
                  [0x00, 0x00, 0x00]]
    tiles['F'] = [[0x00, 0x00, 0x00],
                  [0x00, 0xFF, 0xFF],
                  [0x00, 0xFF, 0x00]]
    tiles['7'] = [[0x00, 0x00, 0x00],
                  [0xFF, 0xFF, 0xFF],
                  [0x00, 0xFF, 0x00]]
    tiles['J'] = [[0x00, 0xFF, 0x00],
                  [0xFF, 0xFF, 0x00],
                  [0x00, 0x00, 0x00]]
    tiles['L'] = [[0x00, 0xFF, 0x00],
                  [0x00, 0xFF, 0xFF],
                  [0x00, 0x00, 0x00]]
    tiles['.'] = [[0x00, 0x00, 0x00],
                  [0x00, 0x00, 0x00],
                  [0x00, 0x00, 0x00]]
    new_map = []
    for row in dmap:
        new_top = []
        new_bottom = []
        new_middle = []
        for elem in row:
            new_tile = None
            if elem == 'S':
                new_tile = tiles[start2pipe(dmap, start_pos)]
            else:
                new_tile = tiles[elem]
            new_top.extend(new_tile[0])
            new_middle.extend(new_tile[1])
            new_bottom.extend(new_tile[2])
        new_map.append(new_top)
        new_map.append(new_middle)
        new_map.append(new_bottom)

def find_nest(dmap, tree, pos):
    depth = 0
    seenq = []
    workq = list(tree.keys())
    potential_nest = []
    nest_tree = {}
    for node in workq:
        potentials = [north(dmap, node),
                      south(dmap, node),
                      east(dmap, node),
                      west(dmap, node)]
        for p in potentials:
            clamped_p = clamped_access(dmap, p)
            if clamped_p is not None and clamped_p == '.':
                potential_nest.append(clamped_p)
    candidate_nest = []
    for p in potential_nest:
        interior_p = False
        potentials = [north(dmap, p),
                      south(dmap, p),
                      east(dmap, p),
                      west(dmap, p)]
        for none_hunt in potentials:
            if none_hunt is not None:
                interior_p = True
            else:
                interior_p = False
        if interior_p:
            candidate_nest.append(p)

    new_map = explode_map(dmap, pos)

    for candidate in candidate_nest:
        # ok, so:
        #
        # . convert the entire candidate to grey
        # . flood fill, counting nodes we fill
        # . if there is no escape to the edge, that's the path
        pass

spos = None
data = []
with open(sys.argv[1]) as fh:
    linecnt = 0
    for line in fh:
        if 'S' in line:
            spos = (line.index('S'), linecnt)
        linecnt += 1
        data.append(line)

print("starting position:", spos)
startcon = analyze_pipe(data, spos)
print("start:", startcon)
tree, depths = dfs(data, spos)
print("depths:", depths, depths / 2)

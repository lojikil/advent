import sys
import functools
import itertools

picker = lambda x, y: x or y
gears = []

def adjacent_p(mat, rowidx, start, end):
    # rowidx - 1, start - 1; row - 1, start; row - 1, start + 1
    # rowidx, start - 1; rowidx, start + 1
    # rowidx + 1, start - 1; row + 1, start; row + 1, start + 1
    def clamped_access(pair):
        row, col = pair
        if row < 0 or col < 0:
            return False
        elif row >= len(mat) or col >= len(mat[0]):
            return False
        else:
            v = mat[row][col]
            print("checking:", row, col, v)
            if v != '.' and not v.isdigit() and v != '\n':
                if v == '*':
                    gears.append((int(mat[rowidx][start:end + 1]), row, col))
                print("returning true for", row, col)
                return True
            else:
                return False

    print("row:", rowidx, "start:", start, "end:", end, "\"{0}\"".format(mat[rowidx][start:end + 1]))
    for idx in range(start, end + 1):
        print("\tidx:", idx)
        above = rowidx - 1
        below = rowidx + 1
        left = idx - 1
        right = idx + 1
        checks = list(map(clamped_access, list(itertools.product([above, below, rowidx], [left, right, idx]))))
        check = functools.reduce(picker, checks, False)
        if check:
            return True
    return False

def find_parts(mat):
    accum = []
    for rowidx in range(0, len(mat)):
        row = mat[rowidx]
        start = -1
        end = -1
        for idx in range(0, len(row)):
            if row[idx].isdigit():
                if start == -1:
                    start = idx
                    end = idx
                else:
                    end = idx
            if start != -1 and not row[idx].isdigit():
                if adjacent_p(mat, rowidx, start, end):
                    accum.append(int(row[start:end + 1]))
                start = -1
                end = -1
    return accum

def group_gears(gears):
    data = {}
    accum = []
    for gear in gears:
        point = (gear[1], gear[2])
        if point in data:
            data[point].append(gear[0])
        else:
            data[point] = [gear[0]]
    for key in data.keys():
        pgears = data[key]
        if len(pgears) == 2:
            accum.append(pgears[0] * pgears[1])
    return sum(accum)

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(1)

with open(sys.argv[1]) as fh:
    data = fh.readlines()
    print(data)
    parts = find_parts(data)
    print(parts)
    print(sum(parts))
    print(gears)
    print(group_gears(gears))

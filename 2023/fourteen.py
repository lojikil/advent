import sys
import collections
import functools
import copy

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

def print_rockfield(d):
    for row in d:
        print(''.join(row))

def transpose(mat):
    newmat = []
    for idx in range(0, len(mat[0])):
        new_row = []
        for yidx in range(0, len(mat)):
            new_row.append(mat[yidx][idx])
        newmat.append(new_row)
    return newmat

def hreflect(mat):
    newmat = []
    for idx in range(len(mat) - 1, -1, -1):
        new_row = []
        row = mat[idx]
        for yidx in range(0, len(row)):
            new_row.append(row[yidx])
        newmat.append(new_row)
    return newmat

def vreflect(mat):
    return transpose(hreflect(transpose(mat)))

def t_n(d):
    # start on row 1, because we know anything in row 0 is
    # already in the northern most section
    new_d = transpose(d)
    #print("transposed:")
    #print_rockfield(new_d)
    cnt = 1
    for g in new_d:
        for i in range(0, len(g)):
            #print(cnt, g, i, g[i])
            if g[i] == 'O':
                #print("here on 35 for", cnt, i)
                for j in range(i - 1, -1, -1):
                    #print("\there on 36 for", cnt, i, j)
                    if g[j] == '#' or g[j] == 'O':
                        if j + 1 < i:
                            #print("\t\there on 39?")
                            g[j + 1] = 'O'
                            g[i] = '.'
                        break
                    if j == 0 and g[j] == '.':
                        #print("here on 44?")
                        g[j] = 'O'
                        g[i] = '.'
        cnt += 1
    #print("returning:")
    #print_rockfield(transpose(new_d))
    return transpose(new_d)

def t_s(d):
    return hreflect(t_n(hreflect(d)))

def t_e(d):
    new_d = copy.deepcopy(d)
    cnt = 1
    for g in new_d:
        for i in range(0, len(g)):
            if g[i] == 'O':
                for j in range(i - 1, -1, -1):
                    if g[j] == '#' or g[j] == 'O':
                        if j + 1 < i:
                            g[j + 1] = 'O'
                            g[i] = '.'
                        break
                    if j == 0 and g[j] == '.':
                        g[j] = 'O'
                        g[i] = '.'
        cnt += 1
    return new_d

def t_w(d):
    return vreflect(t_e(vreflect(d)))

def part1(d):
    #print("original:")
    #print_rockfield(d)
    northernly = t_n(d)
    total = 0
    height = len(d)
    #print("original:")
    #print_rockfield(d)
    #print("tilted:")
    #print_rockfield(northernly)
    for i in range(0, len(northernly)):
        c = collections.Counter(northernly[i])
        total += (c['O'] * (height - i))
    return total

def part2(orig):
    d = copy.deepcopy(orig)
    print("original:")
    print_rockfield(d)
    for i in range(0, 3):
        d = t_e(t_s(t_w(t_n(d))))
        print("cycle", i)
        print_rockfield(d)

    total = 0
    height = len(orig)

    for i in range(0, len(d)):
        c = collections.Counter(d[i])
        total += (c['O'] * (height - i))

    return total

with open(sys.argv[1]) as fh:
    data = []
    for line in fh:
        data.append([x for x in line.strip()])
    rockfield = copy.deepcopy(data)
    print(part1(data))
    print(part2(data))

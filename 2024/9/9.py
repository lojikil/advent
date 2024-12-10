import sys

def part1(fs):
    hdd = []
    freelist = []
    fileid = 0
    physid = 0
    # generate the drive layout
    for inode in range(0, len(fs)):
        cntlen = int(fs[inode])
        #print(f"inode: {inode}, content len: {cntlen}")

        for blockid in range(0, cntlen):
            if (inode % 2) == 0:
                hdd.append(fileid)
            else:
                hdd.append(-1)
                freelist.append(physid)

            physid += 1

        if (inode % 2) == 0:
            fileid += 1
    #print(hdd)

    # compact the disk
    flidx = 0
    for r in range(len(hdd) - 1, -1, -1):
        if hdd[r] == -1:
            continue

        l = freelist[flidx]
        if l >= r:
            break
        else:
            flidx += 1

        hdd[l] = hdd[r]
        hdd[r] = -1

    # checksum
    checksum = 0
    for r in range(0, len(hdd)):
        if hdd[r] == -1:
            break
        checksum += (hdd[r] * r)

    return checksum

def find_free(fl, fm, blocksize):
    for k,v in fm.items():
        if v >= blocksize:
            return k
    return -1

def part2(fs):
    hdd = []
    freelist = []
    fileid = 0
    physid = 0
    start = 0
    filemd = {}
    freemd = {}
    # generate the drive layout
    for inode in range(0, len(fs)):
        cntlen = int(fs[inode])
        #print(f"inode: {inode}, content len: {cntlen}")

        for blockid in range(0, cntlen):
            if (inode % 2) == 0:
                hdd.append(fileid)
            else:
                hdd.append(-1)
                freelist.append(physid)

            physid += 1

        if (inode % 2) == 0:
            filemd[fileid] = cntlen
            fileid += 1
        else:
            freemd[physid] = cntlen

        if inode == 0:
            start = blockid
    print(hdd)

    print(filemd)
    print(freemd)

    # compact the disk
    flidx = 0

    r = len(hdd) - 1
    while r > start:
        print("r:", r)

        if hdd[r] == -1:
            r -= 1
            continue

        l = freelist[flidx]
        fileid = hdd[r]
        cntlen = filemd[fileid]

        freeslot = find_free(freelist, freemd, cntlen)

        if freeslot > r:
            print("109?")
            break
        elif freeslot != -1:
            print("112?", freeslot, cntlen)
            for trace in range(freeslot - 1, freeslot - cntlen, -1):
                print("114?")
                hdd[trace] = hdd[r]
                hdd[r] = -1
                r -= 1
            print('trace:', trace)
        else:
            r -= 1
    # checksum
    checksum = 0
    for r in range(0, len(hdd)):
        if hdd[r] != -1:
            checksum += (hdd[r] * r)

    return checksum

with open(sys.argv[1]) as fh:
    ourfs = fh.readline()
    ret1 = part1(ourfs.strip())
    ret2 = part2(ourfs.strip())

    print("part1:", ret1)
    print("part2:", ret2)

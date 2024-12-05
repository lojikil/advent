import sys
import collections

def rules2map(rules):
    ret = {}

    for rule in rules:
        target, dest = rule.split('|')
        print(f"adding rule for {target} -> {dest}")
        if target not in ret:
            ret[target] = []
        ret[target].append(dest)
    return ret

def part1(rules, pages):
    # ok, so
    # . build the rules into a map
    # . iterate through each page
    # .. set the pointer to the first index
    # .. check if there are rules that require an ordering
    # .. the page is valid if
    # ... current index is greater than the search index
    # . for example
    #
    # [source]
    # ----
    # 3|4
    # 3|5
    # 4|5
    #
    # 3,4,5
    # 4,5,3
    # ----
    #
    # on the first iteration, we look up 3
    #
    # . the next pointer is 4, do we have a rule?
    # . yes, 3 must come before 4, cur = 0, ptr = 1
    # . ok, move ptr to 2
    # . is there a rule for 3 & 5? yes
    # . 3 must come before 5, cur = 0, ptr = 2
    # 
    # NOTE: is there a short cut here? if there is *no rule* for a given
    # ordering, then it's false? in fact we don't even have to comapre numbers?
    # I think in orer for that to work, you also need the rule that if you're at 
    # the last item in the list and there are no rules for it, you're done. Oh,
    # or maybe you don't even need to go with it?
    rmap = rules2map(rules)

    cnt = 0
    for rpage in pages:
        page = rpage.split(',')
        found = False
        for idx in range(0,len(page) - 1):
            pv = page[idx]
            if pv not in rmap:
                print('found a page value not in the rules:', pv)
                found = True
                break
            pr = rmap[pv]
            for idx1 in range(idx+1, len(page)):
                pv1 = page[idx1]
                if pv1 not in pr:
                    print('found a page value not in the correct order:', pv1)
                    found = True
                    break
            if found:
                break
        if not found:
            print("page is valid", rpage)
            cnt += int(page[len(page) // 2])
    return cnt

def isvalidpage(page, rmap):
    found = False
    for idx in range(0,len(page) - 1):
        pv = page[idx]
        if pv not in rmap:
            print('%%p2: found a page value not in the rules:', pv)
            return False
        pr = rmap[pv]
        for idx1 in range(idx+1, len(page)):
            pv1 = page[idx1]
            if pv1 not in pr:
                print('%%p2: found a page value not in the correct order:', pv1)
                return False
    return True

def part2(rules, pages):
    rmap = rules2map(rules)
    cnt = 0
    for rpage in pages:
        page = rpage.split(',')
        if isvalidpage(page, rmap):
            continue
        hidx = 0
        pagelen = len(page)
        pagelen_l = pagelen - 1
        swap = False
        while hidx < pagelen_l:
            pv = page[hidx]
            pr = rmap.get(pv, [])
            #print("hidx: ", hidx)
            for tidx in range(hidx + 1, pagelen):
                tpv = page[tidx]
                tpr = rmap.get(tpv, [])
                if tpv not in pr:
                    swap = True
                    break
            if swap:
                #print(f"swapping: {hidx}, {tidx}")
                tmp = page[tidx]
                page[tidx] = page[hidx]
                page[hidx] = tmp
                swap = False
            else:
                hidx += 1
        cnt += int(page[len(page) // 2])
    return cnt

with open(sys.argv[1]) as fh:
    rules = []
    pages = []
    bseen = False
    for line in fh:
        if line == "\n":
            bseen = True
        elif not bseen:
            rules.append(line.strip())
        else:
            pages.append(line.strip())

    print(pages)
    res1 = part1(rules, pages)
    res2 = part2(rules, pages)
    print("part1:", res1)
    print("part2:", res2)

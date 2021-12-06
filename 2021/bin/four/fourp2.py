from collections import OrderedDict
import re

m = re.compile("board: (\d+) won on play: (\d+) (vertically|horizontally); final score is: (\d+)")
g = OrderedDict()

with open('d') as fh:
    for r in fh:
        l = r.strip()
        mgroup = re.match(m, l)
        if mgroup is not None:
            groups = mgroup.groups()
            if groups[0] not in g:
                print(groups[0])
                g[groups[0]] = groups[3]

print(g)

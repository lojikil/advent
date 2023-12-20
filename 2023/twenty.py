import sys
import collections
import functools
from dataclasses import dataclass

class PulseQueue:
    def __init__(self):
        self.high_pulse = 0
        self.low_pulse = 0
        self.storage = []

    def pop_front(self):
        v = self.storage[0]
        self.storage = self.storage[1:]
        return v

    def append(self, v):
        if v.val == 0:
            self.low_pulse += 1
        else:
            self.high_pulse += 1

        self.storage.append(v)

    def __len__(self):
        return len(self.storage)

    def __contains__(self, item):
        return item in self.storage

    def __getitem__(self, item):
        return self.storage[item]

    def __str__(self):
        return str(self.storage)

    def __repr__(self):
        return repr(self.storage)

@dataclass
class FlipFlop:
    name:str
    state:int
    targets:list[str]

    def reset(self):
        self.state = 0

    def pulse(self, m, q):
        hl = m.val
        nv = 0
        if m.val == 0:
            if self.state == 0:
                self.state = 1
                nv = 1
            else:
                self.state = 0
                nv = 0
            for t in self.targets:
                q.append(Signal(self.name, t, nv))

@dataclass
class Broadcast:
    name:str
    targets:list[str]

    def reset(self):
        pass
    def pulse(self, m, q):
        for t in self.targets:
            q.append(Signal(self.name, t, 0))

@dataclass
class Signal:
    _from:str
    to:str
    val:int

    def __str__(self):
        if self.val == 0:
            v = "Low"
        else:
            v = "High"
        return "{0} -{1}-> {2}".format(self._from, v, self.to)

@dataclass
class Conj:
    name:str
    _from:dict[str,int]
    targets:list[str]

    def reset(self):
        for k in self._from.keys():
            self._from[k] = 0

    def pulse(self, m, q):
        self._from[m._from] = m.val

        if 0 in list(self._from.values()):
            nv = 1
        else:
            nv = 0

        for t in self.targets:
            q.append(Signal(self.name, t, nv))

def part1(r):
    workq = PulseQueue()

    for i in range(0, 1000):
        broad = r["broadcaster"]
        broad.pulse(None, workq)
        # one for the button push
        workq.low_pulse += 1

        while len(workq) != 0:
            c = workq.pop_front()
            print(workq)
            print(c)
            if c.to in r:
                t = r[c.to]
                t.pulse(c, workq)

    return workq.low_pulse * workq.high_pulse

def part2(r):
    workq = PulseQueue()
    cnt = 0
    rx_pulse = False
    print("running part 2...")
    for i in r.values():
        i.reset()

    while not rx_pulse:
        broad = r["broadcaster"]
        broad.pulse(None, workq)
        # one for the button push
        workq.low_pulse += 1
        cnt += 1
        while len(workq) != 0:
            c = workq.pop_front()
            if c.to in r:
                t = r[c.to]
                t.pulse(c, workq)
            elif c.to == "rx" and c.val == 0:
                rx_pulse = True

    return cnt

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

rules = {}
with open(sys.argv[1]) as fh:
    data = []
    for line in fh:
        line = line.strip()
        data.append(line)
        rname, connections = line.split(' -> ')
        name = rname[1:]
        if rname[0] == '&':
            rules[name] = Conj(name, {}, connections.split(', '))
        elif rname[0] == '%':
            rules[name] = FlipFlop(name, 0, connections.split(', '))
        else:
            # broadcast
            rules[rname] = Broadcast(rname, connections.split(', '))

    for line in data:
        name, connections = line.split(' -> ')

        if name[0] == '&' or name[0] == '%':
            name = name[1:]

        for connection in connections.split(', '):
            if connection in rules:
                target = rules[connection]
                if type(target) is Conj:
                    target._from[name] = 0

    print(rules)
    print(part1(rules))
    print(part2(rules))

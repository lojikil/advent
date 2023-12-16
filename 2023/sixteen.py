import sys
import collections
import functools
from dataclasses import dataclass

if len(sys.argv) != 2:
    print("usage: {0} [file]".format(sys.argv[0]))
    sys.exit(0)

@dataclass
class Beam:
    direction:(int, int)
    position:(int, int)

    def advance(self):
        self.position = (self.position[0] + self.direction[0],
                         self.position[1] + self.direction[1])

    def right(self):
        self.direction = (0, 1)

    def left(self):
        self.direction = (0, -1)

    def up(self):
        self.direction = (-1, 0)

    def down(self):
        self.direction = (1, 0)

    def outside_p(self, bm):
        if self.position[0] < 0 or \
           self.position[1] < 0 or \
           self.position[0] >= len(bm) or \
           self.position[1] >= len(bm[0]):
            return True
        return False

    def r_p(self):
        return self.direction == (0, 1)

    def l_p(self):
        return self.direction == (0, -1)

    def u_p(self):
        return self.direction == (-1, 0)

    def d_p(self):
        return self.direction == (1, 0)

def get_map(bm, pos):
    #print("get_map:", pos)
    return bm[pos[0]][pos[1]]

def part1(bm, startbeam=None):
    if startbeam is None:
        beams = [Beam((0, 1), (0,0))]
    else:
        beams = [startbeam]
    seen = collections.Counter()
    while len(beams) > 0:
        new_beams = []
        #print(len(beams), beams, seen)
        for beam in beams:
            if beam.outside_p(bm):
                continue
            seen.update([beam.position])
            mpos = get_map(bm, beam.position)
            if mpos == '.':
                beam.advance()
            elif mpos == '-':
                if beam.u_p() or beam.d_p():
                    beam.left()
                    beam.advance()
                    new_beam = Beam((0, 0), beam.position)
                    new_beam.right()
                    new_beam.advance()
                    new_beams.append(new_beam)
                    seen.update([new_beam.position])
                else:
                    beam.advance()
            elif mpos == '/':
                if beam.u_p():
                    beam.right()
                elif beam.d_p():
                    beam.left()
                elif beam.r_p():
                    beam.up()
                elif beam.l_p():
                    beam.down()
                beam.advance()
            elif mpos == '\\':
                if beam.u_p():
                    beam.left()
                elif beam.d_p():
                    beam.right()
                elif beam.r_p():
                    beam.down()
                elif beam.l_p():
                    beam.up()
                beam.advance()
            elif mpos == '|':
                if beam.l_p() or beam.r_p():
                    beam.up()
                    beam.advance()
                    new_beam = Beam((0, 0), beam.position)
                    new_beam.down()
                    new_beam.advance()
                    new_beams.append(new_beam)
                    seen.update([new_beam.position])
                else:
                    beam.advance()
            #if seen[beam.position] <= 100_000:
            if seen[beam.position] <= 100:
                new_beams.append(beam)
        beams = new_beams
    return len(seen)

def part2(bm):
    # find the local maximum beam
    energized = []
    right = len(bm[0]) - 1
    bottom = len(bm) - 1
    for i in range(0, len(bm)):
        energized.append(part1(bm, Beam((0, 1), (i, 0))))

    for i in range(0, len(bm)):
        energized.append(part1(bm, Beam((0, -1), (i, right))))

    for i in range(0, right):
        energized.append(part1(bm, Beam((1, 0), (0, i))))

    for i in range(0, bottom):
        energized.append(part1(bm, Beam((-1, 0), (bottom, i))))

    return max(energized)

with open(sys.argv[1]) as fh:
    beammap = []
    for line in fh:
        beammap.append(line.strip())
    print(part1(beammap))
    print(part2(beammap))

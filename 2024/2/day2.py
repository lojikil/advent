#!/usr/bin/env python3

from itertools import pairwise


def is_safe_report(report, min_delta=1, max_delta=3):
	deltas = (i - j for i, j in pairwise(report))
	vector_shapes = {(d >= 0, min_delta <= abs(d) <= max_delta) for d in deltas}
	return len(vector_shapes) == 1 and vector_shapes.pop()[1]


def solve_one(lines):
	return sum(
		is_safe_report(int(i) for i in line.split())
		for line in lines
	)


def solve_two(lines):
	return sum(
		any(
			is_safe_report(report[:i] + report[i + 1:])
			for i in range(len(report))
		)
		for report in ([int(i) for i in line.split()] for line in lines)
	)


if __name__ == '__main__':
	import os
	
	cwd = os.path.dirname(os.path.realpath(__file__))
	
	inputs = [
		open(os.path.join(cwd, f)).read()
		for f in ('test.txt', 'input.txt')
	]
	
	inputs = [
		[l[:-1] for l in open(os.path.join(cwd, f)).readlines()]
		for f in ('test.txt', 'input.txt')
	]
	
	print('One:\n\t' + '\n\t'.join(str(solve_one(i)) for i in inputs))
	print('Two:\n\t' + '\n\t'.join(str(solve_two(i)) for i in inputs))

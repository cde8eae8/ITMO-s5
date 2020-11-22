import os
import os.path
import itertools
from subprocess import Popen, PIPE
import random

def rand():
    return ''.join(chr(random.randint(ord('a'), ord('z'))) for i in range(10))

class Shell:
    def __init__(self, cmd):
        self.cmd = cmd
        s = rand()
        self.stdinfilename = f"tmpfile{cmd.replace(' ', '').replace('/', '')}{s}"
        self.stdoutfilename = f'tmpfile-out{s}'
        self.stdin = open(self.stdinfilename, 'w')

    def input(self, data, end='\n'):
        self.stdin.write(str(data))
        self.stdin.write(end)

    def run(self):
        self.stdin.flush()
        self.stdin.close()
        cmd = f'{self.cmd} < {self.stdinfilename} > {self.stdoutfilename}'
        os.system(cmd)

    def readlines(self):
        with open(self.stdoutfilename, 'r') as f:
            return f.readlines()

class ChainList:
    def __init__(self, lists, exc):
        self.lists = [v for i, v in enumerate(lists) if v != exc]
        self.exc = exc

    def __iter__(self):
        return itertools.chain(*self.lists)

    def __len__(self):
        return sum(len(l) for l in self.lists)

def k_split(data, k):
    uniq_values = set(v.y for v in data)
    classes_map = {val:i+1 for i, val in enumerate(uniq_values)}
    cmd = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'cmake-build-debug/codeforces-A')
    shell = Shell(cmd)
    s = f'{len(data)} {len(uniq_values)} {k}'
    shell.input(s)
    shell.input(' '.join(str(classes_map[v.y]) for v in data))
    shell.run()
    parts = []
    lines = list(shell.readlines())
    for line in lines:
        new_part = [data[int(i) - 1] for i in line.split()[1:]]
        parts.append(new_part)
    return [(parts[k],
             list(itertools.chain(*(v for i, v in enumerate(parts) if i != k)))) for k in range(len(parts))]

def query(queries, data, kern, dist, win, win_w):
    cmd = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'cmake-build-debug/codeforces-C')
    shell = Shell(cmd)
    s = f'{len(data)} {len(data[0].xs)}'
    shell.input(s)
    for record in data:
        shell.input(' '.join(str(v) for v in record.xs), end=' ')
        shell.input(record.y)
    shell.input(dist)
    shell.input(kern)
    shell.input(win)
    shell.input(str(win_w))
    shell.input(len(queries))
    for record in queries:
        shell.input(' '.join(str(v) for v in record.xs))
    shell.run()
    return [float(i) for i in list(shell.readlines())]

class F:
    def __init__(self, v):
        self.y = v

    def __repr__(self):
        return str(self.y)

#v = [1, -1, 2, 3, 0, 1, 3, 1, 1, 0, -1, 1, 1, 1, 2, 0, 0, 1, 0, -1, 0, 0, -1, 2, 1, 3, 0, -1, 0, 2, 2, 1, 1, 3, 3, 2, 1, 2, 3, 0, 3, -1, 3, 0, 0, 1, 1, 1, -1, 1, 2, -1, 2, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 3, 3, -1, 0, 2, 1, 0, 1, 1, 3, 1, 2, 2, -1, -1, 0, -1, 1, 0, -2, 3, 1, 3, 0, -2, 0, 1, 0, 0, 1, 1, 0, 1, 2, 0, 2, 1, 1, 3, 0, 2, 0, 2, -1, 1, 0, -1, 1, -1, 0, 1, 0, 3, 0, 1, 0, 1, 2, 3, 0, 0, 3, -2, 0, 0, -1, 1, 0, 0, 1, 1, 1, 0, 3, 1, -1, 0, 0, 2, 1, 3, 0, 1, 2, 1, 3, 0, 1, 3, -1, 1, -1, 3, 0, 3, -1, 0, 2, 1, 1, 0, -1, 2, -1, 0, -1, 3, 0, 1, 1, 1, 1, 2, 2, 0, 3, 2, -2, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, -2, 0, 0, 2, 2, -1, 0, 2, 0, 2, 2, 0, -1, 2, 0, 3, 3, 0, 2, 2, 1, 2, -1, 1, 1, -1, 1, 3, 3, 2, 1, 1, 3, 1, 0, 0, 0, -1, -1, 2, 0, 0, 2, 2, 1, 0, -1, -1, 0, 0, 0, 2, 3, 2, 2, -1, 0, -1, 0, 1, 3, 0, 3, 2, 0, 2, 3, -1, 0, 0, 0, 2, -2, 2, 0, 0, 3, 3, 2, -1, 3, 2, 1, 0, 1, 1, -1, 0, 0, 1, 0, 1, 2, 0, 1, 0, 1, 1, -1, 2, -1, -1, 0, 1, 0, -1, 0, 0, 2, 2, 2, -1, 1, 0, 1, -1, 0, -2, 0, 3, 2, 1, 3, 2, 0, 0, 1, -1, 1, 0, 2, 2, 1, 1, 1, -2, 3, 2, 2, 2, 1, 1, 2, 0, 3, -1, -1, 0, -2, 0, 0, 1, 2, -1, 2, 1, 3, 2, 0, 3, 0, 3, 2, 1, 2, -2, 1, 0, 2, 1, 1, 2, 3, 3, 0, 0, 1, 3, 2, 1, 1, 0, 3, 1, 3, 1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 3, 2, 0, 2, 2, 2, 0, 0, 1, -1, 3, 1, 3, -1, 0, 1, 0, 1, 1, 1, 3, 0, 0, 0, 0, 2, 0, 0, 0, 1, 1, 2, 1, 0, 3, -1, 0, -1, -1, 1, 2, 0, 0, 0, 1, 3, 0, 1, 0, -1, 1, 3, 2, -1, 3, 2, -1, 0, 0, 1, 0, 1, 0, 0, 0, 1, -1, 3, 3, 3, 1, 0, 3, 0, 1, 1, 1, 2, 2, 3, 3, 0, -2, 1, -1, 3, 2, -1, 0, 2, 1, 0, 0, 0, 1, 0, 0, 2, 0, 3, 3, 0, 1, 0, 1, 3, 3, 2, 3, 1, 0, 3, -1, 2, 0, -1, -1, 0, 2, 0, 3, 3, 2, 0, 0, 1, 1, 0, 3, 1, 0, 1, 1, 0, 0, 0, 3, 3, 1, 1, 2, 3, 1, 2, 1, 2, 1, -1, 2, 0, 2, 2, 0, 0, 2, 2, 1, 0, 1, -1, 0, 2, -1, 0, 0, 2, 0, 1, 1, 1, 0, 2, -1, 0, -1, 1, 1, 3, 0, -1, -2, 0, 1, 3, 1, 2, 0, -1, 2, 0, 2, -1, 1, 2, 1, 2, 0, -1, 2, 0, -1, 3, 1, -1, 3, -1, 1, 2, 0, 0, 0, 1, 1, 0, 0, 2, -1, -1, 1, -1, 1, 1, 3, 0, 1, 0, 0, 0, 2, 3, 0, 3, 3, 0, 0, 2, 0, 0, 1, 0, 0, 1, 3, -1, 1, 0, 1, 0, 1, 0, 2, 0, 3, 0, 0, -2, 0, 3, 3, 0, 3, 0, 1, 2, 0, 1, 0, 2, 1, 1, 1, 3, 2, 1, 0, 0, 3, -1, 3, 1, -1, 1, 0, -1, 0, 3, 0, 3, 2, 1, 2, 3, 0, 1, 3, 2, -1, 0, 1, 1, 1, 1, 0, 0, 2, 2, 0, 2, 1, 0, 1, 3, 3, -1, 0, 3, 1, -1, 2, 3, 1, 1, -1, 3, 2, 0, 0, 3, 0, 3, 1, 1, 0, 0, 1, -1, 0, 0, 2, 0, 3, 1, 3, -1, 0, 3, 1, 0, -1, 0, 0, 0, 0, 3, 0, -1, 1, 0, 1, 2, 1, 0, 0, 2, 2, 0, 2, 2, 2, 3, 1, 3, 0, 0, 2, 2, 1, 1, 0, 2, 1, 1, 2, 1, 0, 0, 1, 0, 3, 3, 1, 1, -1, 1, 3, 1, 0, 0, 1, 3, 3, 0, 3, 1, 0, -1, 0, -1, 3, 1, 1, 2, 0, 2, 0, -1, -1, 2, 3, 1, 0, -1, 2, 0, 1, 1, 1, 2, 2, -1, 2, -1, 1, 2, 2, 0, 0, 2, 3, 3, 0, 0, -1, 2, 0, 0, -1, 1, 3, 2, 0, 0, -3, 0, 1, 1, 3, -1, 0, 0, 3, 0, 3, -1, 1, 1, 1, 1, 3, 0, -1, 0, 0, 3, 1, 2, 1, 0, 1, 0, 1, 1, -1, 0, 0, 0, 2, 1, 0, 1, 2, 0, 0, 3, 0, 1, 3, 2, 1, 1, 0, 1, 1, 1, 1, 3, 1, 3, 0, 3, 1, 3, 0, 1, 3, 2, 2, -1, 0, 1, 1, 0, 2, 3, 0, 0, 3, 1, 1, 1, 1, 0, 3, 3, 0, 0, 3, 0, 2, 0, -1, -2, 2, -1, -1, 2, 0, 2, 0, 1, 0, 3, 0, 3, -1, 2, 0, 2, 2, 1, 0, 0, 1, 1, 3, 0, 0, 0, 2, -1, 0, 3, 1, 0, 3, -1, 0, 0, 1, 1, 1, 1, 0, 3, -1, 0, 1, 1, 1, -1, 0, 2, 1, 0, 0, 0, 0, 1, 1, 2, 0, 1, -1, 1, 2, 0, 0, -1, 0, 0, 0, 3, -3]
#v = list(range(10))
#l = k_split([F(i) for i in v], 10)

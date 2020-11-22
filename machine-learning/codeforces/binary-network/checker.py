
from itertools import *
nargs = 10

args = [[]]
for i in range(10):
    args = [arg + [1] for arg in args] + [arg + [0] for arg in args]
args = list(args)
print(args)

with open('input.txt', 'r') as f:
    l = f.readlines()
    n = int(l[0])
    l = [int(l[i + 1]) for i in range(n)]
print(l)



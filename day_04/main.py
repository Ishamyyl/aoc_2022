from pathlib import Path
from itertools import permutations

# print(list(permutations([11, 12, 13, 14, 22, 23, 24, 33, 34, 44], 2)))

with Path("input.txt").open() as f:
    acc = 0
    for l in f:
        l = l.strip()
        a, b = map(lambda x: list(map(int, x.split("-"))), l.split(","))
        r = (a[0] <= b[0] and a[1] >= b[1]) or (a[0] >= b[0] and a[1] <= b[1])
        if r:
            acc += 1
    print("part 1:", acc)


with Path("input.txt").open() as f:
    acc = 0
    for l in f:
        l = l.strip()
        a, b = map(lambda x: list(map(int, x.split("-"))), l.split(","))
        r = (
            (a[0] <= b[0] and a[1] >= b[0])
            or (a[0] <= b[1] and a[1] >= b[1])
            or (b[0] <= a[1] and b[1] >= a[1])
            or (b[0] <= a[1] and b[1] >= a[1])
        )
        if r:
            acc += 1
    print("part 2:", acc)

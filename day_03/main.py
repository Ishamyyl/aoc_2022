from pathlib import Path

LOWERCASE_OFFSET = ord("a") - 1
UPPERCASE_OFFSET = ord("A") - 1


def priority(c):
    if c > LOWERCASE_OFFSET:
        return c - LOWERCASE_OFFSET
    else:
        return c - UPPERCASE_OFFSET + 26


with Path("input.txt").open() as f:
    acc = 0
    for l in f:
        l = l.strip()
        midpoint = len(l) // 2

        # minor optimization: due to the invariant, we could use an early return pattern.
        # but that would still be linear on amortized time, since the inputs are random.
        # however, the extreme simplicity of this approach aint worth changing anything, on these scales.
        c = set(l[:midpoint]) & set(l[midpoint:])

        # guaranteed by the invariant placed on the input to have exactly 1 intersecting element
        c = ord(c.pop())
        acc += priority(c)
    print("part 1:", acc)


with Path("input.txt").open() as f:
    acc = 0
    for l in f:
        l = l.strip()

        # input invariant: guaranteed to have exactly 3 available lines per sequence
        c = set(l) & set(next(f)) & set(next(f))

        # input invariant: guaranteed to have exactly 1 intersecting element
        c = ord(c.pop())
        acc += priority(c)
    print("part 2:", acc)

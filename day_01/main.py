import pathlib

totals = []
with pathlib.Path("input.txt").open() as f:
    count = 0
    for l in f:
        l = l.strip()
        if l == "":
            totals.append(count)
            count = 0
            continue
        count += int(l)

totals.sort(reverse=True)

print("part 1:", sum(totals[0:1]))
print("part 2:", sum(totals[0:3]))

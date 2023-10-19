from pathlib import Path
from re import compile
from copy import deepcopy

initial_stacks = [
    list("QMGCL"),
    list("RDLCTFHG"),
    list("VJFNMTWR"),
    list("JFDVQP"),
    list("NFMSLBT"),
    list("RNVHCDP"),
    list("HCT"),
    list("GSJVZNHP"),
    list("ZFHG"),
]

test_stacks = [
    list("ZN"),
    list("MCD"),
    list("P"),
]

extract = compile("^move (\d+) from (\d+) to (\d+)$")


# (!) modifies the given lists in-place as a side effect
def pop_append_multiple(stacks: list[list[str]], file_name: str) -> str:
    for n, src, dst in parse_file_to_stacks_indexes(file_name):
        src = stacks[src - 1]
        dst = stacks[dst - 1]
        for _ in range(n):
            dst.append(src.pop())
    return "".join(map(lambda x: x[-1], stacks))


# (!) modifies the given lists in-place as a side effect
def move_slice(stacks: list[list[str]], file_name: str) -> str:
    for n, src, dst in parse_file_to_stacks_indexes(file_name):
        src = stacks[src - 1]
        dst = stacks[dst - 1]
        dst.extend(src[-n:])
        del src[-n:]
    return "".join(map(lambda x: x[-1], stacks))


def parse_file_to_stacks_indexes(file_name: str) -> str:
    with Path(file_name).open() as f:
        for l in f:
            l = l.strip("\n")
            count, source, dest = map(int, extract.findall(l)[0])
            yield count, source, dest


print("part 1:", pop_append_multiple(deepcopy(initial_stacks), "input.txt"))
print("part 2:", move_slice(deepcopy(initial_stacks), "input.txt"))

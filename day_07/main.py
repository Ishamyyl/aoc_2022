from pathlib import Path
from pprint import pprint
from functools import reduce


def iter_file(file_name: str):
    with Path(file_name).open() as f:
        for l in f:
            yield l.strip("\n")


dirs = {"size": 0, "dirs": {}}


def string_of_path(p: list[str]):
    return "/".join(p)


def r_dir_func(r, x):
    return r["dirs"][x]


def r_size_func(x):
    return r_dir_func


def parse(file_name: str):
    p = []
    for l in iter_file(file_name):
        l = l.split(" ")
        d = reduce(r_dir_func, p, dirs)
        print(d)
        match l[0]:
            case "$":
                match l[1]:
                    case "cd":
                        dn = l[2]
                        if dn == "..":
                            p.pop()
                        else:
                            p.append(dn)
                            if dn not in d["dirs"]:
                                d["dirs"][dn] = {"size": 0, "dirs": {}}
                    case "ls":
                        pass
            case "dir":
                pass
            case x:
                reduce(r_size_func(int(x)), p, dirs)

    pprint(dirs)


midsize_breakpoint = 100000


def sum_dirs(d: dict, n: int) -> int:
    for k, v in d.items():
        print(k, v)
    return 0


parse("test.txt")
# print(sum_dirs(dirs, midsize_breakpoint))

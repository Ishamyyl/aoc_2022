def win_lose_score(a, b):
    match (a, b):
        case ("A", "Z") | ("C", "Y") | ("B", "X"):
            return 0
        case ("A", "X") | ("B", "Y") | ("C", "Z"):
            return 3
    return 6


def play_score(a):
    match a:
        case "A" | "X":
            return 1
        case "B" | "Y":
            return 2
        case "C" | "Z":
            return 3
    return 0


def strat(opp, out):
    match (opp, out):
        case ("A", "Y") | ("B", "X") | ("C", "Z"):
            return "X"
        case ("A", "Z") | ("B", "Y") | ("C", "X"):
            return "Y"
        case ("A", "X") | ("B", "Z") | ("C", "Y"):
            return "Z"


with open("input.txt") as f:
    acc = 0
    for l in f:
        opp, me = l.strip().split(" ")
        acc += win_lose_score(opp, me) + play_score(me)
    print("part 1:", acc)

with open("input.txt") as f:
    acc = 0
    for l in f:
        opp, me = l.strip().split(" ")
        me = strat(opp, me)
        acc += win_lose_score(opp, me) + play_score(me)
    print("part 2:", acc)

with open("input.txt") as f:
    acc = 0
    for l in f:
        opp, me = l.strip().split(" ")
        opp = 1 << ord(opp) - ord("A")
        me = ord(me) - ord("X")
        acc += me + 1  # play score
        me = 1 << me
        if opp == me:  # draw score
            acc += 3
            continue
        me = (me >> 1 | me << 2) & 0b111  # rotate right 1 and mask to 3 bits
        if opp == me:  # win score
            acc += 6
    print("part 1:", acc)

with open("input.txt") as f:
    acc = 0
    for l in f:
        opp, me = l.strip().split(" ")
        opp = 1 << ord(opp) - ord("A")
        me = ord(me) - ord("X")
        acc += me * 3  # round score
        me = 1 << me
        if opp == me:
            acc += 1  # play Rock score
            continue
        me = (me >> 1 | me << 2) & 0b111  # rotate right 1 and mask to 3 bits
        if opp == me:
            acc += 2  # play Paper score
            continue
        acc += 3  # play Scissors score
    print("part 2:", acc)

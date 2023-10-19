## In a stream of characters, the Header is 4 unique chars.
## Part 1) Given the input, what is the index of the char after the Header, or how many characters need to be processed to consume the Header?

# from python import pathlib
# from python import mmap

# import bytearray

import pathlib
import mmap

# from pathlib import Path
# from mmap import mmap


def sliding_window_file(file_path: str, buf_size: int) -> int:
    with pathlib.Path(file_path).open("r+b") as f:
        # we don't care about the actual characters, just unique bytes
        seen = bytearray(buf_size)
        # seen = List[int](buf_size)
        # since input is a file, mmap allows us to read the file without loading it to memory
        with mmap.mmap(f.fileno(), 0) as mm:
            # since we want to return the position of the end, we start relative to there rather than relative to 0, looking behind
            # +1 for right-inclusive slice, to reach the end of the file
            for pos in range(buf_size, len(mm) + 1):
                is_unique = True  # flag for whether to stop searching or not
                for c in mm[pos - buf_size : pos]:  # lookbehind
                    if c in seen:  # has been seen, therefore not unique to this window
                        seen.clear()  # start over, then ...
                        is_unique = False
                        break  # ... slide to next window
                    seen.append(c)  # unseen is now seen
                # did we finish checking all `buf_size` elements without finding a duplicate?
                if is_unique:
                    return pos
                # no? slide to the next window


print("part 1:", sliding_window_file("input.txt", 4))
print("part 2:", sliding_window_file("input.txt", 14))

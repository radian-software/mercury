"""
Your standard util module for anything that's needed by more than one
other module.
"""

import sys
import time

import xdg


# Directory where all the Mercury files are stored by default.
MERCURY_DIR = xdg.XDG_DATA_HOME / "mercury"

# Timestamp for when module is imported, used to display relative
# times in log messages.
START_TIME = time.time()


def is_sorted(lst, key=id):
    """
    Check if the list is in sorted order, optionally mapping every
    element using the provided key first. Return a boolean.
    """
    return all(key(lst[i]) <= key(lst[i + 1]) for i in range(len(lst) - 1))


def merge_sorted_seqs(left, right, key=id):
    """
    Merge sequences in sorted order. left and right are iterables
    which should be sorted according to the provided key. Return an
    iterable of all the items from both sequences interleaved in
    sorted order.
    """
    sentinel = object()
    left_iter = iter(left)
    right_iter = iter(right)
    cur_left = next(left_iter, sentinel)
    cur_right = next(right_iter, sentinel)
    while cur_left is not sentinel or cur_right is not sentinel:
        if cur_right is sentinel or key(cur_left) <= key(cur_right):
            yield cur_left
            cur_left = next(left_iter, sentinel)
        else:
            yield cur_right
            cur_right = next(right_iter, sentinel)


def log(fmt, *args, **kwargs):
    """
    Log message to stderr with relative timestamp. Same arguments as
    format.
    """
    print(
        "[{:09.2f}s]".format(time.time() - START_TIME),
        fmt.format(*args, **kwargs),
        file=sys.stderr,
    )

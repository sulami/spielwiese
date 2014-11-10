#!/usr/bin/env python3

def c2h(i):
    if i > 255 ** 4:
        raise Exception("ARGB Overflow")
    return '{:08x}'.format(i ^ 0xff000000)

print(c2h(993455))


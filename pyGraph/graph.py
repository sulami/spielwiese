#!/usr/bin/env python3
# coding: utf-8

from sys import argv

BLOCKS = ( ' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█' )

def to_float(i):
    try:
        return float(i)
    except ValueError:
        print('Error: Argument "{}" is not a number'.format(i))
        exit(2)

def graph(args, height=1, sign='#'):
    max = to_float(args[0])
    normalized = []

    for a in args:
        a = to_float(a)
        if a > max:
            max = a

    for a in args:
        a = to_float(a)
        normalized.append(a/max*100)

    b = lambda v, h: int(round(v*h/100, 0))

    if height == 1:
        for v in normalized:
            print(BLOCKS[b(v, len(BLOCKS)-1)], end='')
        print()
    elif height > 1:
        for l in range(height):
            for v in normalized:
                print(sign if b(v, height) >= height-l else ' ', end='')
            print()
    else:
        exit(1)

def main():
    if len(argv) <= 2:
        exit(1)
    graph(argv[1:])

if __name__ == '__main__':
    main()


#!/usr/bin/env py3
# coding: utf-8

from sys import argv

BLOCKS = ( '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█' )

def to_float(i):
    try:
        return float(i)
    except ValueError:
        print('Error: Argument "{}" is not a number'.format(i))
        exit(2)

def graph(args, height=1, sign='#'):
    min = to_float(args[0])
    max = to_float(args[0])
    values = []
    normalized = []

    for a in args:
        a = to_float(a)
        values.append(a)
        if a < min:
            min = a
        if a > max:
            max = a

    for v in values:
        normalized.append(v/max*100)

    for i, v in enumerate(values):
        print(BLOCKS[int(round(normalized[i]*8/100, 0))-1], end='')
    print()

def main():
    if len(argv) <= 2:
        exit(1)
    graph(argv[1:], 1)

if __name__ == '__main__':
    main()


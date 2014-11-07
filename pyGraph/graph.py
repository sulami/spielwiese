#!/usr/bin/env py3
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

    if height == 1:
        for v in normalized:
            print(BLOCKS[int(round(v*(len(BLOCKS)-1)/100, 0))], end='')
        print()
    elif height > 1:
        for l in range(height):
            for v in normalized:
                print(sign if int(round(v*height/100, 0)) >= height-l
                           else ' ', end='')
            print()
    else:
        exit(1)

def main():
    if len(argv) <= 2:
        exit(1)
    graph(argv[1:])

if __name__ == '__main__':
    main()


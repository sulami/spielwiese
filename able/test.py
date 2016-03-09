#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from csv import reader, writer
from errno import EINVAL
from re import sub
from sys import argv


def work(row):
    row.pop(2)

    return [sub(r"e(?P<middle>\w)e", r"a\1a", cell)
            for _, cell in enumerate(row)]


def main():
    if len(argv) is not 2:
        print("Usage: {} <filename>".format(argv[0]))
        exit(EINVAL)

    with open(argv[1], 'r') as in_file, open('output.csv', 'w') as out_file:
        input_reader = reader(in_file)
        output_writer = writer(out_file)

        for row in input_reader:
            output_writer.writerow(work(row))


if __name__ == '__main__':
    main()

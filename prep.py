#!/usr/bin/env python

from sys import stdin
from re  import compile, match

regex = compile('\w=')
code = []
var = []

for line in stdin:
    code.append(line)
    if regex.match(line):
        v = line.split('=')[0]
        if v not in var:
            var.append(v)

for v in var:
    print('int {}=0;'.format(v))

for line in code:
    print(line, end='')


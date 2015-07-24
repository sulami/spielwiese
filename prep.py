#!/usr/bin/env python3

from sys import stdin
from re  import compile, match

regex = compile('[a-zA-Z0-9]+=')
code = []
var = []

for line in stdin:
    code.append(line)
    if regex.match(line):
        v = line.split('=')[0]
        if v not in var:
            var.append(v)

for v in var:
    print('var {}=bigInt(0);'.format(v))

for line in code:
    print(line, end='')


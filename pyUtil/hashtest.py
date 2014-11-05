#!/usr/bin/env python

"""
" Find and list collisions for hash algorithms
"""

import hashlib

HASH = hashlib.md5
LIMIT = 10
COUNT = 1000000

l = {}
f = {}

def hash(i):
    m = HASH()
    m.update(i)
    l[i] = m.hexdigest()[:LIMIT]

def check_for_collision():
    for key, value in l.items():
        if value not in f:
            f[value] = [key]
        else:
            f[value].append(key)

    for key, value in f.items():
        if len(value) > 1:
            print(key, value)

def main():
    print('Testing {} iterations for {} limited to {} characters'.format(
          COUNT, HASH.__name__, LIMIT))
    for i in xrange(COUNT):
        hash(b'{}'.format(i))
    check_for_collision()

if __name__ == '__main__':
    main()


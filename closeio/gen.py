#!/usr/bin/env python
# -*- coding: utf-8 -*-

x = [1,2,3,"foo",4,5,"bar",6,7]
  # [1,_,3,_____,4,5,_____,_,7]

def gen(arg):
    for index, item in enumerate(arg):
        if isinstance(item, int) and item % 2 or not index % 2:
            yield item

print([i for i in gen(x)])

print([item for index,item in enumerate(x) if isinstance(item, int) and item % 2 or not index % 2])


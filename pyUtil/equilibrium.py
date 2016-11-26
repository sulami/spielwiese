#!/usr/bin/env python

def solution(data):
    """This solves the equilibrium problem"""
    if not data:
        return -1
    sum_prefix = 0
    sum_suffix = sum(data[1:])
    if sum_suffix == 0:
        return 0
    for i in range(len(data) - 1):
        sum_prefix += data[i]
        sum_suffix -= data[i+1]
        if sum_prefix == sum_suffix:
            return i + 1
    return -1

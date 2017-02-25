def flatten(l):
    if not l:
        return l
    if type(l[0]) == list:
        return flatten(l[0]) + flatten(l[1:])
    return [l[0]] + flatten(l[1:])


def test(case):
    print '{} => {}'.format(str(case), flatten(case))


test([])
test([1])
test([1,2,3])
test([1,[2,3]])
test([1, [], 3])
test([1, [2, [], 3, [[[]], 4, 5]], [], 6])

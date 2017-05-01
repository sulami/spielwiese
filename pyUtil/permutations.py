def permutations(lst):
    if not lst or len(lst) == 1:
        return [lst]

    perms = []
    for i in range(len(lst)):
        xs = lst[:]
        x = xs.pop(i)
        perms += [[x] + rest for rest in permutations(xs)]

    return perms

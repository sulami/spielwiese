def forSum(l):
    retval = 0
    for i in range(len(l)):
        retval += l[i]
    return retval

def whileSum(l):
    retval = 0
    i = 0
    while i < len(l):
        retval += l[i]
        i += 1
    return retval

def recSum(l, i=0):
    if len(l) > 1:
        return l[0] + recSum(l[1:])
    else:
        return l[0]


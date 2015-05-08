def combo(a, b):
    r = []
    for i in range(max(len(a), len(b))):
        if i <= len(a):
            r.append(a[i])
        if i <= len(b):
            r.append(b[i])
    return r


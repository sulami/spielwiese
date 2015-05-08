# This is fairly wonky and not really working as desired. Works fine when
# only using single digits, but cannot handle multi-digit numbers properly.
def do(s, n, r, l):
    if n < 9:
        n += 1
        l = do(s+' + '+str(n), n, r+n, l)
        l = do(s+' - '+str(n), n, r-n, l)
        if len(s) > 3:
            if s[-3] == '+':
                l = do(s+str(n), n, r-(n-1)+(n-1)*10+n, l)
            elif s[-3] == '-':
                l = do(s+str(n), n, r-(n-1)-(n-1)*10-n, l)
        else:
            l = do(s+str(n), n, r-(n-1)+(n-1)*10+n, l)
        return l
    else:
        if r == 10:
            l.append(s + ' = 10')
        elif r == 10:
            l.append(s + ' = 10')
        return l

def cal():
    l = do('1', 1, 1, [])
    for i in range(len(l)):
        print(l[i])


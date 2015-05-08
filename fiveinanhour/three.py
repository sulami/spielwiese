def fib():
    f = [0, 1]
    for i in range(98):
        f.append(f[-2] + f[-1])
    return f


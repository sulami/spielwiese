def pype(x, *fs):
    """
    Pipe function. Takes an initial value and any number of functions/methods.
    Methods as strings. Additional args are supported for functions & methods
    by suppling a step as a tuple/list with function/method as the first
    element and the args as the rest. The pipe input is used as the last
    argument in this case. Currently no kwargs.
    """
    while fs:
        f = fs[0]
        args = []
        if isinstance(f, (list, tuple)):
            args = list(f[1:])
            f = f[0]
        if isinstance(f, str):
            if f.startswith('.'):
                x = getattr(x, f[1:])(*args)
            else:
                x = x[f]
        elif isinstance(f, int):
            x = x[f]
        else:
            x = f(*args + [x])
        fs = fs[1:]
    return x

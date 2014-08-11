def table(a=12, b=12):
    for i in xrange(12):
        for j in xrange(12):
            print "%3d" % ((i + 1) * (j + 1)),
        print

if __name__ == "__main__":
    table()


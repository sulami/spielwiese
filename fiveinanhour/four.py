def large(l):
    nums = {}
    numl = []
    for i in range(len(l)):
        nums[int(str(l[i])[0])] = l[i]
        numl.append(int(str(l[i])[0]))
    numl.sort()
    numl.reverse()
    for i in range(len(numl)):
        numl[i] = str(nums[numl[i]])
    return ''.join(numl)


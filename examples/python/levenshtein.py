import time
def timer(f, *args):
    start = time.time()
    retval = f(*args)
    print "Time:", time.time() - start
    return retval

def ld0(s1, s2):
    if len(s1) == 0: return len(s2)
    if len(s2) == 0: return len(s1)
    if s1[0] == s2[0]:
        return min( ld0(s1[1:], s2[1:]) + 0,
                    ld0(s1, s2[1:]) + 1,
                    ld0(s1[1:], s2) + 1)
    else:
        return min( ld0(s1[1:], s2[1:]) + 1,
                    ld0(s1, s2[1:]) + 1,
                    ld0(s1[1:], s2) + 1)

def ld1(s1, s2):
    if len(s1) == 0: return len(s2)
    if len(s2) == 0: return len(s1)
    cost = 0 if (s1[0] == s2[0]) else 1
    return min( ld1(s1[1:], s2[1:]) + cost,
                ld1(s1, s2[1:]) + 1,
                ld1(s1[1:], s2) + 1)

def memoize(f):
    def m(*args):
        if args not in cache:
            cache[args] = f(*args)
        return cache[args]
    return m

@memoize
def ld2(s1, s2):
    if len(s1) == 0: return len(s2)
    if len(s2) == 0: return len(s1)
    cost = 0 if (s1[0] == s2[0]) else 1
    return min( ld2(s1[1:], s2[1:]) + cost,
                ld2(s1, s2[1:]) + 1,
                ld2(s1[1:], s2) + 1)

def ld3(s1, s2):
    global cache
    cache = {}
    return ld2(s1, s2)


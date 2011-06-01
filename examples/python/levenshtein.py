def memoize(f):
    def memoized_f(x, y):
        if (x,y) not in cache:
            cache[(x, y)] = f(x, y)
        return cache[(x, y)]
    return memoized_f

def levenshteinDistance(s1, s2):
    global cache
    cache = {}
    m = len(s1)
    n = len(s2)
    cache = {}
    @memoize
    def dist(m, n):
        if m == 0: return n
        if n == 0: return m
        cost = 0 if (s1[m - 1] == s2[n - 1]) else 1
        return min( dist(m - 1, n - 1) + cost,
                    dist(m, n - 1) + 1,
                    dist(m - 1, n) + 1)
    return dist(m, n)

ld = levenshteinDistance



def sort2(L):
    if len(L) < 2: return L
    return merge(sort2(L[:len(L)//2]),
                 sort2(L[len(L)//2:]))

def merge(L1, L2):
    retval = []
    p1 = 0
    p2 = 0
    while p1 < len(L1) and p2 < len(L2):
        if L1[p1] < L2[p2]:
            retval.append(L1[p1])
            p1 += 1
        else:
            retval.append(L2[p2])
            p2 += 1
    if p1 < len(L1):
        retval = retval + L2[p2:]
    else:
        retval = retval + L1[p1:]
    return retval

sort2([5, 2, 8, 1, 6, 3, 1, 9])
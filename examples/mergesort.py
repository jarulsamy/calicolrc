

def mergesort(L):
    if len(L) < 2: return L
    return merge(mergesort(L[:len(L)//2]),
                 mergesort(L[len(L)//2:]))

def merge(L1, L2):
    if len(L1) == 0: return L2    
    if len(L2) == 0: return L1
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
    if p1 == len(L1):
        retval += L2[p2:]
    else:
        retval += L1[p1:]
    return retval

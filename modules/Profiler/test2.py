import sys
import clr
clr.AddReference("../Profiler.dll")

if sys.platform == 'cli':
    import pyprof

def fact(n):
    if n == 1:
        return 1
    return fact(n - 1) * n

fact(5)

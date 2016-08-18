import numpy.oldnumeric as MyNumeric

#log = open("numeric.log", "w")
#log = sys.stdout

def wrapper(f, path):
    def newf(*args, **kwargs):
        print >> log, "NUMERIC: %s(%s, %s)" % (path, args, kwargs)
        retval = f(*args, **kwargs)
        print >> log, "returns", retval
        #if type(retval) in [int, float]:
        return retval
        #else:
        #    return Wrapper(retval, path)
    return newf

class Wrapper:
    def __init__(self, orig, name):
        self.orig = orig
        self.name = name

    def __getattr__(self, item):
        print >> log, "Using: " + self.name + ": " + item + "..."
        a = getattr(self.orig, item)
        return wrapper(a, self.name + "." + item)
        ## NUMERIC: zeros((2, 'f'), {})
        ## NUMERIC: zeros(((2,7), 'f'), {})
        #return Array([0] * size)

    def __call__(self, *args, **kwargs):
        return self.orig(*args, **kwargs)

    def _add(self):
        print >> log, "Using: Numeric_add ..."
        return Wrapper(MyNumeric_add, "Numeric_add")

    def _multiply(self):
        print >> log, "Using: Numeric_multiply ..."
        return Wrapper(MyNumeric_multiply, "Numeric_multiply")

    def __repr__(self):
        return self.name

    add = property(_add)
    multiply = property(_multiply)

#Numeric = Wrapper(MyNumeric, "Numeric")

// C# Numeric replacement

// List
using System.Collections.Generic;
// IList
using System.Collections;

public static class Extensions
{
	public static T[] Slice<T> (this T[] source, int start)
	{
	        int end = source.Length;
		if (start == 0 && end == source.Length)
		    return source;
		int len = end - start;
		// Return new array.
		T[] res = new T[len];
		for (int i = 0; i < len; i++) {
		    res [i] = source [i + start];
		}
		return res;
	}

	public static T[] Slice<T> (this T[] source, int start, int end)
	{
		if (start == 0 && end == source.Length)
		    return source;
		// Handles negative ends.
		if (end < 0) {
		    end = source.Length + end;
		}
		int len = end - start;
		// Return new array.
		T[] res = new T[len];
		for (int i = 0; i < len; i++) {
		    res [i] = source [i + start];
		}
		return res;
	}

	public static object[] Slice (this IList source, int start, int end)
	{
		// Handles negative ends.
		if (end < 0) {
			end = source.Count + end;
		}
		int len = end - start;
		// Return new array.
		object[] res = new object[len];
		for (int i = 0; i < len; i++) {
			res [i] = source [i + start];
		}
		return res;
	}
}

public class Numeric {

    public delegate Function<double> Thunk ();

    public array ndim(int, params int args, Thunk thunk=null) {
	/*
	  Makes a multi-dimensional array of random floats. (Replaces
	  RandomArray).
	*/
	if (thunk == null) {
	    thunk = delegate { return random.random(); };
	}
	if (args.length == 0) {
	    return new List<double>([thunk.Invoke() for i in xrange(n)];
	}
		A = List<List<double>>();
	for (i in range(n)) {
	    A.append( ndim(args[0], args.Slice(1), thunk=thunk) ) ;
	}
	return A;
    }

    public class array {
	object array;

	public array (IList data, typecode='f') {
	    // Array([1, 2])
	    // Array([[1, 2], [3, 4]])
	    if (type(data) == array) {
		self.array = data[:];
	    } else if (type(data[0]) in [int, float, long, bool]) {
		// array.array of floats
		this.array = double [] (data);
	    } else {
		// list of Arrays
		self.array = map(array, data);
	    }
	}

    def __getitem__(self, item):
        return self.array[item]

    def __setitem__(self, item, value):
        self.array[item] = value

    def __len__(self):
        return len(self.array)

    def copy(self):
        return self.array[:]

    def __repr__(self):
        if type(self.array) is list:
            return str(map(lambda v: str(v.array.tolist()), self.array))
        else:
            return str(self.array.tolist())

    def __lt__(self, other):
        if type(self.array) is list:
            return array(map(lambda v: v < other, self.array))
        return array([f < other for f in self.array])

    def __gt__(self, other):
        if type(self.array) is list:
            return array(map(lambda v: v > other, self.array))
        return array([f > other for f in self.array])

    def __mul__(self, other):
        if type(other) in [int, float, long]:
            return array(map(lambda v: v * other, self.array))
        else: # array * [0, 1] maybe difference between list and array?
            return array(map(lambda a,b: a * b, self.array, other))

    def __sub__(self, other):
        if type(other) in [int, float, long]:
            return array(map(lambda v: v - other, self.array))
        else: # array - [0, 1]
            return array(map(lambda a,b: a - b, self.array, other))

    def __rsub__(self, other):
        if type(other) in [int, float, long]:
            return array(map(lambda v: other - v, self.array))
        else: # array - [0, 1]
            return array(map(lambda a,b: b - a, self.array, other))

    def __add__(self, other):
        if type(other) in [int, float, long]:
            return array(map(lambda v: v + other, self.array))
        else: # array + [0, 1]
            #print "add a", self.array
            #print "add b", other
            return array(map(lambda a,b: a + b, self.array, other))

    def __pow__(self, other):
        if type(other) in [int, float, long]:
            return array(map(lambda v: v ** other, self.array))
        else: # array ** [0, 1]
            return array(map(lambda a,b: a ** b, self.array, other))

    def __abs__(self):
        return array(map(lambda v: abs(v), self.array))

    def getShape(self):
        if type(self.array) == list:
            return (len(self.array), len(self.array[0]))
        else:
            return (len(self.array), )

    shape = property(getShape)
    __rmul__ = __mul__
    __rgt__ = __gt__
    __rlt__ = __lt__
    __radd__ = __add__
		}
fabs = abs
exp = math.exp

def put(toArray, arange, fromArray):
    for i in arange:
        if type(fromArray) in [int, float, long]:
            toArray[i] = fromArray
        else:
            toArray[i] = fromArray[i]

def arange(size):
    return range(size)

def zeros(dims, typecode='f'):
    if type(dims) == type(1):
        dims = (dims,)
    return array(ndim(*dims, thunk=lambda: 0.0))

public class add {
    public static object reduce(vector) {
        /*
	  Can be a vector or matrix. If data are bool, sum Trues.
        */
        if (type(vector) is list) { // matrix
            return array(map(add.reduce, vector));
        } else {
            return sum(vector); // Numeric_array, return scalar
	}
	
	public static array outerproduct(array a, array b) {
	    array result = zeros((len(a), len(b)));
	    for i in range(len(a)) {
		    for j in range(len(b)) {
			    result[i][j] = a[i] * b[j];
			}
		}
	    return result;
	}
    }
}

public class multiply {
    public static object reduce(vector) {
	/*
	  Can be a vector or matrix. If data are bool, sum Trues.
	*/
        if (type(vector) is list) { // matrix
            return array(map(multiply.reduce, vector));
	} else {
            return reduce(operator.mul, vector); // Numeric.array, return scalar
	}
    }
}

    public static array matrixmultiply(array a, array b) {
	//print "x a:", a
	//print "x b:", b
	// a = [[0, 1], [2, 3], [4, 5]], b = [0, 1]
    if type(b[0]) in [float, int, long] 
        retval = zeros(len(a))
        for i in range(len(a)):
            for j in range(len(a[0])):
                retval[i] = retval[i] + (a[i][j] * b[j])
    else:
        retval = zeros(len(b[0]))
        for i in range(len(a)):
            retval = retval + (a[i] * b[i])
    return retval;

		}
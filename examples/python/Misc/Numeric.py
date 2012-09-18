from __future__ import print_function

import math
import random

class Array:
    def __init__(self, matrix):
        self.matrix = matrix
        self.shape = shape(matrix)

    def __mul__(self, value):
        return Array(multiply(self.matrix, self.shape, value))

    def __rmul__(self, value):
        return Array(multiply(self.matrix, self.shape, value))

    def __sub__(self, value):
        return Array(subtract(self.matrix, self.shape, value))

    def __rsub__(self, value):
        return Array(subtract(self.matrix, self.shape, value))
        
    def __add__(self, value):
        if isinstance(value, (int, float)):
            return Array(add(self.matrix, self.shape, value))
        else:
            return Array(add_reduce_matrix([self.matrix, value]))
        
    def __radd__(self, value):
        if isinstance(value, (int, float)):
            return Array(add(self.matrix, self.shape, value))
        else:
            return Array(add_reduce_matrix([self.matrix, value]))
        
    def __pow__(self, value):
        return Array(pow(self.matrix, self.shape, value))

    def __gt__(self, value):
        return Array(gt(self.matrix, self.shape, value))

    def __lt__(self, value):
        return Array(lt(self.matrix, self.shape, value))

    def __len__(self):
        return len(self.matrix)

    def __getitem__(self, position):
        retval = self.matrix[position]
        print("getitem", position, self.matrix, retval)
        if isinstance(retval, (list, tuple)):
            return Array(retval)
        else:
            return retval

    def __setitem__(self, position, value):
        self.matrix[position] = value

    def tolist(self):
        return Array(self.matrix)

    def __repr__(self):
        return repr(self.matrix)


def ndim(n, *args):
    """
    Makes a multi-dimensional array of random floats. (Replaces RandomArray).
    """
    if not args: 
        return [random.random() for i in xrange(n)]
    A = [] 
    for i in range(n):
        A.append( ndim(*args) ) 
    return A 

def multiply(matrix, shape, value):
    if len(shape) == 1:
        return [m * value for m in matrix]
    else:
        return [multiply(matrix[i], shape[1:], value) for i in range(len(shape))]

def subtract(matrix, shape, value):
    if len(shape) == 1:
        return [m - value for m in matrix]
    else:
        return [subtract(matrix[i], shape[1:], value) for i in range(len(shape))]

def add(matrix, shape, value):
    print("add:", matrix, value)
    if len(shape) == 1:
        return [m + value for m in matrix]
    else:
        return [add(matrix[i], shape[1:], value) for i in range(len(shape))]

def pow(matrix, shape, value):
    if len(shape) == 1:
        return [m ** value for m in matrix]
    else:
        return [pow(matrix[i], shape[1:], value) for i in range(len(shape))]

def lt(matrix, shape, value):
    if len(shape) == 1:
        return [int(m < value) for m in matrix]
    else:
        return [lt(matrix[i], shape[1:], value) for i in range(len(shape))]

def gt(matrix, shape, value):
    if len(shape) == 1:
        return [int(m > value) for m in matrix]
    else:
        return [gt(matrix[i], shape[1:], value) for i in range(len(shape))]

def shape(matrix):
    if isinstance(matrix, (list, tuple)):
        return [len(matrix)] + shape(matrix[0])
    else:
        return []

def array(matrix, type='f'):
    """
    Might be an Array already? If so, copy the matrix.
    """
    return Array(matrix)

def zeros(size, type):
    if isinstance(size, (int, float)):
        return Array(ndim(int(size))) * 0.0
    else:
        return Array(ndim(*size)) * 0.0

def arange(size):
    """
    Range of values in array. Used in put.
    """
    return (0, size)

def put(arr, span, other):
    """
    other is a array or value
    """
    if isinstance(other, (list, tuple, Array)):
        for i in range(span[0], span[1]):
            arr[i] = other[i]
    else:
        for i in range(span[0], span[1]):
            arr[i] = other

def add_reduce(arr):
    return sum(arr)

def multiply_reduce(arr):
    retval = 0
    for v in arr:
        retval *= v
    return retval

def fabs(arr):
    """
    Absolute value of array.
    """
    if isinstance(arr, (float, int)):
        return abs(arr)
    else:
        return Array([fabs(v) for v in arr])

def argmax(arr):
    """
    Find maximum value, return position.
    """
    return arr.find(max(arr))

def matrixmultiply(a1, a2):
    print("numpy.dot(", a1, a2, ")")
    result = [a1[i] * a2[i] for i in range(len(a1))]
    result = add_reduce_matrix(result)
    print("return:", result)
    return result

def add_reduce_matrix(arr):
    results = arr[0]
    for i in range(1, len(arr)):
        for j in range(len(arr[i])):
            results[j] += arr[i][j]
    return results

def outerproduct(array1, array2):
    retval = []
    for item in array1:
        retval.append([item2 * item for item2 in array2])
    return retval

def exp(value):
    return math.exp(value)

def tanh(value):
    return math.tanh(value)

import os

print(os.path.dirname(__file__))
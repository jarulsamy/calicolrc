#############################################################
# Scheme.py
# These are native implementations of functions to allow
# the register machine translation to run in Python

from __future__ import print_function
import fractions
import operator
import types
import math
import time
import sys
import os

#############################################################
# Python implementation notes:
#
# Each symbol is a singleton for easy comparison reasons:
# Symbol("x") is Symbol("x")
#
# Python's list is used as Scheme's vector.
#
# The List() class is used for Scheme's con-cell based lists.
#
# Lists implement iter, so you can use Python's iter tools
# (such as [x for x in List(1, 2, 3)])
#
# A couple of functions are O(2n) because they have a 
# reverse. Should be fixed to be O(n).
#############################################################

## Global symbols:

# Set to a dictionary-like object for global-shared namespace:
DLR_ENV = {key:getattr(__builtins__, key) for key in dir(__builtins__)}

class Symbol(object):
    def __init__(self, name):
        self.name = name
        self.hash = hash(name)

    def __repr__(self):
        return "%s" % self.name

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.hash == other.hash

    def __hash__(self):
        return hash(self.name)

    def __iter__(self):
        # So that EmptyList will be treated as []
        return self

    def next(self):
        # So that EmptyList will be treated as []
        raise StopIteration

symbols = {}

def make_symbol(string):
    if not (string in symbols):
        symbols[string] = Symbol(string)
    return symbols[string]

void_value = make_symbol("<void>")

def make_initial_env_extended(names, procs):
    ## If you wish to extend the environment to 
    ## include native values, do so here:
    return make_initial_environment(names, procs)

### Lists:

class cons(object):
    # build a cons cell
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        retval = ""
        current = self
        while isinstance(current, cons):
            if retval:
                retval += " "
            retval += repr(current.car)
            current = current.cdr
        if current != symbol_emptylist:
            retval += " . " + repr(current)
        return "(%s)" % retval

    def __iter__(self):
        self.current = self
        return self

    def next(self): # Python 3: def __next__(self)
        if not isinstance(self.current, cons):
            raise StopIteration
        else:
            retval = self.current.car
            self.current = self.current.cdr
            return retval

def List(*args):
    # Scheme list
    retval = symbol_emptylist
    i = 0
    while i < len(args):
        arg = args[len(args) - i - 1]
        retval = cons(arg, retval)
        i += 1
    return retval

def reverse(lyst):
    retval = symbol_emptylist
    current = lyst
    while isinstance(current, cons):
        retval = cons(current.car, retval)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    return retval

def length(lyst):
    current = lyst
    count = 0
    while isinstance(current, cons):
        current = current.cdr
        count += 1
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    return count

def Map(f, lyst):
    retval = symbol_emptylist
    current = lyst
    while isinstance(current, cons):
        retval = cons(f(current.car), retval)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    # FIXME: rewrite without reverse
    return reverse(retval)

def for_each(f, lyst):
    current = lyst
    while isinstance(current, cons):
        f(current.car)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")

def sort(f, lyst):
    return List(*sorted(lyst))

def append(*objs):
    retval = objs[-1]
    # FIXME: rewrite without reversed
    for obj in reversed(objs[:-1]):
        # FIXME: rewrite without reverse
        current = reverse(obj)
        while isinstance(current, cons):
            retval = cons(current.car, retval)
            current = current.cdr
    return retval

def car(lyst):
    return lyst.car

def cdr(lyst):
    return lyst.cdr

def caar(lyst):
    return lyst.car.car

def cadr(lyst):
    return lyst.cdr.car

def cddr(lyst):
    return lyst.cdr.cdr

def cdar(lyst):
    return lyst.car.cdr

def caddr(lyst):
    return lyst.cdr.cdr.car

def cadar(lyst):
    return lyst.car.cdr.car

def cdddr(lyst):
    return lyst.cdr.cdr.cdr

def cadddr(lyst):
    return lyst.cdr.cdr.cdr.car

def cddddr(lyst):
    return lyst.cdr.cdr.cdr.cdr

def set_car_b(cell, item):
    cell.car = item

def set_cdr_b(cell, item):
    cell.cdr = item

def list_tail(lyst, pos):
    if pos < 0:
        raise Exception("invalid list-ref position: " + pos)
    current = lyst
    while pos != 0:
        current = current.cdr
        pos = pos - 1
    return current

def list_head(lyst, pos):
    retval = symbol_emptylist
    current = lyst
    while pos != 0:
        retval = cons(current.car, retval)
        current = current.cdr
        pos = pos - 1
    # FIXME: rewrite without reverse
    return reverse(retval)

def list_ref(lyst, pos):
    if pos < 0:
        raise Exception("invalid list-ref position: " + pos)
    current = lyst
    while pos != 0:
        current = current.cdr
        pos = pos - 1
    return current.car

def vector_length(vec):
    return len(vec)

### Native make- functions:

def make_proc(*args):
    return List(symbol_procedure, *args)

def make_macro(*args):
    return List(symbol_macro_transformer, *args)

def make_cont(*args):
    return List(symbol_continuation, *args)

def make_cont2(*args):
    return List(symbol_continuation2, *args)

def make_cont3(*args):
    return List(symbol_continuation3, *args)

def make_cont4(*args):
    return List(symbol_continuation4, *args)

def make_fail(*args):
    return List(symbol_fail_continuation, *args)

def make_handler(*args):
    return List(symbol_handler, *args)

def make_handler2(*args):
    return List(symbol_handler2, *args)

### Native other functions:

def length_one_q(ls):
    return isinstance(ls, cons) and (ls.cdr is symbol_emptylist)

def length_two_q(ls):
    return (isinstance(ls, cons) and 
            isinstance(ls.cdr, cons) and 
            (ls.cdr.cdr is symbol_emptylist))

def length_at_least_q(n, ls):
    length = len(list(ls))
    return length >= n

def all_numeric_q(ls):
    for item in ls:
        if not number_q(item):
            return False
    return True

def all_char_q(ls):
    for item in ls:
        if not char_q(item):
            return False
    return True

### Questions:

def even_q(n):
    return n % 2 == 0

def odd_q(n):
    return n % 2 == 1

def eq_q(o1, o2):
    return o1 is o2

def equal_q(o1, o2):
    return o1 == o2

def char_q(item):
    return isinstance(item, str) and len(item) == 1

def string_q(item):
    return isinstance(item, str)

def char_whitespace_q(c):
    return c in [' ', '\t', '\n', '\r']

def char_alphabetic_q(c):
    return (('A' <= c <= 'Z') or 
            ('a' <= c <= 'z'))

def char_numeric_q(c):
    return '0' <= c <= '9'

def char_is__q(c1, c2):
    return c1 == c2

def pair_q(item):
    return isinstance(item, cons)

def number_q(item):
    return isinstance(item, (int, long, float, Fraction))

def null_q(item):
    return item is symbol_emptylist

def boolean_q(item):
    return isinstance(item, bool)

def true_q(item):
    if item is False:
        return False
    else:
        return True

def list_q(item):
    ## return proper_list?
    current = item
    while isinstance(current, cons):
        current = current.cdr
    return current is symbol_emptylist

def procedure_q(item):
    return pair_q(item) and (car(item) is symbol_procedure)

def symbol_q(item):
    return isinstance(item, Symbol)

def vector_q(item):
    return isinstance(item, list)

def pair_q(item):
    return isinstance(item, cons)

def iterator_q(item):
    return False
    # FIXME:
    #return not list_q(item)

def get_iterator(generator):
    return iter(generator)

def get_type(obj):
    return type(obj)

### Math and applications:

class Fraction(fractions.Fraction):
    def __repr__(self):
        return "%s/%s" % (self.numerator, self.denominator)
    def __str__(self):
        return "%s/%s" % (self.numerator, self.denominator)

def modulo(a, b):
    return a % b

def quotient(a, b):
    return int(a / b)

def remainder(a, b):
    return a % b

def sqrt(number):
    return math.sqrt(number)

def plus(*args):
    return reduce(operator.add, args, 0)

def minus(*args):
    if len(args) == 0:
        return 0
    elif len(args) == 1:
        return -args[0]
    else:
        return reduce(operator.sub, args[1:], args[0])

def multiply(*args):
    return reduce(operator.mul, args, 1)

def divide(*args):
    return args[0] / args[1]

def Equal(a, b):
    return a == b

def LessThan(a, b):
    return a < b

def LessThanEqual(a, b):
    return a <= b

def GreaterThanEqual(a, b):
    return a >= b

def GreaterThan(a, b):
    return a > b

def memq(item, lyst):
    retval = symbol_emptylist
    current = lyst
    while isinstance(current, cons):
        if current.car == item:
            return current.cdr
        current = current.cdr
    return False

### Converters:

def char_to_integer(c):
    return ord(c)

def integer_to_char(i):
    return chr(i)

def number_to_string(number):
    return str(number)

def string_to_integer(s):
    return int(s)

def string_to_symbol(string):
    return make_symbol(string)

def list_to_string(lyst):
    retval = ""
    current = lyst
    while isinstance(current, cons):
        retval += str(current.car)
        current = current.cdr
    return retval

def list_to_vector(lyst):
    # this works because cons implements iter
    return list(lyst)

def vector_to_list(vector):
    return List(*vector)

def vector_ref(vector, position):
    return vector[position]

### Strings:

def string_append(s1, s2):
    return str(s1) + str(s2)

def string_ref(string, pos):
    return string[pos]

def string(s):
    return str(s)

def string_to_decimal(s):
    return float(s)

def string_to_rational(s):
    return Fraction(s)

def string_split(string, delim):
    return List(*string.split(delim))

def symbol_to_string(symbol):
    return symbol.name

def member(item, lyst):
    current = lyst
    while isinstance(current, cons):
        if item == lyst:
            return True
        current = current.cdr
    return False

def string_is__q(s1, s2):
    return s1 == s2

def string_length(s):
    return len(s)

def string_to_number(s):
    if "/" in s:
        return string_to_rational(s)
    elif "." in s:
        return string_to_decimal(s)
    else:
        return string_to_integer(s)

def string_to_list(s):
    return List(*s.split())

def stringLessThan_q(s1, s2):
    return s1 < s2

def substring(s, start, stop):
    return s[start:stop]

### Functions:

def Apply(f, lyst):
    return apply(f, list_to_vector(lyst))

### Annotated expression support:

def tagged_list_hat(keyword, op, length):
    def tagged_list(asexp):
        return (list_q_hat(asexp) and
                op(length_hat(asexp), length) and
                symbol_q_hat(car_hat(asexp)) and 
                eq_q_hat(car_hat(asexp), keyword))
    return tagged_list

### Misc:

def error(function, formatting, *args):
    sys.stderr.write(format(formatting, *args))
    raise Exception(format(formatting, *args))

def display(item):
    print(item, end="")

def printf(formatting, *items):
    print(format(formatting, *items), end="")

def newline():
    print()

def trampoline():
    while pc:
        pc()
        #if end_of_session_q(final_reg):
        #    break
        #elif exception_q(final_reg):
        #    break
    return final_reg

def box(item):
    return List(item)

def raw_read_line(prompt):
    try:
        return raw_input(prompt)
    except EOFError:
        return ""

def format(formatting, *lyst):
    args = list_to_vector(lyst)
    retval = ""
    i = 0
    count = 0
    while i < len(formatting):
        if formatting[i] == '\\':
            i += 1
            retval += formatting[i]
        elif formatting[i] == "~":
            if formatting[i+1] == 's':
                i += 1
                retval += repr(args[count])
                count += 1
            elif formatting[i+1] == 'a':
                i += 1
                retval += str(args[count])
                count += 1
            elif formatting[i+1] == '%':
                i += 1
                retval += "\n"
            else:
                retval += formatting[i] # unknown ~X
        else:
            retval += formatting[i]
        i += 1
    return retval

def safe_print(item):
    if procedure_q(item):
        print("<procedure>")
    elif environment_q(item):
        print("<environment>")
    elif boolean_q(item):
        if item:
            print("#t")
        else:
            print("#f")
    else:
        print(item)

def search_frame(frame, variable):
    if isinstance(frame, cons):
        bindings = car(frame)
        variables = cadr(frame)
        i = 0
        while not null_q(variables):
            if eq_q(car(variables), variable):
                return bindings[i];
            variables = cdr(variables)
            i += 1
        return False
    else:
        raise Exception("invalid frame")

def read_content(filename):
    return open(filename).read()

def file_exists_q(path):
    return os.path.isfile(path)

def get_current_time():
    return time.time()

def current_directory():
    return os.getcwd()

def Range(*args):
    return List(*range(*args))

def assv(x, ls):
    while isinstance(ls, cons):
        if x is caar(ls):
            return ls.car
        ls = ls.cdr
    return False

def memv(x, ls):
    current = ls
    while isinstance(current, cons):
        if (item1 == current.car):
            return current
        current = current.cdr
    return False

def make_vector(size):
    return [0] * size

def vector_native(ls):
    return List(ls)

def vector_set_b(vec, pos, value):
    vec[pos] = value

### External env interface:

def using(libraries, environment):
    retval = symbol_emptylist
    for library in libraries:
        lib = __import__(library)
        sym = make_symbol(library)
        set_global_value_b(sym, lib)
        retval = cons(sym, retval)
    return reverse(retval)

def dlr_proc_q(item):
    return callable(item)

def dlr_env_contains(item):
    return item.name in DLR_ENV

def set_global_value_b(variable, value):
    DLR_ENV[variable.name] = value

def dlr_env_lookup(variable):
    return DLR_ENV[variable.name]

def dlr_object_contains(obj, components):
    # components: (math sqrt)
    retval = obj
    for component in cdr(components):
        if hasattr(retval, component.name):
            retval = getattr(obj, component.name)
        else:
            return False
    return True

def get_external_member(obj, components):
    # components: (math sqrt)
    retval = obj
    for component in cdr(components):
        if hasattr(retval, component.name):
            retval = getattr(obj, component.name)
        else:
            return void_value
    return retval

def dlr_apply(f, args):
    return f(*args)

def dlr_func(schemeProc):
    def f(*args):
        globals()["proc_reg"] = schemeProc
        globals()["args_reg"] = List(*args)
        globals()["handler_reg"] = REP_handler
        globals()["k2_reg"] = REP_k
        globals()["pc"] = apply_proc
        return trampoline()
    return f

def set_global_docstring_b(variable, docstring):
    pass

def get_external_members(obj):
    return List(*[make_symbol(x) for x in  dir(obj)])

#########################################
# Calico External functions not used here
#########################################
# apply_star # external apply
# callback0
# callback1
# callback2
# handle_debug_info
# highlight_expression
# next_item
# set_external_member_b
#########################################

# end of Scheme.py
#############################################################

#############################################################
# Scheme.py
# These are native implementations of functions to allow
# the register machine translation to run in Python

from __future__ import print_function
import fractions
import time
import os

## Global symbols:

class Symbol:
    def __init__(self, name):
        self.name = name
        self.hash = hash(name)

    def __repr__(self):
        return "'%s" % self.name

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

### Lists:

class cons:
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

def sort(lyst):
    return List(*sorted(lyst))

def for_each(f, lyst):
    current = lyst
    while isinstance(current, cons):
        f(current.car)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")

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

### Questions:

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
    return item in [True, False]

def true_q(item):
    if item is False:
        return False
    else:
        return True

def list_q(item):
    ## return proper_list?
    if isinstance(item, cons):
        current = item
        while isinstance(current, cons):
            current = current.cdr
        return current is symbol_emptylist
    return False

def procedure_q(item):
    return pair_q(item) and (car(item) is symbol_procedure)

def symbol_q(item):
    return isinstance(item, Symbol)

def vector_q(item):
    return isinstance(item, list)

def pair_q(item):
    return isinstance(item, cons)

def iterator_q(item):
    return not list_q(item)

def get_iterator(generator):
    return iter(generator)

### Math and applications:

class Fraction(fractions.Fraction):
    def __repr__(self):
        return "%s/%s" % (self.numerator, self.denominator)
    def __str__(self):
        return "%s/%s" % (self.numerator, self.denominator)

def plus(a, b):
    return a + b

def minus(a, b):
    return a - b

def multiply(a, b):
    return a * b

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

def display(item):
    print(item, end="")

def printf_prim(formatting, *items):
    print(format(formatting, *items))

def newline():
    print()

def trampoline():
    while pc:
        pc()
    return final_reg

def box(item):
    return List(item)

def raw_read_line(prompt):
    return raw_input(prompt)

def format(formatting, *args):
    return "format(%s, %s)" % (formatting, args)

def safe_print(item):
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

def dlr_env_contains(item):
    return False

def dlr_proc_q(item):
    return False

# dlr_apply
# dlr_env_contains
# dlr_env_lookup
# dlr_func
# dlr_object_contains
# dlr_proc_q

# _
# apply_star
# assv
# char_to_integer
# current_directory
# callback0
# callback1
# callback2
# equal_q
# even_q
# for_each
# get_external_member
# get_external_members
# get_type
# handle_debug_info
# highlight_expression
# integer_to_char
# make_vector
# memv
# modulo
# newline
# next_item
# odd_q
# printf
# printf_prim
# quotient
# read_content
# remainder
# set_external_member_b
# set_global_docstring_b
# set_global_value_b
# slash
# sort
# sqrt
# _star
# to_
# using_prim
# vector_length
# vector_native
# vector_set_b

# end of Scheme.py
#############################################################

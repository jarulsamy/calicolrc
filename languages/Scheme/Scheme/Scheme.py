# included from Scheme.py

class Symbol:
    def __init__(self, name):
        self.name = name
        self.hash = hash(name)

    def __repr__(self):
        return "'%s" % self.name

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.hash == other.hash

def box(item):
    return List(item)

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

def set_car_b(lyst, item):
    lyst.car = item

def string_append(s1, s2):
    if s1 is None:
        if s2 is None:
            return ""
        else:
            return s2
    elif s2 is None:
        return s1
    else:
        return str(s1) + str(s2)

def eq_q(o1, o2):
    return o1 == o2

def list_ref(lyst, pos):
    current = lyst
    while pos != 0:
        current = current.cdr
        pos = pos - 1
    return current.car

def string_ref(string, pos):
    return string[pos]

def string(s):
    return str(s)

def error(function, *args):
    print("Error in %s: %s" % (function, args))

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
        if isinstance(self.current, cons):
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

def Map(f, lyst):
    retval = symbol_emptylist
    current = reverse(lyst)
    while isinstance(current, cons):
        retval = cons(f(current.car), retval)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    return retval

def Apply(f, lyst):
    return apply(f, list_to_vector(lyst))

def tagged_list_hat(tag, f, arg_count):
    def tagged_list(*args):
        if not f(len(args)):
            raise Exception("invalid arguments to " + tag)
        return List(tag, *args)
    return tagged_list

def Equal(a, b):
    return a == b

def LessThanEqual(a, b):
    return a <= b

def GreaterThanEqual(a, b):
    return a >= b

def list_to_vector(lyst):
    retval = list()
    current = lyst
    while isinstance(current, cons):
        retval.append(current.car)
        current = current.cdr
    return retval

def raw_read_line(prompt):
    return raw_input(prompt)

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

def char_whitespace_q(c):
    return c in [' ', '\t', '\n', '\0', '\r']

def char_alphabetic_q(c):
    return (('A' <= c <= 'Z') or 
            ('a' <= c <= 'z'))

def char_numeric_q(c):
    return '0' <= c <= '9'

def char_is__q(c1, c2):
    return c1 == c2

def plus(a, b):
    return a + b

def minus(a, b):
    return a - b

def pair_q(item):
    return isinstance(item, cons)

def number_q(item):
    return isinstance(item, (int, long, float))

def safe_print(item):
    print(item)

def list_to_string(lyst):
    retval = ""
    current = lyst
    while isinstance(current, cons):
        retval += str(current.car)
        current = current.cdr
    return retval

def string_to_integer(s):
    return int(s)

def null_q(item):
    return item == symbol_emptylist

def append(*objs):
    retval = objs[-1]
    for obj in reversed(objs[:-1]):
        current = reverse(obj)
        while isinstance(current, cons):
            retval = cons(current.car, retval)
            current = current.cdr
    return retval
            
def string_to_symbol(string):
    return Symbol(string)

def display(item):
    print(item)

def trampoline():
    while pc:
        pc()
    return final_reg

####################################################
## Scheme in Python
##
## Jim Marshall
## Doug Blank
####################################################


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
    try:
        return Fraction(s)
    except:
        return False

def string_split(string, delim):
    return List(*string.split(delim))

def symbol_to_string(symbol):
    return symbol.name

def member(item, lyst):
    current = lyst
    while isinstance(current, cons):
        if item == current.car:
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
    global pc, exception_reg
    while pc:
        try:
            pc()
        except KeyboardInterrupt:
            exception_reg = make_exception("KeyboardInterrupt", "Keyboard interrupt", symbol_none, symbol_none, symbol_none)
            pc = apply_handler2            
        except Exception, e:
            exception_reg = make_exception("UnhandledException", e.message, symbol_none, symbol_none, symbol_none)
            pc = apply_handler2
    return final_reg

def box(item):
    return List(item)

def raw_read_line(prompt):
    try:
        return raw_input(prompt)
    except:
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

symbol_emptylist = make_symbol("()")
symbol_lit_aexp = make_symbol("lit-aexp")
symbol_var_aexp = make_symbol("var-aexp")
symbol_lexical_address_aexp = make_symbol("lexical-address-aexp")
symbol_if_aexp = make_symbol("if-aexp")
symbol_assign_aexp = make_symbol("assign-aexp")
symbol_func_aexp = make_symbol("func-aexp")
symbol_callback0_aexp = make_symbol("callback0-aexp")
symbol_callback1_aexp = make_symbol("callback1-aexp")
symbol_callback2_aexp = make_symbol("callback2-aexp")
symbol_define_aexp = make_symbol("define-aexp")
symbol_define_b_aexp = make_symbol("define!-aexp")
symbol_define_syntax_aexp = make_symbol("define-syntax-aexp")
symbol_begin_aexp = make_symbol("begin-aexp")
symbol_lambda_aexp = make_symbol("lambda-aexp")
symbol_mu_lambda_aexp = make_symbol("mu-lambda-aexp")
symbol_trace_lambda_aexp = make_symbol("trace-lambda-aexp")
symbol_mu_trace_lambda_aexp = make_symbol("mu-trace-lambda-aexp")
symbol_app_aexp = make_symbol("app-aexp")
symbol_try_catch_aexp = make_symbol("try-catch-aexp")
symbol_try_finally_aexp = make_symbol("try-finally-aexp")
symbol_try_catch_finally_aexp = make_symbol("try-catch-finally-aexp")
symbol_raise_aexp = make_symbol("raise-aexp")
symbol_choose_aexp = make_symbol("choose-aexp")
symbol_undefined = make_symbol("undefined")
symbol_continuation = make_symbol("continuation")
symbol_none = make_symbol("none")
symbol_quasiquote = make_symbol("quasiquote")
symbol_let = make_symbol("let")
symbol_else = make_symbol("else")
symbol_eq_q = make_symbol("eq?")
symbol_quote = make_symbol("quote")
symbol_memq = make_symbol("memq")
symbol_define = make_symbol("define")
symbol_lambda = make_symbol("lambda")
symbol_args = make_symbol("args")
symbol_if = make_symbol("if")
symbol_Equal = make_symbol("=")
symbol_length = make_symbol("length")
symbol_error = make_symbol("error")
symbol_car = make_symbol("car")
symbol_append = make_symbol("append")
symbol_list_to_vector = make_symbol("list->vector")
symbol_cons = make_symbol("cons")
symbol_List = make_symbol("list")
symbol_unit = make_symbol("unit")
symbol_composite = make_symbol("composite")
symbol_continuation2 = make_symbol("continuation2")
symbol_set_b = make_symbol("set!")
symbol_r = make_symbol("r")
symbol_cond = make_symbol("cond")
symbol_else_code = make_symbol("else-code")
symbol_Apply = make_symbol("apply")
symbol_cdr = make_symbol("cdr")
symbol_x = make_symbol("x")
symbol_and = make_symbol("and")
symbol_pair_q = make_symbol("pair?")
symbol_not = make_symbol("not")
symbol_begin = make_symbol("begin")
symbol_cases = make_symbol("cases")
symbol_stdin = make_symbol("stdin")
symbol_end_marker = make_symbol("end-marker")
symbol_continuation3 = make_symbol("continuation3")
symbol_continuation4 = make_symbol("continuation4")
symbol_dot = make_symbol("dot")
symbol_fail_continuation = make_symbol("fail-continuation")
symbol_handler = make_symbol("handler")
symbol_exception = make_symbol("exception")
symbol_handler2 = make_symbol("handler2")
symbol_procedure = make_symbol("procedure")
symbol_ok = make_symbol("ok")
symbol_macro_transformer = make_symbol("macro-transformer")
symbol_letrec = make_symbol("letrec")
symbol_bool = make_symbol("bool")
symbol_or = make_symbol("or")
symbol__is_to_ = make_symbol("=>")
symbol_th = make_symbol("th")
symbol_goto = make_symbol("goto")
symbol_start_state = make_symbol("start-state")
symbol_shift = make_symbol("shift")
symbol_replace = make_symbol("replace")
symbol_drop = make_symbol("drop")
symbol_token_start_state = make_symbol("token-start-state")
symbol_emit = make_symbol("emit")
symbol_apply_action = make_symbol("apply-action")
symbol_integer = make_symbol("integer")
symbol_decimal = make_symbol("decimal")
symbol_rational = make_symbol("rational")
symbol_identifier = make_symbol("identifier")
symbol_boolean = make_symbol("boolean")
symbol_character = make_symbol("character")
symbol_named_character = make_symbol("named-character")
symbol_string = make_symbol("string")
symbol_comment_state = make_symbol("comment-state")
symbol_lparen = make_symbol("lparen")
symbol_lbracket = make_symbol("lbracket")
symbol_rparen = make_symbol("rparen")
symbol_rbracket = make_symbol("rbracket")
symbol_apostrophe = make_symbol("apostrophe")
symbol_backquote = make_symbol("backquote")
symbol_comma_state = make_symbol("comma-state")
symbol_hash_prefix_state = make_symbol("hash-prefix-state")
symbol_string_state = make_symbol("string-state")
symbol_identifier_state = make_symbol("identifier-state")
symbol_signed_state = make_symbol("signed-state")
symbol_decimal_point_state = make_symbol("decimal-point-state")
symbol_whole_number_state = make_symbol("whole-number-state")
symbol_comma_at = make_symbol("comma-at")
symbol_comma = make_symbol("comma")
symbol_character_state = make_symbol("character-state")
symbol_lvector = make_symbol("lvector")
symbol_alphabetic_character_state = make_symbol("alphabetic-character-state")
symbol_named_character_state = make_symbol("named-character-state")
symbol_string_escape_state = make_symbol("string-escape-state")
symbol_signed_decimal_point_state = make_symbol("signed-decimal-point-state")
symbol_fractional_number_state = make_symbol("fractional-number-state")
symbol_rational_number_state = make_symbol("rational-number-state")
symbol_suffix_state = make_symbol("suffix-state")
symbol_rational_number_state_star = make_symbol("rational-number-state*")
symbol_signed_exponent_state = make_symbol("signed-exponent-state")
symbol_exponent_state = make_symbol("exponent-state")
symbol_apply_state = make_symbol("apply-state")
symbol_unquote = make_symbol("unquote")
symbol_unquote_splicing = make_symbol("unquote-splicing")
symbol_environment = make_symbol("environment")
symbol_func = make_symbol("func")
symbol_define_b = make_symbol("define!")
symbol_let_star = make_symbol("let*")
symbol_case = make_symbol("case")
symbol_record_case = make_symbol("record-case")
symbol_try = make_symbol("try")
symbol_catch = make_symbol("catch")
symbol_finally = make_symbol("finally")
symbol_raise = make_symbol("raise")
symbol_define_syntax = make_symbol("define-syntax")
symbol_choose = make_symbol("choose")
symbol_define_datatype = make_symbol("define-datatype")
symbol_trace_lambda = make_symbol("trace-lambda")
symbol_pattern_macro = make_symbol("pattern-macro")
symbol_callback0 = make_symbol("callback0")
symbol_callback1 = make_symbol("callback1")
symbol_callback2 = make_symbol("callback2")
symbol_aunparse = make_symbol("aunparse")
symbol_goodbye = make_symbol("goodbye")
symbol_m = make_symbol("m")
symbol_dotdotdot = make_symbol("...")
symbol_application = make_symbol("application")
symbol_unknown = make_symbol("unknown")
symbol_macro_generated_exp = make_symbol("macro-generated-exp")
symbol_b_procedure_d = make_symbol("<procedure>")
symbol_b_environment_d = make_symbol("<environment>")
symbol_Map = make_symbol("map")
symbol_multiply = make_symbol("*")
symbol_plus = make_symbol("+")
symbol_minus = make_symbol("-")
symbol_divide = make_symbol("/")
symbol_p = make_symbol("%")
symbol_LessThan = make_symbol("<")
symbol_LessThanEqual = make_symbol("<=")
symbol_GreaterThan = make_symbol(">")
symbol_GreaterThanEqual = make_symbol(">=")
symbol_abort = make_symbol("abort")
symbol_abs = make_symbol("abs")
symbol_assv = make_symbol("assv")
symbol_boolean_q = make_symbol("boolean?")
symbol_caddr = make_symbol("caddr")
symbol_cadr = make_symbol("cadr")
symbol_call_with_current_continuation = make_symbol("call-with-current-continuation")
symbol_call_cc = make_symbol("call/cc")
symbol_char_q = make_symbol("char?")
symbol_char_is__q = make_symbol("char=?")
symbol_char_whitespace_q = make_symbol("char-whitespace?")
symbol_char_alphabetic_q = make_symbol("char-alphabetic?")
symbol_char_numeric_q = make_symbol("char-numeric?")
symbol_char_to_integer = make_symbol("char->integer")
symbol_current_time = make_symbol("current-time")
symbol_cut = make_symbol("cut")
symbol_dir = make_symbol("dir")
symbol_display = make_symbol("display")
symbol_current_environment = make_symbol("current-environment")
symbol_equal_q = make_symbol("equal?")
symbol_eval = make_symbol("eval")
symbol_eval_ast = make_symbol("eval-ast")
symbol_exit = make_symbol("exit")
symbol_for_each = make_symbol("for-each")
symbol_format = make_symbol("format")
symbol_get = make_symbol("get")
symbol_get_stack_trace = make_symbol("get-stack-trace")
symbol_import = make_symbol("import")
symbol_integer_to_char = make_symbol("integer->char")
symbol_list_to_string = make_symbol("list->string")
symbol_list_ref = make_symbol("list-ref")
symbol_load = make_symbol("load")
symbol_make_set = make_symbol("make-set")
symbol_make_vector = make_symbol("make-vector")
symbol_member = make_symbol("member")
symbol_memv = make_symbol("memv")
symbol_newline = make_symbol("newline")
symbol_null_q = make_symbol("null?")
symbol_number_to_string = make_symbol("number->string")
symbol_number_q = make_symbol("number?")
symbol_parse = make_symbol("parse")
symbol_parse_string = make_symbol("parse-string")
symbol_print = make_symbol("print")
symbol_printf = make_symbol("printf")
symbol_Range = make_symbol("range")
symbol_read_string = make_symbol("read-string")
symbol_require = make_symbol("require")
symbol_reverse = make_symbol("reverse")
symbol_set_car_b = make_symbol("set-car!")
symbol_set_cdr_b = make_symbol("set-cdr!")
symbol_snoc = make_symbol("snoc")
symbol_rac = make_symbol("rac")
symbol_rdc = make_symbol("rdc")
symbol_sqrt = make_symbol("sqrt")
symbol_odd_q = make_symbol("odd?")
symbol_even_q = make_symbol("even?")
symbol_quotient = make_symbol("quotient")
symbol_remainder = make_symbol("remainder")
symbol_string_length = make_symbol("string-length")
symbol_string_ref = make_symbol("string-ref")
symbol_string_q = make_symbol("string?")
symbol_string_to_number = make_symbol("string->number")
symbol_string_is__q = make_symbol("string=?")
symbol_substring = make_symbol("substring")
symbol_symbol_q = make_symbol("symbol?")
symbol_unparse = make_symbol("unparse")
symbol_unparse_procedure = make_symbol("unparse-procedure")
symbol_using = make_symbol("using")
symbol_set_use_stack_trace_b = make_symbol("set-use-stack-trace!")
symbol_vector = make_symbol("vector")
symbol_vector_ref = make_symbol("vector-ref")
symbol_vector_set_b = make_symbol("vector-set!")
symbol_void = make_symbol("void")
symbol_zero_q = make_symbol("zero?")
symbol_current_directory = make_symbol("current-directory")
symbol_cd = make_symbol("cd")
symbol_round = make_symbol("round")
symbol_empty = make_symbol("empty")
symbol_instantiate_hat = make_symbol("instantiate^")
symbol_substitution = make_symbol("substitution")
symbol_apply_sub_hat = make_symbol("apply-sub^")
symbol_atom = make_symbol("atom")
symbol_pair = make_symbol("pair")
symbol_b__q_q_q_d = make_symbol("<???>")
symbol_b_fail_d = make_symbol("<fail>")
symbol_b_handler_d = make_symbol("<handler>")
symbol_b_void_d = make_symbol("<void>")
symbol_exiting = make_symbol("exiting")
symbol_the = make_symbol("the")
symbol_interpreter = make_symbol("interpreter")

def lit_aexp(*args):
    args = List(*args)
    return cons(symbol_lit_aexp, args)

def var_aexp(*args):
    args = List(*args)
    return cons(symbol_var_aexp, args)

def lexical_address_aexp(*args):
    args = List(*args)
    return cons(symbol_lexical_address_aexp, args)

def if_aexp(*args):
    args = List(*args)
    return cons(symbol_if_aexp, args)

def assign_aexp(*args):
    args = List(*args)
    return cons(symbol_assign_aexp, args)

def func_aexp(*args):
    args = List(*args)
    return cons(symbol_func_aexp, args)

def callback0_aexp(*args):
    args = List(*args)
    return cons(symbol_callback0_aexp, args)

def callback1_aexp(*args):
    args = List(*args)
    return cons(symbol_callback1_aexp, args)

def callback2_aexp(*args):
    args = List(*args)
    return cons(symbol_callback2_aexp, args)

def define_aexp(*args):
    args = List(*args)
    return cons(symbol_define_aexp, args)

def define_b_aexp(*args):
    args = List(*args)
    return cons(symbol_define_b_aexp, args)

def define_syntax_aexp(*args):
    args = List(*args)
    return cons(symbol_define_syntax_aexp, args)

def begin_aexp(*args):
    args = List(*args)
    return cons(symbol_begin_aexp, args)

def lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_lambda_aexp, args)

def mu_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_mu_lambda_aexp, args)

def trace_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_trace_lambda_aexp, args)

def mu_trace_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_mu_trace_lambda_aexp, args)

def app_aexp(*args):
    args = List(*args)
    return cons(symbol_app_aexp, args)

def try_catch_aexp(*args):
    args = List(*args)
    return cons(symbol_try_catch_aexp, args)

def try_finally_aexp(*args):
    args = List(*args)
    return cons(symbol_try_finally_aexp, args)

def try_catch_finally_aexp(*args):
    args = List(*args)
    return cons(symbol_try_catch_finally_aexp, args)

def raise_aexp(*args):
    args = List(*args)
    return cons(symbol_raise_aexp, args)

def choose_aexp(*args):
    args = List(*args)
    return cons(symbol_choose_aexp, args)

pc = symbol_undefined
aclauses_reg = symbol_undefined
action_reg = symbol_undefined
adatum_list_reg = symbol_undefined
adatum_reg = symbol_undefined
ap1_reg = symbol_undefined
ap2_reg = symbol_undefined
ap_reg = symbol_undefined
apair1_reg = symbol_undefined
apair2_reg = symbol_undefined
args_reg = symbol_undefined
avar_reg = symbol_undefined
ax_reg = symbol_undefined
bindings_reg = symbol_undefined
bodies_reg = symbol_undefined
buffer_reg = symbol_undefined
cdrs_reg = symbol_undefined
char_reg = symbol_undefined
chars_reg = symbol_undefined
clauses_reg = symbol_undefined
components_reg = symbol_undefined
contours_reg = symbol_undefined
datum_reg = symbol_undefined
depth_reg = symbol_undefined
dk_reg = symbol_undefined
env2_reg = symbol_undefined
env_reg = symbol_undefined
exception_reg = symbol_undefined
exp_reg = symbol_undefined
expected_terminator_reg = symbol_undefined
exps_reg = symbol_undefined
fail_reg = symbol_undefined
fields_reg = symbol_undefined
filename_reg = symbol_undefined
filenames_reg = symbol_undefined
final_reg = symbol_undefined
frames_reg = symbol_undefined
generator_reg = symbol_undefined
gk_reg = symbol_undefined
handler_reg = symbol_undefined
i_reg = symbol_undefined
id_reg = symbol_undefined
info_reg = symbol_undefined
input_reg = symbol_undefined
iterator_reg = symbol_undefined
k2_reg = symbol_undefined
k_reg = symbol_undefined
keyword_reg = symbol_undefined
line_reg = symbol_undefined
list1_reg = symbol_undefined
list2_reg = symbol_undefined
lists_reg = symbol_undefined
ls1_reg = symbol_undefined
ls2_reg = symbol_undefined
ls_reg = symbol_undefined
lst_reg = symbol_undefined
macro_reg = symbol_undefined
module_reg = symbol_undefined
msg_reg = symbol_undefined
name_reg = symbol_undefined
offset_reg = symbol_undefined
p1_reg = symbol_undefined
p2_reg = symbol_undefined
pair1_reg = symbol_undefined
pair2_reg = symbol_undefined
path_reg = symbol_undefined
pattern_reg = symbol_undefined
proc_reg = symbol_undefined
procs_reg = symbol_undefined
s_reg = symbol_undefined
senv_reg = symbol_undefined
sexps_reg = symbol_undefined
sk_reg = symbol_undefined
src_reg = symbol_undefined
sum_reg = symbol_undefined
token_type_reg = symbol_undefined
tokens_reg = symbol_undefined
v1_reg = symbol_undefined
v2_reg = symbol_undefined
value1_reg = symbol_undefined
value2_reg = symbol_undefined
value3_reg = symbol_undefined
value4_reg = symbol_undefined
value_reg = symbol_undefined
var_info_reg = symbol_undefined
var_reg = symbol_undefined
variant_reg = symbol_undefined
variants_reg = symbol_undefined
vars_reg = symbol_undefined
x_reg = symbol_undefined
y_reg = symbol_undefined
temp_2 = symbol_undefined
temp_3 = symbol_undefined
temp_4 = symbol_undefined
temp_1 = symbol_undefined
def apply_cont():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont_1_d(chars, fail, k):
    globals()['value3_reg'] = fail
    globals()['value2_reg'] = chars
    globals()['value1_reg'] = value_reg
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont3

def b_cont_2_d(v1, info, k):
    globals()['value_reg'] = List(pair_tag, v1, value_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_3_d(x, info, k):
    globals()['k_reg'] = make_cont(b_cont_2_d, value_reg, info, k)
    globals()['info_reg'] = symbol_none
    globals()['x_reg'] = cdr(x)
    globals()['pc'] = annotate_cps

def b_cont_4_d(k):
    globals()['value_reg'] = list_to_vector(value_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_5_d(v1, k):
    globals()['value_reg'] = cons(v1, value_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_6_d(x, k):
    globals()['k_reg'] = make_cont(b_cont_5_d, value_reg, k)
    globals()['x_reg'] = cdr(x)
    globals()['pc'] = unannotate_cps

def b_cont_7_d(x, k):
    globals()['k_reg'] = make_cont(b_cont_5_d, value_reg, k)
    globals()['x_reg'] = caddr(x)
    globals()['pc'] = unannotate_cps

def b_cont_8_d(end, tokens_left, fail, k):
    globals()['value4_reg'] = fail
    globals()['value3_reg'] = tokens_left
    globals()['value2_reg'] = end
    globals()['value1_reg'] = value_reg
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont4

def b_cont_9_d(end, tokens, fail, k):
    globals()['value4_reg'] = fail
    globals()['value3_reg'] = rest_of(tokens)
    globals()['value2_reg'] = end
    globals()['value1_reg'] = value_reg
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont4

def b_cont_10_d(src, start, tokens, handler, fail, k):
    globals()['k_reg'] = make_cont4(b_cont4_3_d, src, start, value_reg, k)
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['src_reg'] = src
    globals()['tokens_reg'] = rest_of(tokens)
    globals()['pc'] = read_sexp

def b_cont_11_d():
    globals()['final_reg'] = value_reg
    globals()['pc'] = pc_halt_signal

def b_cont_12_d(adatum, senv, info, handler, fail, k):
    formals_list = symbol_undefined
    name = symbol_undefined
    name = untag_atom_hat(cadr_hat(adatum))
    formals_list = (value_reg if list_q(value_reg) else cons(last(value_reg), head(value_reg)))
    globals()['k_reg'] = make_cont2(b_cont2_16_d, name, value_reg, info, k)
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = cons(formals_list, senv)
    globals()['adatum_list_reg'] = cdddr_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont_13_d(adatum, senv, info, handler, fail, k):
    formals_list = symbol_undefined
    formals_list = (value_reg if list_q(value_reg) else cons(last(value_reg), head(value_reg)))
    globals()['k_reg'] = make_cont2(b_cont2_17_d, value_reg, info, k)
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = cons(formals_list, senv)
    globals()['adatum_list_reg'] = cddr_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont_14_d(aclauses, name, info, fail, k):
    globals()['value2_reg'] = fail
    globals()['value1_reg'] = define_syntax_aexp(name, value_reg, aclauses, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont_15_d(senv, info, handler, fail, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = replace_info(value_reg, info)
    globals()['pc'] = aparse

def b_cont_16_d(senv, info, handler, fail, k):
    globals()['k_reg'] = make_cont(b_cont_15_d, senv, info, handler, fail, k)
    globals()['info_reg'] = symbol_none
    globals()['x_reg'] = value_reg
    globals()['pc'] = annotate_cps

def b_cont_17_d(adatum, senv, info, handler, fail, k):
    if original_source_info_q(adatum):
        globals()['k_reg'] = k
        globals()['fail_reg'] = fail
        globals()['handler_reg'] = handler
        globals()['senv_reg'] = senv
        globals()['adatum_reg'] = replace_info(value_reg, snoc(symbol_quasiquote, info))
        globals()['pc'] = aparse
    else:
        globals()['k_reg'] = k
        globals()['fail_reg'] = fail
        globals()['handler_reg'] = handler
        globals()['senv_reg'] = senv
        globals()['adatum_reg'] = replace_info(value_reg, info)
        globals()['pc'] = aparse

def b_cont_18_d(adatum, senv, info, handler, fail, k):
    globals()['k_reg'] = make_cont(b_cont_17_d, adatum, senv, info, handler, fail, k)
    globals()['info_reg'] = symbol_none
    globals()['x_reg'] = value_reg
    globals()['pc'] = annotate_cps

def b_cont_19_d(info, fail, k):
    globals()['value2_reg'] = fail
    globals()['value1_reg'] = lit_aexp(cadr(value_reg), info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont_20_d(info, fail, k):
    globals()['value2_reg'] = fail
    globals()['value1_reg'] = lit_aexp(value_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont_21_d(msg, info, handler, fail):
    globals()['fail_reg'] = fail
    globals()['exception_reg'] = make_exception("ParseError", format("~s ~a", msg, value_reg), get_srcfile(info), get_start_line(info), get_start_char(info))
    globals()['handler_reg'] = handler
    globals()['pc'] = apply_handler2

def b_cont_22_d(bindings, k):
    globals()['value_reg'] = append(List(symbol_let), append(List(List(car_hat(bindings))), List(value_reg)))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_23_d(clauses, var, k):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if eq_q_hat(car_hat(clause), symbol_else):
        globals()['value_reg'] = cons(clause, value_reg)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont
    else:
        if symbol_q_hat(car_hat(clause)):
            globals()['value_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            globals()['k_reg'] = k
            globals()['pc'] = apply_cont
        else:
            globals()['value_reg'] = cons(append(List(append(List(symbol_memq), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            globals()['k_reg'] = k
            globals()['pc'] = apply_cont

def b_cont_24_d(fields, name, k2):
    constructor_def = symbol_undefined
    constructor_def = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(symbol_args), List(append(List(symbol_if), append(List(append(List(symbol_Equal), append(List(append(List(symbol_length), List(symbol_args))), List(length_hat(fields))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), List("wrong number of arguments")))))))))))))
    globals()['value2_reg'] = constructor_def
    globals()['value1_reg'] = name
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont_25_d(cdrs, fields, name, k):
    globals()['value_reg'] = append(List(symbol_if), append(List(append(List(cadar_hat(fields)), List(append(List(symbol_car), List(cdrs))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), append(List("~a is not of type ~a"), append(List(append(List(symbol_car), List(cdrs))), List(append(List(symbol_quote), List(cadar_hat(fields))))))))))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_26_d(adatum, macro_keyword, fail, k):
    if has_source_info_q(value_reg):
        globals()['value2_reg'] = fail
        globals()['value1_reg'] = value_reg
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        info = symbol_undefined
        info = get_source_info(adatum)
        if original_source_info_q(adatum):
            globals()['value2_reg'] = fail
            globals()['value1_reg'] = replace_info(value_reg, snoc(macro_keyword, info))
            globals()['k_reg'] = k
            globals()['pc'] = apply_cont2
        else:
            globals()['value2_reg'] = fail
            globals()['value1_reg'] = replace_info(value_reg, info)
            globals()['k_reg'] = k
            globals()['pc'] = apply_cont2

def b_cont_27_d(adatum, macro_keyword, fail, k):
    globals()['k_reg'] = make_cont(b_cont_26_d, adatum, macro_keyword, fail, k)
    globals()['info_reg'] = symbol_none
    globals()['x_reg'] = value_reg
    globals()['pc'] = annotate_cps

def b_cont_28_d(aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k):
    if value_reg:
        globals()['k2_reg'] = make_cont2(b_cont2_48_d, fail, k)
        globals()['ap_reg'] = right_apattern
        globals()['s_reg'] = value_reg
        globals()['pattern_reg'] = right_pattern
        globals()['pc'] = instantiate_hat
    else:
        globals()['k_reg'] = k
        globals()['fail_reg'] = fail
        globals()['handler_reg'] = handler
        globals()['adatum_reg'] = adatum
        globals()['aclauses_reg'] = cdr_hat(aclauses)
        globals()['clauses_reg'] = cdr(clauses)
        globals()['pc'] = process_macro_clauses_hat

def b_cont_29_d(aclauses, adatum, clauses, left_apattern, left_pattern, right_apattern, right_pattern, handler, fail, k):
    globals()['k_reg'] = make_cont(b_cont_28_d, aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k)
    globals()['ap2_reg'] = adatum
    globals()['ap1_reg'] = left_apattern
    globals()['p2_reg'] = value_reg
    globals()['p1_reg'] = left_pattern
    globals()['pc'] = unify_patterns_hat

def b_cont_30_d(v1, k):
    globals()['value_reg'] = append(List(symbol_append), append(List(v1), List(value_reg)))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_31_d(ax, depth, k):
    globals()['k_reg'] = make_cont(b_cont_30_d, value_reg, k)
    globals()['depth_reg'] = depth
    globals()['ax_reg'] = cdr_hat(ax)
    globals()['pc'] = qq_expand_cps

def b_cont_32_d(k):
    globals()['value_reg'] = append(List(symbol_list_to_vector), List(value_reg))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_33_d(depth, k):
    globals()['k_reg'] = make_cont(b_cont_32_d, k)
    globals()['depth_reg'] = depth
    globals()['ax_reg'] = value_reg
    globals()['pc'] = qq_expand_cps

def b_cont_34_d(ax, k):
    globals()['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_35_d(k):
    globals()['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_36_d(v1, k):
    globals()['value_reg'] = append(List(symbol_List), List(append(List(symbol_append), append(List(v1), List(value_reg)))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_37_d(ax, depth, k):
    globals()['k_reg'] = make_cont(b_cont_36_d, value_reg, k)
    globals()['depth_reg'] = depth
    globals()['ax_reg'] = cdr_hat(ax)
    globals()['pc'] = qq_expand_cps

def b_cont_38_d(k):
    globals()['value_reg'] = append(List(symbol_List), List(value_reg))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_39_d(ax, k):
    globals()['value_reg'] = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_40_d(k):
    globals()['value_reg'] = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont_41_d(args, handler, fail, k2):
    globals()['k_reg'] = make_cont2(b_cont2_76_d, args, handler, k2)
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = initial_contours(cadr(args))
    globals()['adatum_reg'] = value_reg
    globals()['pc'] = aparse

def b_cont_42_d(handler, fail, k2):
    globals()['k_reg'] = make_cont2(b_cont2_77_d, handler, k2)
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = initial_contours(toplevel_env)
    globals()['adatum_reg'] = value_reg
    globals()['pc'] = aparse

def b_cont_43_d(handler, fail, k2):
    globals()['k_reg'] = k2
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = initial_contours(toplevel_env)
    globals()['adatum_reg'] = value_reg
    globals()['pc'] = aparse

def b_cont_44_d(fail, k2):
    globals()['value2_reg'] = fail
    globals()['value1_reg'] = value_reg
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont_45_d(x, y, k):
    if value_reg:
        globals()['k_reg'] = k
        globals()['y_reg'] = cdr(y)
        globals()['x_reg'] = cdr(x)
        globals()['pc'] = equal_objects_q
    else:
        globals()['value_reg'] = False
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont

def b_cont_46_d(i, v1, v2, k):
    if value_reg:
        globals()['k_reg'] = k
        globals()['i_reg'] = (i) - (1)
        globals()['v2_reg'] = v2
        globals()['v1_reg'] = v1
        globals()['pc'] = equal_vectors_q
    else:
        globals()['value_reg'] = False
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont

def b_cont_47_d(ls, x, y, info, handler, fail, k):
    if value_reg:
        globals()['value2_reg'] = fail
        globals()['value1_reg'] = y
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = k
        globals()['fail_reg'] = fail
        globals()['handler_reg'] = handler
        globals()['info_reg'] = info
        globals()['ls_reg'] = ls
        globals()['y_reg'] = cdr(y)
        globals()['x_reg'] = x
        globals()['pc'] = member_loop

def b_cont_48_d(pattern, var, k):
    if value_reg:
        globals()['value_reg'] = True
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont
    else:
        globals()['k_reg'] = k
        globals()['pattern_reg'] = cdr(pattern)
        globals()['var_reg'] = var
        globals()['pc'] = occurs_q

def b_cont_49_d(ap2, p1, p2, k):
    if value_reg:
        globals()['value_reg'] = False
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont
    else:
        globals()['value_reg'] = make_sub(symbol_unit, p1, p2, ap2)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont

def b_cont_50_d(s_car, k):
    if not(value_reg):
        globals()['value_reg'] = False
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont
    else:
        globals()['value_reg'] = make_sub(symbol_composite, s_car, value_reg)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont

def b_cont_51_d(apair1, apair2, pair1, pair2, k):
    if not(value_reg):
        globals()['value_reg'] = False
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_100_d, apair2, pair2, value_reg, k)
        globals()['ap_reg'] = cdr_hat(apair1)
        globals()['s_reg'] = value_reg
        globals()['pattern_reg'] = cdr(pair1)
        globals()['pc'] = instantiate_hat

def apply_cont2():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont2_1_d(token, k):
    globals()['value1_reg'] = cons(token, value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_2_d():
    globals()['final_reg'] = value1_reg
    globals()['pc'] = pc_halt_signal

def b_cont2_3_d(k):
    globals()['value1_reg'] = binding_value(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_4_d(k):
    globals()['value1_reg'] = dlr_env_lookup(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_5_d(v1, info, k):
    globals()['value1_reg'] = app_aexp(v1, value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_6_d(adatum, senv, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_5_d, value1_reg, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_list_reg'] = cdr_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont2_7_d(info, k):
    globals()['value1_reg'] = choose_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_8_d(info, k):
    globals()['value1_reg'] = raise_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_9_d(cexps, cvar, body, info, k):
    globals()['value1_reg'] = try_catch_finally_aexp(body, cvar, cexps, value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_10_d(adatum, cvar, senv, body, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_9_d, value1_reg, cvar, body, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_list_reg'] = try_catch_finally_exps_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont2_11_d(adatum, senv, info, handler, k):
    cvar = symbol_undefined
    cvar = catch_var_hat(adatum)
    globals()['k_reg'] = make_cont2(b_cont2_10_d, adatum, cvar, senv, value1_reg, info, handler, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = cons(List(cvar), senv)
    globals()['adatum_list_reg'] = catch_exps_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont2_12_d(body, info, k):
    globals()['value1_reg'] = try_finally_aexp(body, value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_13_d(adatum, senv, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_12_d, value1_reg, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_list_reg'] = try_finally_exps_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont2_14_d(cvar, body, info, k):
    globals()['value1_reg'] = try_catch_aexp(body, cvar, value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_15_d(adatum, senv, info, handler, k):
    cvar = symbol_undefined
    cvar = catch_var_hat(adatum)
    globals()['k_reg'] = make_cont2(b_cont2_14_d, cvar, value1_reg, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = cons(List(cvar), senv)
    globals()['adatum_list_reg'] = catch_exps_hat(adatum)
    globals()['pc'] = aparse_all

def b_cont2_16_d(name, formals, info, k):
    if list_q(formals):
        globals()['value1_reg'] = trace_lambda_aexp(name, formals, value1_reg, info)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        globals()['value1_reg'] = mu_trace_lambda_aexp(name, head(formals), last(formals), value1_reg, info)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2

def b_cont2_17_d(formals, info, k):
    if list_q(formals):
        globals()['value1_reg'] = lambda_aexp(formals, value1_reg, info)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        globals()['value1_reg'] = mu_lambda_aexp(head(formals), last(formals), value1_reg, info)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2

def b_cont2_18_d(info, k):
    globals()['value1_reg'] = begin_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_19_d(adatum, info, k):
    globals()['value1_reg'] = define_b_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_20_d(adatum, info, k):
    globals()['value1_reg'] = define_b_aexp(define_var_hat(adatum), "", value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_21_d(adatum, info, k):
    globals()['value1_reg'] = define_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_22_d(adatum, info, k):
    globals()['value1_reg'] = define_aexp(define_var_hat(adatum), "", value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_23_d(info, k):
    globals()['value1_reg'] = callback2_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_24_d(info, k):
    globals()['value1_reg'] = callback1_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_25_d(info, k):
    globals()['value1_reg'] = callback0_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_26_d(info, k):
    globals()['value1_reg'] = func_aexp(value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_27_d(adatum, info, k):
    var_info = symbol_undefined
    var_info = get_source_info(cadr_hat(adatum))
    globals()['value1_reg'] = assign_aexp(untag_atom_hat(cadr_hat(adatum)), value1_reg, var_info, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_28_d(v1, v2, info, k):
    globals()['value1_reg'] = if_aexp(v1, v2, value1_reg, info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_29_d(adatum, senv, v1, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_28_d, v1, value1_reg, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = cadddr_hat(adatum)
    globals()['pc'] = aparse

def b_cont2_30_d(adatum, senv, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_29_d, adatum, senv, value1_reg, info, handler, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = caddr_hat(adatum)
    globals()['pc'] = aparse

def b_cont2_31_d(v1, info, k):
    globals()['value1_reg'] = if_aexp(v1, value1_reg, lit_aexp(False, symbol_none), info)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_32_d(adatum, senv, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_31_d, value1_reg, info, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = caddr_hat(adatum)
    globals()['pc'] = aparse

def b_cont2_33_d(senv, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = value1_reg
    globals()['pc'] = aparse

def b_cont2_34_d(a, k):
    globals()['value1_reg'] = cons(a, value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_35_d(adatum_list, senv, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_34_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_list_reg'] = cdr_hat(adatum_list)
    globals()['pc'] = aparse_all

def b_cont2_36_d(v1, k):
    globals()['value1_reg'] = cons(v1, value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_37_d(senv, src, tokens_left, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['src_reg'] = src
    globals()['tokens_reg'] = tokens_left
    globals()['pc'] = aparse_sexps

def b_cont2_38_d(bodies, k):
    globals()['value_reg'] = append(List(symbol_let), append(List(value1_reg), append(value2_reg, at_hat(bodies))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont2_39_d(procs, vars, k2):
    globals()['value2_reg'] = cons(append(List(symbol_set_b), append(List(car_hat(vars)), List(car_hat(procs)))), value2_reg)
    globals()['value1_reg'] = cons(append(List(car_hat(vars)), List(append(List(symbol_quote), List(symbol_undefined)))), value1_reg)
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont2_40_d(exp, k):
    globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_r), List(exp))), value1_reg)), List(append(List(symbol_cond), value2_reg))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont2_41_d(clauses, var, k2):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if eq_q_hat(car_hat(clause), symbol_else):
        globals()['value2_reg'] = cons(List(symbol_else, List(symbol_else_code)), value2_reg)
        globals()['value1_reg'] = cons(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
        globals()['k_reg'] = k2
        globals()['pc'] = apply_cont2
    else:
        if symbol_q_hat(car_hat(clause)):
            name = symbol_undefined
            name = car_hat(clause)
            globals()['value2_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), List(List(name))), value2_reg)
            globals()['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
            globals()['k_reg'] = k2
            globals()['pc'] = apply_cont2
        else:
            name = symbol_undefined
            name = caar_hat(clause)
            globals()['value2_reg'] = cons(append(List(append(List(symbol_memq), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), List(List(name))), value2_reg)
            globals()['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
            globals()['k_reg'] = k2
            globals()['pc'] = apply_cont2

def b_cont2_42_d(clauses, var, k2):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if eq_q_hat(car_hat(clause), symbol_else):
        globals()['value2_reg'] = cons(append(List(symbol_else), List(List(symbol_else_code))), value2_reg)
        globals()['value1_reg'] = cons(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
        globals()['k_reg'] = k2
        globals()['pc'] = apply_cont2
    else:
        if symbol_q_hat(car_hat(clause)):
            name = symbol_undefined
            name = car_hat(clause)
            globals()['value2_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(name), List(append(List(symbol_cdr), List(var))))))), value2_reg)
            globals()['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg)
            globals()['k_reg'] = k2
            globals()['pc'] = apply_cont2
        else:
            name = symbol_undefined
            name = caar_hat(clause)
            globals()['value2_reg'] = cons(append(List(append(List(symbol_memq), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(name), List(append(List(symbol_cdr), List(var))))))), value2_reg)
            globals()['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg)
            globals()['k_reg'] = k2
            globals()['pc'] = apply_cont2

def b_cont2_43_d(type_tester_name, k):
    tester_def = symbol_undefined
    tester_def = append(List(symbol_define), append(List(type_tester_name), List(append(List(symbol_lambda), append(List(List(symbol_x)), List(append(List(symbol_and), append(List(append(List(symbol_pair_q), List(symbol_x))), List(append(List(symbol_not), List(append(List(symbol_not), List(append(List(symbol_memq), append(List(append(List(symbol_car), List(symbol_x))), List(append(List(symbol_quote), List(value1_reg))))))))))))))))))
    globals()['value_reg'] = append(List(symbol_begin), append(List(tester_def), value2_reg))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont2_44_d(def_, name, k2):
    globals()['value2_reg'] = cons(def_, value2_reg)
    globals()['value1_reg'] = cons(name, value1_reg)
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont2_45_d(variants, k2):
    globals()['k2_reg'] = make_cont2(b_cont2_44_d, value2_reg, value1_reg, k2)
    globals()['variants_reg'] = cdr_hat(variants)
    globals()['pc'] = make_dd_variant_constructors_hat

def b_cont2_46_d(exp, type_name, type_tester_name, k):
    globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_r), List(exp))), value1_reg)), List(append(List(symbol_if), append(List(append(List(symbol_not), List(append(List(type_tester_name), List(symbol_r))))), append(List(append(List(symbol_error), append(List(append(List(symbol_quote), List(symbol_cases))), append(List("~a is not a valid ~a"), append(List(symbol_r), List(append(List(symbol_quote), List(type_name)))))))), List(append(List(symbol_cond), value2_reg))))))))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont

def b_cont2_47_d(macro_keyword, k):
    globals()['value1_reg'] = replace_info(value1_reg, snoc(macro_keyword, get_source_info(value1_reg)))
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_48_d(fail, k):
    globals()['value1_reg'] = value2_reg
    globals()['value2_reg'] = fail
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_49_d():
    globals()['_starlast_fail_star'] = value2_reg
    globals()['final_reg'] = value1_reg
    globals()['pc'] = pc_halt_signal

def b_cont2_50_d():
    globals()['k_reg'] = REP_k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = REP_handler
    globals()['env_reg'] = toplevel_env
    globals()['exp_reg'] = value1_reg
    globals()['pc'] = m

def b_cont2_51_d():
    globals()['final_reg'] = True
    globals()['pc'] = pc_halt_signal

def b_cont2_52_d():
    globals()['k_reg'] = make_cont2(b_cont2_51_d)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = try_parse_handler
    globals()['senv_reg'] = initial_contours(toplevel_env)
    globals()['src_reg'] = symbol_stdin
    globals()['tokens_reg'] = value1_reg
    globals()['pc'] = aparse_sexps

def b_cont2_53_d(exp, k):
    handle_debug_info(exp, value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_54_d(exp, k):
    pop_stack_trace(exp)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_55_d(args, exp, env, info, handler, k):
    if _staruse_stack_trace_star:
        push_stack_trace(exp)
    if dlr_proc_q(value1_reg):
        result = symbol_undefined
        result = dlr_apply(value1_reg, args)
        if _staruse_stack_trace_star:
            pop_stack_trace(exp)
        globals()['value1_reg'] = result
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        if procedure_object_q(value1_reg):
            if _staruse_stack_trace_star:
                globals()['k2_reg'] = make_cont2(b_cont2_54_d, exp, k)
                globals()['fail_reg'] = value2_reg
                globals()['handler_reg'] = handler
                globals()['info_reg'] = info
                globals()['env2_reg'] = env
                globals()['args_reg'] = args
                globals()['proc_reg'] = value1_reg
                globals()['pc'] = apply_proc
            else:
                globals()['k2_reg'] = k
                globals()['fail_reg'] = value2_reg
                globals()['handler_reg'] = handler
                globals()['info_reg'] = info
                globals()['env2_reg'] = env
                globals()['args_reg'] = args
                globals()['proc_reg'] = value1_reg
                globals()['pc'] = apply_proc
        else:
            globals()['fail_reg'] = value2_reg
            globals()['handler_reg'] = handler
            globals()['info_reg'] = info
            globals()['msg_reg'] = format("attempt to apply non-procedure '~a'", value1_reg)
            globals()['pc'] = runtime_error

def b_cont2_56_d(exp, operator, env, info, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_55_d, value1_reg, exp, env, info, handler, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exp_reg'] = operator
    globals()['pc'] = m

def b_cont2_57_d(handler):
    globals()['fail_reg'] = value2_reg
    globals()['exception_reg'] = value1_reg
    globals()['handler_reg'] = handler
    globals()['pc'] = apply_handler2

def b_cont2_58_d(v, k):
    globals()['value1_reg'] = v
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_59_d(fexps, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_58_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exps_reg'] = fexps
    globals()['pc'] = eval_sequence

def b_cont2_60_d(aclauses, clauses, k):
    set_binding_value_b(value1_reg, make_pattern_macro_hat(clauses, aclauses))
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_61_d(docstring, var, k):
    if procedure_object_q(value1_reg):
        set_global_value_b(var, dlr_func(value1_reg))
    else:
        set_global_value_b(var, value1_reg)
    set_global_docstring_b(var, docstring)
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_62_d(docstring, rhs_value, k):
    set_binding_value_b(value1_reg, rhs_value)
    set_binding_docstring_b(value1_reg, docstring)
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_63_d(docstring, var, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_62_d, docstring, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['var_reg'] = var
    globals()['pc'] = lookup_binding_in_first_frame

def b_cont2_64_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = binding_value(value1_reg)
    set_binding_value_b(value1_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_2_d, value1_reg, old_value, value2_reg)
    globals()['value2_reg'] = new_fail
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_65_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = dlr_env_lookup(value1_reg)
    set_global_value_b(value1_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_4_d, old_value, value1_reg, value2_reg)
    globals()['value2_reg'] = new_fail
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_66_d(var, var_info, env, handler, k):
    globals()['sk_reg'] = make_cont2(b_cont2_64_d, value1_reg, k)
    globals()['dk_reg'] = make_cont3(b_cont3_4_d, value1_reg, k)
    globals()['gk_reg'] = make_cont2(b_cont2_65_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['var_info_reg'] = var_info
    globals()['env_reg'] = env
    globals()['var_reg'] = var
    globals()['pc'] = lookup_variable

def b_cont2_67_d(else_exp, then_exp, env, handler, k):
    if value1_reg:
        globals()['k_reg'] = k
        globals()['fail_reg'] = value2_reg
        globals()['handler_reg'] = handler
        globals()['env_reg'] = env
        globals()['exp_reg'] = then_exp
        globals()['pc'] = m
    else:
        globals()['k_reg'] = k
        globals()['fail_reg'] = value2_reg
        globals()['handler_reg'] = handler
        globals()['env_reg'] = env
        globals()['exp_reg'] = else_exp
        globals()['pc'] = m

def b_cont2_68_d(k):
    globals()['value1_reg'] = callback2(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_69_d(k):
    globals()['value1_reg'] = callback1(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_70_d(k):
    globals()['value1_reg'] = callback0(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_71_d(k):
    globals()['value1_reg'] = dlr_func(value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_72_d(exps, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exps_reg'] = cdr(exps)
    globals()['pc'] = m_star

def b_cont2_73_d(exps, env, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exps_reg'] = cdr(exps)
    globals()['pc'] = eval_sequence

def b_cont2_74_d(e, handler):
    globals()['fail_reg'] = value2_reg
    globals()['exception_reg'] = e
    globals()['handler_reg'] = handler
    globals()['pc'] = apply_handler2

def b_cont2_75_d(trace_depth, k2):
    globals()['trace_depth'] = (trace_depth) - (1)
    printf("~areturn: ~s~%", make_trace_depth_string(trace_depth), value1_reg)
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont2_76_d(args, handler, k2):
    globals()['k_reg'] = k2
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = cadr(args)
    globals()['exp_reg'] = value1_reg
    globals()['pc'] = m

def b_cont2_77_d(handler, k2):
    globals()['k_reg'] = k2
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = toplevel_env
    globals()['exp_reg'] = value1_reg
    globals()['pc'] = m

def b_cont2_78_d(handler, k2):
    globals()['k_reg'] = make_cont4(b_cont4_11_d, handler, k2)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['src_reg'] = symbol_stdin
    globals()['tokens_reg'] = value1_reg
    globals()['pc'] = read_sexp

def b_cont2_79_d(handler, k2):
    globals()['k_reg'] = make_cont4(b_cont4_12_d, handler, k2)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['src_reg'] = symbol_stdin
    globals()['tokens_reg'] = value1_reg
    globals()['pc'] = read_sexp

def b_cont2_80_d(k):
    if null_q(load_stack):
        printf("WARNING: empty load-stack encountered!\n")
    else:
        globals()['load_stack'] = cdr(load_stack)
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_81_d(filename, env2, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_80_d, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env2_reg'] = env2
    globals()['src_reg'] = filename
    globals()['tokens_reg'] = value1_reg
    globals()['pc'] = read_and_eval_asexps

def b_cont2_82_d(src, tokens_left, env2, handler, k):
    if token_type_q(first(tokens_left), symbol_end_marker):
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = k
        globals()['fail_reg'] = value2_reg
        globals()['handler_reg'] = handler
        globals()['env2_reg'] = env2
        globals()['src_reg'] = src
        globals()['tokens_reg'] = tokens_left
        globals()['pc'] = read_and_eval_asexps

def b_cont2_83_d(src, tokens_left, env2, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_82_d, src, tokens_left, env2, handler, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env2
    globals()['exp_reg'] = value1_reg
    globals()['pc'] = m

def b_cont2_84_d(filenames, env2, info, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['info_reg'] = info
    globals()['env2_reg'] = env2
    globals()['filenames_reg'] = cdr(filenames)
    globals()['pc'] = load_files

def b_cont2_85_d(lst, k2):
    if member(car(lst), value1_reg):
        globals()['k_reg'] = k2
        globals()['pc'] = apply_cont2
    else:
        globals()['value1_reg'] = cons(car(lst), value1_reg)
        globals()['k_reg'] = k2
        globals()['pc'] = apply_cont2

def b_cont2_86_d(filename, handler, k2):
    module = symbol_undefined
    module = make_toplevel_env()
    set_binding_value_b(value1_reg, module)
    globals()['k_reg'] = k2
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['info_reg'] = symbol_none
    globals()['env2_reg'] = module
    globals()['filename_reg'] = filename
    globals()['pc'] = load_file

def b_cont2_87_d(args, sym, info, handler, k):
    if null_q(cdr(args)):
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        if not(environment_q(value1_reg)):
            globals()['fail_reg'] = value2_reg
            globals()['handler_reg'] = handler
            globals()['info_reg'] = info
            globals()['msg_reg'] = format("invalid module '~a'", sym)
            globals()['pc'] = runtime_error
        else:
            globals()['k_reg'] = k
            globals()['fail_reg'] = value2_reg
            globals()['handler_reg'] = handler
            globals()['info_reg'] = info
            globals()['env_reg'] = value1_reg
            globals()['args_reg'] = cdr(args)
            globals()['pc'] = get_primitive

def b_cont2_88_d(ls1, k2):
    globals()['value1_reg'] = cons(car(ls1), value1_reg)
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont2_89_d(lists, k2):
    globals()['k2_reg'] = k2
    globals()['fail_reg'] = value2_reg
    globals()['ls2_reg'] = value1_reg
    globals()['ls1_reg'] = car(lists)
    globals()['pc'] = append2

def b_cont2_90_d(iterator, proc, env, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['iterator_reg'] = iterator
    globals()['proc_reg'] = proc
    globals()['pc'] = iterate_continue

def b_cont2_91_d(iterator, proc, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['iterator_reg'] = iterator
    globals()['proc_reg'] = proc
    globals()['pc'] = iterate_collect_continue

def b_cont2_92_d(list1, proc, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['list1_reg'] = cdr(list1)
    globals()['proc_reg'] = proc
    globals()['pc'] = map1

def b_cont2_93_d(list1, proc, k):
    globals()['value1_reg'] = cons(dlr_apply(proc, List(car(list1))), value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_94_d(list1, list2, proc, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['list2_reg'] = cdr(list2)
    globals()['list1_reg'] = cdr(list1)
    globals()['proc_reg'] = proc
    globals()['pc'] = map2

def b_cont2_95_d(list1, list2, proc, k):
    globals()['value1_reg'] = cons(dlr_apply(proc, List(car(list1), car(list2))), value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_96_d(lists, proc, env, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_36_d, value1_reg, k)
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['lists_reg'] = Map(cdr, lists)
    globals()['proc_reg'] = proc
    globals()['pc'] = mapN

def b_cont2_97_d(lists, proc, k):
    globals()['value1_reg'] = cons(dlr_apply(proc, Map(car, lists)), value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont2_98_d(arg_list, proc, env, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value2_reg
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['lists_reg'] = Map(cdr, arg_list)
    globals()['proc_reg'] = proc
    globals()['pc'] = for_each_primitive

def b_cont2_99_d(new_acdr1, new_cdr1, s_car, k):
    globals()['k_reg'] = make_cont(b_cont_50_d, s_car, k)
    globals()['ap2_reg'] = value2_reg
    globals()['ap1_reg'] = new_acdr1
    globals()['p2_reg'] = value1_reg
    globals()['p1_reg'] = new_cdr1
    globals()['pc'] = unify_patterns_hat

def b_cont2_100_d(apair2, pair2, s_car, k):
    globals()['k2_reg'] = make_cont2(b_cont2_99_d, value2_reg, value1_reg, s_car, k)
    globals()['ap_reg'] = cdr_hat(apair2)
    globals()['s_reg'] = s_car
    globals()['pattern_reg'] = cdr(pair2)
    globals()['pc'] = instantiate_hat

def b_cont2_101_d(a, aa, ap, k2):
    globals()['value2_reg'] = cons_hat(aa, value2_reg, get_source_info(ap))
    globals()['value1_reg'] = cons(a, value1_reg)
    globals()['k_reg'] = k2
    globals()['pc'] = apply_cont2

def b_cont2_102_d(ap, pattern, s, k2):
    globals()['k2_reg'] = make_cont2(b_cont2_101_d, value1_reg, value2_reg, ap, k2)
    globals()['ap_reg'] = cdr_hat(ap)
    globals()['s_reg'] = s
    globals()['pattern_reg'] = cdr(pattern)
    globals()['pc'] = instantiate_hat

def b_cont2_103_d(s2, k2):
    globals()['k2_reg'] = k2
    globals()['ap_reg'] = value2_reg
    globals()['s_reg'] = s2
    globals()['pattern_reg'] = value1_reg
    globals()['pc'] = instantiate_hat

def apply_cont3():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont3_1_d(src, handler, k):
    if token_type_q(value1_reg, symbol_end_marker):
        globals()['value2_reg'] = value3_reg
        globals()['value1_reg'] = List(value1_reg)
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont2(b_cont2_1_d, value1_reg, k)
        globals()['fail_reg'] = value3_reg
        globals()['handler_reg'] = handler
        globals()['src_reg'] = src
        globals()['chars_reg'] = value2_reg
        globals()['pc'] = scan_input_loop

def b_cont3_2_d():
    globals()['final_reg'] = value1_reg
    globals()['pc'] = pc_halt_signal

def b_cont3_3_d(k):
    globals()['value1_reg'] = get_external_member(value1_reg, value2_reg)
    globals()['value2_reg'] = value3_reg
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_cont3_4_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = get_external_member(value1_reg, value2_reg)
    set_external_member_b(value1_reg, value2_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_3_d, value2_reg, value1_reg, old_value, value3_reg)
    globals()['value2_reg'] = new_fail
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def apply_cont4():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont4_1_d(src, start, k):
    globals()['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    globals()['info_reg'] = make_info(src, start, value2_reg)
    globals()['x_reg'] = list_to_vector(value1_reg)
    globals()['pc'] = annotate_cps

def b_cont4_2_d(src, start, k):
    globals()['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    globals()['info_reg'] = make_info(src, start, value2_reg)
    globals()['x_reg'] = value1_reg
    globals()['pc'] = annotate_cps

def b_cont4_3_d(src, start, v, k):
    globals()['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    globals()['info_reg'] = make_info(src, start, value2_reg)
    globals()['x_reg'] = List(v, value1_reg)
    globals()['pc'] = annotate_cps

def b_cont4_4_d(sexp1, k):
    globals()['value1_reg'] = cons(sexp1, value1_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont4

def b_cont4_5_d(src, handler, k):
    globals()['k_reg'] = make_cont4(b_cont4_4_d, value1_reg, k)
    globals()['fail_reg'] = value4_reg
    globals()['handler_reg'] = handler
    globals()['src_reg'] = src
    globals()['tokens_reg'] = value3_reg
    globals()['pc'] = read_vector_sequence

def b_cont4_6_d(expected_terminator, sexp1, src, handler, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = value4_reg
    globals()['handler_reg'] = handler
    globals()['src_reg'] = src
    globals()['expected_terminator_reg'] = expected_terminator
    globals()['tokens_reg'] = value3_reg
    globals()['sexps_reg'] = cons(sexp1, value1_reg)
    globals()['pc'] = close_sexp_sequence

def b_cont4_7_d(expected_terminator, src, handler, k):
    if token_type_q(first(value3_reg), symbol_dot):
        globals()['k_reg'] = make_cont4(b_cont4_6_d, expected_terminator, value1_reg, src, handler, k)
        globals()['fail_reg'] = value4_reg
        globals()['handler_reg'] = handler
        globals()['src_reg'] = src
        globals()['tokens_reg'] = rest_of(value3_reg)
        globals()['pc'] = read_sexp
    else:
        globals()['k_reg'] = make_cont4(b_cont4_4_d, value1_reg, k)
        globals()['fail_reg'] = value4_reg
        globals()['handler_reg'] = handler
        globals()['src_reg'] = src
        globals()['expected_terminator_reg'] = expected_terminator
        globals()['tokens_reg'] = value3_reg
        globals()['pc'] = read_sexp_sequence

def b_cont4_8_d():
    globals()['final_reg'] = value1_reg
    globals()['pc'] = pc_halt_signal

def b_cont4_9_d(senv, src, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_37_d, senv, src, value3_reg, handler, k)
    globals()['fail_reg'] = value4_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = senv
    globals()['adatum_reg'] = value1_reg
    globals()['pc'] = aparse

def b_cont4_10_d():
    globals()['_startokens_left_star'] = value3_reg
    globals()['k_reg'] = make_cont2(b_cont2_50_d)
    globals()['fail_reg'] = value4_reg
    globals()['handler_reg'] = REP_handler
    globals()['senv_reg'] = initial_contours(toplevel_env)
    globals()['adatum_reg'] = value1_reg
    globals()['pc'] = aparse

def b_cont4_11_d(handler, k2):
    if token_type_q(first(value3_reg), symbol_end_marker):
        globals()['k_reg'] = k2
        globals()['fail_reg'] = value4_reg
        globals()['handler_reg'] = handler
        globals()['senv_reg'] = initial_contours(toplevel_env)
        globals()['adatum_reg'] = value1_reg
        globals()['pc'] = aparse
    else:
        globals()['fail_reg'] = value4_reg
        globals()['handler_reg'] = handler
        globals()['src_reg'] = symbol_stdin
        globals()['tokens_reg'] = value3_reg
        globals()['msg_reg'] = "tokens left over"
        globals()['pc'] = read_error

def b_cont4_12_d(handler, k2):
    if token_type_q(first(value3_reg), symbol_end_marker):
        globals()['value2_reg'] = value4_reg
        globals()['k_reg'] = k2
        globals()['pc'] = apply_cont2
    else:
        globals()['fail_reg'] = value4_reg
        globals()['handler_reg'] = handler
        globals()['src_reg'] = symbol_stdin
        globals()['tokens_reg'] = value3_reg
        globals()['msg_reg'] = "tokens left over"
        globals()['pc'] = read_error

def b_cont4_13_d(src, env2, handler, k):
    globals()['k_reg'] = make_cont2(b_cont2_83_d, src, value3_reg, env2, handler, k)
    globals()['fail_reg'] = value4_reg
    globals()['handler_reg'] = handler
    globals()['senv_reg'] = initial_contours(env2)
    globals()['adatum_reg'] = value1_reg
    globals()['pc'] = aparse

def apply_fail():
    Apply(cadr(fail_reg), cddr(fail_reg))

def b_fail_1_d():
    globals()['final_reg'] = "no more choices"
    globals()['pc'] = pc_halt_signal

def b_fail_2_d(binding, old_value, fail):
    set_binding_value_b(binding, old_value)
    globals()['fail_reg'] = fail
    globals()['pc'] = apply_fail

def b_fail_3_d(components, dlr_obj, old_value, fail):
    set_external_member_b(dlr_obj, components, old_value)
    globals()['fail_reg'] = fail
    globals()['pc'] = apply_fail

def b_fail_4_d(old_value, var, fail):
    set_global_value_b(var, old_value)
    globals()['fail_reg'] = fail
    globals()['pc'] = apply_fail

def b_fail_5_d(exps, env, handler, fail, k):
    globals()['k_reg'] = k
    globals()['fail_reg'] = fail
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exps_reg'] = cdr(exps)
    globals()['pc'] = eval_choices

def apply_handler():
    Apply(cadr(handler_reg), cddr(handler_reg))

def b_handler_1_d():
    globals()['final_reg'] = List(symbol_exception, exception_reg)
    globals()['pc'] = pc_halt_signal

def apply_handler2():
    Apply(cadr(handler_reg), cddr(handler_reg))

def b_handler2_1_d():
    globals()['final_reg'] = List(symbol_exception, exception_reg)
    globals()['pc'] = pc_halt_signal

def b_handler2_2_d():
    globals()['_starlast_fail_star'] = fail_reg
    globals()['final_reg'] = List(symbol_exception, exception_reg)
    globals()['pc'] = pc_halt_signal

def b_handler2_3_d():
    globals()['final_reg'] = False
    globals()['pc'] = pc_halt_signal

def b_handler2_4_d(cexps, cvar, env, handler, k):
    new_env = symbol_undefined
    new_env = extend(env, List(cvar), List(exception_reg))
    globals()['k_reg'] = k
    globals()['handler_reg'] = handler
    globals()['env_reg'] = new_env
    globals()['exps_reg'] = cexps
    globals()['pc'] = eval_sequence

def b_handler2_5_d(fexps, env, handler):
    globals()['k_reg'] = make_cont2(b_cont2_74_d, exception_reg, handler)
    globals()['handler_reg'] = handler
    globals()['env_reg'] = env
    globals()['exps_reg'] = fexps
    globals()['pc'] = eval_sequence

def b_handler2_6_d(cexps, cvar, fexps, env, handler, k):
    new_env = symbol_undefined
    new_env = extend(env, List(cvar), List(exception_reg))
    catch_handler = symbol_undefined
    catch_handler = try_finally_handler(fexps, env, handler)
    globals()['k_reg'] = make_cont2(b_cont2_59_d, fexps, env, handler, k)
    globals()['handler_reg'] = catch_handler
    globals()['env_reg'] = new_env
    globals()['exps_reg'] = cexps
    globals()['pc'] = eval_sequence

def apply_proc():
    Apply(cadr(proc_reg), cddr(proc_reg))

def b_proc_1_d(bodies, formals, env):
    if Equal(length(args_reg), length(formals)):
        globals()['k_reg'] = k2_reg
        globals()['env_reg'] = extend(env, formals, args_reg)
        globals()['exps_reg'] = bodies
        globals()['pc'] = eval_sequence
    else:
        globals()['msg_reg'] = "incorrect number of arguments in application"
        globals()['pc'] = runtime_error

def b_proc_2_d(bodies, formals, runt, env):
    if GreaterThanEqual(length(args_reg), length(formals)):
        new_env = symbol_undefined
        new_env = extend(env, cons(runt, formals), cons(list_tail(args_reg, length(formals)), list_head(args_reg, length(formals))))
        globals()['k_reg'] = k2_reg
        globals()['env_reg'] = new_env
        globals()['exps_reg'] = bodies
        globals()['pc'] = eval_sequence
    else:
        globals()['msg_reg'] = "not enough arguments in application"
        globals()['pc'] = runtime_error

def b_proc_3_d(bodies, name, trace_depth, formals, env):
    if Equal(length(args_reg), length(formals)):
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, args_reg))
        globals()['trace_depth'] = (trace_depth) + (1)
        globals()['k_reg'] = make_cont2(b_cont2_75_d, trace_depth, k2_reg)
        globals()['env_reg'] = extend(env, formals, args_reg)
        globals()['exps_reg'] = bodies
        globals()['pc'] = eval_sequence
    else:
        globals()['msg_reg'] = "incorrect number of arguments in application"
        globals()['pc'] = runtime_error

def b_proc_4_d(bodies, name, trace_depth, formals, runt, env):
    if GreaterThanEqual(length(args_reg), length(formals)):
        new_env = symbol_undefined
        new_env = extend(env, cons(runt, formals), cons(list_tail(args_reg, length(formals)), list_head(args_reg, length(formals))))
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, args_reg))
        globals()['trace_depth'] = (trace_depth) + (1)
        globals()['k_reg'] = make_cont2(b_cont2_75_d, trace_depth, k2_reg)
        globals()['env_reg'] = new_env
        globals()['exps_reg'] = bodies
        globals()['pc'] = eval_sequence
    else:
        globals()['msg_reg'] = "not enough arguments in application"
        globals()['pc'] = runtime_error

def b_proc_5_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_6_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Equal(car(args_reg), 0)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_7_d():
    globals()['final_reg'] = end_of_session
    globals()['pc'] = pc_halt_signal

def b_proc_8_d():
    if length_one_q(args_reg):
        globals()['k_reg'] = make_cont(b_cont_42_d, handler_reg, fail_reg, k2_reg)
        globals()['info_reg'] = symbol_none
        globals()['x_reg'] = car(args_reg)
        globals()['pc'] = annotate_cps
    else:
        if length_two_q(args_reg):
            globals()['k_reg'] = make_cont(b_cont_41_d, args_reg, handler_reg, fail_reg, k2_reg)
            globals()['info_reg'] = symbol_none
            globals()['x_reg'] = car(args_reg)
            globals()['pc'] = annotate_cps
        else:
            globals()['msg_reg'] = "incorrect number of arguments to eval"
            globals()['pc'] = runtime_error

def b_proc_9_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to eval-ast"
        globals()['pc'] = runtime_error
    else:
        if not(list_q(car(args_reg))):
            globals()['msg_reg'] = "eval-ast called on non-abstract syntax tree argument"
            globals()['pc'] = runtime_error
        else:
            globals()['k_reg'] = k2_reg
            globals()['env_reg'] = toplevel_env
            globals()['exp_reg'] = car(args_reg)
            globals()['pc'] = m

def b_proc_10_d():
    globals()['k_reg'] = make_cont(b_cont_43_d, handler_reg, fail_reg, k2_reg)
    globals()['info_reg'] = symbol_none
    globals()['x_reg'] = car(args_reg)
    globals()['pc'] = annotate_cps

def b_proc_11_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to string-length"
        globals()['pc'] = runtime_error
    else:
        if not(string_q(car(args_reg))):
            globals()['msg_reg'] = "string-length called on non-string argument"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(string_length, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_12_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to string-ref"
        globals()['pc'] = runtime_error
    else:
        if not(string_q(car(args_reg))):
            globals()['msg_reg'] = "string-ref called with non-string first argument"
            globals()['pc'] = runtime_error
        else:
            if not(number_q(cadr(args_reg))):
                globals()['msg_reg'] = "string-ref called with non-numberic second argument"
                globals()['pc'] = runtime_error
            else:
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = Apply(string_ref, args_reg)
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2

def b_proc_13_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = aunparse(car(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_14_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = aunparse(car(caddr(car(args_reg))))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_15_d():
    globals()['k_reg'] = make_cont2(b_cont2_78_d, handler_reg, k2_reg)
    globals()['src_reg'] = symbol_stdin
    globals()['input_reg'] = car(args_reg)
    globals()['pc'] = scan_input

def b_proc_16_d():
    globals()['k_reg'] = make_cont2(b_cont2_79_d, handler_reg, k2_reg)
    globals()['src_reg'] = symbol_stdin
    globals()['input_reg'] = car(args_reg)
    globals()['pc'] = scan_input

def b_proc_17_d():
    proc = symbol_undefined
    proc_args = symbol_undefined
    proc_args = cadr(args_reg)
    proc = car(args_reg)
    globals()['args_reg'] = proc_args
    globals()['proc_reg'] = proc
    globals()['pc'] = apply_proc

def b_proc_18_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to sqrt"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "sqrt called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(sqrt, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_19_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to odd?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = odd_q(car(args_reg))
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_20_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to even?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = even_q(car(args_reg))
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_21_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to quotient"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(quotient, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_22_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to remainder"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(remainder, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_23_d():
    for_each(safe_print, args_reg)
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_24_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(string, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_25_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = substring(car(args_reg), cadr(args_reg), caddr(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_26_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = number_to_string(car(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_27_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = assv(car(args_reg), cadr(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_28_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = memv(car(args_reg), cadr(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_29_d():
    s = symbol_undefined
    s = format("~a", car(args_reg))
    globals()['_starneed_newline_star'] = true_q(not(ends_with_newline_q(s)))
    display(s)
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_30_d():
    globals()['_starneed_newline_star'] = False
    newline()
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_31_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to load"
        globals()['pc'] = runtime_error
    else:
        globals()['k_reg'] = k2_reg
        globals()['env2_reg'] = toplevel_env
        globals()['filename_reg'] = car(args_reg)
        globals()['pc'] = load_file

def b_proc_32_d():
    if length_one_q(args_reg):
        globals()['ls_reg'] = car(args_reg)
        globals()['sum_reg'] = 0
        globals()['x_reg'] = car(args_reg)
        globals()['pc'] = length_loop
    else:
        globals()['msg_reg'] = "incorrect number of arguments to length"
        globals()['pc'] = runtime_error

def b_proc_33_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = format("incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument", args_reg)
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(symbol_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_34_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to number?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(number_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_35_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to boolean?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(boolean_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_36_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to string?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(string_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_37_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(char_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_38_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char=?"
        globals()['pc'] = runtime_error
    else:
        if (not(char_q(car(args_reg)))) or (not(char_q(cadr(args_reg)))):
            globals()['msg_reg'] = "char=? requires arguments of type char"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(char_is__q, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_39_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char-whitespace?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(char_whitespace_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_40_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char->integer"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(char_to_integer, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_41_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to integer->char"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(integer_to_char, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_42_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char-alphabetic?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(char_alphabetic_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_43_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to char-numeric?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(char_numeric_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_44_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to null?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(null_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_45_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to pair?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(pair_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_46_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to cons"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(cons, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_47_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to car"
        globals()['pc'] = runtime_error
    else:
        if not(pair_q(car(args_reg))):
            globals()['msg_reg'] = format("car called on non-pair ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(car, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_48_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to cdr"
        globals()['pc'] = runtime_error
    else:
        if not(pair_q(car(args_reg))):
            globals()['msg_reg'] = format("cdr called on non-pair ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(cdr, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_49_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to cadr"
        globals()['pc'] = runtime_error
    else:
        if not(length_at_least_q(2, car(args_reg))):
            globals()['msg_reg'] = format("cadr called on incorrect list structure ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(cadr, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_50_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to caddr"
        globals()['pc'] = runtime_error
    else:
        if not(length_at_least_q(3, car(args_reg))):
            globals()['msg_reg'] = format("caddr called on incorrect list structure ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(caddr, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_51_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = args_reg
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_52_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to set"
        globals()['pc'] = runtime_error
    else:
        globals()['lst_reg'] = car(args_reg)
        globals()['pc'] = make_set

def b_proc_53_d():
    if not(all_numeric_q(args_reg)):
        globals()['msg_reg'] = "+ called on non-numeric argument(s)"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(plus, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_54_d():
    if null_q(args_reg):
        globals()['msg_reg'] = "incorrect number of arguments to -"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "- called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(minus, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_55_d():
    if not(all_numeric_q(args_reg)):
        globals()['msg_reg'] = "* called on non-numeric argument(s)"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(multiply, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_56_d():
    if null_q(args_reg):
        globals()['msg_reg'] = "incorrect number of arguments to /"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "/ called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            if member(0, cdr(args_reg)):
                globals()['msg_reg'] = "division by zero"
                globals()['pc'] = runtime_error
            else:
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = Apply(divide, args_reg)
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2

def b_proc_57_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to %"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "% called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            if Equal(cadr(args_reg), 0):
                globals()['msg_reg'] = "modulo by zero"
                globals()['pc'] = runtime_error
            else:
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = Apply(modulo, args_reg)
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2

def b_proc_58_d():
    if not(length_at_least_q(2, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to <"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "< called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(LessThan, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_59_d():
    if not(length_at_least_q(2, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to >"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "> called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(GreaterThan, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_60_d():
    if not(length_at_least_q(2, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to <="
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "<= called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(LessThanEqual, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_61_d():
    if not(length_at_least_q(2, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to >="
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = ">= called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(GreaterThanEqual, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_62_d():
    if not(length_at_least_q(2, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to ="
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "= called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(Equal, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_63_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to abs"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "abs called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(abs, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_64_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to equal?"
        globals()['pc'] = runtime_error
    else:
        globals()['k_reg'] = make_cont(b_cont_44_d, fail_reg, k2_reg)
        globals()['y_reg'] = cadr(args_reg)
        globals()['x_reg'] = car(args_reg)
        globals()['pc'] = equal_objects_q

def b_proc_65_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to eq?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(eq_q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_66_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to memq"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(memq, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_67_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to member"
        globals()['pc'] = runtime_error
    else:
        globals()['k_reg'] = k2_reg
        globals()['ls_reg'] = cadr(args_reg)
        globals()['y_reg'] = cadr(args_reg)
        globals()['x_reg'] = car(args_reg)
        globals()['pc'] = member_loop

def b_proc_68_d():
    if (null_q(args_reg)) or (length_at_least_q(4, args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to range"
        globals()['pc'] = runtime_error
    else:
        if not(all_numeric_q(args_reg)):
            globals()['msg_reg'] = "range called on non-numeric argument(s)"
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(Range, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_69_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(snoc, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_70_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(rac, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_71_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(rdc, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_72_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to set-car!"
        globals()['pc'] = runtime_error
    else:
        if not(pair_q(car(args_reg))):
            globals()['msg_reg'] = format("set-car! called on non-pair ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(set_car_b, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_73_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to set-cdr!"
        globals()['pc'] = runtime_error
    else:
        if not(pair_q(car(args_reg))):
            globals()['msg_reg'] = format("set-cdr! called on non-pair ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(set_cdr_b, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_74_d():
    filename = symbol_undefined
    filename = car(args_reg)
    if null_q(cdr(args_reg)):
        globals()['k_reg'] = k2_reg
        globals()['info_reg'] = symbol_none
        globals()['filename_reg'] = filename
        globals()['pc'] = load_file
    else:
        module_name = symbol_undefined
        module_name = cadr(args_reg)
        globals()['k_reg'] = make_cont2(b_cont2_86_d, filename, handler_reg, k2_reg)
        globals()['env_reg'] = env2_reg
        globals()['var_reg'] = module_name
        globals()['pc'] = lookup_binding_in_first_frame

def b_proc_75_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = car(_starstack_trace_star)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_76_d():
    globals()['k_reg'] = k2_reg
    globals()['env_reg'] = env2_reg
    globals()['pc'] = get_primitive

def b_proc_77_d(k):
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = car(args_reg)
    globals()['k_reg'] = k
    globals()['pc'] = apply_cont2

def b_proc_78_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to call/cc"
        globals()['pc'] = runtime_error
    else:
        proc = symbol_undefined
        proc = car(args_reg)
        if not(procedure_object_q(proc)):
            globals()['msg_reg'] = "call/cc called with non-procedure"
            globals()['pc'] = runtime_error
        else:
            fake_k = symbol_undefined
            fake_k = make_proc(b_proc_77_d, k2_reg)
            if dlr_proc_q(proc):
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = dlr_apply(proc, List(fake_k))
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2
            else:
                globals()['args_reg'] = List(fake_k)
                globals()['proc_reg'] = proc
                globals()['pc'] = apply_proc

def b_proc_79_d():
    if null_q(args_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = void_value
        globals()['k_reg'] = REP_k
        globals()['pc'] = apply_cont2
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = car(args_reg)
        globals()['k_reg'] = REP_k
        globals()['pc'] = apply_cont2

def b_proc_80_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to require"
        globals()['pc'] = runtime_error
    else:
        if true_q(car(args_reg)):
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = symbol_ok
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2
        else:
            globals()['pc'] = apply_fail

def b_proc_81_d():
    if not(null_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to cut"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = REP_fail
        globals()['value1_reg'] = symbol_ok
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_82_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to reverse"
        globals()['pc'] = runtime_error
    else:
        if not(list_q(args_reg)):
            globals()['msg_reg'] = format("reverse called on incorrect list structure ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(reverse, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_83_d():
    globals()['lists_reg'] = args_reg
    globals()['pc'] = append_all

def b_proc_84_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to string->number"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(string_to_number, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_85_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to string=?"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(string_is__q, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_86_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to list->vector"
        globals()['pc'] = runtime_error
    else:
        if not(list_q(car(args_reg))):
            globals()['msg_reg'] = format("list->vector called on incorrect list structure ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = Apply(list_to_vector, args_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2

def b_proc_87_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to list->string"
        globals()['pc'] = runtime_error
    else:
        if not(list_q(car(args_reg))):
            globals()['msg_reg'] = format("list->string called on incorrect list structure ~s", car(args_reg))
            globals()['pc'] = runtime_error
        else:
            if not(all_char_q(car(args_reg))):
                globals()['msg_reg'] = format("list->string called on non-char list ~s", car(args_reg))
                globals()['pc'] = runtime_error
            else:
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = Apply(list_to_string, args_reg)
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2

def b_proc_88_d():
    globals()['lst_reg'] = directory(args_reg, env2_reg)
    globals()['pc'] = make_set

def b_proc_89_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = get_current_time()
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_90_d():
    globals()['k_reg'] = k2_reg
    globals()['env_reg'] = env2_reg
    globals()['proc_reg'] = car(args_reg)
    globals()['args_reg'] = cdr(args_reg)
    globals()['pc'] = map_primitive

def b_proc_91_d():
    globals()['k_reg'] = k2_reg
    globals()['env_reg'] = env2_reg
    globals()['lists_reg'] = cdr(args_reg)
    globals()['proc_reg'] = car(args_reg)
    globals()['pc'] = for_each_primitive

def b_proc_92_d():
    if LessThan(length(args_reg), 1):
        globals()['msg_reg'] = "incorrect number of arguments to format"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(format, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_93_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = env2_reg
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_94_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = using(args_reg, env2_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_95_d():
    if not(length_one_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to not"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = not(car(args_reg))
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_96_d():
    Apply(printf, args_reg)
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = void_value
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_97_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(vector_native, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_98_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = vector_set_b(car(args_reg), cadr(args_reg), caddr(args_reg))
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_99_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(vector_ref, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_100_d():
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = Apply(make_vector, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def b_proc_101_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to 'error' (should be 2)"
        globals()['pc'] = runtime_error
    else:
        location = symbol_undefined
        message = symbol_undefined
        location = format("Error in '~a': ", car(args_reg))
        message = string_append(location, Apply(format, cdr(args_reg)))
        globals()['msg_reg'] = message
        globals()['pc'] = runtime_error

def b_proc_102_d():
    if not(length_two_q(args_reg)):
        globals()['msg_reg'] = "incorrect number of arguments to list-ref"
        globals()['pc'] = runtime_error
    else:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = Apply(list_ref, args_reg)
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2

def b_proc_103_d():
    if null_q(args_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = current_directory()
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        if length_one_q(args_reg):
            if string_q(car(args_reg)):
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = current_directory(car(args_reg))
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2
            else:
                globals()['msg_reg'] = "directory must be a string"
                globals()['pc'] = runtime_error
        else:
            globals()['msg_reg'] = "incorrect number of arguments to current-directory"
            globals()['pc'] = runtime_error

def b_proc_104_d():
    if (length_one_q(args_reg)) and (number_q(car(args_reg))):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = round(car(args_reg))
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['msg_reg'] = "round requires exactly one number"
        globals()['pc'] = runtime_error

def b_proc_105_d():
    if (length_one_q(args_reg)) and (boolean_q(car(args_reg))):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = set_use_stack_trace(car(args_reg))
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['msg_reg'] = "set-stack-trace! requires exactly one boolean"
        globals()['pc'] = runtime_error

def b_proc_106_d(external_function_object):
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = apply_star(external_function_object, args_reg)
    globals()['k_reg'] = k2_reg
    globals()['pc'] = apply_cont2

def apply_macro():
    Apply(cadr(macro_reg), cddr(macro_reg))

def b_macro_1_d():
    if symbol_q_hat(cadr_hat(datum_reg)):
        name = symbol_undefined
        bindings = symbol_undefined
        vars = symbol_undefined
        exps = symbol_undefined
        bodies = symbol_undefined
        name = cadr_hat(datum_reg)
        bindings = caddr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cdddr_hat(datum_reg)
        globals()['value_reg'] = append(List(symbol_letrec), append(List(List(append(List(name), List(append(List(symbol_lambda), append(List(vars), at_hat(bodies))))))), List(append(List(name), at_hat(exps)))))
        globals()['pc'] = apply_cont
    else:
        bindings = symbol_undefined
        vars = symbol_undefined
        exps = symbol_undefined
        bodies = symbol_undefined
        bindings = cadr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cddr_hat(datum_reg)
        globals()['value_reg'] = append(List(append(List(symbol_lambda), append(List(vars), at_hat(bodies)))), at_hat(exps))
        globals()['pc'] = apply_cont

def b_macro_2_d():
    decls = symbol_undefined
    vars = symbol_undefined
    procs = symbol_undefined
    bodies = symbol_undefined
    decls = cadr_hat(datum_reg)
    vars = map_hat(car_hat, decls)
    procs = map_hat(cadr_hat, decls)
    bodies = cddr_hat(datum_reg)
    globals()['k2_reg'] = make_cont2(b_cont2_38_d, bodies, k_reg)
    globals()['procs_reg'] = procs
    globals()['vars_reg'] = vars
    globals()['pc'] = create_letrec_assignments_hat

def b_macro_3_d():
    name = symbol_undefined
    formals = symbol_undefined
    bodies = symbol_undefined
    bodies = cddr_hat(datum_reg)
    formals = cdadr_hat(datum_reg)
    name = caadr_hat(datum_reg)
    globals()['value_reg'] = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(formals), at_hat(bodies))))))
    globals()['pc'] = apply_cont

def b_macro_4_d():
    exps = symbol_undefined
    exps = cdr_hat(datum_reg)
    if null_q_hat(exps):
        globals()['value_reg'] = True
        globals()['pc'] = apply_cont
    else:
        if null_q_hat(cdr_hat(exps)):
            globals()['value_reg'] = car_hat(exps)
            globals()['pc'] = apply_cont
        else:
            globals()['value_reg'] = append(List(symbol_if), append(List(car_hat(exps)), append(List(append(List(symbol_and), at_hat(cdr_hat(exps)))), List(False))))
            globals()['pc'] = apply_cont

def b_macro_5_d():
    exps = symbol_undefined
    exps = cdr_hat(datum_reg)
    if null_q_hat(exps):
        globals()['value_reg'] = False
        globals()['pc'] = apply_cont
    else:
        if null_q_hat(cdr_hat(exps)):
            globals()['value_reg'] = car_hat(exps)
            globals()['pc'] = apply_cont
        else:
            globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(car_hat(exps)))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_or), at_hat(cdr_hat(exps))))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(symbol_bool), List(List(symbol_else_code))))))))
            globals()['pc'] = apply_cont

def b_macro_6_d():
    clauses = symbol_undefined
    clauses = cdr_hat(datum_reg)
    if null_q_hat(clauses):
        globals()['adatum_reg'] = datum_reg
        globals()['msg_reg'] = "empty (cond) expression"
        globals()['pc'] = amacro_error
    else:
        first_clause = symbol_undefined
        other_clauses = symbol_undefined
        other_clauses = cdr_hat(clauses)
        first_clause = car_hat(clauses)
        if (null_q_hat(first_clause)) or (not(list_q_hat(first_clause))):
            globals()['adatum_reg'] = first_clause
            globals()['msg_reg'] = "improper cond clause"
            globals()['pc'] = amacro_error
        else:
            test_exp = symbol_undefined
            then_exps = symbol_undefined
            then_exps = cdr_hat(first_clause)
            test_exp = car_hat(first_clause)
            if eq_q_hat(test_exp, symbol_else):
                if null_q_hat(then_exps):
                    globals()['adatum_reg'] = first_clause
                    globals()['msg_reg'] = "improper else clause"
                    globals()['pc'] = amacro_error
                else:
                    if null_q_hat(cdr_hat(then_exps)):
                        globals()['value_reg'] = car_hat(then_exps)
                        globals()['pc'] = apply_cont
                    else:
                        globals()['value_reg'] = append(List(symbol_begin), at_hat(then_exps))
                        globals()['pc'] = apply_cont
            else:
                if null_q_hat(then_exps):
                    if null_q_hat(other_clauses):
                        globals()['value_reg'] = append(List(symbol_let), append(List(List(append(List(symbol_bool), List(test_exp)))), List(append(List(symbol_if), append(List(symbol_bool), List(symbol_bool))))))
                        globals()['pc'] = apply_cont
                    else:
                        globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_cond), at_hat(other_clauses)))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(symbol_bool), List(List(symbol_else_code))))))))
                        globals()['pc'] = apply_cont
                else:
                    if eq_q_hat(car_hat(then_exps), symbol__is_to_):
                        if null_q_hat(cdr_hat(then_exps)):
                            globals()['adatum_reg'] = first_clause
                            globals()['msg_reg'] = "improper => clause"
                            globals()['pc'] = amacro_error
                        else:
                            if null_q_hat(other_clauses):
                                globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), List(append(List(symbol_th), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(cadr_hat(then_exps))))))))), List(append(List(symbol_if), append(List(symbol_bool), List(append(List(List(symbol_th)), List(symbol_bool))))))))
                                globals()['pc'] = apply_cont
                            else:
                                globals()['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), append(List(append(List(symbol_th), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(cadr_hat(then_exps))))))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_cond), at_hat(other_clauses))))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(append(List(List(symbol_th)), List(symbol_bool))), List(List(symbol_else_code))))))))
                                globals()['pc'] = apply_cont
                    else:
                        if null_q_hat(other_clauses):
                            if null_q_hat(cdr_hat(then_exps)):
                                globals()['value_reg'] = append(List(symbol_if), append(List(test_exp), List(car_hat(then_exps))))
                                globals()['pc'] = apply_cont
                            else:
                                globals()['value_reg'] = append(List(symbol_if), append(List(test_exp), List(append(List(symbol_begin), at_hat(then_exps)))))
                                globals()['pc'] = apply_cont
                        else:
                            if null_q_hat(cdr_hat(then_exps)):
                                globals()['value_reg'] = append(List(symbol_if), append(List(test_exp), append(List(car_hat(then_exps)), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                globals()['pc'] = apply_cont
                            else:
                                globals()['value_reg'] = append(List(symbol_if), append(List(test_exp), append(List(append(List(symbol_begin), at_hat(then_exps))), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                globals()['pc'] = apply_cont

def b_macro_7_d():
    bindings = symbol_undefined
    bodies = symbol_undefined
    bodies = cddr_hat(datum_reg)
    bindings = cadr_hat(datum_reg)
    globals()['bodies_reg'] = bodies
    globals()['bindings_reg'] = bindings
    globals()['pc'] = nest_let_star_bindings_hat

def b_macro_8_d():
    exp = symbol_undefined
    clauses = symbol_undefined
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    globals()['k2_reg'] = make_cont2(b_cont2_40_d, exp, k_reg)
    globals()['clauses_reg'] = clauses
    globals()['var_reg'] = symbol_r
    globals()['pc'] = case_clauses_to_cond_clauses_hat

def b_macro_9_d():
    exp = symbol_undefined
    clauses = symbol_undefined
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    globals()['k2_reg'] = make_cont2(b_cont2_40_d, exp, k_reg)
    globals()['clauses_reg'] = clauses
    globals()['var_reg'] = symbol_r
    globals()['pc'] = record_case_clauses_to_cond_clauses_hat

def b_macro_10_d():
    datatype_name = symbol_undefined
    type_tester_name = symbol_undefined
    datatype_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(datatype_name), "?"))
    if not(eq_q_hat(caddr_hat(datum_reg), type_tester_name)):
        globals()['adatum_reg'] = caddr_hat(datum_reg)
        globals()['msg_reg'] = format("datatype tester predicate not named ~a", type_tester_name)
        globals()['pc'] = amacro_error
    else:
        variants = symbol_undefined
        variants = cdddr_hat(datum_reg)
        globals()['k2_reg'] = make_cont2(b_cont2_43_d, type_tester_name, k_reg)
        globals()['variants_reg'] = variants
        globals()['pc'] = make_dd_variant_constructors_hat

def b_macro_11_d():
    type_name = symbol_undefined
    type_tester_name = symbol_undefined
    exp = symbol_undefined
    clauses = symbol_undefined
    type_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(type_name), "?"))
    exp = caddr_hat(datum_reg)
    clauses = cdddr_hat(datum_reg)
    globals()['k2_reg'] = make_cont2(b_cont2_46_d, exp, type_name, type_tester_name, k_reg)
    globals()['clauses_reg'] = clauses
    globals()['var_reg'] = symbol_r
    globals()['pc'] = record_case_clauses_to_cond_clauses_hat

def next_avail(n):
    return string_ref(chars_to_scan, n)

def remaining(n):
    return (1) + (n)

def initialize_scan_counters():
    globals()['scan_line'] = 1
    globals()['scan_char'] = 1
    globals()['scan_position'] = 1
    globals()['last_scan_line'] = scan_line
    globals()['last_scan_char'] = scan_char
    globals()['last_scan_position'] = scan_position

def increment_scan_counters(chars):
    globals()['last_scan_line'] = scan_line
    globals()['last_scan_char'] = scan_char
    globals()['last_scan_position'] = scan_position
    if char_is__q(next_avail(chars), '\n'):
        globals()['scan_line'] = (1) + (scan_line)
        globals()['scan_char'] = 1
    else:
        globals()['scan_char'] = (1) + (scan_char)
    globals()['scan_position'] = (1) + (scan_position)

def mark_token_start():
    globals()['token_start_line'] = scan_line
    globals()['token_start_char'] = scan_char
    globals()['token_start_position'] = scan_position

def scan_input():
    initialize_scan_counters()
    globals()['chars_to_scan'] = string_append(input_reg, string('\0'))
    globals()['chars_reg'] = 0
    globals()['pc'] = scan_input_loop

def scan_input_loop():
    globals()['k_reg'] = make_cont3(b_cont3_1_d, src_reg, handler_reg, k_reg)
    globals()['buffer_reg'] = symbol_emptylist
    globals()['action_reg'] = List(symbol_goto, symbol_start_state)
    globals()['pc'] = apply_action

def apply_action():
    if (car(action_reg)) is (symbol_shift):
        next = symbol_undefined
        next = list_ref(action_reg, 1)
        increment_scan_counters(chars_reg)
        globals()['buffer_reg'] = cons(next_avail(chars_reg), buffer_reg)
        globals()['chars_reg'] = remaining(chars_reg)
        globals()['action_reg'] = next
        globals()['pc'] = apply_action
    else:
        if (car(action_reg)) is (symbol_replace):
            new_char = symbol_undefined
            next = symbol_undefined
            next = list_ref(action_reg, 2)
            new_char = list_ref(action_reg, 1)
            increment_scan_counters(chars_reg)
            globals()['chars_reg'] = remaining(chars_reg)
            globals()['buffer_reg'] = cons(new_char, buffer_reg)
            globals()['action_reg'] = next
            globals()['pc'] = apply_action
        else:
            if (car(action_reg)) is (symbol_drop):
                next = symbol_undefined
                next = list_ref(action_reg, 1)
                increment_scan_counters(chars_reg)
                globals()['chars_reg'] = remaining(chars_reg)
                globals()['action_reg'] = next
                globals()['pc'] = apply_action
            else:
                if (car(action_reg)) is (symbol_goto):
                    state = symbol_undefined
                    state = list_ref(action_reg, 1)
                    if (state) is (symbol_token_start_state):
                        mark_token_start()
                    action = symbol_undefined
                    action = apply_state(state, next_avail(chars_reg))
                    if (action) is (symbol_error):
                        globals()['pc'] = unexpected_char_error
                    else:
                        globals()['action_reg'] = action
                        globals()['pc'] = apply_action
                else:
                    if (car(action_reg)) is (symbol_emit):
                        token_type = symbol_undefined
                        token_type = list_ref(action_reg, 1)
                        globals()['k_reg'] = make_cont(b_cont_1_d, chars_reg, fail_reg, k_reg)
                        globals()['token_type_reg'] = token_type
                        globals()['pc'] = convert_buffer_to_token
                    else:
                        raise Exception("symbol_apply_action: " + format("invalid action: ~a", *[action_reg]))

def scan_error():
    globals()['exception_reg'] = make_exception("ScanError", msg_reg, src_reg, line_reg, char_reg)
    globals()['pc'] = apply_handler2

def unexpected_char_error():
    c = symbol_undefined
    c = next_avail(chars_reg)
    if char_is__q(c, '\0'):
        globals()['char_reg'] = scan_char
        globals()['line_reg'] = scan_line
        globals()['msg_reg'] = "unexpected end of input"
        globals()['pc'] = scan_error
    else:
        globals()['char_reg'] = scan_char
        globals()['line_reg'] = scan_line
        globals()['msg_reg'] = format("unexpected character '~a' encountered", c)
        globals()['pc'] = scan_error

def convert_buffer_to_token():
    buffer = symbol_undefined
    buffer = reverse(buffer_reg)
    if (token_type_reg) is (symbol_end_marker):
        globals()['value_reg'] = make_token1(symbol_end_marker)
        globals()['pc'] = apply_cont
    else:
        if (token_type_reg) is (symbol_integer):
            globals()['value_reg'] = make_token2(symbol_integer, list_to_string(buffer))
            globals()['pc'] = apply_cont
        else:
            if (token_type_reg) is (symbol_decimal):
                globals()['value_reg'] = make_token2(symbol_decimal, list_to_string(buffer))
                globals()['pc'] = apply_cont
            else:
                if (token_type_reg) is (symbol_rational):
                    globals()['value_reg'] = make_token2(symbol_rational, list_to_string(buffer))
                    globals()['pc'] = apply_cont
                else:
                    if (token_type_reg) is (symbol_identifier):
                        globals()['value_reg'] = make_token2(symbol_identifier, string_to_symbol(list_to_string(buffer)))
                        globals()['pc'] = apply_cont
                    else:
                        if (token_type_reg) is (symbol_boolean):
                            globals()['value_reg'] = make_token2(symbol_boolean, (char_is__q(car(buffer), 't')) or (char_is__q(car(buffer), 'T')))
                            globals()['pc'] = apply_cont
                        else:
                            if (token_type_reg) is (symbol_character):
                                globals()['value_reg'] = make_token2(symbol_character, car(buffer))
                                globals()['pc'] = apply_cont
                            else:
                                if (token_type_reg) is (symbol_named_character):
                                    name = symbol_undefined
                                    name = list_to_string(buffer)
                                    if string_is__q(name, "nul"):
                                        globals()['value_reg'] = make_token2(symbol_character, '\0')
                                        globals()['pc'] = apply_cont
                                    else:
                                        if string_is__q(name, "space"):
                                            globals()['value_reg'] = make_token2(symbol_character, ' ')
                                            globals()['pc'] = apply_cont
                                        else:
                                            if string_is__q(name, "tab"):
                                                globals()['value_reg'] = make_token2(symbol_character, '\t')
                                                globals()['pc'] = apply_cont
                                            else:
                                                if string_is__q(name, "newline"):
                                                    globals()['value_reg'] = make_token2(symbol_character, '\n')
                                                    globals()['pc'] = apply_cont
                                                else:
                                                    if string_is__q(name, "linefeed"):
                                                        globals()['value_reg'] = make_token2(symbol_character, '\n')
                                                        globals()['pc'] = apply_cont
                                                    else:
                                                        if string_is__q(name, "backspace"):
                                                            globals()['value_reg'] = make_token2(symbol_character, '\b')
                                                            globals()['pc'] = apply_cont
                                                        else:
                                                            if string_is__q(name, "return"):
                                                                globals()['value_reg'] = make_token2(symbol_character, '\r')
                                                                globals()['pc'] = apply_cont
                                                            else:
                                                                if string_is__q(name, "page"):
                                                                    globals()['value_reg'] = make_token2(symbol_character, u"\u000C")
                                                                    globals()['pc'] = apply_cont
                                                                else:
                                                                    globals()['char_reg'] = token_start_char
                                                                    globals()['line_reg'] = token_start_line
                                                                    globals()['msg_reg'] = format("invalid character name #\\~a", name)
                                                                    globals()['pc'] = scan_error
                                else:
                                    if (token_type_reg) is (symbol_string):
                                        globals()['value_reg'] = make_token2(symbol_string, list_to_string(buffer))
                                        globals()['pc'] = apply_cont
                                    else:
                                        globals()['value_reg'] = make_token1(token_type_reg)
                                        globals()['pc'] = apply_cont

def make_token1(token_type):
    start = symbol_undefined
    end = symbol_undefined
    end = List(last_scan_line, last_scan_char, last_scan_position)
    start = List(token_start_line, token_start_char, token_start_position)
    if (token_type) is (symbol_end_marker):
        return List(token_type, end, end)
    else:
        return List(token_type, start, end)

def make_token2(token_type, token_info):
    return List(token_type, token_info, List(token_start_line, token_start_char, token_start_position), List(last_scan_line, last_scan_char, last_scan_position))

def token_type_q(token, class_):
    return (car(token)) is (class_)

def get_token_start(token):
    return rac(rdc(token))

def get_token_end(token):
    return rac(token)

def get_token_start_line(token):
    return car(get_token_start(token))

def get_token_start_char(token):
    return cadr(get_token_start(token))

def get_token_start_pos(token):
    return caddr(get_token_start(token))

def rac(ls):
    if null_q(cdr(ls)):
        return car(ls)
    else:
        current = symbol_undefined
        current = cdr(ls)
        while pair_q(cdr(current)):
            current = cdr(current)
        return car(current)

def rdc(ls):
    if null_q(cdr(ls)):
        return List()
    else:
        retval = symbol_undefined
        front = symbol_undefined
        current = symbol_undefined
        retval = List(car(ls))
        front = retval
        current = cdr(ls)
        while pair_q(cdr(current)):
            set_cdr_b(retval, List(car(current)))
            retval = cdr(retval)
            current = cdr(current)
        return front

def snoc(x, ls):
    if null_q(ls):
        return List(x)
    else:
        retval = symbol_undefined
        front = symbol_undefined
        current = symbol_undefined
        retval = List(car(ls))
        front = retval
        current = cdr(ls)
        while pair_q(current):
            set_cdr_b(retval, List(car(current)))
            retval = cdr(retval)
            current = cdr(current)
        set_cdr_b(retval, List(x))
        return front

def char_delimiter_q(c):
    return (char_whitespace_q(c)) or (char_is__q(c, "'")) or (char_is__q(c, '(')) or (char_is__q(c, '[')) or (char_is__q(c, ')')) or (char_is__q(c, ']')) or (char_is__q(c, '"')) or (char_is__q(c, ';')) or (char_is__q(c, '#')) or (char_is__q(c, '\0'))

def char_initial_q(c):
    return (char_alphabetic_q(c)) or (char_is__q(c, '!')) or (char_is__q(c, '$')) or (char_is__q(c, '%')) or (char_is__q(c, '&')) or (char_is__q(c, '*')) or (char_is__q(c, '/')) or (char_is__q(c, ':')) or (char_is__q(c, '<')) or (char_is__q(c, '=')) or (char_is__q(c, '>')) or (char_is__q(c, '?')) or (char_is__q(c, '^')) or (char_is__q(c, '_')) or (char_is__q(c, '~'))

def char_special_subsequent_q(c):
    return (char_is__q(c, '+')) or (char_is__q(c, '-')) or (char_is__q(c, '@')) or (char_is__q(c, '.'))

def char_subsequent_q(c):
    return (char_initial_q(c)) or (char_numeric_q(c)) or (char_special_subsequent_q(c))

def char_sign_q(c):
    return (char_is__q(c, '+')) or (char_is__q(c, '-'))

def char_boolean_q(c):
    return (char_is__q(c, 't')) or (char_is__q(c, 'T')) or (char_is__q(c, 'f')) or (char_is__q(c, 'F'))

def apply_state(state, c):
    if (state) is (symbol_start_state):
        if char_whitespace_q(c):
            return List(symbol_drop, List(symbol_goto, symbol_start_state))
        else:
            if char_is__q(c, ';'):
                return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if char_is__q(c, '\0'):
                    return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                else:
                    return List(symbol_goto, symbol_token_start_state)
    else:
        if (state) is (symbol_token_start_state):
            if char_is__q(c, '('):
                return List(symbol_drop, List(symbol_emit, symbol_lparen))
            else:
                if char_is__q(c, '['):
                    return List(symbol_drop, List(symbol_emit, symbol_lbracket))
                else:
                    if char_is__q(c, ')'):
                        return List(symbol_drop, List(symbol_emit, symbol_rparen))
                    else:
                        if char_is__q(c, ']'):
                            return List(symbol_drop, List(symbol_emit, symbol_rbracket))
                        else:
                            if char_is__q(c, "'"):
                                return List(symbol_drop, List(symbol_emit, symbol_apostrophe))
                            else:
                                if char_is__q(c, '`'):
                                    return List(symbol_drop, List(symbol_emit, symbol_backquote))
                                else:
                                    if char_is__q(c, ','):
                                        return List(symbol_drop, List(symbol_goto, symbol_comma_state))
                                    else:
                                        if char_is__q(c, '#'):
                                            return List(symbol_drop, List(symbol_goto, symbol_hash_prefix_state))
                                        else:
                                            if char_is__q(c, '"'):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_state))
                                            else:
                                                if char_initial_q(c):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if char_sign_q(c):
                                                        return List(symbol_shift, List(symbol_goto, symbol_signed_state))
                                                    else:
                                                        if char_is__q(c, '.'):
                                                            return List(symbol_shift, List(symbol_goto, symbol_decimal_point_state))
                                                        else:
                                                            if char_numeric_q(c):
                                                                return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                            else:
                                                                return symbol_error
        else:
            if (state) is (symbol_comment_state):
                if char_is__q(c, '\n'):
                    return List(symbol_drop, List(symbol_goto, symbol_start_state))
                else:
                    if char_is__q(c, '\0'):
                        return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                    else:
                        return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if (state) is (symbol_comma_state):
                    if char_is__q(c, '@'):
                        return List(symbol_drop, List(symbol_emit, symbol_comma_at))
                    else:
                        return List(symbol_emit, symbol_comma)
                else:
                    if (state) is (symbol_hash_prefix_state):
                        if char_boolean_q(c):
                            return List(symbol_shift, List(symbol_emit, symbol_boolean))
                        else:
                            if char_is__q(c, '\\'):
                                return List(symbol_drop, List(symbol_goto, symbol_character_state))
                            else:
                                if char_is__q(c, '('):
                                    return List(symbol_drop, List(symbol_emit, symbol_lvector))
                                else:
                                    return symbol_error
                    else:
                        if (state) is (symbol_character_state):
                            if char_alphabetic_q(c):
                                return List(symbol_shift, List(symbol_goto, symbol_alphabetic_character_state))
                            else:
                                if not(char_is__q(c, '\0')):
                                    return List(symbol_shift, List(symbol_emit, symbol_character))
                                else:
                                    return symbol_error
                        else:
                            if (state) is (symbol_alphabetic_character_state):
                                if char_alphabetic_q(c):
                                    return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    return List(symbol_emit, symbol_character)
                            else:
                                if (state) is (symbol_named_character_state):
                                    if char_delimiter_q(c):
                                        return List(symbol_emit, symbol_named_character)
                                    else:
                                        return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    if (state) is (symbol_string_state):
                                        if char_is__q(c, '"'):
                                            return List(symbol_drop, List(symbol_emit, symbol_string))
                                        else:
                                            if char_is__q(c, '\\'):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_escape_state))
                                            else:
                                                if not(char_is__q(c, '\0')):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    return symbol_error
                                    else:
                                        if (state) is (symbol_string_escape_state):
                                            if char_is__q(c, '"'):
                                                return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                            else:
                                                if char_is__q(c, '\\'):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    if char_is__q(c, 'b'):
                                                        return List(symbol_replace, '\b', List(symbol_goto, symbol_string_state))
                                                    else:
                                                        if char_is__q(c, 'f'):
                                                            return List(symbol_replace, u"\u000C", List(symbol_goto, symbol_string_state))
                                                        else:
                                                            if char_is__q(c, 'n'):
                                                                return List(symbol_replace, '\n', List(symbol_goto, symbol_string_state))
                                                            else:
                                                                if char_is__q(c, 't'):
                                                                    return List(symbol_replace, '\t', List(symbol_goto, symbol_string_state))
                                                                else:
                                                                    if char_is__q(c, 'r'):
                                                                        return List(symbol_replace, '\r', List(symbol_goto, symbol_string_state))
                                                                    else:
                                                                        return symbol_error
                                        else:
                                            if (state) is (symbol_identifier_state):
                                                if char_subsequent_q(c):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if char_delimiter_q(c):
                                                        return List(symbol_emit, symbol_identifier)
                                                    else:
                                                        return symbol_error
                                            else:
                                                if (state) is (symbol_signed_state):
                                                    if char_numeric_q(c):
                                                        return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                    else:
                                                        if char_is__q(c, '.'):
                                                            return List(symbol_shift, List(symbol_goto, symbol_signed_decimal_point_state))
                                                        else:
                                                            if char_delimiter_q(c):
                                                                return List(symbol_emit, symbol_identifier)
                                                            else:
                                                                if char_subsequent_q(c):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                else:
                                                    if (state) is (symbol_decimal_point_state):
                                                        if char_numeric_q(c):
                                                            return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                        else:
                                                            if char_delimiter_q(c):
                                                                return List(symbol_emit, symbol_dot)
                                                            else:
                                                                if char_subsequent_q(c):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                    else:
                                                        if (state) is (symbol_signed_decimal_point_state):
                                                            if char_numeric_q(c):
                                                                return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                            else:
                                                                if char_delimiter_q(c):
                                                                    return List(symbol_emit, symbol_identifier)
                                                                else:
                                                                    if char_subsequent_q(c):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                    else:
                                                                        return symbol_error
                                                        else:
                                                            if (state) is (symbol_whole_number_state):
                                                                if char_numeric_q(c):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                                else:
                                                                    if char_is__q(c, '.'):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if char_is__q(c, '/'):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state))
                                                                        else:
                                                                            if (char_is__q(c, 'e')) or (char_is__q(c, 'E')):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                            else:
                                                                                if char_delimiter_q(c):
                                                                                    return List(symbol_emit, symbol_integer)
                                                                                else:
                                                                                    if char_subsequent_q(c):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                            else:
                                                                if (state) is (symbol_fractional_number_state):
                                                                    if char_numeric_q(c):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if (char_is__q(c, 'e')) or (char_is__q(c, 'E')):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                        else:
                                                                            if char_delimiter_q(c):
                                                                                return List(symbol_emit, symbol_decimal)
                                                                            else:
                                                                                if char_subsequent_q(c):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                else:
                                                                    if (state) is (symbol_rational_number_state):
                                                                        if char_numeric_q(c):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                        else:
                                                                            if char_delimiter_q(c):
                                                                                return List(symbol_emit, symbol_identifier)
                                                                            else:
                                                                                if char_subsequent_q(c):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                    else:
                                                                        if (state) is (symbol_rational_number_state_star):
                                                                            if char_numeric_q(c):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                            else:
                                                                                if char_delimiter_q(c):
                                                                                    return List(symbol_emit, symbol_rational)
                                                                                else:
                                                                                    if char_subsequent_q(c):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                                        else:
                                                                            if (state) is (symbol_suffix_state):
                                                                                if char_sign_q(c):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_signed_exponent_state))
                                                                                else:
                                                                                    if char_numeric_q(c):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if char_delimiter_q(c):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if char_subsequent_q(c):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                            else:
                                                                                if (state) is (symbol_signed_exponent_state):
                                                                                    if char_numeric_q(c):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if char_delimiter_q(c):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if char_subsequent_q(c):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                                else:
                                                                                    if (state) is (symbol_exponent_state):
                                                                                        if char_numeric_q(c):
                                                                                            return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                        else:
                                                                                            if char_delimiter_q(c):
                                                                                                return List(symbol_emit, symbol_decimal)
                                                                                            else:
                                                                                                if char_subsequent_q(c):
                                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                                else:
                                                                                                    return symbol_error
                                                                                    else:
                                                                                        raise Exception("symbol_apply_state: " + format("invalid state: ~a", *[state]))

def aatom_q(x):
    return (pair_q(x)) and ((car(x)) is (atom_tag))

def apair_q(x):
    return (pair_q(x)) and ((car(x)) is (pair_tag))

def annotated_q(x):
    return (pair_q(x)) and (((car(x)) is (atom_tag)) or ((car(x)) is (pair_tag)))

def untag_atom_hat(aatom):
    return cadr(aatom)

def atom_q_hat(asexp):
    return (car(asexp)) is (atom_tag)

def pair_q_hat(asexp):
    return (car(asexp)) is (pair_tag)

def null_q_hat(asexp):
    return (atom_q_hat(asexp)) and (null_q(untag_atom_hat(asexp)))

def symbol_q_hat(asexp):
    return (atom_q_hat(asexp)) and (symbol_q(untag_atom_hat(asexp)))

def string_q_hat(asexp):
    return (atom_q_hat(asexp)) and (string_q(untag_atom_hat(asexp)))

def vector_q_hat(asexp):
    return (atom_q_hat(asexp)) and (vector_q(untag_atom_hat(asexp)))

def car_hat(asexp):
    return cadr(asexp)

def cdr_hat(asexp):
    return caddr(asexp)

def cadr_hat(asexp):
    return car_hat(cdr_hat(asexp))

def cdar_hat(asexp):
    return cdr_hat(car_hat(asexp))

def caar_hat(asexp):
    return car_hat(car_hat(asexp))

def cddr_hat(asexp):
    return cdr_hat(cdr_hat(asexp))

def cdddr_hat(asexp):
    return cdr_hat(cdr_hat(cdr_hat(asexp)))

def caddr_hat(asexp):
    return car_hat(cdr_hat(cdr_hat(asexp)))

def cdadr_hat(asexp):
    return cdr_hat(car_hat(cdr_hat(asexp)))

def cadar_hat(asexp):
    return car_hat(cdr_hat(car_hat(asexp)))

def caadr_hat(asexp):
    return car_hat(car_hat(cdr_hat(asexp)))

def cadddr_hat(asexp):
    return car_hat(cdr_hat(cdr_hat(cdr_hat(asexp))))

def eq_q_hat(asexp, sym):
    return (cadr(asexp)) is (sym)

def vector_to_list_hat(asexp):
    return vector_to_list(cadr(asexp))

def symbol_to_string_hat(asexp):
    return symbol_to_string(cadr(asexp))

def list_q_hat(asexp):
    return (null_q_hat(asexp)) or ((pair_q_hat(asexp)) and (list_q_hat(caddr(asexp))))

def at_hat(alist):
    if null_q_hat(alist):
        return symbol_emptylist
    else:
        return cons(car_hat(alist), at_hat(cdr_hat(alist)))

def length_hat(asexp):
    if null_q_hat(asexp):
        return 0
    else:
        return (1) + (length_hat(cdr_hat(asexp)))

def cons_hat(a, b, info):
    return List(pair_tag, a, b, info)

def map_hat(f_hat, asexp):
    if null_q_hat(asexp):
        return List(atom_tag, symbol_emptylist, symbol_none)
    else:
        return cons_hat(f_hat(car_hat(asexp)), map_hat(f_hat, cdr_hat(asexp)), symbol_none)

def annotate_cps():
    if not(_starreader_generates_annotated_sexps_q_star):
        globals()['value_reg'] = x_reg
        globals()['pc'] = apply_cont
    else:
        if annotated_q(x_reg):
            globals()['value_reg'] = x_reg
            globals()['pc'] = apply_cont
        else:
            if pair_q(x_reg):
                globals()['k_reg'] = make_cont(b_cont_3_d, x_reg, info_reg, k_reg)
                globals()['info_reg'] = symbol_none
                globals()['x_reg'] = car(x_reg)
                globals()['pc'] = annotate_cps
            else:
                globals()['value_reg'] = List(atom_tag, x_reg, info_reg)
                globals()['pc'] = apply_cont

def unannotate_cps():
    if aatom_q(x_reg):
        globals()['x_reg'] = cadr(x_reg)
        globals()['pc'] = unannotate_cps
    else:
        if apair_q(x_reg):
            globals()['k_reg'] = make_cont(b_cont_7_d, x_reg, k_reg)
            globals()['x_reg'] = cadr(x_reg)
            globals()['pc'] = unannotate_cps
        else:
            if pair_q(x_reg):
                globals()['k_reg'] = make_cont(b_cont_6_d, x_reg, k_reg)
                globals()['x_reg'] = car(x_reg)
                globals()['pc'] = unannotate_cps
            else:
                if vector_q(x_reg):
                    globals()['k_reg'] = make_cont(b_cont_4_d, k_reg)
                    globals()['x_reg'] = vector_to_list(x_reg)
                    globals()['pc'] = unannotate_cps
                else:
                    globals()['value_reg'] = x_reg
                    globals()['pc'] = apply_cont

def make_info(src, start, end):
    return cons(src, append(start, end))

def replace_info(asexp, new_info):
    if atom_q_hat(asexp):
        return List(atom_tag, cadr(asexp), new_info)
    else:
        return List(pair_tag, cadr(asexp), caddr(asexp), new_info)

def get_srcfile(info):
    return car(info)

def get_start_line(info):
    return cadr(info)

def get_start_char(info):
    return caddr(info)

def get_start_pos(info):
    return cadddr(info)

def get_end_line(info):
    return car(cddddr(info))

def get_end_char(info):
    return cadr(cddddr(info))

def get_end_pos(info):
    return caddr(cddddr(info))

def get_source_info(asexp):
    return rac(asexp)

def source_info_q(x):
    return ((x) is (symbol_none)) or (list_q(x))

def has_source_info_q(asexp):
    return not((get_source_info(asexp)) is (symbol_none))

def original_source_info_q(asexp):
    return (has_source_info_q(asexp)) and (Equal(length(get_source_info(asexp)), 7))

def macro_derived_source_info_q(asexp):
    return (has_source_info_q(asexp)) and (Equal(length(get_source_info(asexp)), 8))

def first(x):
    return car(x)

def rest_of(x):
    return cdr(x)

def unexpected_token_error():
    token = symbol_undefined
    token = first(tokens_reg)
    if token_type_q(token, symbol_end_marker):
        globals()['msg_reg'] = "unexpected end of input"
        globals()['pc'] = read_error
    else:
        globals()['msg_reg'] = format("unexpected '~a' encountered", car(token))
        globals()['pc'] = read_error

def read_error():
    token = symbol_undefined
    token = first(tokens_reg)
    globals()['exception_reg'] = make_exception("ReadError", msg_reg, src_reg, get_token_start_line(token), get_token_start_char(token))
    globals()['pc'] = apply_handler2

def read_sexp():
    start = symbol_undefined
    end = symbol_undefined
    end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if (car(temp_1)) is (symbol_integer):
        str = symbol_undefined
        str = list_ref(temp_1, 1)
        globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
        globals()['info_reg'] = make_info(src_reg, start, end)
        globals()['x_reg'] = string_to_integer(str)
        globals()['pc'] = annotate_cps
    else:
        if (car(temp_1)) is (symbol_decimal):
            str = symbol_undefined
            str = list_ref(temp_1, 1)
            globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
            globals()['info_reg'] = make_info(src_reg, start, end)
            globals()['x_reg'] = string_to_decimal(str)
            globals()['pc'] = annotate_cps
        else:
            if (car(temp_1)) is (symbol_rational):
                str = symbol_undefined
                str = list_ref(temp_1, 1)
                num = symbol_undefined
                num = string_to_rational(str)
                if true_q(num):
                    globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    globals()['info_reg'] = make_info(src_reg, start, end)
                    globals()['x_reg'] = num
                    globals()['pc'] = annotate_cps
                else:
                    globals()['msg_reg'] = format("cannot represent ~a", str)
                    globals()['pc'] = read_error
            else:
                if (car(temp_1)) is (symbol_boolean):
                    bool = symbol_undefined
                    bool = list_ref(temp_1, 1)
                    globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    globals()['info_reg'] = make_info(src_reg, start, end)
                    globals()['x_reg'] = bool
                    globals()['pc'] = annotate_cps
                else:
                    if (car(temp_1)) is (symbol_character):
                        char = symbol_undefined
                        char = list_ref(temp_1, 1)
                        globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                        globals()['info_reg'] = make_info(src_reg, start, end)
                        globals()['x_reg'] = char
                        globals()['pc'] = annotate_cps
                    else:
                        if (car(temp_1)) is (symbol_string):
                            str = symbol_undefined
                            str = list_ref(temp_1, 1)
                            globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                            globals()['info_reg'] = make_info(src_reg, start, end)
                            globals()['x_reg'] = str
                            globals()['pc'] = annotate_cps
                        else:
                            if (car(temp_1)) is (symbol_identifier):
                                id = symbol_undefined
                                id = list_ref(temp_1, 1)
                                globals()['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                                globals()['info_reg'] = make_info(src_reg, start, end)
                                globals()['x_reg'] = id
                                globals()['pc'] = annotate_cps
                            else:
                                if (car(temp_1)) is (symbol_apostrophe):
                                    globals()['keyword_reg'] = symbol_quote
                                    globals()['pc'] = read_abbreviation
                                else:
                                    if (car(temp_1)) is (symbol_backquote):
                                        globals()['keyword_reg'] = symbol_quasiquote
                                        globals()['pc'] = read_abbreviation
                                    else:
                                        if (car(temp_1)) is (symbol_comma):
                                            globals()['keyword_reg'] = symbol_unquote
                                            globals()['pc'] = read_abbreviation
                                        else:
                                            if (car(temp_1)) is (symbol_comma_at):
                                                globals()['keyword_reg'] = symbol_unquote_splicing
                                                globals()['pc'] = read_abbreviation
                                            else:
                                                if (car(temp_1)) is (symbol_lparen):
                                                    tokens = symbol_undefined
                                                    tokens = rest_of(tokens_reg)
                                                    globals()['k_reg'] = make_cont4(b_cont4_2_d, src_reg, start, k_reg)
                                                    globals()['expected_terminator_reg'] = symbol_rparen
                                                    globals()['tokens_reg'] = tokens
                                                    globals()['pc'] = read_sexp_sequence
                                                else:
                                                    if (car(temp_1)) is (symbol_lbracket):
                                                        tokens = symbol_undefined
                                                        tokens = rest_of(tokens_reg)
                                                        globals()['k_reg'] = make_cont4(b_cont4_2_d, src_reg, start, k_reg)
                                                        globals()['expected_terminator_reg'] = symbol_rbracket
                                                        globals()['tokens_reg'] = tokens
                                                        globals()['pc'] = read_sexp_sequence
                                                    else:
                                                        if (car(temp_1)) is (symbol_lvector):
                                                            globals()['k_reg'] = make_cont4(b_cont4_1_d, src_reg, start, k_reg)
                                                            globals()['tokens_reg'] = rest_of(tokens_reg)
                                                            globals()['pc'] = read_vector_sequence
                                                        else:
                                                            globals()['pc'] = unexpected_token_error

def read_abbreviation():
    start = symbol_undefined
    keyword_end = symbol_undefined
    keyword_end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    globals()['k_reg'] = make_cont(b_cont_10_d, src_reg, start, tokens_reg, handler_reg, fail_reg, k_reg)
    globals()['info_reg'] = make_info(src_reg, start, keyword_end)
    globals()['x_reg'] = keyword_reg
    globals()['pc'] = annotate_cps

def read_vector_sequence():
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if (car(temp_1)) is (symbol_rparen):
        globals()['expected_terminator_reg'] = symbol_rparen
        globals()['sexps_reg'] = symbol_emptylist
        globals()['pc'] = close_sexp_sequence
    else:
        if (car(temp_1)) is (symbol_dot):
            globals()['msg_reg'] = "unexpected dot (.)"
            globals()['pc'] = read_error
        else:
            globals()['k_reg'] = make_cont4(b_cont4_5_d, src_reg, handler_reg, k_reg)
            globals()['pc'] = read_sexp

def read_sexp_sequence():
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if memq(car(temp_1), List(symbol_rparen, symbol_rbracket)):
        globals()['sexps_reg'] = symbol_emptylist
        globals()['pc'] = close_sexp_sequence
    else:
        if (car(temp_1)) is (symbol_dot):
            globals()['msg_reg'] = "unexpected dot (.)"
            globals()['pc'] = read_error
        else:
            globals()['k_reg'] = make_cont4(b_cont4_7_d, expected_terminator_reg, src_reg, handler_reg, k_reg)
            globals()['pc'] = read_sexp

def close_sexp_sequence():
    end = symbol_undefined
    end = get_token_end(first(tokens_reg))
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if memq(car(temp_1), List(symbol_rparen, symbol_rbracket)):
        if token_type_q(first(tokens_reg), expected_terminator_reg):
            globals()['value4_reg'] = fail_reg
            globals()['value3_reg'] = rest_of(tokens_reg)
            globals()['value2_reg'] = end
            globals()['value1_reg'] = sexps_reg
            globals()['pc'] = apply_cont4
        else:
            if (expected_terminator_reg) is (symbol_rparen):
                globals()['msg_reg'] = "parenthesized list terminated by bracket"
                globals()['pc'] = read_error
            else:
                if (expected_terminator_reg) is (symbol_rbracket):
                    globals()['msg_reg'] = "bracketed list terminated by parenthesis"
                    globals()['pc'] = read_error
    else:
        globals()['pc'] = unexpected_token_error

def make_binding(value):
    return cons(value, "")

def binding_value(binding):
    return car(binding)

def binding_docstring(binding):
    return cdr(binding)

def set_binding_value_b(binding, value):
    return set_car_b(binding, value)

def set_binding_docstring_b(binding, docstring):
    return set_cdr_b(binding, docstring)

def make_frame(variables, values):
    return List(list_to_vector(Map(make_binding, values)), variables)

def empty_frame_q(frame):
    return null_q(cadr(frame))

def frame_bindings(frame):
    return car(frame)

def environment_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_environment))

def make_empty_environment():
    return List(symbol_environment, make_frame(symbol_emptylist, symbol_emptylist))

def make_initial_environment(vars, vals):
    return List(symbol_environment, make_frame(vars, vals))

def first_frame(env):
    return cadr(env)

def first_frame_vars(env):
    return cadr(first_frame(env))

def initial_contours(env):
    return cdr(first_frame(env))

def frames(env):
    return cdr(env)

def add_binding(new_var, new_binding, frame):
    bindings = symbol_undefined
    vars = symbol_undefined
    vars = cadr(frame)
    bindings = vector_to_list(car(frame))
    return List(list_to_vector(append(bindings, List(new_binding))), append(vars, List(new_var)))

def set_first_frame_b(env, new_frame):
    return set_car_b(cdr(env), new_frame)

def extend(env, variables, values):
    return cons(symbol_environment, cons(make_frame(variables, values), cdr(env)))

def search_env(env, variable):
    return search_frames(cdr(env), variable)

def search_frames(frames, variable):
    if null_q(frames):
        return False
    else:
        binding = symbol_undefined
        binding = search_frame(car(frames), variable)
        if binding:
            return binding
        else:
            return search_frames(cdr(frames), variable)

def in_first_frame_q(var, env):
    return true_q(memq(var, first_frame_vars(env)))

def get_first_frame_value(var, env):
    return binding_value(search_frame(first_frame(env), var))

def lookup_value_by_lexical_address():
    bindings = symbol_undefined
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = binding_value(vector_ref(bindings, offset_reg))
    globals()['pc'] = apply_cont2

def lookup_binding_by_lexical_address():
    bindings = symbol_undefined
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    globals()['value2_reg'] = fail_reg
    globals()['value1_reg'] = vector_ref(bindings, offset_reg)
    globals()['pc'] = apply_cont2

def lookup_value():
    globals()['sk_reg'] = make_cont2(b_cont2_3_d, k_reg)
    globals()['dk_reg'] = make_cont3(b_cont3_3_d, k_reg)
    globals()['gk_reg'] = make_cont2(b_cont2_4_d, k_reg)
    globals()['pc'] = lookup_variable

def lookup_variable():
    binding = symbol_undefined
    binding = search_env(env_reg, var_reg)
    if binding:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = binding
        globals()['k_reg'] = sk_reg
        globals()['pc'] = apply_cont2
    else:
        components = symbol_undefined
        components = split_variable(var_reg)
        if (null_q(cdr(components))) and (dlr_env_contains(car(components))):
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = car(components)
            globals()['k_reg'] = gk_reg
            globals()['pc'] = apply_cont2
        else:
            if (not(null_q(cdr(components)))) and (dlr_env_contains(car(components))) and (dlr_object_contains(dlr_env_lookup(car(components)), components)):
                globals()['value3_reg'] = fail_reg
                globals()['value2_reg'] = components
                globals()['value1_reg'] = dlr_env_lookup(car(components))
                globals()['k_reg'] = dk_reg
                globals()['pc'] = apply_cont3
            else:
                if null_q(cdr(components)):
                    globals()['info_reg'] = var_info_reg
                    globals()['msg_reg'] = format("unbound variable '~a'", var_reg)
                    globals()['pc'] = runtime_error
                else:
                    globals()['module_reg'] = env_reg
                    globals()['path_reg'] = ""
                    globals()['components_reg'] = components
                    globals()['pc'] = lookup_variable_components

def lookup_variable_components():
    var = symbol_undefined
    binding = symbol_undefined
    var = car(components_reg)
    binding = search_env(module_reg, var)
    if binding:
        if null_q(cdr(components_reg)):
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = binding
            globals()['k_reg'] = sk_reg
            globals()['pc'] = apply_cont2
        else:
            value = symbol_undefined
            new_path = symbol_undefined
            new_path = (format("~a", var) if string_is__q(path_reg, "") else format("~a.~a", path_reg, var))
            value = binding_value(binding)
            if environment_q(value):
                globals()['module_reg'] = value
                globals()['path_reg'] = new_path
                globals()['components_reg'] = cdr(components_reg)
                globals()['pc'] = lookup_variable_components
            else:
                if dlr_object_contains(value, components_reg):
                    globals()['value3_reg'] = fail_reg
                    globals()['value2_reg'] = components_reg
                    globals()['value1_reg'] = value
                    globals()['k_reg'] = dk_reg
                    globals()['pc'] = apply_cont3
                else:
                    globals()['info_reg'] = var_info_reg
                    globals()['msg_reg'] = format("'~a' is not a module", new_path)
                    globals()['pc'] = runtime_error
    else:
        if string_is__q(path_reg, ""):
            globals()['info_reg'] = var_info_reg
            globals()['msg_reg'] = format("unbound module '~a'", var)
            globals()['pc'] = runtime_error
        else:
            globals()['info_reg'] = var_info_reg
            globals()['msg_reg'] = format("unbound variable '~a' in module '~a'", var, path_reg)
            globals()['pc'] = runtime_error

def lookup_binding_in_first_frame():
    frame = symbol_undefined
    frame = first_frame(env_reg)
    binding = symbol_undefined
    binding = search_frame(frame, var_reg)
    if binding:
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = binding
        globals()['pc'] = apply_cont2
    else:
        new_binding = symbol_undefined
        new_binding = make_binding(symbol_undefined)
        new_frame = symbol_undefined
        new_frame = add_binding(var_reg, new_binding, frame)
        set_first_frame_b(env_reg, new_frame)
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = new_binding
        globals()['pc'] = apply_cont2

def split_variable(var):
    strings = symbol_undefined
    strings = string_split(symbol_to_string(var), '.')
    if member("", strings):
        return symbol_emptylist
    else:
        return Map(string_to_symbol, strings)

def head(formals):
    if symbol_q(formals):
        return symbol_emptylist
    else:
        if pair_q(cdr(formals)):
            return cons(car(formals), head(cdr(formals)))
        else:
            return List(car(formals))

def last(formals):
    if symbol_q(formals):
        return formals
    else:
        if pair_q(cdr(formals)):
            return last(cdr(formals))
        else:
            return cdr(formals)

def anything_q(datum):
    return True

def application_q_hat(asexp):
    return (list_q_hat(asexp)) and (not(null_q_hat(asexp))) and (not(reserved_keyword_q(untag_atom_hat(car_hat(asexp)))))

def reserved_keyword_q(x):
    return (symbol_q(x)) and (not((memq(x, get_reserved_keywords())) is (False)))

def get_reserved_keywords():
    return List(symbol_quote, symbol_func, symbol_define_b, symbol_quasiquote, symbol_lambda, symbol_if, symbol_set_b, symbol_define, symbol_begin, symbol_cond, symbol_and, symbol_or, symbol_let, symbol_let_star, symbol_letrec, symbol_case, symbol_record_case, symbol_try, symbol_catch, symbol_finally, symbol_raise, symbol_define_syntax, symbol_choose, symbol_define_datatype, symbol_cases, symbol_trace_lambda)

def mit_style_define_q_hat(asexp):
    return not(symbol_q_hat(cadr_hat(asexp)))

def literal_q(datum):
    return (number_q(datum)) or (boolean_q(datum)) or (null_q(datum)) or (char_q(datum)) or (string_q(datum))

def literal_q_hat(asexp):
    return ((car(asexp)) is (atom_tag)) and ((number_q(untag_atom_hat(asexp))) or (boolean_q(untag_atom_hat(asexp))) or (null_q(untag_atom_hat(asexp))) or (char_q(untag_atom_hat(asexp))) or (string_q(untag_atom_hat(asexp))))

def syntactic_sugar_q_hat(asexp):
    return (pair_q_hat(asexp)) and (symbol_q_hat(car_hat(asexp))) and (in_first_frame_q(untag_atom_hat(car_hat(asexp)), macro_env))

def define_var_hat(x):
    return untag_atom_hat(cadr_hat(x))

def define_docstring_hat(x):
    return untag_atom_hat(caddr_hat(x))

def try_body_hat(x):
    return cadr_hat(x)

def catch_var_hat(x):
    return untag_atom_hat(cadr_hat(caddr_hat(x)))

def catch_exps_hat(x):
    return cddr_hat(caddr_hat(x))

def try_finally_exps_hat(x):
    return cdr_hat(caddr_hat(x))

def try_catch_finally_exps_hat(x):
    return cdr_hat(cadddr_hat(x))

def aparse():
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    if literal_q_hat(adatum_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = lit_aexp(untag_atom_hat(adatum_reg), info)
        globals()['pc'] = apply_cont2
    else:
        if symbol_q_hat(adatum_reg):
            if _staruse_lexical_address_star:
                globals()['info_reg'] = info
                globals()['depth_reg'] = 0
                globals()['id_reg'] = untag_atom_hat(adatum_reg)
                globals()['pc'] = get_lexical_address
            else:
                globals()['value2_reg'] = fail_reg
                globals()['value1_reg'] = var_aexp(untag_atom_hat(adatum_reg), info)
                globals()['pc'] = apply_cont2
        else:
            if vector_q_hat(adatum_reg):
                globals()['k_reg'] = make_cont(b_cont_20_d, info, fail_reg, k_reg)
                globals()['x_reg'] = adatum_reg
                globals()['pc'] = unannotate_cps
            else:
                if quote_q_hat(adatum_reg):
                    globals()['k_reg'] = make_cont(b_cont_19_d, info, fail_reg, k_reg)
                    globals()['x_reg'] = adatum_reg
                    globals()['pc'] = unannotate_cps
                else:
                    if quasiquote_q_hat(adatum_reg):
                        globals()['k_reg'] = make_cont(b_cont_18_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                        globals()['depth_reg'] = 0
                        globals()['ax_reg'] = cadr_hat(adatum_reg)
                        globals()['pc'] = qq_expand_cps
                    else:
                        if unquote_q_hat(adatum_reg):
                            globals()['msg_reg'] = "misplaced"
                            globals()['pc'] = aparse_error
                        else:
                            if unquote_splicing_q_hat(adatum_reg):
                                globals()['msg_reg'] = "misplaced"
                                globals()['pc'] = aparse_error
                            else:
                                if syntactic_sugar_q_hat(adatum_reg):
                                    globals()['k_reg'] = make_cont2(b_cont2_33_d, senv_reg, handler_reg, k_reg)
                                    globals()['pc'] = expand_once_hat
                                else:
                                    if if_then_q_hat(adatum_reg):
                                        globals()['k_reg'] = make_cont2(b_cont2_32_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                        globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                        globals()['pc'] = aparse
                                    else:
                                        if if_else_q_hat(adatum_reg):
                                            globals()['k_reg'] = make_cont2(b_cont2_30_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                            globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                            globals()['pc'] = aparse
                                        else:
                                            if assignment_q_hat(adatum_reg):
                                                globals()['k_reg'] = make_cont2(b_cont2_27_d, adatum_reg, info, k_reg)
                                                globals()['adatum_reg'] = caddr_hat(adatum_reg)
                                                globals()['pc'] = aparse
                                            else:
                                                if func_q_hat(adatum_reg):
                                                    globals()['k_reg'] = make_cont2(b_cont2_26_d, info, k_reg)
                                                    globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                    globals()['pc'] = aparse
                                                else:
                                                    if callback0_q_hat(adatum_reg):
                                                        globals()['k_reg'] = make_cont2(b_cont2_25_d, info, k_reg)
                                                        globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                        globals()['pc'] = aparse
                                                    else:
                                                        if callback1_q_hat(adatum_reg):
                                                            globals()['k_reg'] = make_cont2(b_cont2_24_d, info, k_reg)
                                                            globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                            globals()['pc'] = aparse
                                                        else:
                                                            if callback2_q_hat(adatum_reg):
                                                                globals()['k_reg'] = make_cont2(b_cont2_23_d, info, k_reg)
                                                                globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                                globals()['pc'] = aparse
                                                            else:
                                                                if define_q_hat(adatum_reg):
                                                                    if mit_style_define_q_hat(adatum_reg):
                                                                        globals()['k_reg'] = make_cont(b_cont_16_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                        globals()['datum_reg'] = adatum_reg
                                                                        globals()['macro_reg'] = mit_define_transformer_hat
                                                                        globals()['pc'] = apply_macro
                                                                    else:
                                                                        if Equal(length_hat(adatum_reg), 3):
                                                                            globals()['k_reg'] = make_cont2(b_cont2_22_d, adatum_reg, info, k_reg)
                                                                            globals()['adatum_reg'] = caddr_hat(adatum_reg)
                                                                            globals()['pc'] = aparse
                                                                        else:
                                                                            if (Equal(length_hat(adatum_reg), 4)) and (string_q_hat(caddr_hat(adatum_reg))):
                                                                                globals()['k_reg'] = make_cont2(b_cont2_21_d, adatum_reg, info, k_reg)
                                                                                globals()['adatum_reg'] = cadddr_hat(adatum_reg)
                                                                                globals()['pc'] = aparse
                                                                            else:
                                                                                globals()['msg_reg'] = "bad concrete syntax:"
                                                                                globals()['pc'] = aparse_error
                                                                else:
                                                                    if define_b_q_hat(adatum_reg):
                                                                        if mit_style_define_q_hat(adatum_reg):
                                                                            globals()['k_reg'] = make_cont(b_cont_16_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                            globals()['datum_reg'] = adatum_reg
                                                                            globals()['macro_reg'] = mit_define_transformer_hat
                                                                            globals()['pc'] = apply_macro
                                                                        else:
                                                                            if Equal(length_hat(adatum_reg), 3):
                                                                                globals()['k_reg'] = make_cont2(b_cont2_20_d, adatum_reg, info, k_reg)
                                                                                globals()['adatum_reg'] = caddr_hat(adatum_reg)
                                                                                globals()['pc'] = aparse
                                                                            else:
                                                                                if (Equal(length_hat(adatum_reg), 4)) and (string_q_hat(caddr_hat(adatum_reg))):
                                                                                    globals()['k_reg'] = make_cont2(b_cont2_19_d, adatum_reg, info, k_reg)
                                                                                    globals()['adatum_reg'] = cadddr_hat(adatum_reg)
                                                                                    globals()['pc'] = aparse
                                                                                else:
                                                                                    globals()['msg_reg'] = "bad concrete syntax:"
                                                                                    globals()['pc'] = aparse_error
                                                                    else:
                                                                        if define_syntax_q_hat(adatum_reg):
                                                                            name = symbol_undefined
                                                                            aclauses = symbol_undefined
                                                                            aclauses = cddr_hat(adatum_reg)
                                                                            name = define_var_hat(adatum_reg)
                                                                            globals()['k_reg'] = make_cont(b_cont_14_d, aclauses, name, info, fail_reg, k_reg)
                                                                            globals()['x_reg'] = aclauses
                                                                            globals()['pc'] = unannotate_cps
                                                                        else:
                                                                            if begin_q_hat(adatum_reg):
                                                                                if null_q_hat(cdr_hat(adatum_reg)):
                                                                                    globals()['msg_reg'] = "bad concrete syntax:"
                                                                                    globals()['pc'] = aparse_error
                                                                                else:
                                                                                    if null_q_hat(cddr_hat(adatum_reg)):
                                                                                        globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                                                        globals()['pc'] = aparse
                                                                                    else:
                                                                                        globals()['k_reg'] = make_cont2(b_cont2_18_d, info, k_reg)
                                                                                        globals()['adatum_list_reg'] = cdr_hat(adatum_reg)
                                                                                        globals()['pc'] = aparse_all
                                                                            else:
                                                                                if lambda_q_hat(adatum_reg):
                                                                                    globals()['k_reg'] = make_cont(b_cont_13_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                    globals()['x_reg'] = cadr_hat(adatum_reg)
                                                                                    globals()['pc'] = unannotate_cps
                                                                                else:
                                                                                    if trace_lambda_q_hat(adatum_reg):
                                                                                        globals()['k_reg'] = make_cont(b_cont_12_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                        globals()['x_reg'] = caddr_hat(adatum_reg)
                                                                                        globals()['pc'] = unannotate_cps
                                                                                    else:
                                                                                        if try_q_hat(adatum_reg):
                                                                                            if Equal(length_hat(adatum_reg), 2):
                                                                                                globals()['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                globals()['pc'] = aparse
                                                                                            else:
                                                                                                if (Equal(length_hat(adatum_reg), 3)) and (catch_q_hat(caddr_hat(adatum_reg))):
                                                                                                    globals()['k_reg'] = make_cont2(b_cont2_15_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                    globals()['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                    globals()['pc'] = aparse
                                                                                                else:
                                                                                                    if (Equal(length_hat(adatum_reg), 3)) and (finally_q_hat(caddr_hat(adatum_reg))):
                                                                                                        globals()['k_reg'] = make_cont2(b_cont2_13_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                        globals()['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                        globals()['pc'] = aparse
                                                                                                    else:
                                                                                                        if (Equal(length_hat(adatum_reg), 4)) and (catch_q_hat(caddr_hat(adatum_reg))) and (finally_q_hat(cadddr_hat(adatum_reg))):
                                                                                                            globals()['k_reg'] = make_cont2(b_cont2_11_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                            globals()['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                            globals()['pc'] = aparse
                                                                                                        else:
                                                                                                            globals()['msg_reg'] = "bad try syntax:"
                                                                                                            globals()['pc'] = aparse_error
                                                                                        else:
                                                                                            if raise_q_hat(adatum_reg):
                                                                                                globals()['k_reg'] = make_cont2(b_cont2_8_d, info, k_reg)
                                                                                                globals()['adatum_reg'] = cadr_hat(adatum_reg)
                                                                                                globals()['pc'] = aparse
                                                                                            else:
                                                                                                if choose_q_hat(adatum_reg):
                                                                                                    globals()['k_reg'] = make_cont2(b_cont2_7_d, info, k_reg)
                                                                                                    globals()['adatum_list_reg'] = cdr_hat(adatum_reg)
                                                                                                    globals()['pc'] = aparse_all
                                                                                                else:
                                                                                                    if application_q_hat(adatum_reg):
                                                                                                        globals()['k_reg'] = make_cont2(b_cont2_6_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                        globals()['adatum_reg'] = car_hat(adatum_reg)
                                                                                                        globals()['pc'] = aparse
                                                                                                    else:
                                                                                                        globals()['msg_reg'] = "bad concrete syntax:"
                                                                                                        globals()['pc'] = aparse_error

def aparse_all():
    if null_q_hat(adatum_list_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont2(b_cont2_35_d, adatum_list_reg, senv_reg, handler_reg, k_reg)
        globals()['adatum_reg'] = car_hat(adatum_list_reg)
        globals()['pc'] = aparse

def aparse_error():
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    globals()['k_reg'] = make_cont(b_cont_21_d, msg_reg, info, handler_reg, fail_reg)
    globals()['x_reg'] = adatum_reg
    globals()['pc'] = unannotate_cps

def aparse_sexps():
    if token_type_q(first(tokens_reg), symbol_end_marker):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont4(b_cont4_9_d, senv_reg, src_reg, handler_reg, k_reg)
        globals()['pc'] = read_sexp

def get_lexical_address():
    if null_q(senv_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = var_aexp(id_reg, info_reg)
        globals()['pc'] = apply_cont2
    else:
        if memq(id_reg, car(senv_reg)):
            globals()['offset_reg'] = 0
            globals()['contours_reg'] = car(senv_reg)
            globals()['pc'] = get_lexical_address_offset
        else:
            globals()['depth_reg'] = (depth_reg) + (1)
            globals()['senv_reg'] = cdr(senv_reg)
            globals()['pc'] = get_lexical_address

def get_lexical_address_offset():
    if (car(contours_reg)) is (id_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = lexical_address_aexp(depth_reg, offset_reg, id_reg, info_reg)
        globals()['pc'] = apply_cont2
    else:
        globals()['offset_reg'] = (offset_reg) + (1)
        globals()['contours_reg'] = cdr(contours_reg)
        globals()['pc'] = get_lexical_address_offset

def create_letrec_assignments_hat():
    if null_q_hat(vars_reg):
        globals()['value2_reg'] = symbol_emptylist
        globals()['value1_reg'] = symbol_emptylist
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_39_d, procs_reg, vars_reg, k2_reg)
        globals()['procs_reg'] = cdr_hat(procs_reg)
        globals()['vars_reg'] = cdr_hat(vars_reg)
        globals()['pc'] = create_letrec_assignments_hat

def amacro_error():
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    globals()['exception_reg'] = make_exception("MacroError", msg_reg, get_start_line(info), get_srcfile(info), get_start_char(info))
    globals()['pc'] = apply_handler2

def nest_let_star_bindings_hat():
    if (null_q_hat(bindings_reg)) or (null_q_hat(cdr_hat(bindings_reg))):
        globals()['value_reg'] = append(List(symbol_let), append(List(bindings_reg), at_hat(bodies_reg)))
        globals()['pc'] = apply_cont
    else:
        globals()['k_reg'] = make_cont(b_cont_22_d, bindings_reg, k_reg)
        globals()['bindings_reg'] = cdr_hat(bindings_reg)
        globals()['pc'] = nest_let_star_bindings_hat

def case_clauses_to_simple_cond_clauses_hat():
    if null_q_hat(clauses_reg):
        globals()['value_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont
    else:
        globals()['k_reg'] = make_cont(b_cont_23_d, clauses_reg, var_reg, k_reg)
        globals()['clauses_reg'] = cdr_hat(clauses_reg)
        globals()['pc'] = case_clauses_to_simple_cond_clauses_hat

def case_clauses_to_cond_clauses_hat():
    if null_q_hat(clauses_reg):
        globals()['value2_reg'] = symbol_emptylist
        globals()['value1_reg'] = symbol_emptylist
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_41_d, clauses_reg, var_reg, k2_reg)
        globals()['clauses_reg'] = cdr_hat(clauses_reg)
        globals()['pc'] = case_clauses_to_cond_clauses_hat

def record_case_clauses_to_cond_clauses_hat():
    if null_q_hat(clauses_reg):
        globals()['value2_reg'] = symbol_emptylist
        globals()['value1_reg'] = symbol_emptylist
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_42_d, clauses_reg, var_reg, k2_reg)
        globals()['clauses_reg'] = cdr_hat(clauses_reg)
        globals()['pc'] = record_case_clauses_to_cond_clauses_hat

def make_dd_variant_constructors_hat():
    if null_q_hat(variants_reg):
        globals()['value2_reg'] = symbol_emptylist
        globals()['value1_reg'] = symbol_emptylist
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_45_d, variants_reg, k2_reg)
        globals()['variant_reg'] = car_hat(variants_reg)
        globals()['pc'] = make_dd_variant_constructor_hat

def make_dd_variant_constructor_hat():
    name = symbol_undefined
    fields = symbol_undefined
    fields = cdr_hat(variant_reg)
    name = car_hat(variant_reg)
    globals()['k_reg'] = make_cont(b_cont_24_d, fields, name, k2_reg)
    globals()['cdrs_reg'] = symbol_args
    globals()['fields_reg'] = fields
    globals()['name_reg'] = name
    globals()['pc'] = verify_dd_constructor_fields_hat

def verify_dd_constructor_fields_hat():
    if null_q_hat(fields_reg):
        globals()['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(name_reg))), List(symbol_args)))
        globals()['pc'] = apply_cont
    else:
        globals()['k_reg'] = make_cont(b_cont_25_d, cdrs_reg, fields_reg, name_reg, k_reg)
        globals()['cdrs_reg'] = append(List(symbol_cdr), List(cdrs_reg))
        globals()['fields_reg'] = cdr_hat(fields_reg)
        globals()['pc'] = verify_dd_constructor_fields_hat

def make_macro_env_hat():
    return make_initial_environment(List(symbol_and, symbol_or, symbol_cond, symbol_let, symbol_letrec, symbol_let_star, symbol_case, symbol_record_case, symbol_define_datatype, symbol_cases), List(and_transformer_hat, or_transformer_hat, cond_transformer_hat, let_transformer_hat, letrec_transformer_hat, let_star_transformer_hat, case_transformer_hat, record_case_transformer_hat, define_datatype_transformer_hat, cases_transformer_hat))

def make_pattern_macro_hat(clauses, aclauses):
    return List(symbol_pattern_macro, clauses, aclauses)

def pattern_macro_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_pattern_macro))

def macro_clauses(macro):
    return cadr(macro)

def macro_aclauses(macro):
    return caddr(macro)

def define_syntax_clause_q(x):
    return (list_q(x)) and (Equal(length(x), 2)) and (pattern_q(car(x))) and (pattern_q(cadr(x)))

def define_syntax_clause_q_hat(x):
    return (list_q_hat(x)) and (Equal(length_hat(x), 2)) and (apattern_q(car_hat(x))) and (apattern_q(cadr_hat(x)))

def apattern_q(x):
    return (aatom_q(x)) or ((apair_q(x)) and (apattern_q(cadr(x))) and (apattern_q(caddr(x))))

def list_of_define_syntax_clauses_q_hat(alist):
    return (null_q_hat(alist)) or ((define_syntax_clause_q_hat(car_hat(alist))) and (list_of_define_syntax_clauses_q_hat(cdr_hat(alist))))

def expand_once_hat():
    macro_keyword = symbol_undefined
    macro_keyword = untag_atom_hat(car_hat(adatum_reg))
    macro = symbol_undefined
    macro = get_first_frame_value(macro_keyword, macro_env)
    if pattern_macro_q(macro):
        globals()['k_reg'] = make_cont2(b_cont2_47_d, macro_keyword, k_reg)
        globals()['aclauses_reg'] = macro_aclauses(macro)
        globals()['clauses_reg'] = macro_clauses(macro)
        globals()['pc'] = process_macro_clauses_hat
    else:
        globals()['k_reg'] = make_cont(b_cont_27_d, adatum_reg, macro_keyword, fail_reg, k_reg)
        globals()['datum_reg'] = adatum_reg
        globals()['macro_reg'] = macro
        globals()['pc'] = apply_macro

def process_macro_clauses_hat():
    if null_q(clauses_reg):
        globals()['msg_reg'] = "no matching clause found for"
        globals()['pc'] = aparse_error
    else:
        left_pattern = symbol_undefined
        right_pattern = symbol_undefined
        left_apattern = symbol_undefined
        right_apattern = symbol_undefined
        right_apattern = cadar_hat(aclauses_reg)
        left_apattern = caar_hat(aclauses_reg)
        right_pattern = cadar(clauses_reg)
        left_pattern = caar(clauses_reg)
        globals()['k_reg'] = make_cont(b_cont_29_d, aclauses_reg, adatum_reg, clauses_reg, left_apattern, left_pattern, right_apattern, right_pattern, handler_reg, fail_reg, k_reg)
        globals()['x_reg'] = adatum_reg
        globals()['pc'] = unannotate_cps

def qq_expand_cps():
    if quasiquote_q_hat(ax_reg):
        globals()['k_reg'] = make_cont(b_cont_35_d, k_reg)
        globals()['depth_reg'] = (depth_reg) + (1)
        globals()['ax_reg'] = cdr_hat(ax_reg)
        globals()['pc'] = qq_expand_cps
    else:
        if (unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg)):
            if GreaterThan(depth_reg, 0):
                globals()['k_reg'] = make_cont(b_cont_34_d, ax_reg, k_reg)
                globals()['depth_reg'] = (depth_reg) - (1)
                globals()['ax_reg'] = cdr_hat(ax_reg)
                globals()['pc'] = qq_expand_cps
            else:
                if (unquote_q_hat(ax_reg)) and (not(null_q_hat(cdr_hat(ax_reg)))) and (null_q_hat(cddr_hat(ax_reg))):
                    globals()['value_reg'] = cadr_hat(ax_reg)
                    globals()['pc'] = apply_cont
                else:
                    globals()['value_reg'] = append(List(symbol_quote), List(ax_reg))
                    globals()['pc'] = apply_cont
        else:
            if vector_q_hat(ax_reg):
                globals()['k_reg'] = make_cont(b_cont_33_d, depth_reg, k_reg)
                globals()['info_reg'] = symbol_none
                globals()['x_reg'] = vector_to_list_hat(ax_reg)
                globals()['pc'] = annotate_cps
            else:
                if not(pair_q_hat(ax_reg)):
                    globals()['value_reg'] = append(List(symbol_quote), List(ax_reg))
                    globals()['pc'] = apply_cont
                else:
                    if null_q_hat(cdr_hat(ax_reg)):
                        globals()['ax_reg'] = car_hat(ax_reg)
                        globals()['pc'] = qq_expand_list_cps
                    else:
                        globals()['k_reg'] = make_cont(b_cont_31_d, ax_reg, depth_reg, k_reg)
                        globals()['ax_reg'] = car_hat(ax_reg)
                        globals()['pc'] = qq_expand_list_cps

def qq_expand_list_cps():
    if quasiquote_q_hat(ax_reg):
        globals()['k_reg'] = make_cont(b_cont_40_d, k_reg)
        globals()['depth_reg'] = (depth_reg) + (1)
        globals()['ax_reg'] = cdr_hat(ax_reg)
        globals()['pc'] = qq_expand_cps
    else:
        if (unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg)):
            if GreaterThan(depth_reg, 0):
                globals()['k_reg'] = make_cont(b_cont_39_d, ax_reg, k_reg)
                globals()['depth_reg'] = (depth_reg) - (1)
                globals()['ax_reg'] = cdr_hat(ax_reg)
                globals()['pc'] = qq_expand_cps
            else:
                if unquote_q_hat(ax_reg):
                    globals()['value_reg'] = append(List(symbol_List), cdr_hat(ax_reg))
                    globals()['pc'] = apply_cont
                else:
                    if null_q_hat(cddr_hat(ax_reg)):
                        globals()['value_reg'] = cadr_hat(ax_reg)
                        globals()['pc'] = apply_cont
                    else:
                        globals()['value_reg'] = append(List(symbol_append), cdr_hat(ax_reg))
                        globals()['pc'] = apply_cont
        else:
            if vector_q_hat(ax_reg):
                globals()['k_reg'] = make_cont(b_cont_38_d, k_reg)
                globals()['pc'] = qq_expand_cps
            else:
                if not(pair_q_hat(ax_reg)):
                    globals()['value_reg'] = append(List(symbol_quote), List(List(ax_reg)))
                    globals()['pc'] = apply_cont
                else:
                    if null_q_hat(cdr_hat(ax_reg)):
                        globals()['k_reg'] = make_cont(b_cont_38_d, k_reg)
                        globals()['ax_reg'] = car_hat(ax_reg)
                        globals()['pc'] = qq_expand_list_cps
                    else:
                        globals()['k_reg'] = make_cont(b_cont_37_d, ax_reg, depth_reg, k_reg)
                        globals()['ax_reg'] = car_hat(ax_reg)
                        globals()['pc'] = qq_expand_list_cps

def aunparse(aexp):
    if (car(aexp)) is (symbol_lit_aexp):
        datum = symbol_undefined
        datum = list_ref(aexp, 1)
        if literal_q(datum):
            return datum
        else:
            if vector_q(datum):
                return datum
            else:
                return append(List(symbol_quote), List(datum))
    else:
        if (car(aexp)) is (symbol_var_aexp):
            id = symbol_undefined
            id = list_ref(aexp, 1)
            return id
        else:
            if (car(aexp)) is (symbol_lexical_address_aexp):
                id = symbol_undefined
                id = list_ref(aexp, 3)
                return id
            else:
                if (car(aexp)) is (symbol_if_aexp):
                    test_aexp = symbol_undefined
                    then_aexp = symbol_undefined
                    else_aexp = symbol_undefined
                    else_aexp = list_ref(aexp, 3)
                    then_aexp = list_ref(aexp, 2)
                    test_aexp = list_ref(aexp, 1)
                    return append(List(symbol_if), append(List(aunparse(test_aexp)), append(List(aunparse(then_aexp)), List(aunparse(else_aexp)))))
                else:
                    if (car(aexp)) is (symbol_assign_aexp):
                        var = symbol_undefined
                        rhs_exp = symbol_undefined
                        rhs_exp = list_ref(aexp, 2)
                        var = list_ref(aexp, 1)
                        return append(List(symbol_set_b), append(List(var), List(aunparse(rhs_exp))))
                    else:
                        if (car(aexp)) is (symbol_func_aexp):
                            exp = symbol_undefined
                            exp = list_ref(aexp, 1)
                            return append(List(symbol_func), List(aunparse(exp)))
                        else:
                            if (car(aexp)) is (symbol_callback0_aexp):
                                exp = symbol_undefined
                                exp = list_ref(aexp, 1)
                                return append(List(symbol_callback0), List(aunparse(exp)))
                            else:
                                if (car(aexp)) is (symbol_callback1_aexp):
                                    exp = symbol_undefined
                                    exp = list_ref(aexp, 1)
                                    return append(List(symbol_callback1), List(aunparse(exp)))
                                else:
                                    if (car(aexp)) is (symbol_callback2_aexp):
                                        exp = symbol_undefined
                                        exp = list_ref(aexp, 1)
                                        return append(List(symbol_callback2), List(aunparse(exp)))
                                    else:
                                        if (car(aexp)) is (symbol_define_aexp):
                                            id = symbol_undefined
                                            docstring = symbol_undefined
                                            rhs_exp = symbol_undefined
                                            rhs_exp = list_ref(aexp, 3)
                                            docstring = list_ref(aexp, 2)
                                            id = list_ref(aexp, 1)
                                            if string_is__q(docstring, ""):
                                                return append(List(symbol_define), append(List(id), List(aunparse(rhs_exp))))
                                            else:
                                                return append(List(symbol_define), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                        else:
                                            if (car(aexp)) is (symbol_define_b_aexp):
                                                id = symbol_undefined
                                                docstring = symbol_undefined
                                                rhs_exp = symbol_undefined
                                                rhs_exp = list_ref(aexp, 3)
                                                docstring = list_ref(aexp, 2)
                                                id = list_ref(aexp, 1)
                                                if string_is__q(docstring, ""):
                                                    return append(List(symbol_define_b), append(List(id), List(aunparse(rhs_exp))))
                                                else:
                                                    return append(List(symbol_define_b), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                            else:
                                                if (car(aexp)) is (symbol_define_syntax_aexp):
                                                    name = symbol_undefined
                                                    clauses = symbol_undefined
                                                    clauses = list_ref(aexp, 2)
                                                    name = list_ref(aexp, 1)
                                                    return append(List(symbol_define_syntax), append(List(name), clauses))
                                                else:
                                                    if (car(aexp)) is (symbol_begin_aexp):
                                                        exps = symbol_undefined
                                                        exps = list_ref(aexp, 1)
                                                        return append(List(symbol_begin), Map(aunparse, exps))
                                                    else:
                                                        if (car(aexp)) is (symbol_lambda_aexp):
                                                            formals = symbol_undefined
                                                            bodies = symbol_undefined
                                                            bodies = list_ref(aexp, 2)
                                                            formals = list_ref(aexp, 1)
                                                            return append(List(symbol_lambda), append(List(formals), Map(aunparse, bodies)))
                                                        else:
                                                            if (car(aexp)) is (symbol_mu_lambda_aexp):
                                                                formals = symbol_undefined
                                                                runt = symbol_undefined
                                                                bodies = symbol_undefined
                                                                bodies = list_ref(aexp, 3)
                                                                runt = list_ref(aexp, 2)
                                                                formals = list_ref(aexp, 1)
                                                                return append(List(symbol_lambda), append(List(append(formals, runt)), Map(aunparse, bodies)))
                                                            else:
                                                                if (car(aexp)) is (symbol_app_aexp):
                                                                    operator = symbol_undefined
                                                                    operands = symbol_undefined
                                                                    operands = list_ref(aexp, 2)
                                                                    operator = list_ref(aexp, 1)
                                                                    return append(List(aunparse(operator)), Map(aunparse, operands))
                                                                else:
                                                                    if (car(aexp)) is (symbol_try_catch_aexp):
                                                                        body = symbol_undefined
                                                                        catch_var = symbol_undefined
                                                                        catch_exps = symbol_undefined
                                                                        catch_exps = list_ref(aexp, 3)
                                                                        catch_var = list_ref(aexp, 2)
                                                                        body = list_ref(aexp, 1)
                                                                        return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps))))))
                                                                    else:
                                                                        if (car(aexp)) is (symbol_try_finally_aexp):
                                                                            body = symbol_undefined
                                                                            finally_exps = symbol_undefined
                                                                            finally_exps = list_ref(aexp, 2)
                                                                            body = list_ref(aexp, 1)
                                                                            return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_finally), Map(aunparse, finally_exps)))))
                                                                        else:
                                                                            if (car(aexp)) is (symbol_try_catch_finally_aexp):
                                                                                body = symbol_undefined
                                                                                catch_var = symbol_undefined
                                                                                catch_exps = symbol_undefined
                                                                                finally_exps = symbol_undefined
                                                                                finally_exps = list_ref(aexp, 4)
                                                                                catch_exps = list_ref(aexp, 3)
                                                                                catch_var = list_ref(aexp, 2)
                                                                                body = list_ref(aexp, 1)
                                                                                return append(List(symbol_try), append(List(aunparse(body)), append(List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps)))), List(append(List(symbol_finally), Map(aunparse, finally_exps))))))
                                                                            else:
                                                                                if (car(aexp)) is (symbol_raise_aexp):
                                                                                    exp = symbol_undefined
                                                                                    exp = list_ref(aexp, 1)
                                                                                    return append(List(symbol_raise), List(aunparse(exp)))
                                                                                else:
                                                                                    if (car(aexp)) is (symbol_choose_aexp):
                                                                                        exps = symbol_undefined
                                                                                        exps = list_ref(aexp, 1)
                                                                                        return append(List(symbol_choose), Map(aunparse, exps))
                                                                                    else:
                                                                                        raise Exception("symbol_aunparse: " + format("bad abstract syntax: ~s", *[aexp]))

def exception_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_exception))

def read_line(prompt):
    printf(prompt)
    input_ = symbol_undefined
    input_ = raw_input()
    return format("~s", input_)

def handle_exception(exc):
    stack = symbol_undefined
    message = symbol_undefined
    error_type = symbol_undefined
    error_type = car(cadr(exc))
    message = cadr(cadr(exc))
    stack = cadddr(cddr(cadr(exc)))
    printf("~%Traceback (most recent call last):~%")
    while not(null_q(stack)):
        display(format_exception_line(car(stack)))
        stack = cdr(stack)
    printf("~a: ~a~%", error_type, message)

def format_exception_line(line):
    filename = symbol_undefined
    line_number = symbol_undefined
    column_number = symbol_undefined
    column_number = caddr(line)
    line_number = cadr(line)
    filename = car(line)
    if Equal(length(line), 3):
        return format("  File \"~a\", line ~a, col ~a~%", filename, line_number, column_number)
    else:
        return format("  File \"~a\", line ~a, col ~a, in ~a~%", filename, line_number, column_number, cadddr(line))

def start_rm():
    globals()['toplevel_env'] = make_toplevel_env()
    globals()['macro_env'] = make_macro_env_hat()
    return read_eval_print_loop_rm()

def read_eval_print_loop_rm():
    input_ = symbol_undefined
    input_ = raw_read_line("==> ")
    result = symbol_undefined
    result = execute_rm(input_, symbol_stdin)
    if not(void_q(result)):
        if exception_q(result):
            handle_exception(result)
        else:
            safe_print(result)
    if _starneed_newline_star:
        newline()
    if end_of_session_q(result):
        globals()['final_reg'] = symbol_goodbye
        globals()['pc'] = pc_halt_signal
    else:
        return read_eval_print_loop_rm()

def execute_string_rm(input_):
    return execute_rm(input_, symbol_stdin)

def execute_file_rm(filename):
    return execute_rm(read_content(filename), filename)

def execute_rm(input_, src):
    globals()['load_stack'] = symbol_emptylist
    initialize_execute()
    globals()['k_reg'] = REP_k
    globals()['fail_reg'] = _starlast_fail_star
    globals()['handler_reg'] = REP_handler
    globals()['src_reg'] = src
    globals()['input_reg'] = input_
    globals()['pc'] = scan_input
    result = symbol_undefined
    result = trampoline()
    if exception_q(result):
        return result
    else:
        globals()['_startokens_left_star'] = result
        if token_type_q(first(_startokens_left_star), symbol_end_marker):
            return void_value
        else:
            return execute_loop_rm(src)

def execute_loop_rm(src):
    execute_next_expression_rm(src)
    result = symbol_undefined
    result = trampoline()
    if (exception_q(result)) or (end_of_session_q(result)) or (token_type_q(first(_startokens_left_star), symbol_end_marker)):
        return result
    else:
        return execute_loop_rm(src)

def execute_next_expression_rm(src):
    globals()['k_reg'] = make_cont4(b_cont4_10_d)
    globals()['fail_reg'] = _starlast_fail_star
    globals()['handler_reg'] = REP_handler
    globals()['src_reg'] = src
    globals()['tokens_reg'] = _startokens_left_star
    globals()['pc'] = read_sexp

def try_parse(input_):
    globals()['load_stack'] = symbol_emptylist
    globals()['k_reg'] = make_cont2(b_cont2_52_d)
    globals()['fail_reg'] = _starlast_fail_star
    globals()['handler_reg'] = try_parse_handler
    globals()['src_reg'] = symbol_stdin
    globals()['input_reg'] = input_
    globals()['pc'] = scan_input
    return trampoline()

def initialize_globals():
    globals()['toplevel_env'] = make_toplevel_env()
    globals()['macro_env'] = make_macro_env_hat()
    globals()['load_stack'] = symbol_emptylist
    initialize_execute()
    globals()['_starlast_fail_star'] = REP_fail

def make_debugging_k(exp, k):
    return make_cont2(b_cont2_53_d, exp, k)

def get_use_stack_trace():
    return _staruse_stack_trace_star

def set_use_stack_trace(value):
    globals()['_staruse_stack_trace_star'] = value

def initialize_stack_trace():
    return set_car_b(_starstack_trace_star, symbol_emptylist)

def initialize_execute():
    globals()['_closure_depth'] = 0
    globals()['_trace_pause'] = False
    return initialize_stack_trace()

def push_stack_trace(exp):
    return set_car_b(_starstack_trace_star, cons(exp, car(_starstack_trace_star)))

def pop_stack_trace(exp):
    if not(null_q(car(_starstack_trace_star))):
        return set_car_b(_starstack_trace_star, cdr(car(_starstack_trace_star)))

def m():
    if _startracing_on_q_star:
        highlight_expression(exp_reg)
    k = symbol_undefined
    k = (make_debugging_k(exp_reg, k_reg) if _startracing_on_q_star else k_reg)
    if (car(exp_reg)) is (symbol_lit_aexp):
        datum = symbol_undefined
        datum = list_ref(exp_reg, 1)
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = datum
        globals()['k_reg'] = k
        globals()['pc'] = apply_cont2
    else:
        if (car(exp_reg)) is (symbol_var_aexp):
            id = symbol_undefined
            info = symbol_undefined
            info = list_ref(exp_reg, 2)
            id = list_ref(exp_reg, 1)
            globals()['k_reg'] = k
            globals()['var_info_reg'] = info
            globals()['var_reg'] = id
            globals()['pc'] = lookup_value
        else:
            if (car(exp_reg)) is (symbol_lexical_address_aexp):
                depth = symbol_undefined
                offset = symbol_undefined
                offset = list_ref(exp_reg, 2)
                depth = list_ref(exp_reg, 1)
                globals()['k_reg'] = k
                globals()['frames_reg'] = frames(env_reg)
                globals()['offset_reg'] = offset
                globals()['depth_reg'] = depth
                globals()['pc'] = lookup_value_by_lexical_address
            else:
                if (car(exp_reg)) is (symbol_func_aexp):
                    exp = symbol_undefined
                    exp = list_ref(exp_reg, 1)
                    globals()['k_reg'] = make_cont2(b_cont2_71_d, k)
                    globals()['exp_reg'] = exp
                    globals()['pc'] = m
                else:
                    if (car(exp_reg)) is (symbol_callback0_aexp):
                        exp = symbol_undefined
                        exp = list_ref(exp_reg, 1)
                        globals()['k_reg'] = make_cont2(b_cont2_70_d, k)
                        globals()['exp_reg'] = exp
                        globals()['pc'] = m
                    else:
                        if (car(exp_reg)) is (symbol_callback1_aexp):
                            exp = symbol_undefined
                            exp = list_ref(exp_reg, 1)
                            globals()['k_reg'] = make_cont2(b_cont2_69_d, k)
                            globals()['exp_reg'] = exp
                            globals()['pc'] = m
                        else:
                            if (car(exp_reg)) is (symbol_callback2_aexp):
                                exp = symbol_undefined
                                exp = list_ref(exp_reg, 1)
                                globals()['k_reg'] = make_cont2(b_cont2_68_d, k)
                                globals()['exp_reg'] = exp
                                globals()['pc'] = m
                            else:
                                if (car(exp_reg)) is (symbol_if_aexp):
                                    test_exp = symbol_undefined
                                    then_exp = symbol_undefined
                                    else_exp = symbol_undefined
                                    else_exp = list_ref(exp_reg, 3)
                                    then_exp = list_ref(exp_reg, 2)
                                    test_exp = list_ref(exp_reg, 1)
                                    globals()['k_reg'] = make_cont2(b_cont2_67_d, else_exp, then_exp, env_reg, handler_reg, k)
                                    globals()['exp_reg'] = test_exp
                                    globals()['pc'] = m
                                else:
                                    if (car(exp_reg)) is (symbol_assign_aexp):
                                        var = symbol_undefined
                                        rhs_exp = symbol_undefined
                                        var_info = symbol_undefined
                                        var_info = list_ref(exp_reg, 3)
                                        rhs_exp = list_ref(exp_reg, 2)
                                        var = list_ref(exp_reg, 1)
                                        globals()['k_reg'] = make_cont2(b_cont2_66_d, var, var_info, env_reg, handler_reg, k)
                                        globals()['exp_reg'] = rhs_exp
                                        globals()['pc'] = m
                                    else:
                                        if (car(exp_reg)) is (symbol_define_aexp):
                                            var = symbol_undefined
                                            docstring = symbol_undefined
                                            rhs_exp = symbol_undefined
                                            rhs_exp = list_ref(exp_reg, 3)
                                            docstring = list_ref(exp_reg, 2)
                                            var = list_ref(exp_reg, 1)
                                            globals()['k_reg'] = make_cont2(b_cont2_63_d, docstring, var, env_reg, handler_reg, k)
                                            globals()['exp_reg'] = rhs_exp
                                            globals()['pc'] = m
                                        else:
                                            if (car(exp_reg)) is (symbol_define_b_aexp):
                                                var = symbol_undefined
                                                docstring = symbol_undefined
                                                rhs_exp = symbol_undefined
                                                rhs_exp = list_ref(exp_reg, 3)
                                                docstring = list_ref(exp_reg, 2)
                                                var = list_ref(exp_reg, 1)
                                                globals()['k_reg'] = make_cont2(b_cont2_61_d, docstring, var, k)
                                                globals()['exp_reg'] = rhs_exp
                                                globals()['pc'] = m
                                            else:
                                                if (car(exp_reg)) is (symbol_define_syntax_aexp):
                                                    name = symbol_undefined
                                                    clauses = symbol_undefined
                                                    aclauses = symbol_undefined
                                                    aclauses = list_ref(exp_reg, 3)
                                                    clauses = list_ref(exp_reg, 2)
                                                    name = list_ref(exp_reg, 1)
                                                    globals()['k_reg'] = make_cont2(b_cont2_60_d, aclauses, clauses, k)
                                                    globals()['env_reg'] = macro_env
                                                    globals()['var_reg'] = name
                                                    globals()['pc'] = lookup_binding_in_first_frame
                                                else:
                                                    if (car(exp_reg)) is (symbol_begin_aexp):
                                                        exps = symbol_undefined
                                                        exps = list_ref(exp_reg, 1)
                                                        globals()['k_reg'] = k
                                                        globals()['exps_reg'] = exps
                                                        globals()['pc'] = eval_sequence
                                                    else:
                                                        if (car(exp_reg)) is (symbol_lambda_aexp):
                                                            formals = symbol_undefined
                                                            bodies = symbol_undefined
                                                            bodies = list_ref(exp_reg, 2)
                                                            formals = list_ref(exp_reg, 1)
                                                            globals()['value2_reg'] = fail_reg
                                                            globals()['value1_reg'] = closure(formals, bodies, env_reg)
                                                            globals()['k_reg'] = k
                                                            globals()['pc'] = apply_cont2
                                                        else:
                                                            if (car(exp_reg)) is (symbol_mu_lambda_aexp):
                                                                formals = symbol_undefined
                                                                runt = symbol_undefined
                                                                bodies = symbol_undefined
                                                                bodies = list_ref(exp_reg, 3)
                                                                runt = list_ref(exp_reg, 2)
                                                                formals = list_ref(exp_reg, 1)
                                                                globals()['value2_reg'] = fail_reg
                                                                globals()['value1_reg'] = mu_closure(formals, runt, bodies, env_reg)
                                                                globals()['k_reg'] = k
                                                                globals()['pc'] = apply_cont2
                                                            else:
                                                                if (car(exp_reg)) is (symbol_trace_lambda_aexp):
                                                                    name = symbol_undefined
                                                                    formals = symbol_undefined
                                                                    bodies = symbol_undefined
                                                                    bodies = list_ref(exp_reg, 3)
                                                                    formals = list_ref(exp_reg, 2)
                                                                    name = list_ref(exp_reg, 1)
                                                                    globals()['value2_reg'] = fail_reg
                                                                    globals()['value1_reg'] = trace_closure(name, formals, bodies, env_reg)
                                                                    globals()['k_reg'] = k
                                                                    globals()['pc'] = apply_cont2
                                                                else:
                                                                    if (car(exp_reg)) is (symbol_mu_trace_lambda_aexp):
                                                                        name = symbol_undefined
                                                                        formals = symbol_undefined
                                                                        runt = symbol_undefined
                                                                        bodies = symbol_undefined
                                                                        bodies = list_ref(exp_reg, 4)
                                                                        runt = list_ref(exp_reg, 3)
                                                                        formals = list_ref(exp_reg, 2)
                                                                        name = list_ref(exp_reg, 1)
                                                                        globals()['value2_reg'] = fail_reg
                                                                        globals()['value1_reg'] = mu_trace_closure(name, formals, runt, bodies, env_reg)
                                                                        globals()['k_reg'] = k
                                                                        globals()['pc'] = apply_cont2
                                                                    else:
                                                                        if (car(exp_reg)) is (symbol_try_catch_aexp):
                                                                            body = symbol_undefined
                                                                            cvar = symbol_undefined
                                                                            cexps = symbol_undefined
                                                                            cexps = list_ref(exp_reg, 3)
                                                                            cvar = list_ref(exp_reg, 2)
                                                                            body = list_ref(exp_reg, 1)
                                                                            new_handler = symbol_undefined
                                                                            new_handler = try_catch_handler(cvar, cexps, env_reg, handler_reg, k)
                                                                            globals()['k_reg'] = k
                                                                            globals()['handler_reg'] = new_handler
                                                                            globals()['exp_reg'] = body
                                                                            globals()['pc'] = m
                                                                        else:
                                                                            if (car(exp_reg)) is (symbol_try_finally_aexp):
                                                                                body = symbol_undefined
                                                                                fexps = symbol_undefined
                                                                                fexps = list_ref(exp_reg, 2)
                                                                                body = list_ref(exp_reg, 1)
                                                                                new_handler = symbol_undefined
                                                                                new_handler = try_finally_handler(fexps, env_reg, handler_reg)
                                                                                globals()['k_reg'] = make_cont2(b_cont2_59_d, fexps, env_reg, handler_reg, k)
                                                                                globals()['handler_reg'] = new_handler
                                                                                globals()['exp_reg'] = body
                                                                                globals()['pc'] = m
                                                                            else:
                                                                                if (car(exp_reg)) is (symbol_try_catch_finally_aexp):
                                                                                    body = symbol_undefined
                                                                                    cvar = symbol_undefined
                                                                                    cexps = symbol_undefined
                                                                                    fexps = symbol_undefined
                                                                                    fexps = list_ref(exp_reg, 4)
                                                                                    cexps = list_ref(exp_reg, 3)
                                                                                    cvar = list_ref(exp_reg, 2)
                                                                                    body = list_ref(exp_reg, 1)
                                                                                    new_handler = symbol_undefined
                                                                                    new_handler = try_catch_finally_handler(cvar, cexps, fexps, env_reg, handler_reg, k)
                                                                                    globals()['k_reg'] = make_cont2(b_cont2_59_d, fexps, env_reg, handler_reg, k)
                                                                                    globals()['handler_reg'] = new_handler
                                                                                    globals()['exp_reg'] = body
                                                                                    globals()['pc'] = m
                                                                                else:
                                                                                    if (car(exp_reg)) is (symbol_raise_aexp):
                                                                                        exp = symbol_undefined
                                                                                        exp = list_ref(exp_reg, 1)
                                                                                        globals()['k_reg'] = make_cont2(b_cont2_57_d, handler_reg)
                                                                                        globals()['exp_reg'] = exp
                                                                                        globals()['pc'] = m
                                                                                    else:
                                                                                        if (car(exp_reg)) is (symbol_choose_aexp):
                                                                                            exps = symbol_undefined
                                                                                            exps = list_ref(exp_reg, 1)
                                                                                            globals()['k_reg'] = k
                                                                                            globals()['exps_reg'] = exps
                                                                                            globals()['pc'] = eval_choices
                                                                                        else:
                                                                                            if (car(exp_reg)) is (symbol_app_aexp):
                                                                                                operator = symbol_undefined
                                                                                                operands = symbol_undefined
                                                                                                info = symbol_undefined
                                                                                                info = list_ref(exp_reg, 3)
                                                                                                operands = list_ref(exp_reg, 2)
                                                                                                operator = list_ref(exp_reg, 1)
                                                                                                globals()['k_reg'] = make_cont2(b_cont2_56_d, exp_reg, operator, env_reg, info, handler_reg, k)
                                                                                                globals()['exps_reg'] = operands
                                                                                                globals()['pc'] = m_star
                                                                                            else:
                                                                                                raise Exception("symbol_m: " + format("bad abstract syntax: '~s'", *[exp_reg]))

def make_exception(exception, message, source, line, column):
    return List(exception, message, source, line, column, make_stack_trace())

def make_stack_trace():
    trace = symbol_undefined
    trace = car(_starstack_trace_star)
    return reverse(Map(format_stack_trace, trace))

def get_procedure_name(aexp):
    if macro_derived_source_info_q(aexp):
        return rac(get_source_info(aexp))
    else:
        if (car(aexp)) is (symbol_app_aexp):
            operator = symbol_undefined
            operator = list_ref(aexp, 1)
            if (car(operator)) is (symbol_lexical_address_aexp):
                id = symbol_undefined
                id = list_ref(operator, 3)
                return id
            else:
                if (car(operator)) is (symbol_var_aexp):
                    id = symbol_undefined
                    id = list_ref(operator, 1)
                    return id
                else:
                    if (car(operator)) is (symbol_lambda_aexp):
                        formals = symbol_undefined
                        formals = list_ref(operator, 1)
                        return append(List(symbol_lambda), append(List(formals), List(symbol_dotdotdot)))
                    else:
                        if (car(operator)) is (symbol_mu_lambda_aexp):
                            formals = symbol_undefined
                            runt = symbol_undefined
                            runt = list_ref(operator, 2)
                            formals = list_ref(operator, 1)
                            return append(List(symbol_lambda), append(List(append(formals, runt)), List(symbol_dotdotdot)))
                        else:
                            if (car(operator)) is (symbol_trace_lambda_aexp):
                                name = symbol_undefined
                                name = list_ref(operator, 1)
                                return name
                            else:
                                if (car(operator)) is (symbol_mu_trace_lambda_aexp):
                                    name = symbol_undefined
                                    name = list_ref(operator, 1)
                                    return name
                                else:
                                    return symbol_application
        else:
            return symbol_unknown

def format_stack_trace(exp):
    info = symbol_undefined
    info = rac(exp)
    if (info) is (symbol_none):
        return symbol_macro_generated_exp
    else:
        return List(get_srcfile(info), get_start_line(info), get_start_char(info), get_procedure_name(exp))

def runtime_error():
    if (info_reg) is (symbol_none):
        globals()['exception_reg'] = make_exception("RunTimeError", msg_reg, symbol_none, symbol_none, symbol_none)
        globals()['pc'] = apply_handler2
    else:
        src = symbol_undefined
        line_number = symbol_undefined
        char_number = symbol_undefined
        char_number = get_start_char(info_reg)
        line_number = get_start_line(info_reg)
        src = get_srcfile(info_reg)
        globals()['exception_reg'] = make_exception("RunTimeError", msg_reg, src, line_number, char_number)
        globals()['pc'] = apply_handler2

def m_star():
    if null_q(exps_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont2(b_cont2_72_d, exps_reg, env_reg, handler_reg, k_reg)
        globals()['exp_reg'] = car(exps_reg)
        globals()['pc'] = m

def eval_sequence():
    if null_q(cdr(exps_reg)):
        globals()['exp_reg'] = car(exps_reg)
        globals()['pc'] = m
    else:
        globals()['k_reg'] = make_cont2(b_cont2_73_d, exps_reg, env_reg, handler_reg, k_reg)
        globals()['exp_reg'] = car(exps_reg)
        globals()['pc'] = m

def try_catch_handler(cvar, cexps, env, handler, k):
    return make_handler2(b_handler2_4_d, cexps, cvar, env, handler, k)

def try_finally_handler(fexps, env, handler):
    return make_handler2(b_handler2_5_d, fexps, env, handler)

def try_catch_finally_handler(cvar, cexps, fexps, env, handler, k):
    return make_handler2(b_handler2_6_d, cexps, cvar, fexps, env, handler, k)

def eval_choices():
    if null_q(exps_reg):
        globals()['pc'] = apply_fail
    else:
        new_fail = symbol_undefined
        new_fail = make_fail(b_fail_5_d, exps_reg, env_reg, handler_reg, fail_reg, k_reg)
        globals()['fail_reg'] = new_fail
        globals()['exp_reg'] = car(exps_reg)
        globals()['pc'] = m

def closure(formals, bodies, env):
    return make_proc(b_proc_1_d, bodies, formals, env)

def mu_closure(formals, runt, bodies, env):
    return make_proc(b_proc_2_d, bodies, formals, runt, env)

def make_trace_depth_string(level):
    if Equal(level, 0):
        return ""
    else:
        return string_append(" |", make_trace_depth_string((level) - (1)))

def trace_closure(name, formals, bodies, env):
    trace_depth = symbol_undefined
    trace_depth = 0
    return make_proc(b_proc_3_d, bodies, name, trace_depth, formals, env)

def continuation_object_q(x):
    return (pair_q(x)) and (memq(car(x), List(symbol_continuation, symbol_continuation2, symbol_continuation3, symbol_continuation4)))

def mu_trace_closure(name, formals, runt, bodies, env):
    trace_depth = symbol_undefined
    trace_depth = 0
    return make_proc(b_proc_4_d, bodies, name, trace_depth, formals, runt, env)

def all_char_q(ls):
    return (null_q(ls)) or ((char_q(car(ls))) and (all_char_q(cdr(ls))))

def void_q(x):
    return (x) is (void_value)

def end_of_session_q(x):
    return (x) is (end_of_session)

def procedure_object_q(x):
    return (procedure_q(x)) or ((pair_q(x)) and ((car(x)) is (symbol_procedure)))

def environment_object_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_environment))

def ends_with_newline_q(s):
    len = symbol_undefined
    len = string_length(s)
    return equal_q(substring(s, (len) - (1), len), "\n")

def load_file():
    if member(filename_reg, load_stack):
        printf("skipping recursive load of ~a~%", filename_reg)
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = void_value
        globals()['pc'] = apply_cont2
    else:
        if not(string_q(filename_reg)):
            globals()['msg_reg'] = format("filename '~a' is not a string", filename_reg)
            globals()['pc'] = runtime_error
        else:
            if not(file_exists_q(filename_reg)):
                globals()['msg_reg'] = format("attempted to load nonexistent file '~a'", filename_reg)
                globals()['pc'] = runtime_error
            else:
                globals()['load_stack'] = cons(filename_reg, load_stack)
                globals()['k_reg'] = make_cont2(b_cont2_81_d, filename_reg, env2_reg, handler_reg, k_reg)
                globals()['src_reg'] = filename_reg
                globals()['input_reg'] = read_content(filename_reg)
                globals()['pc'] = scan_input

def read_and_eval_asexps():
    if token_type_q(first(tokens_reg), symbol_end_marker):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = void_value
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont4(b_cont4_13_d, src_reg, env2_reg, handler_reg, k_reg)
        globals()['pc'] = read_sexp

def load_files():
    if null_q(filenames_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = void_value
        globals()['pc'] = apply_cont2
    else:
        globals()['k_reg'] = make_cont2(b_cont2_84_d, filenames_reg, env2_reg, info_reg, handler_reg, k_reg)
        globals()['filename_reg'] = car(filenames_reg)
        globals()['pc'] = load_file

def length_loop():
    if null_q(x_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = sum_reg
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        if not(pair_q(x_reg)):
            globals()['msg_reg'] = format("length called on improper list ~s", ls_reg)
            globals()['pc'] = runtime_error
        else:
            globals()['sum_reg'] = (sum_reg) + (1)
            globals()['x_reg'] = cdr(x_reg)
            globals()['pc'] = length_loop

def make_set():
    if null_q(lst_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = lst_reg
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_85_d, lst_reg, k2_reg)
        globals()['lst_reg'] = cdr(lst_reg)
        globals()['pc'] = make_set

def equal_objects_q():
    if ((null_q(x_reg)) and (null_q(y_reg))) or ((boolean_q(x_reg)) and (boolean_q(y_reg)) and (((x_reg) and (y_reg)) or ((not(x_reg)) and (not(y_reg))))) or ((symbol_q(x_reg)) and (symbol_q(y_reg)) and ((x_reg) is (y_reg))) or ((number_q(x_reg)) and (number_q(y_reg)) and (Equal(x_reg, y_reg))) or ((char_q(x_reg)) and (char_q(y_reg)) and (char_is__q(x_reg, y_reg))) or (((x_reg) is (void_value)) and ((y_reg) is (void_value))) or ((string_q(x_reg)) and (string_q(y_reg)) and (string_is__q(x_reg, y_reg))):
        globals()['value_reg'] = True
        globals()['pc'] = apply_cont
    else:
        if (pair_q(x_reg)) and (pair_q(y_reg)):
            globals()['k_reg'] = make_cont(b_cont_45_d, x_reg, y_reg, k_reg)
            globals()['y_reg'] = car(y_reg)
            globals()['x_reg'] = car(x_reg)
            globals()['pc'] = equal_objects_q
        else:
            if (vector_q(x_reg)) and (vector_q(y_reg)) and (Equal(vector_length(x_reg), vector_length(y_reg))):
                globals()['i_reg'] = (vector_length(x_reg)) - (1)
                globals()['v2_reg'] = y_reg
                globals()['v1_reg'] = x_reg
                globals()['pc'] = equal_vectors_q
            else:
                globals()['value_reg'] = False
                globals()['pc'] = apply_cont

def equal_vectors_q():
    if LessThan(i_reg, 0):
        globals()['value_reg'] = True
        globals()['pc'] = apply_cont
    else:
        globals()['k_reg'] = make_cont(b_cont_46_d, i_reg, v1_reg, v2_reg, k_reg)
        globals()['y_reg'] = vector_ref(v2_reg, i_reg)
        globals()['x_reg'] = vector_ref(v1_reg, i_reg)
        globals()['pc'] = equal_objects_q

def member_loop():
    if null_q(y_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = False
        globals()['pc'] = apply_cont2
    else:
        if not(pair_q(y_reg)):
            globals()['msg_reg'] = format("member called on improper list ~s", ls_reg)
            globals()['pc'] = runtime_error
        else:
            globals()['k_reg'] = make_cont(b_cont_47_d, ls_reg, x_reg, y_reg, info_reg, handler_reg, fail_reg, k_reg)
            globals()['y_reg'] = car(y_reg)
            globals()['pc'] = equal_objects_q

def get_primitive():
    sym = symbol_undefined
    sym = car(args_reg)
    globals()['k_reg'] = make_cont2(b_cont2_87_d, args_reg, sym, info_reg, handler_reg, k_reg)
    globals()['var_info_reg'] = symbol_none
    globals()['var_reg'] = sym
    globals()['pc'] = lookup_value

def append2():
    if null_q(ls1_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = ls2_reg
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_88_d, ls1_reg, k2_reg)
        globals()['ls1_reg'] = cdr(ls1_reg)
        globals()['pc'] = append2

def append_all():
    if null_q(lists_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        if null_q(cdr(lists_reg)):
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = car(lists_reg)
            globals()['k_reg'] = k2_reg
            globals()['pc'] = apply_cont2
        else:
            if not(list_q(car(lists_reg))):
                globals()['msg_reg'] = format("append called on incorrect list structure ~s", car(lists_reg))
                globals()['pc'] = runtime_error
            else:
                globals()['k2_reg'] = make_cont2(b_cont2_89_d, lists_reg, k2_reg)
                globals()['lists_reg'] = cdr(lists_reg)
                globals()['pc'] = append_all

def directory(args, env):
    if (null_q(args)) or (environment_q(car(args))):
        return sort(symbolLessThan_q, (append(get_variables_from_frames(frames(macro_env)), get_variables_from_frames(frames(env))) if null_q(args) else get_variables_from_frames(frames(car(args)))))
    else:
        return get_external_members(car(args))

def get_variables_from_frame(frame):
    return cadr(frame)

def get_variables_from_frames(frames):
    return flatten(Map(get_variables_from_frame, frames))

def symbolLessThan_q(a, b):
    a_string = symbol_undefined
    b_string = symbol_undefined
    b_string = symbol_to_string(b)
    a_string = symbol_to_string(a)
    return stringLessThan_q(a_string, b_string)

def flatten(lists):
    if null_q(lists):
        return symbol_emptylist
    else:
        if list_q(car(lists)):
            return append(flatten(car(lists)), flatten(cdr(lists)))
        else:
            return cons(car(lists), flatten(cdr(lists)))

def map_primitive():
    if iterator_q(car(args_reg)):
        globals()['generator_reg'] = car(args_reg)
        globals()['pc'] = iterate_collect
    else:
        len = symbol_undefined
        list_args = symbol_undefined
        list_args = listify(args_reg)
        len = length(args_reg)
        if Equal(len, 1):
            globals()['list1_reg'] = car(list_args)
            globals()['pc'] = map1
        else:
            if Equal(len, 2):
                globals()['list2_reg'] = cadr(list_args)
                globals()['list1_reg'] = car(list_args)
                globals()['pc'] = map2
            else:
                globals()['lists_reg'] = list_args
                globals()['pc'] = mapN

def listify(arg_list):
    if null_q(arg_list):
        return symbol_emptylist
    else:
        if list_q(car(arg_list)):
            return cons(car(arg_list), listify(cdr(arg_list)))
        else:
            if vector_q(car(arg_list)):
                return cons(vector_to_list(car(arg_list)), listify(cdr(arg_list)))
            else:
                if string_q(car(arg_list)):
                    return cons(string_to_list(car(arg_list)), listify(cdr(arg_list)))
                else:
                    raise Exception("symbol_Map: " + format("cannot use object type '~a' in map", *[get_type(car(arg_list))]))

def iterate():
    iterator = symbol_undefined
    iterator = get_iterator(generator_reg)
    globals()['iterator_reg'] = iterator
    globals()['pc'] = iterate_continue

def iterate_continue():
    item = symbol_undefined
    item = next_item(iterator_reg)
    if null_q(item):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_90_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        globals()['info_reg'] = symbol_none
        globals()['env2_reg'] = env_reg
        globals()['args_reg'] = List(item)
        globals()['pc'] = apply_proc

def iterate_collect():
    iterator = symbol_undefined
    iterator = get_iterator(generator_reg)
    globals()['iterator_reg'] = iterator
    globals()['pc'] = iterate_collect_continue

def iterate_collect_continue():
    item = symbol_undefined
    item = next_item(iterator_reg)
    if null_q(item):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        globals()['k2_reg'] = make_cont2(b_cont2_91_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        globals()['info_reg'] = symbol_none
        globals()['env2_reg'] = env_reg
        globals()['args_reg'] = List(item)
        globals()['pc'] = apply_proc

def map1():
    if null_q(list1_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        if dlr_proc_q(proc_reg):
            globals()['k_reg'] = make_cont2(b_cont2_93_d, list1_reg, proc_reg, k_reg)
            globals()['list1_reg'] = cdr(list1_reg)
            globals()['pc'] = map1
        else:
            globals()['k2_reg'] = make_cont2(b_cont2_92_d, list1_reg, proc_reg, env_reg, handler_reg, k_reg)
            globals()['info_reg'] = symbol_none
            globals()['env2_reg'] = env_reg
            globals()['args_reg'] = List(car(list1_reg))
            globals()['pc'] = apply_proc

def map2():
    if null_q(list1_reg):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        if dlr_proc_q(proc_reg):
            globals()['k_reg'] = make_cont2(b_cont2_95_d, list1_reg, list2_reg, proc_reg, k_reg)
            globals()['list2_reg'] = cdr(list2_reg)
            globals()['list1_reg'] = cdr(list1_reg)
            globals()['pc'] = map2
        else:
            globals()['k2_reg'] = make_cont2(b_cont2_94_d, list1_reg, list2_reg, proc_reg, env_reg, handler_reg, k_reg)
            globals()['info_reg'] = symbol_none
            globals()['env2_reg'] = env_reg
            globals()['args_reg'] = List(car(list1_reg), car(list2_reg))
            globals()['pc'] = apply_proc

def mapN():
    if null_q(car(lists_reg)):
        globals()['value2_reg'] = fail_reg
        globals()['value1_reg'] = symbol_emptylist
        globals()['pc'] = apply_cont2
    else:
        if dlr_proc_q(proc_reg):
            globals()['k_reg'] = make_cont2(b_cont2_97_d, lists_reg, proc_reg, k_reg)
            globals()['lists_reg'] = Map(cdr, lists_reg)
            globals()['pc'] = mapN
        else:
            globals()['k2_reg'] = make_cont2(b_cont2_96_d, lists_reg, proc_reg, env_reg, handler_reg, k_reg)
            globals()['info_reg'] = symbol_none
            globals()['env2_reg'] = env_reg
            globals()['args_reg'] = Map(car, lists_reg)
            globals()['pc'] = apply_proc

def for_each_primitive():
    if iterator_q(car(lists_reg)):
        globals()['generator_reg'] = car(lists_reg)
        globals()['pc'] = iterate
    else:
        arg_list = symbol_undefined
        arg_list = listify(lists_reg)
        if null_q(car(arg_list)):
            globals()['value2_reg'] = fail_reg
            globals()['value1_reg'] = void_value
            globals()['pc'] = apply_cont2
        else:
            if dlr_proc_q(proc_reg):
                dlr_apply(proc_reg, Map(car, arg_list))
                globals()['lists_reg'] = Map(cdr, arg_list)
                globals()['pc'] = for_each_primitive
            else:
                globals()['k2_reg'] = make_cont2(b_cont2_98_d, arg_list, proc_reg, env_reg, handler_reg, k_reg)
                globals()['info_reg'] = symbol_none
                globals()['env2_reg'] = env_reg
                globals()['args_reg'] = Map(car, arg_list)
                globals()['pc'] = apply_proc

def make_toplevel_env():
    primitives = symbol_undefined
    primitives = List(List(symbol_multiply, times_prim), List(symbol_plus, plus_prim), List(symbol_minus, minus_prim), List(symbol_divide, divide_prim), List(symbol_p, modulo_prim), List(symbol_LessThan, lt_prim), List(symbol_LessThanEqual, lt_or_eq_prim), List(symbol_Equal, equal_sign_prim), List(symbol_GreaterThan, gt_prim), List(symbol_GreaterThanEqual, gt_or_eq_prim), List(symbol_abort, abort_prim), List(symbol_abs, abs_prim), List(symbol_append, append_prim), List(symbol_Apply, apply_prim), List(symbol_assv, assv_prim), List(symbol_boolean_q, boolean_q_prim), List(symbol_caddr, caddr_prim), List(symbol_cadr, cadr_prim), List(symbol_call_with_current_continuation, call_cc_prim), List(symbol_call_cc, call_cc_prim), List(symbol_car, car_prim), List(symbol_cdr, cdr_prim), List(symbol_char_q, char_q_prim), List(symbol_char_is__q, char_is__q_prim), List(symbol_char_whitespace_q, char_whitespace_q_prim), List(symbol_char_alphabetic_q, char_alphabetic_q_prim), List(symbol_char_numeric_q, char_numeric_q_prim), List(symbol_char_to_integer, char_to_integer_prim), List(symbol_cons, cons_prim), List(symbol_current_time, current_time_prim), List(symbol_cut, cut_prim), List(symbol_dir, dir_prim), List(symbol_display, display_prim), List(symbol_current_environment, current_environment_prim), List(symbol_eq_q, eq_q_prim), List(symbol_equal_q, equal_q_prim), List(symbol_error, error_prim), List(symbol_eval, eval_prim), List(symbol_eval_ast, eval_ast_prim), List(symbol_exit, exit_prim), List(symbol_for_each, for_each_prim), List(symbol_format, format_prim), List(symbol_get, get_prim), List(symbol_get_stack_trace, get_stack_trace_prim), List(symbol_import, import_prim), List(symbol_integer_to_char, integer_to_char_prim), List(symbol_length, length_prim), List(symbol_List, list_prim), List(symbol_list_to_vector, list_to_vector_prim), List(symbol_list_to_string, list_to_string_prim), List(symbol_list_ref, list_ref_prim), List(symbol_load, load_prim), List(symbol_make_set, make_set_prim), List(symbol_make_vector, make_vector_prim), List(symbol_Map, map_prim), List(symbol_member, member_prim), List(symbol_memq, memq_prim), List(symbol_memv, memv_prim), List(symbol_newline, newline_prim), List(symbol_not, not_prim), List(symbol_null_q, null_q_prim), List(symbol_number_to_string, number_to_string_prim), List(symbol_number_q, number_q_prim), List(symbol_pair_q, pair_q_prim), List(symbol_parse, parse_prim), List(symbol_parse_string, parse_string_prim), List(symbol_print, print_prim), List(symbol_printf, printf_prim), List(symbol_Range, range_prim), List(symbol_read_string, read_string_prim), List(symbol_require, require_prim), List(symbol_reverse, reverse_prim), List(symbol_set_car_b, set_car_b_prim), List(symbol_set_cdr_b, set_cdr_b_prim), List(symbol_snoc, snoc_prim), List(symbol_rac, rac_prim), List(symbol_rdc, rdc_prim), List(symbol_sqrt, sqrt_prim), List(symbol_odd_q, odd_q_prim), List(symbol_even_q, even_q_prim), List(symbol_quotient, quotient_prim), List(symbol_remainder, remainder_prim), List(symbol_string, string_prim), List(symbol_string_length, string_length_prim), List(symbol_string_ref, string_ref_prim), List(symbol_string_q, string_q_prim), List(symbol_string_to_number, string_to_number_prim), List(symbol_string_is__q, string_is__q_prim), List(symbol_substring, substring_prim), List(symbol_symbol_q, symbol_q_prim), List(symbol_unparse, unparse_prim), List(symbol_unparse_procedure, unparse_procedure_prim), List(symbol_using, using_prim), List(symbol_set_use_stack_trace_b, set_use_stack_trace_b_prim), List(symbol_vector, vector_prim), List(symbol_vector_ref, vector_ref_prim), List(symbol_vector_set_b, vector_set_b_prim), List(symbol_void, void_prim), List(symbol_zero_q, zero_q_prim), List(symbol_current_directory, current_directory_prim), List(symbol_cd, current_directory_prim), List(symbol_round, round_prim))
    return make_initial_env_extended(Map(car, primitives), Map(cadr, primitives))

def make_external_proc(external_function_object):
    return make_proc(b_proc_106_d, external_function_object)

def pattern_q(x):
    return (null_q(x)) or (number_q(x)) or (boolean_q(x)) or (symbol_q(x)) or ((pair_q(x)) and (pattern_q(car(x))) and (pattern_q(cdr(x))))

def pattern_variable_q(x):
    return (symbol_q(x)) and (equal_q("?", substring(symbol_to_string(x), 0, 1)))

def constant_q(x):
    return (not(pattern_variable_q(x))) and (not(pair_q(x)))

def occurs_q():
    if constant_q(pattern_reg):
        globals()['value_reg'] = False
        globals()['pc'] = apply_cont
    else:
        if pattern_variable_q(pattern_reg):
            globals()['value_reg'] = equal_q(var_reg, pattern_reg)
            globals()['pc'] = apply_cont
        else:
            globals()['k_reg'] = make_cont(b_cont_48_d, pattern_reg, var_reg, k_reg)
            globals()['pattern_reg'] = car(pattern_reg)
            globals()['pc'] = occurs_q

def unify_patterns_hat():
    if pattern_variable_q(p1_reg):
        if pattern_variable_q(p2_reg):
            globals()['value_reg'] = make_sub(symbol_unit, p1_reg, p2_reg, ap2_reg)
            globals()['pc'] = apply_cont
        else:
            globals()['k_reg'] = make_cont(b_cont_49_d, ap2_reg, p1_reg, p2_reg, k_reg)
            globals()['pattern_reg'] = p2_reg
            globals()['var_reg'] = p1_reg
            globals()['pc'] = occurs_q
    else:
        if pattern_variable_q(p2_reg):
            temp_1 = p2_reg
            temp_2 = p1_reg
            temp_3 = ap2_reg
            temp_4 = ap1_reg
            globals()['p1_reg'] = temp_1
            globals()['p2_reg'] = temp_2
            globals()['ap1_reg'] = temp_3
            globals()['ap2_reg'] = temp_4
            globals()['pc'] = unify_patterns_hat
        else:
            if (constant_q(p1_reg)) and (constant_q(p2_reg)) and (equal_q(p1_reg, p2_reg)):
                globals()['value_reg'] = make_sub(symbol_empty)
                globals()['pc'] = apply_cont
            else:
                if (pair_q(p1_reg)) and (pair_q(p2_reg)):
                    globals()['apair2_reg'] = ap2_reg
                    globals()['apair1_reg'] = ap1_reg
                    globals()['pair2_reg'] = p2_reg
                    globals()['pair1_reg'] = p1_reg
                    globals()['pc'] = unify_pairs_hat
                else:
                    globals()['value_reg'] = False
                    globals()['pc'] = apply_cont

def unify_pairs_hat():
    globals()['k_reg'] = make_cont(b_cont_51_d, apair1_reg, apair2_reg, pair1_reg, pair2_reg, k_reg)
    globals()['ap2_reg'] = car_hat(apair2_reg)
    globals()['ap1_reg'] = car_hat(apair1_reg)
    globals()['p2_reg'] = car(pair2_reg)
    globals()['p1_reg'] = car(pair1_reg)
    globals()['pc'] = unify_patterns_hat

def instantiate_hat():
    if constant_q(pattern_reg):
        globals()['value2_reg'] = ap_reg
        globals()['value1_reg'] = pattern_reg
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        if pattern_variable_q(pattern_reg):
            globals()['avar_reg'] = ap_reg
            globals()['var_reg'] = pattern_reg
            globals()['pc'] = apply_sub_hat
        else:
            if pair_q(pattern_reg):
                globals()['k2_reg'] = make_cont2(b_cont2_102_d, ap_reg, pattern_reg, s_reg, k2_reg)
                globals()['ap_reg'] = car_hat(ap_reg)
                globals()['pattern_reg'] = car(pattern_reg)
                globals()['pc'] = instantiate_hat
            else:
                raise Exception("symbol_instantiate_hat: " + format("bad pattern: ~a", *[pattern_reg]))

def make_sub(*args):
    args = List(*args)
    return cons(symbol_substitution, args)

def apply_sub_hat():
    temp_1 = symbol_undefined
    temp_1 = cdr(s_reg)
    if (car(temp_1)) is (symbol_empty):
        globals()['value2_reg'] = avar_reg
        globals()['value1_reg'] = var_reg
        globals()['k_reg'] = k2_reg
        globals()['pc'] = apply_cont2
    else:
        if (car(temp_1)) is (symbol_unit):
            new_var = symbol_undefined
            new_pattern = symbol_undefined
            new_apattern = symbol_undefined
            new_apattern = list_ref(temp_1, 3)
            new_pattern = list_ref(temp_1, 2)
            new_var = list_ref(temp_1, 1)
            if equal_q(var_reg, new_var):
                globals()['value2_reg'] = new_apattern
                globals()['value1_reg'] = new_pattern
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2
            else:
                globals()['value2_reg'] = avar_reg
                globals()['value1_reg'] = var_reg
                globals()['k_reg'] = k2_reg
                globals()['pc'] = apply_cont2
        else:
            if (car(temp_1)) is (symbol_composite):
                s1 = symbol_undefined
                s2 = symbol_undefined
                s2 = list_ref(temp_1, 2)
                s1 = list_ref(temp_1, 1)
                globals()['k2_reg'] = make_cont2(b_cont2_103_d, s2, k2_reg)
                globals()['s_reg'] = s1
                globals()['pc'] = apply_sub_hat
            else:
                raise Exception("symbol_apply_sub_hat: " + format("bad substitution: ~a", *[s_reg]))

chars_to_scan = symbol_undefined
scan_line = symbol_undefined
scan_char = symbol_undefined
scan_position = symbol_undefined
last_scan_line = symbol_undefined
last_scan_char = symbol_undefined
last_scan_position = symbol_undefined
token_start_line = symbol_undefined
token_start_char = symbol_undefined
token_start_position = symbol_undefined
atom_tag = box(symbol_atom)
pair_tag = box(symbol_pair)
_starreader_generates_annotated_sexps_q_star = True
_staruse_lexical_address_star = True
quote_q_hat = tagged_list_hat(symbol_quote, Equal, 2)
quasiquote_q_hat = tagged_list_hat(symbol_quasiquote, Equal, 2)
unquote_q_hat = tagged_list_hat(symbol_unquote, GreaterThanEqual, 2)
unquote_splicing_q_hat = tagged_list_hat(symbol_unquote_splicing, GreaterThanEqual, 2)
if_then_q_hat = tagged_list_hat(symbol_if, Equal, 3)
if_else_q_hat = tagged_list_hat(symbol_if, Equal, 4)
assignment_q_hat = tagged_list_hat(symbol_set_b, Equal, 3)
func_q_hat = tagged_list_hat(symbol_func, Equal, 2)
callback0_q_hat = tagged_list_hat(symbol_callback0, Equal, 2)
callback1_q_hat = tagged_list_hat(symbol_callback1, Equal, 2)
callback2_q_hat = tagged_list_hat(symbol_callback2, Equal, 2)
define_q_hat = tagged_list_hat(symbol_define, GreaterThanEqual, 3)
define_b_q_hat = tagged_list_hat(symbol_define_b, GreaterThanEqual, 3)
define_syntax_q_hat = tagged_list_hat(symbol_define_syntax, GreaterThanEqual, 3)
begin_q_hat = tagged_list_hat(symbol_begin, GreaterThanEqual, 2)
lambda_q_hat = tagged_list_hat(symbol_lambda, GreaterThanEqual, 3)
trace_lambda_q_hat = tagged_list_hat(symbol_trace_lambda, GreaterThanEqual, 4)
raise_q_hat = tagged_list_hat(symbol_raise, Equal, 2)
choose_q_hat = tagged_list_hat(symbol_choose, GreaterThanEqual, 1)
try_q_hat = tagged_list_hat(symbol_try, GreaterThanEqual, 2)
catch_q_hat = tagged_list_hat(symbol_catch, GreaterThanEqual, 3)
finally_q_hat = tagged_list_hat(symbol_finally, GreaterThanEqual, 2)
let_transformer_hat = make_macro(b_macro_1_d)
letrec_transformer_hat = make_macro(b_macro_2_d)
mit_define_transformer_hat = make_macro(b_macro_3_d)
and_transformer_hat = make_macro(b_macro_4_d)
or_transformer_hat = make_macro(b_macro_5_d)
cond_transformer_hat = make_macro(b_macro_6_d)
let_star_transformer_hat = make_macro(b_macro_7_d)
case_transformer_hat = make_macro(b_macro_8_d)
record_case_transformer_hat = make_macro(b_macro_9_d)
define_datatype_transformer_hat = make_macro(b_macro_10_d)
cases_transformer_hat = make_macro(b_macro_11_d)
macro_env = make_macro_env_hat()
REP_k = make_cont2(b_cont2_49_d)
REP_handler = make_handler2(b_handler2_2_d)
REP_fail = make_fail(b_fail_1_d)
_starlast_fail_star = REP_fail
_startokens_left_star = symbol_undefined
try_parse_handler = make_handler2(b_handler2_3_d)
_startracing_on_q_star = False
_starstack_trace_star = List(symbol_emptylist)
_staruse_stack_trace_star = True
void_prim = make_proc(b_proc_5_d)
zero_q_prim = make_proc(b_proc_6_d)
exit_prim = make_proc(b_proc_7_d)
end_of_session = List(symbol_exiting, symbol_the, symbol_interpreter)
eval_prim = make_proc(b_proc_8_d)
eval_ast_prim = make_proc(b_proc_9_d)
parse_prim = make_proc(b_proc_10_d)
string_length_prim = make_proc(b_proc_11_d)
string_ref_prim = make_proc(b_proc_12_d)
unparse_prim = make_proc(b_proc_13_d)
unparse_procedure_prim = make_proc(b_proc_14_d)
parse_string_prim = make_proc(b_proc_15_d)
read_string_prim = make_proc(b_proc_16_d)
apply_prim = make_proc(b_proc_17_d)
sqrt_prim = make_proc(b_proc_18_d)
odd_q_prim = make_proc(b_proc_19_d)
even_q_prim = make_proc(b_proc_20_d)
quotient_prim = make_proc(b_proc_21_d)
remainder_prim = make_proc(b_proc_22_d)
print_prim = make_proc(b_proc_23_d)
string_prim = make_proc(b_proc_24_d)
substring_prim = make_proc(b_proc_25_d)
number_to_string_prim = make_proc(b_proc_26_d)
assv_prim = make_proc(b_proc_27_d)
memv_prim = make_proc(b_proc_28_d)
display_prim = make_proc(b_proc_29_d)
newline_prim = make_proc(b_proc_30_d)
_starneed_newline_star = False
load_prim = make_proc(b_proc_31_d)
load_stack = symbol_emptylist
length_prim = make_proc(b_proc_32_d)
symbol_q_prim = make_proc(b_proc_33_d)
number_q_prim = make_proc(b_proc_34_d)
boolean_q_prim = make_proc(b_proc_35_d)
string_q_prim = make_proc(b_proc_36_d)
char_q_prim = make_proc(b_proc_37_d)
char_is__q_prim = make_proc(b_proc_38_d)
char_whitespace_q_prim = make_proc(b_proc_39_d)
char_to_integer_prim = make_proc(b_proc_40_d)
integer_to_char_prim = make_proc(b_proc_41_d)
char_alphabetic_q_prim = make_proc(b_proc_42_d)
char_numeric_q_prim = make_proc(b_proc_43_d)
null_q_prim = make_proc(b_proc_44_d)
pair_q_prim = make_proc(b_proc_45_d)
cons_prim = make_proc(b_proc_46_d)
car_prim = make_proc(b_proc_47_d)
cdr_prim = make_proc(b_proc_48_d)
cadr_prim = make_proc(b_proc_49_d)
caddr_prim = make_proc(b_proc_50_d)
list_prim = make_proc(b_proc_51_d)
make_set_prim = make_proc(b_proc_52_d)
plus_prim = make_proc(b_proc_53_d)
minus_prim = make_proc(b_proc_54_d)
times_prim = make_proc(b_proc_55_d)
divide_prim = make_proc(b_proc_56_d)
modulo_prim = make_proc(b_proc_57_d)
lt_prim = make_proc(b_proc_58_d)
gt_prim = make_proc(b_proc_59_d)
lt_or_eq_prim = make_proc(b_proc_60_d)
gt_or_eq_prim = make_proc(b_proc_61_d)
equal_sign_prim = make_proc(b_proc_62_d)
abs_prim = make_proc(b_proc_63_d)
equal_q_prim = make_proc(b_proc_64_d)
eq_q_prim = make_proc(b_proc_65_d)
memq_prim = make_proc(b_proc_66_d)
member_prim = make_proc(b_proc_67_d)
range_prim = make_proc(b_proc_68_d)
snoc_prim = make_proc(b_proc_69_d)
rac_prim = make_proc(b_proc_70_d)
rdc_prim = make_proc(b_proc_71_d)
set_car_b_prim = make_proc(b_proc_72_d)
set_cdr_b_prim = make_proc(b_proc_73_d)
import_prim = make_proc(b_proc_74_d)
get_stack_trace_prim = make_proc(b_proc_75_d)
get_prim = make_proc(b_proc_76_d)
call_cc_prim = make_proc(b_proc_78_d)
abort_prim = make_proc(b_proc_79_d)
require_prim = make_proc(b_proc_80_d)
cut_prim = make_proc(b_proc_81_d)
reverse_prim = make_proc(b_proc_82_d)
append_prim = make_proc(b_proc_83_d)
string_to_number_prim = make_proc(b_proc_84_d)
string_is__q_prim = make_proc(b_proc_85_d)
list_to_vector_prim = make_proc(b_proc_86_d)
list_to_string_prim = make_proc(b_proc_87_d)
dir_prim = make_proc(b_proc_88_d)
current_time_prim = make_proc(b_proc_89_d)
map_prim = make_proc(b_proc_90_d)
for_each_prim = make_proc(b_proc_91_d)
format_prim = make_proc(b_proc_92_d)
current_environment_prim = make_proc(b_proc_93_d)
using_prim = make_proc(b_proc_94_d)
not_prim = make_proc(b_proc_95_d)
printf_prim = make_proc(b_proc_96_d)
vector_prim = make_proc(b_proc_97_d)
vector_set_b_prim = make_proc(b_proc_98_d)
vector_ref_prim = make_proc(b_proc_99_d)
make_vector_prim = make_proc(b_proc_100_d)
error_prim = make_proc(b_proc_101_d)
list_ref_prim = make_proc(b_proc_102_d)
current_directory_prim = make_proc(b_proc_103_d)
round_prim = make_proc(b_proc_104_d)
set_use_stack_trace_b_prim = make_proc(b_proc_105_d)
toplevel_env = make_toplevel_env()
pc_halt_signal = False
def run(setup, *args):
    args = List(*args)
    Apply(setup, args)
    return trampoline()


if __name__ == '__main__':
    print('Calico Scheme, version 3.0.0')
    print('----------------------------')
    print('Use (exit) to exit')
    start_rm()

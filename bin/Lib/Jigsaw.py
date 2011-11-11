from System.Math import (Abs, Acos, Asin, Atan, Ceiling, Cos,
                         Exp, Floor, IEEERemainder, Log, Log10,
                         Max, Min, Pow, Round, Sign, Sin, Sqrt,
                         Tan, Truncate)

def In(item, *args):
    # Convert in(a, ...) => In(a, ...)
    return item in args

def If(boolean, ifexp, elseexp):
    # Convert if(a, b, c) => If(a, lambda: b, lambda: c)
    return ifexp() if boolean else elseexp()

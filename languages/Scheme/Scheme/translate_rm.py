from __future__ import division, print_function

class Translator:
    def __init__(self):
        self.program = []
        self.symbols = ["()"]

    def to_ignore(self):
        # FIXME: get rid of these by marking them as native:
        return [
            "void-value", "restart-rm", "raw-read-line", "trampoline",
            "read-content", 
            "string->integer", "string->decimal", "string->rational", 
            "string-split", 
            "get-current-time", "type?", 
            "execute", "execute-loop", "execute-string", "execute-file",
            "read-eval-print-loop", "unparse", "unparse-exps", "qq-expand-cps_",
            "qq-expand-list-cps_", 
            ## defined in Scheme.cs:
            "true?", "safe-print", "make-safe",
            "init-cont", "init-cont2", "init-cont3", "init-cont4",
            "init-handler", "init-handler2", "init-fail",
            "make-cont", "make-cont2", "make-cont3", "make-cont4", "make-macro", "make-proc",
            "make-fail", "make-handler", "make-handler2"
        ]

    def parse(self, text):
        self.program = self.parser(self.lexer(text))

    def parse_file(self, filename):
        text = open(filename).read()
        self.program = self.parser(self.lexer(text))

    def function_q(self, expr):
        if len(expr) > 2:
            if isinstance(expr[2], list):
                if expr[0] in ["define", "define+", "define*"] and len(expr[2]) > 0:
                    if expr[2][0] == "lambda":
                        return True
                    elif isinstance(expr[1], list):
                        return True
        return False

    def fix_name(self, name):
        if name == "()":
            return "emptylist"
        elif name == "def":
            return "def_"
        elif name == "input":
            return "input_"
        elif name == "class":
            return "class_"
        elif name == "list":
            return "List"
        elif name == "apply":
            return "Apply"
        elif name == "map":
            return "Map"
        elif name == "=":
            return "Equal"
        elif name == "<=":
            return "LessThanEqual"
        elif name == ">=":
            return "GreaterThanEqual"
        elif name == "read":
            return "raw_input"
        elif name.startswith('"'):
            return name
        elif name.startswith("#"):
            return self.replace_char(name)
        elif name.startswith("<") and name.endswith(">"):
            name = name[1:-1]
        for pattern in [("->", "_to_"), (">", "to_"), ("<", "LessThan"), ("*", "_star"),
                        ("=", "_is_"), ("-", "_"), ("?", "_q"), ("!", "_b"), ("/", "slash"),
                        (".", "dot"), ("+", "plus"), ("%" "percent"), ("^", "_hat")]:
            name = name.replace(pattern[0], pattern[1])
        return name

    def make_symbol_name(self, symbol):
        if symbol not in self.symbols:
            self.symbols.append(symbol)
        return "symbol_" + self.fix_name(symbol)

    def lexer(self, text):
        """
        Lexer for Scheme code. Takes a string of Scheme code and breaks it
        up into a flat list of the important parts.
    
        > lexer("(define x (lambda (x) (cond ((test? x) (func x)) (else return))))"))
        ['(', 'define', 'x', '(', 'lambda', '(', 'x', ')', '(', 'cond', '(', '(', 'test?',
         'x', ')', '(', 'func', 'x', ')', ')', '(', 'else', 'return', ')', ')', ')', ')']
        """
        state = None
        current = ""
        retval = []
        i = 0
        while i < len(text):
            c = text[i]
            if state == "in-comment":
                if c == "\n":
                    state = None
                # else, ignore
            elif state == "in-string":
                if c == '"':
                    state = None
                    current += c
                    retval.append(current)
                    current = ""
                else:
                    current += c
            else:
                if c in ["(", ")", "'"]:
                    if current:
                        retval.append(current)
                        current = ""
                    retval.append(c)
                elif c == "#":
                    if current:
                        retval.append(current)
                        current = ""
                    i += 1
                    if text[i] != "\\":
                        current = "#" + text[i]
                    else:
                        i += 1
                        current = "#\\" + text[i]
                        i += 1
                        while (not text[i] in [' ', ')']) and i < len(text):
                            current += text[i]
                            i += 1
                        i -= 1
                elif c == ";":
                    if current:
                        retval.append(current)
                        current = ""
                    state = "in-comment"
                elif c == '"':
                    if current:
                        retval.append(current)
                        current = ""
                    current = '"'
                    state = "in-string"
                elif c in [" ", "\n"]:
                    if current:
                        retval.append(current)
                        current = ""
                else:
                    current += c
            i += 1
        if current:
            retval.append(current)
        return retval

    def replace_char(self, name):
        if name == "#t":
            return "True"
        elif name == "#f":
            return "False"
        elif name == "#\\newline":
            return "'\\n'"
        elif name == "#\\nul":
            return "'\\0'"
        elif name == "#\\return":
            return "'\\r'"
        elif name == "#\\space":
            return "' '"
        elif name == "#\\tab":
            return "'\\t'"
        elif name == "#\\backspace":
            return "'\\b'"
        elif name == "#\\page":
            return "u\"\\u000C\""
        elif name == "#\\'":
            return "\"'\""
        elif name == "#\\\\":
            return "'\\\\'"
        elif len(name) == 3:
            return "'%s'" % name[2]
        else:
            raise Exception("unknown char: " + name)

    def parser(self, lexed):
        """
        Take a lexed list and parse it into a tree of s-expressions represented
        as lists. Side-effect: collects and replaces symbols.
        
        > parser(lexer("(define x (lambda (x) (cond ((test? x) (func x)) (else return))))"))
        [['define', 'x', ['lambda', ['x'], ['cond', [['test?', 'x'], 
                                                     ['func', 'x']], ['else', 'return']]]]]
        """
        retval = []
        stack = []
        current = None
        i = 0
        while i < len(lexed):
            item = lexed[i]
            if item == "(": ## (define x 1)   ((test? 1) 1)
                if current is not None:
                    stack.append(current)
                current = []
            elif item == ")":
                if len(stack) > 0:
                    temp = stack.pop()
                    if current is not None:
                        temp.append(current)
                    current = temp
                else:
                    if current is not None:
                        retval.append(current)
                    current = None
            elif item == "'": ## quoted
                if i + 1 < len(lexed):
                    if lexed[i + 1] != "(":
                        i += 1
                        current.append(self.make_symbol_name(lexed[i]))
                    else:
                        i += 2
                        current.append("symbol_emptylist")
                else: # same as any item
                    current.append(item)
            else:
                current.append(item)
            i += 1
        if current:
            retval.append(current)
        if stack:
            raise Exception("stack:", stack)
        return retval

class PythonTranslator(Translator):
    def preamble(self):
        print("""####################################################
## Scheme in Python
##
##
##
####################################################

""")
        print(file("Scheme.py").read())

    def process_function_definition(self, expr, locals, indent):
        ## (define x (lambda (x) (cond ((test? x) (func x)) (else return))))
        convert_args = ""
        if isinstance(expr[1], list):
            function_name = self.fix_name(expr[1][0])
            args = self.fix_name(expr[1][1])
        else:
            function_name = self.fix_name(expr[1])
            if isinstance(expr[2][1], list):
                args = ", ".join(map(self.fix_name, expr[2][1]))
            else:
                args = "*%s" % self.fix_name(expr[2][1]) # var args
                convert_args = "%s = List(*%s)" % (self.fix_name(expr[2][1]), 
                                                   self.fix_name(expr[2][1]))
        if ", dot, " in args:
            args = args.replace(", dot, ", ", *") # var args on end
            var_arg = args.rsplit("*", 1)[1]
            convert_args = "%s = List(*%s)" % (var_arg, var_arg)
        print("def %s(%s):" % (function_name, args))
        if convert_args:
            print(" " * (indent + 4), end="")
            print(convert_args)
        body = expr[2][2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        print()

    def process_infix_op(self, expr):
        retval = "(%s)" % self.process_app(expr[1])
        for e in expr[2:]:
            retval += " %s (%s)" % (expr[0], self.process_app(e))
        return retval

    def process_app(self, expr):
        if isinstance(expr, list):
            if expr[0] in ['and', 'or', '+', '-', '*']:
                return self.process_infix_op(expr)
            if expr[0] == 'if':
                return "(%s if %s else %s)" % (self.process_app(expr[2]),
                                               self.process_app(expr[1]),
                                               self.process_app(expr[3]))
            else:
                ## function call:
                return "%s(%s)" % (self.fix_name(expr[0]),
                                   ", ".join([self.process_app(e) for e in expr[1:]]))
                
        else:
            return self.fix_name(expr)

    def process_let(self, expr, locals, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            print(" " * indent, end="")
            # locals:
            print("%s = %s" % (self.fix_name(pair[0]), self.process_app(pair[1])))
        locals.extend([pair[0] for pair in expr[1]])
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent)

    def process_if(self, expr, locals, indent):
        ## (if 1 2 3)
        print(" " * indent, end="")
        print("if %s:" % self.process_app(expr[1]))
        self.process_statement(expr[2], locals, indent + 4)
        if len(expr) > 3:
            print(" " * indent, end="")
            print("else:")
            self.process_statement(expr[3], locals, indent + 4)

    def get_define_name(self, expr):
        ## (define function ...)
        return expr[1]

    def check_global(self, name, locals):
        if name.startswith("temp_") or name in locals:
            return self.fix_name(name)
        else:
            return "globals()['%s']" % self.fix_name(name)

    def process_assignment(self, expr, locals, indent):
        print(" " * indent, end="")
        print("%s = %s" % (self.check_global(expr[1], locals), 
                           self.process_app(expr[2])))

    def process_return(self, expr, indent):
        print(" " * indent, end="")
        print("return %s" % self.process_app(expr[1]))

    def process_definition(self, expr, locals, indent):
        print(" " * indent, end="")
        # global
        print("%s = %s" % (self.fix_name(expr[1]), self.process_app(expr[2])))

    def process_cond(self, expr, locals, indent):
        ## (cond (test result) ...)
        print(" " * indent, end="")
        print("if %s:" % self.process_app(expr[1][0]))
        self.process_statement(expr[1][1], locals, indent + 4)
        for rest in expr[2:]:
            print(" " * indent, end="")
            if rest[0] == "else":
                print("else:")
            else:
                print("elif %s:" % self.process_app(rest[0]))
            self.process_statement(rest[1], locals, indent + 4)

    def process_statement(self, expr, locals, indent):
        if self.function_q(expr):
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_function_definition(expr, locals, indent)
        elif expr[0] == "define": # global variable
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_definition(expr, locals, indent)
        elif expr[0] == "define-native":
            pass
        elif expr[0] == "let":
            self.process_let(expr, locals, indent)
        elif expr[0] == "if":
            self.process_if(expr, locals, indent)
        elif expr[0] == "cond":
            self.process_cond(expr, locals, indent)
        elif expr[0] == "load":
            pass
        elif expr[0] == "set!":
            self.process_assignment(expr, locals, indent)
        elif expr[0] == "begin":
            for e in expr[1:]:
                self.process_statement(e, locals, indent)
        elif expr[0] == "return*":
            self.process_return(expr, indent)
        else: # must be a function call
            print(" " * indent, end="")
            print(self.process_app(expr))
                
    def translate(self):
        self.preamble()
        for symbol in self.symbols:
            print("%s = Symbol(\"%s\")" % (self.make_symbol_name(symbol), symbol))
        print()
        for statement in self.program:
            self.process_statement(statement, [], 0)

if __name__ == "__main__":
    import sys
    pt = PythonTranslator()
    pt.parse_file(sys.argv[1])
    pt.translate()

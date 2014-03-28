from __future__ import division, print_function

class Translator:
    def __init__(self):
        self.program = []
        self.symbols = []

    def functions_to_ignore(self):
        return [
            "void-value", "start-rm", "restart-rm", "read-eval-print-loop-rm",
            "read-line", "raw-read-line", "Main", "read-content", "string->integer",
            "string->decimal", "string->rational", "tagged-list", "testall", 
            "string-split", "get-current-time", "type?", "make-initial-env-extended",
            "make-proc", "execute", "execute-loop", "execute-string", "execute-file",
            "read-eval-print-loop", "unparse", "unparse-exps", "qq-expand-cps_",
            "qq-expand-list-cps_", "init-cont", "init-cont2", "init-cont3", "init-cont4",
            "init-handler", "init-handler2", "init-fail",
            ## defined in Scheme.cs:
            "true?", "atom-tag", "pair-tag", "annotated?", "safe-print", "make-safe",
            ##make-cont make-cont2 make-cont3 make-cont4 make-macro make-proc
            ##make-fail make-handler make-handler2
        ]

    def parse(self, text):
        self.program = self.parser(self.lexer(text))

    def parse_file(self, filename):
        text = open(filename).read()
        self.program = self.parser(self.lexer(text))

    def function_q(self, expr):
        if len(expr) > 2:
            if isinstance(expr[2], list):
                if len(expr[2]) > 0:
                    return expr[2][0] == "lambda"
        return False

    def fix_name(self, name):
        if name.startswith("#"):
            return self.replace_char(name)
        if name.startswith("<") and name.endswith(">"):
            name = name[1:-1]
        for pattern in [("->", "_to_"), (">", "to_"), ("<", "LessThan"), ("*", "_star"),
                        ("=", "_is_"), ("-", "_"), ("?", "_q"), ("!", "_b"), ("/", "slash"),
                        (".", "dot"), ("+", "plus"), ("%" "percent"), ("^" "_hat")]:
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
            return "''"
        elif name == "#\\return":
            return "'\\r'"
        elif name == "#\\space":
            return "' '"
        elif name == "#\\tab":
            return "'\\t'"
        elif name == "#\\backspace":
            return "'\\b'"
        elif name == "#\\page":
            return chr(12)
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
                        current.append(item)
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
        print("""
class Symbol:
    def __init__(self, name):
        self.name = name
""")

    def process_function_definition(self, expr, indent):
        ## (define x (lambda (x) (cond ((test? x) (func x)) (else return))))
        function_name = self.fix_name(expr[1])
        if isinstance(expr[2][1], list):
            args = ", ".join(map(self.fix_name, expr[2][1]))
        else:
            args = "*%s" % self.fix_name(expr[2][1]) # var args
        if ", dot," in args:
            args.replace(", dot,", ", *") # var args on end
        print("def %s(%s):" % (function_name, args))
        body = expr[2][2]
        if isinstance(body[0], list):
            for statement in body:
                self.process_statement(statement, indent + 4)
        else:
            self.process_statement(body, indent + 4)
        print()

    def process_bool(self, expr):
        if isinstance(expr, list):
            if expr[0] in ['and', 'or']:
                return "(%s %s %s)" % (self.process_bool(expr[1]),
                                       expr[0],
                                       self.process_bool(expr[2]))
            else:
                ## function call:
                return "%s(%s)" % (self.fix_name(expr[0]),
                                   ", ".join([self.process_bool(e) for e in expr[1:]]))
                
        else:
            return self.fix_name(expr)

    def process_let(self, expr, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            print(" " * indent, end="")
            print("%s = %s" % (self.fix_name(pair[0]), pair[1]))
        body = expr[2]
        if isinstance(body[0], list):
            for statement in body:
                self.process_statement(statement, indent)
        else:
            self.process_statement(body, indent)

    def process_if(self, expr, indent):
        ## (if 1 2 3)
        print(" " * indent, end="")
        print("if %s:" % self.process_bool(expr[1]))
        self.process_statement(expr[2], indent + 4)
        if len(expr) > 3:
            print(" " * indent, end="")
            print("else:")
            self.process_statement(expr[3], indent + 4)

    def get_function_name(self, expr):
        ## (define function ...)
        return expr[1]

    def process_assignment(self, expr, indent):
        print(" " * indent, end="")
        print("%s = %s" % (self.fix_name(expr[1]), self.process_bool(expr[2])))

    def process_return(self, expr, indent):
        print(" " * indent, end="")
        print("return %s" % self.process_bool(expr[1]))

    def process_definition(self, expr, indent):
        print(" " * indent, end="")
        print("%s = %s" % (self.fix_name(expr[1]), self.process_bool(expr[2])))

    def process_statement(self, expr, indent):
        if self.function_q(expr):
            if not self.get_function_name(expr) in self.functions_to_ignore():
                self.process_function_definition(expr, indent)
        elif expr[0] == "define":
            self.process_definition(expr, indent)
        elif expr[0] == "let":
            self.process_let(expr, indent)
        elif expr[0] == "if":
            self.process_if(expr, indent)
        elif expr[0] == "load":
            pass
        elif expr[0] == "set!":
            self.process_assignment(expr, indent)
        elif expr[0] == "begin":
            for e in expr[1:]:
                self.process_statement(e, indent)
        elif expr[0] == "return*":
            self.process_return(expr, indent)
        else:
            print(" " * indent, end="")
            print(expr)

    def translate(self):
        self.preamble()
        for symbol in self.symbols:
            print("%s = Symbol(\"%s\")" % (self.make_symbol_name(symbol), symbol))
        print()
        for statement in self.program:
            self.process_statement(statement, 0)

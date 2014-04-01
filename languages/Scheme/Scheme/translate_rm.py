from __future__ import division, print_function

class Translator(object):
    def __init__(self):
        self.program = []
        self.symbols = ["()"]

    def Print(self, indent, *args, **kwargs):
        kwargs["file"] = self.fp
        print(" " * indent, end="", file=self.fp)
        print(*args, **kwargs)

    def to_ignore(self):
        # FIXME: get rid of these by marking them as native:
        return [
            "void-value", "restart-rm", "raw-read-line", "trampoline",
            "read-content", 
            "string->integer", "string->decimal", "string->rational", 
            "string-split", 
            "get-current-time", "type?", 
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

    def fix_symbol_name(self, name):
        # name used in symbol_NAME
        return self.fix_name(name)

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
        elif name == "range":
            return "Range"
        elif name == "map":
            return "Map"
        elif name == "=":
            return "Equal"
        elif name == "<=":
            return "LessThanEqual"
        elif name == "<":
            return "LessThan"
        elif name == ">":
            return "GreaterThan"
        elif name == ">=":
            return "GreaterThanEqual"
        elif name == "read":
            return "raw_input"
        elif name == "-":
            return "minus"
        elif name == "*":
            return "multiply"
        elif name == "+":
            return "plus"
        elif name.startswith('"'):
            return name
        elif name.startswith("#"):
            return self.replace_char(name)
        elif name.startswith("<") and name.endswith(">"):
            name = "b_" + name[1:-1] + "_d"
        for pattern in [("->", "_to_"), (">", "to_"), ("<", "LessThan"), ("*", "_star"),
                        ("=", "_is_"), ("-", "_"), ("?", "_q"), ("!", "_b"), ("/", "slash"),
                        (".", "dot"), ("+", "plus"), ("%" "percent"), ("^", "_hat")]:
            name = name.replace(pattern[0], pattern[1])
        return name

    def make_symbol_name(self, symbol):
        if symbol not in self.symbols:
            self.symbols.append(symbol)
        return "symbol_" + self.fix_symbol_name(symbol)

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
        self.Print(0, """####################################################
## Scheme in Python
##
## Jim Marshall
## Doug Blank
####################################################

""")
        self.Print(0, file("Scheme.py").read())

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
        self.Print(indent, "def %s(%s):" % (function_name, args))
        if convert_args:
            self.Print(indent + 4, convert_args)
        body = expr[2][2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "")

    def process_infix_op(self, expr, op):
        retval = "(%s)" % self.process_app(expr[1])
        for e in expr[2:]:
            retval += " %s (%s)" % (op, self.process_app(e))
        return retval

    def process_app(self, expr):
        if isinstance(expr, list):
            if expr[0] in ['and', 'or', '+', '-', '*']:
                return self.process_infix_op(expr, expr[0])
            elif expr[0] == "eq?":
                return self.process_infix_op(expr, "is")
            elif expr[0] == 'if':
                return "(%s if %s else %s)" % (self.process_app(expr[2]),
                                               self.process_app(expr[1]),
                                               self.process_app(expr[3]))
            else:
                ## function call:
                return "%s(%s)" % (self.fix_name(expr[0]),
                                   ", ".join([self.process_app(e) for e in expr[1:]]))
        else:
            return self.fix_name(expr)

    def process_while(self, expr, locals, indent):
        # (while test body...)
        self.Print(indent, "while %s:" % self.process_app(expr[1]))
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)

    def process_let(self, expr, locals, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            # locals:
            self.Print(indent, "%s = %s" % (self.fix_name(pair[0]), self.process_app(pair[1])))
        locals.extend([pair[0] for pair in expr[1]])
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent)

    def process_if(self, expr, locals, indent):
        ## (if 1 2 3)
        self.Print(indent, "if %s:" % self.process_app(expr[1]))
        self.process_statement(expr[2], locals, indent + 4)
        if len(expr) > 3:
            self.Print(indent, "else:")
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
        self.Print(indent, "%s = %s" % (self.check_global(expr[1], locals), 
                           self.process_app(expr[2])))

    def process_return(self, expr, indent):
        self.Print(indent, "return %s" % self.process_app(expr[1]))

    def process_definition(self, expr, locals, indent):
        # global
        self.Print(indent, "%s = %s" % (self.fix_name(expr[1]), self.process_app(expr[2])))

    def process_cond(self, expr, locals, indent):
        ## (cond (test result) ...)
        self.Print(indent, "if %s:" % self.process_app(expr[1][0]))
        self.process_statement(expr[1][1], locals, indent + 4)
        for rest in expr[2:]:
            if rest[0] == "else":
                self.Print(indent, "else:")
            else:
                self.Print(indent, "elif %s:" % self.process_app(rest[0]))
            self.process_statement(rest[1], locals, indent + 4)

    def process_statement(self, expr, locals, indent):
        if self.function_q(expr):
            # handles all define/*/+ functions
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
        elif expr[0] == "while":
            self.process_while(expr, locals, indent)
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
            self.Print(indent, self.process_app(expr))
                
    def translate(self, filename):
        self.fp = open(filename, "w")
        self.preamble()
        for symbol in self.symbols:
            self.Print(0, "%s = make_symbol(\"%s\")" % (self.make_symbol_name(symbol), symbol))
        self.Print(0, "")
        for statement in self.program:
            self.process_statement(statement, [], 0)
        self.Print(0, "")
        self.Print(0, "if __name__ == '__main__':")
        self.Print(0, "    start_rm()")

class CSharpTranslator(Translator):
    def preamble(self):
        self.Print(0, """// -------------------------------------------------
/*
 Scheme in C#

 Jim Marshall
 Doug Blank
*/
// -------------------------------------------------

#pragma warning disable 109
using System;

public class PJScheme:Scheme
{

  static object void_value = null;

  new public static object trampoline () {
	while (pc != null) {
            try {
	        pc ();
	    } catch (Exception e ) {
                if (config.DEBUG > 0) {
                    exception_reg = e.ToString();
                } else {
                    string [] parts = get_parts(e.ToString(), NEWLINE_STRING);
		    exception_reg = format(\"{0}\", parts[0]);
                }
		pc = (Function) apply_handler2;
	    }
	}
	return (final_reg);
  }

  public static Closure dlr_func(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return delegate (object[] args) { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list ((object) args);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object> callback0(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return () => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list ();
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object,object> callback1(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return (object arg) => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object,object,object> callback2(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return (object arg1, object arg2) => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg1, arg2);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static object apply_comparison_rm(object schemeProc, object arg1, object arg2) {
      // used from non-pcs code that evaluates a scheme proc in sort
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg1, arg2);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
  }

   static int _closure_depth = 0;
   static bool _trace_pause = false;

   public static bool get_trace_pause () {
      return _trace_pause;
   }

   public static void set_trace_pause (bool value) {
      _trace_pause = value;
   }

   public static int get_closure_depth ()
   {
      return _closure_depth;
   }

   public static void increment_closure_depth ()
   {
      _closure_depth++;

   }

   public static void decrement_closure_depth ()
   {
      _closure_depth--;
   }

   public static object repeat(object item, object times) {
      object retval = EmptyList;
      for (int i=0; i < ((int)times); i++) {
          retval = cons(item, retval);
      }
      return retval;
   }

   public static bool use_lexical_address(object value) {
	if (!null_q(value)) {
	    value = car(value);
	    _staruse_lexical_address_star = true_q(value);
	}
	return _staruse_lexical_address_star;
   }

   // *tracing-on?*
   public static object tracing_on(object value) {
	if (null_q(value)) {
	    return _startracing_on_q_star;
	} else {
	    value = car(value);
	    _startracing_on_q_star = (bool)value;
	    return null;
	}
   }

""")

    def contains_return(self, statements):
        if statements == []:
            return False
        for statement in statements:
            if isinstance(statement, list):
                if self.contains_return(statement):
                    return True
            elif statement == "return*":
                return True
        return False

    def process_function_definition(self, expr, locals, indent):
        ## (define x (lambda (x) (cond ((test? x) (func x)) (else return))))
        convert_args = ""
        if self.contains_return(expr[2][2:]):
            if "?" in expr[1]:
                return_type = "bool"
            else:
                return_type = "object"
        else:
            return_type = "void"
        if isinstance(expr[1], list):
            function_name = self.fix_name(expr[1][0])
            args = self.fix_name(expr[1][1])
        else:
            function_name = self.fix_name(expr[1])
            if isinstance(expr[2][1], list):
                args = ", ".join(map(self.fix_name, expr[2][1]))
            else:
                args = "params object [] %s" % self.fix_name(expr[2][1]) # var args
                convert_args = "%s = vList(%s);" % (self.fix_name(expr[2][1]), 
                                                   self.fix_name(expr[2][1]))
        if ", dot, " in args:
            args = args.replace(", dot, ", ", params object [] ") # var args on end
            var_arg = args.rsplit("[] ", 1)[1]
            convert_args = "%s = vList(%s);" % (var_arg, var_arg)
        self.Print(indent, "new public static %s %s(%s) {" % (return_type, function_name, self.make_arg_types(args)))
        if convert_args:
            self.Print(indent + 4, convert_args)
        body = expr[2][2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "}")
        self.Print(indent, "")

    def make_arg_types(self, string):
        retval = ""
        if string:
            args = string.split(", ")
            for arg in args:
                if retval:
                    retval += ", "
                if " " in arg:
                    retval += arg
                else:
                    retval += "object " + arg
        return retval

    def process_infix_op(self, expr, op):
        retval = "(%s)" % self.process_app(expr[1])
        for e in expr[2:]:
            retval += " %s (%s)" % (op, self.process_app(e))
        return retval

    def process_app(self, expr):
        if isinstance(expr, list):
            if expr[0] in ['+', '-', '*']:
                return self.process_infix_op(expr, expr[0])
            #elif expr[0] == "eq?":
            #    return self.process_infix_op(expr, "is")
            elif expr[0] == "and":
                return self.process_infix_op(expr, "&&")
            elif expr[0] == "or":
                return self.process_infix_op(expr, "||")
            elif expr[0] == 'if':
                # if 1 2 3
                return "(%s ? %s : %s)" % (self.process_app(expr[1]),
                                           self.process_app(expr[2]),
                                           self.process_app(expr[3]))
            else:
                ## function call:
                if expr[0] == "not":
                    return "%s true_q(%s)" % ("!",
                                              ", ".join([self.process_app(e) for e in expr[1:]]))
                else:
                    return "%s(%s)" % (self.fix_name(expr[0]),
                                       ", ".join([self.process_app(e) for e in expr[1:]]))
        else:
            return self.fix_name(expr)

    def process_while(self, expr, locals, indent):
        # (while test body...)
        self.Print(indent, "while (true_q(%s)) {" % self.process_app(expr[1]))
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "}")

    def process_let(self, expr, locals, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            # locals:
            self.Print(indent, "object %s = %s;" % (self.fix_name(pair[0]), self.process_app(pair[1])))
        locals.extend([pair[0] for pair in expr[1]])
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent)

    def process_if(self, expr, locals, indent):
        ## (if 1 2 3)
        self.Print(indent, "if (true_q(%s)) {" % self.process_app(expr[1]))
        self.process_statement(expr[2], locals, indent + 4)
        if len(expr) > 3:
            self.Print(indent, "} else {")
            self.process_statement(expr[3], locals, indent + 4)
            self.Print(indent, "}")
        else:
            self.Print(indent, "}")

    def get_define_name(self, expr):
        ## (define function ...)
        return expr[1]

    def check_global(self, name, locals):
        #if name.startswith("temp_") or name in locals:
            return self.fix_name(name)

    def process_assignment(self, expr, locals, indent):
        self.Print(indent, "%s = %s;" % (self.check_global(expr[1], locals), 
                           self.process_app(expr[2])))

    def process_return(self, expr, indent):
        self.Print(indent, "return %s;" % self.process_app(expr[1]))

    def process_definition(self, expr, locals, indent):
        # global
        self.Print(indent, "public static object %s = %s;" % (self.fix_name(expr[1]), self.process_app(expr[2])))

    def process_cond(self, expr, locals, indent):
        ## (cond (test result) ...)
        self.Print(indent, "if (true_q(%s)) {" % self.process_app(expr[1][0]))
        self.process_statement(expr[1][1], locals, indent + 4)
        for rest in expr[2:]:
            if rest[0] == "else":
                self.Print(indent, "} else {")
            else:
                self.Print(indent, "} else if (true_q(%s)) {" % self.process_app(rest[0]))
            self.process_statement(rest[1], locals, indent + 4)
            self.Print(indent, "}")

    def process_statement(self, expr, locals, indent):
        if self.function_q(expr):
            # handles all define/*/+ functions
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
        elif expr[0] == "while":
            self.process_while(expr, locals, indent)
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
            self.Print(indent, "%s;" % self.process_app(expr))
                
    def translate(self, filename):
        indent = 0
        self.fp = open(filename, "w")
        self.preamble()
        for symbol in self.symbols:
            self.Print(indent + 4, "public static object %s = make_symbol(\"%s\");" % (self.make_symbol_name(symbol), symbol))
        self.Print(indent + 4, "")
        for statement in self.program:
            self.process_statement(statement, [], indent + 4)
        self.Print(indent + 4, "")
        self.Print(indent + 4, "public static void Main() {")
        self.Print(indent + 8, "start_rm();")
        self.Print(indent + 4, "}")
        self.Print(indent, "}")

    def fix_symbol_name(self, name):
        # name used in symbol_NAME
        if name == "eq?":
            return "eq_q"
        elif (name == "equal?"):
            return "equal_q"
        else:
            return self.fix_name(name)

    def fix_name(self, name):
        if (name == "list"):
            return "vList";
        elif (name == "string"):
            return "string_";
        elif (name == "operator"):
            return "operator_";
        elif (name == "bool"):
            return "bool_";
        elif (name == "char"):
            return "char_";
        elif (name == "eq?"):
            return "Eq";
        elif (name == "equal?"):
            return "Equal";
        else:
            return super(CSharpTranslator, self).fix_name(name)

    def replace_char(self, name):
        if name == "#t":
            return "true"
        elif name == "#f":
            return "false"
        elif name == "#\\page":
            return "'\\f'"
        else:
            return super(CSharpTranslator, self).replace_char(name)

if __name__ == "__main__":
    ## infile outfile
    import sys
    if sys.argv[2].rsplit(".")[1] == "cs":
        pt = CSharpTranslator()
    elif sys.argv[2].rsplit(".")[1] == "py":
        pt = PythonTranslator()
    pt.parse_file(sys.argv[1])
    pt.translate(sys.argv[2])

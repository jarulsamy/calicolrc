from __future__ import print_function

import math
import random
import time
import re 
import sys
import clr

def Compare(tok1, value1, tok2, value2):
    return (tok1 == tok2) and (value1 != None and value1.upper() == value2)

class Tokenizer:
    NUMBER  = "NUMBER"
    STRING  = "STRING"
    IDENT   = "IDENT"
    SPECIAL = "SPECIAL"
    def __init__(self,string):
        self.string = string
        self.cp = 0
        self.value = None
        self.curtoken = None
        self.skip()

    def skip(self):
        oldvalue = self.value
        self.curtoken = None
        self.value = None
        while self.cp<len(self.string) and self.string[self.cp]==' ':
            self.cp += 1
        if self.cp<len(self.string):
            s = self.string[self.cp:]
            num = re.match(r'\d+(\.\d*)?(E[+-]?\d+)?',s)
            if num:
                self.curtoken = Tokenizer.NUMBER
                self.value = float(num.group(0))
                self.cp += len(num.group(0))
            else:
                string = re.match(r'"[^"]*"',s)
                if string:
                    self.curtoken = Tokenizer.STRING
                    self.value = string.group(0)[1:-1]
                    self.cp += len(string.group(0))
                else:
                    ident = re.match(r'[a-zA-Z_][a-zA-Z0-9_\.]*[\$%]?',s)
                    if ident:
                        self.curtoken = Tokenizer.IDENT
                        self.value = ident.group(0)
                        self.cp += len(ident.group(0))
                    else:
                        if re.match(r'<=|>=|<>',s):
                            length = 2
                        else:
                            length = 1
                          
                        self.curtoken = Tokenizer.SPECIAL
                        self.value = s[:length]
                        self.cp += length
        return oldvalue

class BasicInterpreter:
    class Literal:
        def __init__(self,x):
            if isinstance(x,float) and x == int(x):
                x = int(x)
            self.x = x
        def eval(self):
            return self.x
        def __str__(self):
            if isinstance(self.x,str):
                return '"%s"' % self.x
            else:
                return repr(self.x)

    class Variable:
        def __init__(self,vars,x, i):
            self.vars = vars
            self.x = x
            self.i = i
        def eval(self):
            #print("eval of %s" % self.name)
            if self.x in self.vars:
                return self.vars[self.x]
            else:
                try:
                    #print(self.name, self.i.library, self.i.vars)
                    retval = eval(self.x, self.i.library, self.i.vars)
                    return retval
                except:
                    pass
                # Doesn't exist:
                if self.x.endswith('$'):
                    return ""
                else:
                    return 0
        def __str__(self):
            return self.x

    class Neg:
        def __init__(self,x):
            self.x = x
        def eval(self):
            return -self.x.eval()
        def __str__(self):
            return "-("+str(self.x)+")"

    class Not:
        def __init__(self,x):
            self.x = x
        def eval(self):
            if self.x.eval() == 0:
                return -1
            else:
                return 0
        def __str__(self):
            return "NOT("+str(self.x)+")"
    
    class BinOp:
        def __init__(self,a,b,op):
            self.a = a
            self.b = b
            self.op = op
        def eval(self):
            return self.op(self.a.eval(),self.b.eval())
        def __str__(self):
            return "("+str(self.a)+" "+self.op+" "+str(self.b)+")"

    class FCall:
        def __init__(self,f,parms,i):
            self.f = f
            self.parms = parms
            self.i = i
        def run(self):
            return self.eval()
        def eval(self):
            if callable(self.f):
                return self.f(*[x.eval() for x in self.parms])
            else:
                f = eval(self.f, self.i.library, self.i.vars)
                return f(*[x.eval() for x in self.parms])
        def __str__(self):
            return self.f+"("+",".join(map(str,self.parms))+")"

    binops = [
      {'*':   lambda x,y: x*y,
       '/':   lambda x,y: x/float(y),
       'MOD': lambda x,y: int(x)%int(y) },
      {'+':   lambda x,y: x+y,
       '-':   lambda x,y: x-y },
      {'<=':  lambda x,y: x<=y and -1 or 0,
       '<':   lambda x,y: x<y and -1 or 0,
       '>':   lambda x,y: x>y and -1 or 0,
       '>=':  lambda x,y: x>=y and -1 or 0,
       '<>':  lambda x,y: x!=y and -1 or 0,
       '=':   lambda x,y: x==y and -1 or 0 },
      {'AND': lambda x,y: int(x)&int(y) },
      {'OR':  lambda x,y: int(x)|int(y) },
      {'XOR': lambda x,y: int(x)^int(y) }
    ]

    lib_functions = {
      'LEN':    lambda x : len(x),
      'STR$':   lambda x : str(x),
      'LEFT$':  lambda x,y : x[:int(y)],
      'RIGHT$': lambda x,y : x[-int(y):],
      'MID$':   lambda x,y,z=1000000 : x[int(y):int(z)],
      'VAL':    lambda x : float(x),
      'INT':    lambda x : int(x),
      'ABS':    lambda x : abs(x),
      'SIN':    lambda x : math.sin(x),
      'COS':    lambda x : math.cos(x),
      'TAN':    lambda x : math.tan(x),
      'ATN':    lambda x : math.atan(x),
      'SQR':    lambda x : math.sqrt(x),
      'LOG':    lambda x : math.log(x),
      'EXP':    lambda x : math.exp(x),
      'TIME':   lambda x : time.time(),
      'RND':    lambda x : random.random()
    }

    class Print:
        def __init__(self,exprlist,semi):
            self.exprlist = exprlist
            self.semi = semi
        def run(self):
            res = [str(e.eval()) for e in self.exprlist]
            if self.semi:
                print("".join(res), end="")
            else:
                print("".join(res))
        def __str__(self):
            if self.semi:
                return "PRINT "+";".join(map(str,self.exprlist))+";"
            else:
                return "PRINT "+";".join(map(str,self.exprlist))

    class Rem:
        def __init__(self,comment):
            self.comment = comment
        def run(self):
            pass
        def __str__(self):
            return "REM "+self.comment

    class Goto:
        def __init__(self,i,line):
            self.i = i
            self.line = line
        def run(self):
            if self.line in self.i.linenos:
                self.i.ip = self.i.linenos[self.line]
            else:
                raise Exception("Undefined GOTO statement (%i)" % self.line)
        def __str__(self):
            return "GOTO %i" % self.line

    class Assign:
        def __init__(self, vars, var, expr):
            self.vars = vars
            self.var = var
            self.expr = expr
        def run(self):
            #print("assign %s" % self)
            #print(self.expr)
            #print(self.vars)
            self.vars[self.var] = self.expr.eval()
        def __str__(self):
            return self.var+" = "+str(self.expr)

    class List:
        def __init__(self,i):
            self.i = i
        def run(self):
            last = None
            for (l,s) in self.i.program:
                if l == -1:
                    if not isinstance(last,BasicInterpreter.If):
                        print(":", end="")
                else:
                    print()
                    print("%i " % int(l), end="")
                print(str(s), end="")
                last = s
            print()
        def __str__(self):
            return "LIST"

    class Run:
        def __init__(self,i):
            self.i = i
        def run(self):
            self.i.vars.clear()
            self.i.gstack = []
            self.i.fstack = []
            self.i.ip = 0
        def __str__(self):
            return "RUN"

    class Import:
        def __init__(self, i, library_name):
            self.i = i
            self.library_name = library_name
        def run(self):
            sname = self.library_name.eval()
            try:
                clr.AddReference(sname)
            except:
                pass # maybe a Python library
            self.i.library[sname] = __import__(sname)
        def __str__(self):
            return "IMPORT %s" % self.library_name

    class Renum:
        def __init__(self,i):
            self.i = i
        def run(self):
            current = 100 # start at 100
            new_program = []
            for (l, s) in self.i.program:
                if l == -1:
                    new_program.append((l, s))
                else:
                    new_program.append((current, s))
                    current += 10 # increment by 10
            self.i.program[:] = new_program
        def __str__(self):
            return "RENUM"

    class Clear:
        def __init__(self,i):
            self.i = i
        def run(self):
            self.i.vars.clear()
            self.i.gstack = []
            self.i.fstack = []
        def __str__(self):
            return "CLEAR"

    class If:
        def __init__(self,i,expr):
            self.i = i
            self.expr = expr
        def run(self):
            if self.i.ip==-1:
                raise Exception("Immediate mode IF not supported")
            if not self.expr.eval():
                ip = self.i.ip
                program = self.i.program
                L = len(program)
                while ip<L and program[ip][0]<0:
                    ip += 1
                self.i.ip = ip
        def __str__(self):
            return "IF "+str(self.expr)+" THEN "

    class Gosub:
        def __init__(self,i,line):
            self.i = i
            self.line = line
        def run(self):
            if self.line in self.i.linenos:
                self.i.gstack.append(self.i.ip)
                self.i.ip = self.i.linenos[self.line]
            else:
                raise Exception("Undefined GOSUB statement (%i)" % self.line)
        def __str__(self):
            return "GOSUB %i" % self.line

    class Return:
        def __init__(self,i):
            self.i = i
        def run(self):
            if self.i.gstack:
                self.i.ip = self.i.gstack.pop(-1)
            else:
                raise Exception("RETURN without GOSUB")
        def __str__(self):
            return "RETURN"

    class For:
        def __init__(self,i,var,expr1,expr2,expr3):
            self.i = i
            self.var = var
            self.expr1 = expr1
            self.expr2 = expr2
            self.expr3 = expr3
        def run(self):
            if self.i.ip==-1:
                raise Exception("Immediate mode FOR not supported")
            self.i.vars[self.var] = self.expr1.eval()
            self.i.fstack.append( (self.i.ip,
                                   self.var,
                                   self.expr2.eval(),
                                   self.expr3.eval()) )
        def __str__(self):
            return ("FOR "+self.var+"="+
                    str(self.expr1)+" TO "+
                    str(self.expr2)+" STEP "+
                    str(self.expr3))

    class Next:
        def __init__(self,i):
            self.i = i
        def run(self):
            if self.i.fstack:
                (ip,var,limit,inc) = self.i.fstack[-1]
                self.i.vars[var] += inc
                if inc*(self.i.vars[var]-limit)<=0:
                    self.i.ip = ip
                else:
                    del self.i.fstack[-1]
            else:
                raise Exception("NEXT without FOR")
        def __str__(self):
            return "NEXT"

    class End:
        def __init__(self,i):
            self.i = i
        def run(self):
            self.i.ip = -1
        def __str__(self):
            return "END"

    class New:
        def __init__(self,i):
            self.i = i
        def run(self):
            self.i.ip = -1
            del self.i.program[:]
            self.i.vars.clear()
            self.i.gstack = []
            self.i.fstack = []
        def __str__(self):
            return "NEW"

    class Load:
        def __init__(self,i,expr):
            self.i = i
            self.expr = expr

        def run(self):
            f = file(self.expr.eval())
            if self.i.ip!=-1:
                self.i.ip = 0
            del self.i.program[:]
            self.i.gstack = []
            self.i.fstack = []
            try:
                for L in f:
                    if L[-1]=='\n': L=L[:-1]
                    self.i.process(L)
            finally:
                f.close()
        def __str__(self):
            return "LOAD "+str(self.expr)

    class Save:
        def __init__(self,i,expr):
            self.i = i
            self.expr = expr

        def run(self):
            f = open(self.expr.eval(), "w")
            last = None
            for (l,s) in self.i.program:
                if l == -1:
                    if not isinstance(last,BasicInterpreter.If):
                        f.write(":")
                else:
                    f.write("\n")
                    f.write("%i " % int(l))
                f.write(str(s))
                last = s
            f.write("\n")
            f.close()

        def __str__(self):
            return "SAVE "+str(self.expr)


    class Input:
        def __init__(self,i,var,prompt):
            self.i = i
            self.var = var
            self.prompt = prompt
        def run(self):
            s = input(self.prompt)
            if s == None:
                s = "0"
            if self.var.endswith('$'):
                self.i.vars[self.var] = s
            else:
                self.i.vars[self.var] = float(s)
        def __str__(self):
            return 'INPUT "%s";%s' % (self.prompt,self.var)

    def parse_term(self,tk):
        if (tk.curtoken == Tokenizer.NUMBER or 
            tk.curtoken == Tokenizer.STRING):
            return BasicInterpreter.Literal(tk.skip())
        elif tk.curtoken == Tokenizer.IDENT:
            i = tk.skip()
            if (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,"("):
                if i.upper() in BasicInterpreter.lib_functions:
                    f = BasicInterpreter.lib_functions[i.upper()]
                else:
                    f = i # just the name
                parms = []
                tk.skip()
                if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,")"):
                    parms.append(self.parse_expr(tk))
                    while (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,","):
                        tk.skip()
                        parms.append(self.parse_expr(tk))
                if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,")"):
                    raise Exception("Syntax error (')' expected)")
                tk.skip()
                return BasicInterpreter.FCall(f,parms,self)
            elif i=="NOT":
                return BasicInterpreter.Not(self.parse_term(tk))
            else:
                return BasicInterpreter.Variable(self.vars, i, self)
        elif tk.curtoken == Tokenizer.SPECIAL:
            if tk.value == "(":
                tk.skip()
                v = self.parse_expr(tk)
                if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,")"):
                    raise Exception("Syntax error (')' expected)")
                tk.skip()
                return v
            elif tk.value == "-":
                tk.skip()
                return BasicInterpreter.Neg(self.parse_term(tk))
        raise Exception("Syntax error")

    def parse_expr(self,tk,level=-1):
        if level == 0:
            return self.parse_term(tk)
        else:
            if level == -1:
                level = len(BasicInterpreter.binops)
            a = self.parse_expr(tk,level-1)
            while (tk.curtoken in (Tokenizer.SPECIAL, Tokenizer.IDENT) and
                   tk.value.upper() in BasicInterpreter.binops[level-1]):
                name = tk.skip()
                bop = BasicInterpreter.binops[level-1][name.upper()]
                b = self.parse_expr(tk,level-1)
                a = BasicInterpreter.BinOp(a,b,bop)
            return a
    
    def parse_statement(self,tk):
        if (tk.curtoken, tk.value.upper()) == (Tokenizer.IDENT,"PRINT"):
            tk.skip()
            exprlist = []
            semi = False
            if tk.curtoken and (tk.curtoken,tk.value)!=(Tokenizer.SPECIAL,":"):
                exprlist.append(self.parse_expr(tk))
                while (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,";"):
                    tk.skip()
                    if (tk.curtoken is None or
                        (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,":")):
                        semi = True
                        break
                    exprlist.append(self.parse_expr(tk))
            return BasicInterpreter.Print(exprlist,semi)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"GOTO"):
            tk.skip()
            if tk.curtoken != Tokenizer.NUMBER:
                raise Exception("Syntax error (line number expected)")
            return BasicInterpreter.Goto(self,tk.skip())
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"REM"):
            comment = tk.string[tk.cp:]
            tk.cp = len(tk.string)
            tk.skip()
            return BasicInterpreter.Rem(comment)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"GOSUB"):
            tk.skip()
            if tk.curtoken != Tokenizer.NUMBER:
                raise Exception("Syntax error (line number expected)")
            return BasicInterpreter.Gosub(self,tk.skip())
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"RETURN"):
            tk.skip()
            return BasicInterpreter.Return(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"LIST"):
            tk.skip()
            return BasicInterpreter.List(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"RUN"):
            tk.skip()
            return BasicInterpreter.Run(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"RENUM"):
            tk.skip()
            return BasicInterpreter.Renum(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"IMPORT"):
            tk.skip()
            return BasicInterpreter.Import(self,self.parse_expr(tk))
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"CLEAR"):
            tk.skip()
            return BasicInterpreter.Clear(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"NEW"):
            tk.skip()
            return BasicInterpreter.New(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"END"):
            tk.skip()
            return BasicInterpreter.End(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"LOAD"):
            tk.skip()
            return BasicInterpreter.Load(self,self.parse_expr(tk))
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"SAVE"):
            tk.skip()
            return BasicInterpreter.Save(self,self.parse_expr(tk))
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"INPUT"):
            tk.skip()
            if tk.curtoken == Tokenizer.STRING:
                prompt = tk.skip()
                if (tk.curtoken,tk.value.upper()) != (Tokenizer.SPECIAL,";"):
                    raise Exception("Syntax error (';' expected)")
                tk.skip()
            else:
                prompt = "?"
            if tk.curtoken != Tokenizer.IDENT:
                raise Exception("Syntax error (INPUT variable expected)")
            return BasicInterpreter.Input(self,tk.skip(),prompt)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"NEXT"):
            tk.skip()
            return BasicInterpreter.Next(self)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"FOR"):
            tk.skip()
            if tk.curtoken != Tokenizer.IDENT:
                raise Exception("Syntax error (FOR variable expected)")
            var = tk.skip()
            if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,"="):
                raise Exception("Syntax error ('=' expected)")
            tk.skip()
            expr1 = self.parse_expr(tk)
            if (tk.curtoken,tk.value.upper()) != (Tokenizer.IDENT,"TO"):
                raise Exception("Syntax error (TO expected)")
            tk.skip()
            expr2 = self.parse_expr(tk)
            if Compare(tk.curtoken, tk.value, Tokenizer.IDENT, "STEP"):
                tk.skip()
                expr3 = self.parse_expr(tk)
            else:
                expr3 = BasicInterpreter.Literal(1)
            return BasicInterpreter.For(self,var,expr1,expr2,expr3)
        elif (tk.curtoken,tk.value.upper()) == (Tokenizer.IDENT,"IF"):
            tk.skip()
            expr = self.parse_expr(tk)
            if (tk.curtoken,tk.value.upper()) != (Tokenizer.IDENT,"THEN"):
                raise Exception("Syntax error ('THEN' expected)")
            tk.skip()
            return BasicInterpreter.If(self,expr)
        else:
            if tk.curtoken != Tokenizer.IDENT:
                raise Exception("Syntax error")
            varname = tk.skip()
            if (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,"("): # side-effect function call
                if varname.upper() in BasicInterpreter.lib_functions:
                    f = BasicInterpreter.lib_functions[varname.upper()]
                else:
                    f = varname
                parms = []
                tk.skip()
                if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,")"):
                    parms.append(self.parse_expr(tk))
                    while (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,","):
                        tk.skip()
                        parms.append(self.parse_expr(tk))
                if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,")"):
                    raise Exception("Syntax error (')' expected)")
                tk.skip()
                return BasicInterpreter.FCall(f,parms,self)
            if (tk.curtoken,tk.value) != (Tokenizer.SPECIAL,"="):
                raise Exception("Syntax error ('=' expected)")
            tk.skip()
            return BasicInterpreter.Assign(self.vars,
                                           varname,
                                           self.parse_expr(tk))

    def __init__(self, calico):
        self.calico = calico
        self.gstack = []
        self.fstack = []
        self.program = []
        self.linenos = {}
        self.ip = -1
        self.vars = {"calico": self.calico}
        self.library = {}
        self.last_line = 0

    def process(self,L):
        tk = Tokenizer(L)
        line = None
        if tk.curtoken == Tokenizer.NUMBER:
            line = tk.skip()
        try:
            stm = []
            if tk.curtoken:
                s = self.parse_statement(tk)
                if line is None:
                    s.run()
                else:
                    stm.append(s)
                    while isinstance(s,BasicInterpreter.If):
                        if tk.curtoken == Tokenizer.NUMBER:
                            s = BasicInterpreter.Goto(self,tk.skip())
                        else:
                            s = self.parse_statement(tk)
                        stm.append(s)
                while (tk.curtoken,tk.value) == (Tokenizer.SPECIAL,":"):
                    tk.skip()
                    s = self.parse_statement(tk)
                    if line is None:
                        s.run()
                    else:
                        stm.append(s)
                        while isinstance(s,BasicInterpreter.If):
                            if tk.curtoken == Tokenizer.NUMBER:
                                s = BasicInterpreter.Goto(self,tk.skip())
                            else:
                                s = self.parse_statement(tk)
                            stm.append(s)
                if tk.curtoken:
                    raise Exception("Syntax error (extra characters)")
            if line is not None:
                if line in self.linenos:
                    i = self.linenos[line]+1
                    while i<len(self.program) and self.program[i][0] == -1:
                        i += 1
                    del self.program[self.linenos[line]:i]
                    del self.linenos[line]
                if stm:
                    i = 0
                    while i<len(self.program) and self.program[i][0]<line:
                        i += 1
                    self.program[i:i] = ([(line,stm[0])] +
                                         [(-1,x) for x in stm[1:]])
                self.linenos={}
                for i,s in enumerate(self.program):
                    if s[0]>=0:
                        self.linenos[s[0]] = i
        except Exception as e:
            self.calico.Error(e.message + "\n")
            self.calico.Error(tk.string + "\n")
            self.calico.Error(" " * tk.cp + "^\n")
            return False
        return True

    def Execute(self, text):
        """
        """
        for basic_line in text.split("\n"):
            self.ip = -1
            try:
                ok = self.process(basic_line)
            except Exception as e:
                self.calico.Error(e.message + "\n")
                return
            if not ok:
                return
            while not (self.ip == -1 or self.ip > len(self.program)):
                (line, s) = self.program[self.ip]
                if line >= 0:
                    self.last_line = line
                self.ip += 1
                try:
                    s.run()
                except Exception as e:
                    if "Thread was being aborted" not in e.message:
                        self.calico.Error(e.message + "\n")
                        self.calico.Error("Error in line %i\n" % self.last_line)
                    return

    def main(self):
        last_line = 0
        while True:
            if self.ip == -1 or self.ip > len(self.program):
                self.ip = -1
                L = input("> ")
                self.process(L)
            else:
                (line,s) = self.program[self.ip]
                if line>=0:
                    last_line = line
                self.ip += 1
                try:
                    s.run()
                except:
                    print("Error in line %i" % last_line)
                    self.ip = -1

#t = Tokenizer("FOR N=2 to limit")
#basic = BasicInterpreter(calico)
#basic.parse_statement(t)

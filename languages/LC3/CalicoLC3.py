from __future__ import print_function

import clr
clr.AddReference("Calico")
import Calico
import System
clr.AddReference("gtk-sharp")
import Gtk
clr.AddReference("Mono.TextEditor") 
import Mono.TextEditor

import sys, os
sys.path.append(os.path.abspath(os.path.dirname(__file__)))

from lc3 import LC3, lc_hex
import time
import traceback

class CalicoLC3(LC3):
    def __init__(self, calico, debug, trace_pause, filename):
        self.doc = calico.GetDocument("Memory.csv")
        LC3.__init__(self)
        self.calico = calico
        self.debug = debug
        self.trace_pause = trace_pause
        self.filename = filename

    def reset_memory(self):
        LC3.reset_memory(self)
        for i in range(0, 0x0500):
            self._set_memory(i, 0)

    def get_memory(self, location):
        if self.doc:
            row = location // 256
            col = location % 256
            try:
                return int("0" + self.doc.GetData(col + 1, row + 3), 16)
            except:
                return 0
        else:
            return LC3.get_memory(self, location)

    def _set_memory(self, location, value):
        if self.doc:
            row = location // 256
            col = location % 256
            self.doc.SetData(col + 1, row + 3, lc_hex(value))
        else:
            LC3._set_memory(self, location, value)

    def _set_register(self, reg, value):
        if self.doc:
            row = 1
            col = (reg * 2) + 1
            self.doc.SetData(col, row, lc_hex(value))
        else:
            LC3._set_register(self, reg, value)

    def get_register(self, reg):
        if self.doc:
            row = 1
            col = (reg * 2) + 1
            return int("0" + self.doc.GetData(col, row), 16)
        else:
            return LC3.get_register(self, reg)

    def _set_pc(self, value):
        if self.doc:
            row = 0
            col = 1
            self.doc.SetData(col, row, lc_hex(value))
        else:
            LC3._set_pc(self, value)

    def get_pc(self):
        if self.doc:
            row = 0
            col = 1
            return int("0" + self.doc.GetData(col, row), 16)
        else:
            return LC3.get_pc(self)

    def _set_nzp(self, value):
        if self.doc:
            row = 0
            col = 3
            nzp = (int(value & (1 << 15) > 0),
                   int(value == 0),
                   int((value & (1 << 15) == 0) and value != 0))
            self.doc.SetData(col, row, nzp)
        else:
            LC3._set_nzp(self, value)

    def get_nzp(self, register=None):
        if self.doc:
            row = 0
            col = 3
            nzp = eval(self.doc.GetData(col, row))
            if register is not None:
                return nzp[register]
            else:
                return nzp
        else:
            return LC3.get_nzp(self, register)

    def Info(self, string):
        self.calico.Info(string)

    def handleDebug(self, lineno):
        ## First, see if we need to do something:
        #print("check trace:", lineno, self.trace, self.calico.ProgramSpeedValue)
        if not self.debug: 
            #print(1)
            return
        try:
            #print(2)
            if (self.calico.CurrentDocument == None): # FIXME: so that it will work with other files
                #print(3)
                return
            elif (lineno == -1):
                pass
            elif (self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno)):
                # don't return! Fall through and wait
                #print(4)
                pass
            elif (self.calico.ProgramSpeedValue == 100 and not self.trace_pause):
                #print(5)
                return
        except:
            #print(6)
            return
        # Ok, we have something to do:
        #print("lineno", lineno)
        if lineno != -1:
            Calico.MainWindow.InvokeBlocking(lambda: self.setTraceButtons(lineno))
        Calico.MainWindow.InvokeBlocking( lambda: self.calico.UpdateLocal([
            ["PC", lc_hex(self.get_pc())],
            ["NZP", self.get_nzp()],
            ["R0", lc_hex(self.get_register(0))],
            ["R1", lc_hex(self.get_register(1))],
            ["R2", lc_hex(self.get_register(2))],
            ["R3", lc_hex(self.get_register(3))],
            ["R4", lc_hex(self.get_register(4))],
            ["R5", lc_hex(self.get_register(5))],
            ["R6", lc_hex(self.get_register(6))],
            ["R7", lc_hex(self.get_register(7))]]))
        psv = self.calico.ProgramSpeedValue
        #print("psv", psv, self.trace_pause, lineno)
        if (psv == 0 or
            ((lineno != -1) and 
             self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno)) or
            self.trace_pause):
            #print("wait!")
            self.calico.playResetEvent.Reset()
            self.calico.playResetEvent.WaitOne()
            if self.calico.ProgramSpeedValue != 0:
                self.trace_pause = False
        elif (psv < 100): ## then we are in a delay:
            pause = (2.0 / psv)
            ## Force at least a slight sleep, else no GUI controls
            time.sleep(pause)

    def setTraceButtons(self, lineno):
        #print("filename", self.filename)
        if self.filename:
            document = self.calico.GetDocument(self.filename)
            #print("document", document)
            if document:
                if self.calico.ProgramSpeedValue == 0:
                    self.calico.PlayButton.Sensitive = True 
                    self.calico.PauseButton.Sensitive = False
                elif self.trace_pause:
                    self.calico.PlayButton.Sensitive = True
                    self.calico.PauseButton.Sensitive = False
                else:
                    self.calico.PlayButton.Sensitive = False
                    self.calico.PauseButton.Sensitive = True
                document.GotoLine(lineno)
                document.SelectLine(lineno)

    def setPause(self, value):
        self.trace_pause = value

    def setTrace(self, value):
        #print("setTrace:", value)
        self.debug = value

class MyEngine(Calico.Engine):
    def __init__(self, *args, **kwargs):
        Calico.Engine.__init__(*args, **kwargs)
        self.debug = False
        self.trace_pause = False
        self.filename = None
        #print("engine init", self.trace)

    def PostSetup(self, calico):
        #print("post setup", self.trace)
        self.calico = calico
        self.lc3 = CalicoLC3(calico, self.debug, self.trace_pause, self.filename)
        Calico.MainWindow.InvokeBlocking( lambda: calico.UpdateLocal([
            ["PC", lc_hex(self.lc3.get_pc())],
            ["NZP", self.lc3.get_nzp()],
            ["R0", lc_hex(self.lc3.get_register(0))],
            ["R1", lc_hex(self.lc3.get_register(1))],
            ["R2", lc_hex(self.lc3.get_register(2))],
            ["R3", lc_hex(self.lc3.get_register(3))],
            ["R4", lc_hex(self.lc3.get_register(4))],
            ["R5", lc_hex(self.lc3.get_register(5))],
            ["R6", lc_hex(self.lc3.get_register(6))],
            ["R7", lc_hex(self.lc3.get_register(7))]]))

    def Execute(self, text):

        words = [word.strip() for word in text.split()]
        if words[0] == ".help":
            print("Interactive Directives: ")
            print(" .show memory                    - show memory as a spreadsheet")
            print(" .assemble                       - assemble the first .asm file in Calico")
            print(" .step                           - execute the next instruction, increment PC")
            print(" .reset                          - reset LC3 to start state")
            print(" .raw [start [stop]]             - list meory in hex")
            print(" .list                           - list program from memory")
            print(" .dump [start [stop]]            - dump memory as program")
            print(" .regs                           - show registers")
            print(" .set pc HEXVALUE                - set PC")
            print(" .set memory HEXLOCATION HEXVALUE- set memory")
            print(" .set reg VALUE HEXVALUE         - set register")
            print(" .get pc                         - get PC")
            print(" .get memory HEXLOCATION         - get memory")
            print(" .get reg VALUE                  - get register")
            return True
        elif words[0] == ".raw":
            if len(words) > 0:
                start = int("0" + words[1], 16)
            else:
                start = 0x3000
            if len(words) > 1:
                stop = int("0" + words[2], 16)
            else:
                stop = 0x300F
            try:
                for x in range(start, stop):
                    print(lc_hex(x), lc_hex(self.lc3.get_memory(x)))
            except:
                print("Help: .raw [start [stop]]")
            return True
        elif words[0] == ".list":
            try:
                self.lc3.dump()
            except:
                print("Error; did you should run code first?")
            return True
        elif words[0] == ".regs":
            try:
                self.lc3.dump_registers()
            except:
                print("Error; did you should run code first?")
            return True
        elif words[0] == ".dump":
            try:
                self.lc3.dump(*[int("0" + word, 16) for word in words[1:]])
            except:
                print("Error; did you should run code first?")
            return True
        elif words[0] == ".set":
            try:
                if words[1] == "pc":
                    self.lc3.set_pc(int("0" + words[2], 16))
                elif words[1] == "memory":
                    self.lc3.set_memory(int("0" + words[2], 16), int("0" + words[3], 16))
                elif words[1] == "reg":
                    self.lc3.set_register(int(words[2]), int("0" + words[3], 16))
                else:
                    print("Use .set [pc|memory|reg] ...")                
            except:
                print("Hint: .set pc x3000")
                print("      .set reg 1 xFFFF")
                print("      .set memory x300A x1")
            return True
        elif words[0] == ".get":
            try:
                if words[1] == "pc":
                    print(lc_hex(self.lc3.get_pc()))
                elif words[1] == "memory":
                    print(lc_hex(self.lc3.get_memory(int("0" + words[2], 16))))
                elif words[1] == "reg":
                    print(self.lc3.get_register(int(words[2])))
                else:
                    print("Use .get [pc|memory|reg] ...")                
            except:
                print("Hint: .get pc")
                print("      .get reg 1")
                print("      .get memory x300A")
            return True
        elif words[0] == ".reset":
            self.lc3.reset()
            return True
        elif words[0] == ".assemble":
            document = self.calico.GetDocument(".asm")
            if document:
                self.lc3.filename = document.filename
                text = document.GetText()
                self.lc3.assemble(text)
                print("Assembled", document.filename)
            else:
                print("Open an LC3 script")
            return True
        elif words[0] == ".cont":
            try:
                self.run(reset=False)
            except:
                print("Error")
            return True
        elif words[0] == ".step":
            orig_debug = self.lc3.debug
            self.lc3.debug = True
            if self.lc3.get_pc() in self.lc3.source:
                lineno = self.lc3.source[self.lc3.get_pc()]
                if lineno > 0:
                    Calico.MainWindow.InvokeBlocking(lambda: self.lc3.setTraceButtons(lineno))
                    Calico.MainWindow.InvokeBlocking(lambda: self.calico.switchToShell())
            self.lc3.step()
            self.lc3.debug = orig_debug
            return True
        elif words[0] == ".show":
            if len(words) > 0:
                if words[1] == "memory":
                    memory = os.path.join(os.path.abspath(os.path.dirname(__file__)),
                                          "Memory.csv")
                    
                    self.calico.Open(memory)
                    self.lc3.doc = self.calico.GetDocument("Memory.csv")
                    try:
                        print("Loading LC3 operating system...")
                        self.lc3.load_os()
                        self.lc3.reset_registers()
                    except:
                        traceback.print_exc()
                else:
                    print("Invalid show item '%s'; use '.show memory'" % words[1])
            else:
                print("Use: .show memory")
            return True

        ok = False
        try:
            self.lc3.assemble(text)
            ok = True
        except Exception as exc:
            if self.lc3.debug:
                traceback.print_exc()
            if self.lc3.get_pc() in self.lc3.source:
                self.calico.Error("\nAssemble error!\nFile \"%s\", line %s\n" % 
                                  (self.filename, 
                                   self.lc3.source[self.lc3.get_pc()]))
            else:
                self.calico.Error("\nRuntime error!\nFile \"%s\", memory %s\n" % 
                                  (self.filename, 
                                   lc_hex(self.lc3.get_pc())))
            self.calico.Error(exc.message)

        if ok:
            ok = False
            try:
                self.lc3.run()
                self.lc3.dump_registers()
                print("Instructions:", self.lc3.instruction_count)
                print("Cycles: %s (%s milliseconds)" % 
                      (self.lc3.cycle, self.lc3.cycle * 1./2000000))
                ok = True
            except Exception as exc:
                if self.lc3.debug:
                    traceback.print_exc()
                if self.lc3.get_pc() in self.lc3.source:
                    self.calico.Error("\nRuntime error!\nFile \"%s\", line %s\n" % 
                                      (self.filename, 
                                       self.lc3.source[self.lc3.get_pc()]))
                else:
                    self.calico.Error("\nRuntime error!\nFile \"%s\", memory %s\n" % 
                                      (self.filename, 
                                       lc_hex(self.lc3.get_pc())))
                self.calico.Error(exc.message)

        return ok

    def ExecuteFile(self, filename):
        self.lc3.reset_registers()
        self.filename = filename
        text = self.lc3.load(filename)
        self.Execute(text) 
        return True

    def ReadyToExecute(self, text):
        return text.split("\n")[-1].strip() == "" or text.startswith(".")

    def RequestPause(self):
        self.trace_pause = True
        self.lc3.setPause(True)
        Calico.Engine.RequestPause(self)

    def SetTraceOn(self, calico):
        #print("trace on")
        self.debug = True
        self.lc3.setTrace(True)
        Calico.Engine.SetTraceOn(self, calico)

    def SetTraceOff(self):
        #print("trace off")
        self.debug = False        
        self.lc3.setTrace(False)
        Calico.Engine.SetTraceOff(self)

class MyDocument(Calico.TextDocument):
    def Stop(self):
        self.calico.manager["lc3"].engine.lc3.cont = False;
        Calico.TextDocument.Stop(self)

    def GetAuthors(self):
        return System.Array[str]([
            "Douglas S. Blank <dblank@cs.brynmawr.edu>",
        ])

class MyLanguage(Calico.Language):
    def __init__(self):
        self.name = "lc3"
        self.proper_name = "LC3"
        self.extensions = System.Array[str](["asm"])
        self.mimetype = "text/x-assembly"
        self.LineComment = ";;; "
        
    def MakeEngine(self, manager):
        self.engine = MyEngine(manager)

    def MakeDocument(self, calico, filename):
        return MyDocument(calico, filename, self.name, self.mimetype)

    def getExamplesPath(self, root_path):
    	return os.path.join(os.path.abspath(os.path.dirname(__file__)), "examples")

def MakeLanguage():
    Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
        os.path.join(os.path.abspath(os.path.dirname(__file__)), "SyntaxModes"))
    return MyLanguage()

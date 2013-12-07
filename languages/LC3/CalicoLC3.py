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

from lc3 import LC3
import time

class CalicoLC3(LC3):
    def __init__(self, calico, trace, trace_pause, filename):
        LC3.__init__(self)
        self.calico = calico
        self.trace = trace
        self.trace_pause = trace_pause
        self.filename = filename
        #print("init:", self.trace)

    def handleDebug(self, lineno):
        ## First, see if we need to do something:
        #print("check trace:", lineno, self.trace, self.calico.ProgramSpeedValue)
        if lineno == -1 or self.filename is None or not self.trace: 
            #print(1)
            return
        try:
            #print(2)
            if (self.calico.CurrentDocument == None):
                #print(3)
                return
            elif (self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno)):
                # don't return! Fall through and wait
                #print(4)
                pass
            elif (self.calico.ProgramSpeedValue == 100):
                #print(5)
                return
        except:
            #print(6)
            return
        # Ok, we have something to do:
        Calico.MainWindow.InvokeBlocking(lambda: self.setTraceButtons(lineno))
        psv = self.calico.ProgramSpeedValue
        #print("psv", psv)
        if (psv == 0 or
            self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno) or
            self.trace_pause):
            self.calico.playResetEvent.Reset()
            #print("wait!")
            self.calico.playResetEvent.WaitOne()
        elif (psv < 100): ## then we are in a delay:
            pause = (2.0 / psv)
            ## Force at least a slight sleep, else no GUI controls
            time.sleep(pause)

    def setTraceButtons(self, lineno):
        document = self.calico.GetDocument(self.filename)
        if document:
            self.calico.playResetEvent.Reset()
            self.calico.PlayButton.Sensitive = True
            self.calico.PauseButton.Sensitive = False
            document.GotoLine(lineno)
            document.SelectLine(lineno)

    def setPause(self, value):
        self.trace_pause = value

    def setTrace(self, value):
        #print("setTrace:", value)
        self.trace = value

class MyEngine(Calico.Engine):
    def __init__(self, *args, **kwargs):
        Calico.Engine.__init__(*args, **kwargs)
        self.trace = False
        self.trace_pause = False
        self.filename = None
        #print("engine init", self.trace)

    def PostSetup(self, calico):
        #print("post setup", self.trace)
        self.lc3 = CalicoLC3(calico, self.trace, self.trace_pause, self.filename)

    def Execute(self, text):
        ok = False
        try:
            self.lc3.assemble(text)
            ok = True
        except:
            print("Error in assembly!")
        if ok:
            ok = False
            try:
                self.lc3.run()
                ok = True
            except:
                print("Runtime error!")
        self.lc3.filename = None
        return ok

    def ExecuteFile(self, filename):
        self.lc3.filename = filename
        fp = file(filename)
        text = "".join(fp.readlines())
        fp.close()
        self.Execute(text) 
        return True

    def ReadyToExecute(self, text):
        return text.split("\n")[-1].strip() == ""

    def RequestPause(self):
        self.trace_pause = True
        self.lc3.setPause(True)

    def ConfigureTrace(self):
        pass

    def SetTraceOn(self, calico):
        #print("trace on")
        self.trace = True
        self.lc3.setTrace(True)

    def SetTraceOff(self):
        #print("trace off")
        self.trace = False
        self.lc3.setTrace(False)

class MyDocument(Calico.TextDocument):
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

import clr
clr.AddReference("Calico")
import Calico
import System
clr.AddReference("gtk-sharp")
import Gtk
clr.AddReference("Mono.TextEditor") 
import Mono.TextEditor

import sys, os
sys.path.append(os.path.dirname(__file__))

from lc3 import LC3
import time

class CalicoLC3(LC3):
    def __init__(self, calico):
        LC3.__init__(self)
        self.calico = calico
        self.trace_pause = False
        self.trace = False
        self.filename = None

    def handleDebug(self, lineno):
        ## First, see if we need to do something:
        if lineno == -1 or self.filename is None: # or not self.trace: FIXME
            return
        try:
            if (self.calico.CurrentDocument == None):
                return
            elif (self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno)):
                # don't return! Fall through and wait
                pass
            elif (self.calico.ProgramSpeedValue == 100):
                return
        except:
            return
        # Ok, we have something to do:
        Calico.MainWindow.Invoke(lambda: self.setTraceButtons(lineno))
        psv = self.calico.ProgramSpeedValue
        if (psv == 0 or
            self.calico.CurrentDocument.HasBreakpointSetAtLine(lineno) or
            self.trace_pause):
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
        self.trace = value

class MyEngine(Calico.Engine):
    def PostSetup(self, calico):
        self.lc3 = CalicoLC3(calico)

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
        self.lc3.setPause(True)

    def ConfigureTrace(self):
        pass

    def SetTraceOn(self, calico):
        self.lc3.setTrace(True)

    def SetTraceOff(self):
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
    	return os.path.join(os.path.dirname(__file__), "examples")

def MakeLanguage():
    Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
        os.path.join(os.path.dirname(__file__), "SyntaxModes"))
    return MyLanguage()

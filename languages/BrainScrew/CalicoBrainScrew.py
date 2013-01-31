# Sample Language File

import clr
clr.AddReference("Calico")
import Calico
import System
clr.AddReference("gtk-sharp")
import Gtk

import sys, os
sys.path.append(os.path.dirname(__file__))

from bf import *

class MyEngine(Calico.Engine):

    def Execute(self, text):
        self.interpreter.run_commands(text)
        return True

    def ExecuteFile(self, filename):
        fp = file(filename)
        text = "".join(fp.readlines())
        fp.close()
        self.interpreter.run_commands(text)
        return True

    def ReadyToExecute(self, text):
        ## Return True if expression parses ok.
        ##print(self.calico.PropertyNotebook.NPages)
        return True

    def PostSetup(self, calico):
        self.interpreter = BFInterpreter(calico)

class PropertyWindow(Gtk.ScrolledWindow):

    def __init__(self):
        propTree = Gtk.TreeView()

        ## Create name columns
        nameCol = Gtk.TreeViewColumn ()
        nameCol.Title = "Name"

        valCol = Gtk.TreeViewColumn ()
        valCol.Title = "Value"

        ## Add to TreeView
        propTree.AppendColumn(nameCol)
        propTree.AppendColumn(valCol)
    
        ## Create and insert cell renderer
        nameRenderer = Gtk.CellRendererText()
        nameCol.PackStart(nameRenderer, True)
    
        ## Create and insert cell renderer
        valRenderer = Gtk.CellRendererText()
        valCol.PackStart(valRenderer, True)
    
        ## Link it all together
        #nameCol.SetCellDataFunc(nameRenderer, Gtk.TreeCellDataFunc(RenderPropertyName));
        #valCol.SetCellDataFunc(valRenderer, Gtk.TreeCellDataFunc(RenderPropertyValue));
    
        ## Make the value column editable
        valRenderer.Editable = False
        ##valRenderer.Edited += OnPropValRendererEdited;
    
        ## Create a ListStore as the Model
        propList = Gtk.ListStore(type(""))
        propTree.Model = propList
    
        self.Add(propTree)
        self.propList = propList
        self.propTree = propTree

class MyDocument(Calico.TextDocument):

    def GetPropertyNotebookWidget(self):
	    ## Return Property Notebook Widget
        self.property_window = PropertyWindow()
        return self.property_window

    def GetAuthors(self):
        return System.Array[str]([
            "Johannes Charra",
            "Douglas S. Blank <dblank@cs.brynmawr.edu>",
        ])
        

class MyLanguage(Calico.Language):
    def __init__(self):
        self.name = "brainscrew"
        self.proper_name = "BrainScrew"
        self.extensions = System.Array[str](["bs"])
        self.mimetype = "text/x-brainscrew"
        self.LineComment = ";;"
        
    def getExamples(self, menu):
    	pass

    def MakeEngine(self, manager):
        self.engine = MyEngine(manager)

    def MakeDocument(self, calico, filename):
        return MyDocument(calico, filename, self.name, self.mimetype)

    def GetUseLibraryString(self, fullname):
        return ""
        
    def getExamplesPath(self, root_path):
    	return os.path.join(os.path.dirname(__file__), "examples")

def MakeLanguage():
    return MyLanguage()


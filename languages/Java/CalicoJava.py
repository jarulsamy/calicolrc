# This file was constructed automatically by the Calico Project
# See http://calicoproject.org/ for more details.
from __future__ import print_function

import sys
import os
import clr                 # Allows connection to CLR/DLR bits
clr.AddReference("Calico") # Add reference to the main Calico.exe
# Add the DLLs from the lib dir:
import glob
libpath = os.path.abspath(os.path.join(os.path.dirname(__file__), "lib"))
sys.path.append(libpath)
#print(sys.path)
for filename in glob.glob(os.path.join(libpath, "*.dll")):
    filename = os.path.abspath(filename)
    #print(filename)
    clr.AddReference(filename)

import edu
import java
import System
import Calico
import traceback

# This is done here, because of problem loading if in PostSetup:
classpath = java.util.ArrayList()
jarpath = os.path.abspath(os.path.join(os.path.dirname(__file__), "jar"))
for filename in glob.glob(os.path.join(jarpath, "*.jar")):
    fileobj = java.io.File(os.path.abspath(filename))
    #print(filename)
    classpath.Add(fileobj)

options = edu.rice.cs.dynamicjava.Options.DEFAULT
loader = edu.rice.cs.plt.reflect.PathClassLoader(classpath)

class OutputStream(java.io.ByteArrayOutputStream):
    def write(self, *args):
        if len(args) == 1:
            # byte
            retval = chr(args)
        else:
            # byte[], offset, length
            byte, offset, length = args
            retval = ""
            for i in range(offset, length):
                retval += chr(byte[i])
        if self.out_type == "output":
            print(retval, end="")
        elif self.out_type == "error":
            self.calico.Error(retval)

def isNone(v):
    return hasattr(v, "isNone") and v.isNone()

# Now, define the Document, Engine, and Language classes:
class MyLanguageEngine(Calico.Engine):
    def PostSetup(self, calico):
        """
        Do things here that you want to do once (initializations).
        """
        self.calico = calico
        self.interpreter = edu.rice.cs.dynamicjava.interpreter.Interpreter(
            options, loader)

    def SetRedirects(self, stdout, stderr):
        out = OutputStream()
        out.out_type = "output"
        out.calico = self.calico
        err = OutputStream()
        err.out_type = "error"
        err.calico = self.calico
        edu.rice.cs.plt.io.IOUtil.replaceSystemOut(out)
        edu.rice.cs.plt.io.IOUtil.replaceSystemErr(err)
            
    def Execute(self, text, feedback=True):
        """
        This is where you do something for the text (code). This is
        the interpreter.
        """
        try:
            retval = self.interpreter.interpret(text)
            try:
                retval = retval.unwrap()
            except:
                pass
            if not isNone(retval) and feedback:
                if retval:
                    if hasattr(retval, "toString"):
                        print(retval.toString())
                    else:
                        print(retval)
        except Exception, error:
            self.calico.Error(error.message.replace("koala.dynamicjava.interpreter.error.", "") + "\n")
        return True

    def ExecuteFile(self, filename):
        """
        This is the code that will interprete a file.
        """
        print("Run filename '%s'!" % filename)
        try:
            text = "".join(open(filename).readlines())
        except:
            traceback.print_exc()
            return

        self.Execute(text, False)
        return True

    def ReadyToExecute(self, text):
        """
        Return True if expression parses ok and you are ready
        to execute. If you return False, then the user can still
        interactively type text into the Calico Shell.
        """
        lines = text.split("\n")
        return (lines[-1].strip() == "" or # empty line
                (not lines[-1].startswith(" ") and lines[-1].endswith(";"))) # statement

class MyLanguage(Calico.Language):
    """
    The Language class holds the Document and the Engine classes, and
    the languages properties.
    """
    def __init__(self):
        self.name = "java"
        self.proper_name = "Java"
        self.extensions = System.Array[str](["java"])
        self.mimetype = "text/x-java"
        self.LineComment = "//"

    def MakeEngine(self, manager):
        """
        Make and hold the language singleton.
        """
        self.engine = MyLanguageEngine(manager)

    def GetUseLibraryString(self, fullname):
        """
        Given a path to a a library DLL, return the string to add to
        script.
        """
        path, filename = os.path.split(fullname)
        return "import " + filename

    def getExamplesPath(self, root_path):
        """
        Given the root_path to calico, return the path to the examples
        folder.
        """
        return os.path.join(os.path.dirname(__file__), "examples")

# And finally define a method of loading it:
def MakeLanguage():
    """
    Make an instance of the Language, and return it. Usually you do this
    just once, even for non-visible languages.
    """
    return MyLanguage()

try:
    engine = MyLanguageEngine(calico.manager)
    engine.PostSetup(calico)
    ## engine.Execute("1 + 1;", True)\
except:
    pass

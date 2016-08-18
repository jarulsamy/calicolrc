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

# FIXME: get line of error
# HINT: drjava only does that on compile, not interpreter
#inputStream = java.io.ByteArrayInputStream(System.Array[System.Byte](map(ord,list("class Person {"))))
#p = koala.dynamicjava.parser.impl.Parser(inputStream)
#try:
#    print(p.parseStream())
#except Exception, e:
#    pass

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
        if retval == "Derived classes must implement it":
            retval = "java.lang.NullPointerException"
        if self.out_type == "output":
            print(retval, end="")
        elif self.out_type == "error":
            self.calico.Error(retval)

def toType(v):
    if hasattr(v, "__class__") and hasattr(v.__class__, "__name__"):
        if v.__class__.__name__ == "Integer":
            return v.intValue()
        elif v.__class__.__name__ == "Double":
            return v.doubleValue()
        elif v.__class__.__name__ == "Byte":
            return v.byteValue()
        elif v.__class__.__name__ == "Float":
            return v.floatValue()
        elif v.__class__.__name__ == "Long":
            return v.longValue()
        elif v.__class__.__name__ == "Short":
            return v.shortValue()
    return v

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
        self.interpreter.interpret("import cli.*;") # makes all DLLs available at top
        self.interpreter.interpret("cli.Calico.MainWindow calico;")
        self.interpreter.setVariable("calico", calico)

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
            message = error.message
            message = message.replace("koala.dynamicjava.interpreter.error.", "")
            message = message.replace("koala.dynamicjava.parser.wrapper.", "")
            message = message.replace("Derived classes must implement it", "java.lang.NullPointerException");
            self.calico.Error(message + "\n")
        return True

    def Evaluate(self, text):
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
            if not isNone(retval):
                return toType(retval)
            else:
                return None
        except Exception, error:
            message = error.message
            message = message.replace("koala.dynamicjava.interpreter.error.", "")
            message = message.replace("koala.dynamicjava.parser.wrapper.", "")
            message = message.replace("Derived classes must implement it", "java.lang.NullPointerException");
            self.calico.Error(message + "\n")
        return None

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

    def GetMember(self, object_value, string_part):
        """
        Given and object and a string, return the member object.
        """
        try:
            wrapped_class = object_value.getClass()
            unwrapped_class = wrapped_class.unwrap()
        except:
            return None

    def GetMemberNames(self, object_obj):
        """
        Given an oject, return a list of attribute names.
        """
        try: # Java objects:
            wrapped_list = object_obj.getClass().getMethods()
            array_of_methods = wrapped_list.unwrap()
            ## nalize', 'getAnnotation', 'getAnnotations', 'getClass', 'getDeclaredAnnotations', 
            ## 'getDeclaringClass', 'getDefaultValue', 'getExceptionTypes', 'getGenericExceptionTypes', 
            ## 'getGenericParameterTypes', 'getGenericReturnType', 'getModifiers', 'getName', 
            ## 'getParameterAnnotations', 'getParameterTypes', 'getReturnType', 'getTypeParameters', 
            ## 'hashCode', 'instancehelper_equals', 'instancehelper_getClass', 'instancehelper_hashCode', 
            ## 'instancehelper_notify', 'instancehelper_notifyAll', 'instancehelper_toString', 
            ## 'instancehelper_wait', 'invoke', 'isAccessible', 'isAnnotationPresent', 'isBridge', 
            ## 'isSynthetic', 'isVarArgs', 'notify', 'notifyAll', 'setAccessible', 'toGenericString', 
            ## 'toString', 'wait'
            return sorted(set([method.getName() for method in array_of_methods]))
        except:
            try: # C# objects:
                methods = object_obj.GetType().GetMethods()
                return sorted(set([method.Name for method in methods 
                                   if method.Name[0] not in "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"]))
            except:
                return []

    def TryGetVariable(self, string_variable):
        """
        See if the variable is valid and return if it is.
        """
        try:
            retval = self.interpreter.interpret(string_variable)
            try:
                retval = retval.unwrap()
            except:
                pass
            if isNone(retval):
                retval = None
            else:
                retval = toType(retval)
            valid = True
        except:
            retval = None
            valid = False
        return Calico.Variable(valid, retval)

    def GetCompletions(self, string_root):
        """
        Given a string, return all of the matching things.
        """
        array_list = self.interpreter.getVariableNames()
        return sorted([x for x in array_list if x.startswith(string_root)])

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
        basename, ext = filename.split(".")
        return "import " + basename + ";\n"

    def getExamplesPath(self, root_path):
        """
        Given the root_path to calico, return the path to the examples
        folder.
        """
        return os.path.join(os.path.abspath(os.path.dirname(__file__)), "examples")

# And finally define a method of loading it:
def MakeLanguage():
    """
    Make an instance of the Language, and return it. Usually you do this
    just once, even for non-visible languages.
    """
    return MyLanguage()

# Setup environment:
import sys
for dir in ['.', './bin/Lib', './bin/DLLs', './modules', './src']:
    sys.path.append(dir)
import os, traceback
sys.path.append(os.path.abspath("modules"))

# Add some paths for Windows:
import Microsoft
import System
try:
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\")
except:
    registry = None
# Get Mono Path    
if registry:
    version = registry.GetValue("DefaultCLR") # '2.8', '2.8.1', etc
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\%s\\" % version)
    path = registry.GetValue("SdkInstallRoot") # Path to Mono
    if path:
        sys.path.append("%s\\lib\\mono\\gtk-sharp-2.0" % path)
# Get SdlDotNet Path
try:
    registry = Microsoft.Win32.Registry.CurrentUser.CreateSubKey("Software\\SdlDotNet\\")
except:
    registry = None
if registry:
    path = registry.GetValue("")
    if path:
        sys.path.append("%s\\bin" % path)

# Bring in DLLs to import from:
import clr
# Mono graphics:
clr.AddReference("gtk-sharp")
clr.AddReference("pango-sharp")
clr.AddReference("glib-sharp")
#clr.AddReference("gnome-sharp")
# Other DLLs:
clr.AddReference("Microsoft.Scripting")

# Bring .NET References into IronPython scope:
import Gtk
import GLib

# Import pure-Python modules:
import traceback

# Pyjama imports:
from engine import EngineManager

# Setup Runtime environment:
def handle_exception(e):
    print e.__class__.__name__

# Turn on Unhandled Exception Handled:
# FIXME EXCEPTION HANDLER
GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes
from utils import _

def get_registered_languages():
    import glob
    sys.path.append(os.path.abspath("languages"))
    results = {}
    for filename in glob.glob("languages/*.py"):
        if "Sympl" in filename: continue
        try:
            import_name, ext = os.path.basename(filename).rsplit(".")
            exec("import %s as LanguageModule" % import_name)
            lang = LanguageModule.register_language()
            results[lang.language] = lang
        except:
            traceback.print_exc()
            print "Cannot load language file '%s'" % filename
    return results

class PyjamaProject(object):
    def __init__(self, argv):
        self.shell = None
        self.editor = None
        self.languages = get_registered_languages()
        self.engine = EngineManager(self)
        for lang in self.languages:
            language = self.languages[lang]
            lclass = language.get_engine_class()
            if lclass:
                self.engine.register(lclass)
        # Set up all of the engines:
        self.engine.setup()
        self.engine.start()
        request_shell = False
        request_editor = False
        files = []
        for arg in argv:
            if arg == "--shell":
                request_shell = True
            elif arg == "--editor":
                request_editor = True
            else:
                files.append(arg)
                request_editor = True
        if files == [] and not request_editor:
            request_shell = True
        if request_editor:
            from editor import EditorWindow
            self.editor = EditorWindow(self, files)
        if request_shell:
            from shell import ShellWindow
            self.shell = ShellWindow(self)

    def load(self, filename):
        # force into a Python string:
        filename = str(filename.ToString())
        for name in self.languages:
            if filename.endswith("." + self.languages[name].extension):
                return self.engine[name].execute_file(filename)
        raise AttributeError("unknown file extension: '%s'" % filename)

    def eval(self, language, exp):
        return self.engine[language].eval(exp)

    def get_language_from_filename(self, filename):
        for name in self.languages:
            if filename.endswith("." + self.languages[name].extension):
                return name
        return "python" # FIXME: default language come from config

    def Print(self, message):
        self.setup_shell()
        self.shell.message(message, "green")

    def on_run(self, obj, event):
        doc = self.get_current_doc()
        if doc and doc.textview.HasFocus:
            text = str(doc.textview.Buffer.Text)
            doc.execute(text)
        else:
            self.command_pane.textview.HasFocus = True
            text = str(self.command_pane.textview.Buffer.Text)
            doc.execute(text)

    def on_close(self, what):
        if what == "shell":
            if self.shell:
                self.shell.window.Destroy()
                self.shell = None 
        elif what == "editor":
            if self.editor:
                self.editor.window.Destroy()
                self.editor = None 
        if ((self.editor is None) and
            (self.shell is None)):
            Gtk.Application.Quit()

    def setup_shell(self, *args, **kwargs):
        if self.shell is None:
            from shell import ShellWindow
            self.shell = ShellWindow(self)
        else:
            def invoke(sender, args):
                self.shell.window.Present()
            Gtk.Application.Invoke(invoke)

    def setup_editor(self, *args, **kwargs):
        if self.editor is None:
            from editor import EditorWindow
            self.editor = EditorWindow(self)
        else:
            def invoke(sender, args):
                self.editor.window.Present()
            Gtk.Application.Invoke(invoke)

    def grep(self, pattern, file_pat="*.py", dir=".", 
             flags=0, recursive=True):
        """
        grep("FIXME")
        grep("import sys", "*.py", ".", True)
        flags = 0 # re.IGNORECASE
        """
        # Based on code from idlelib
        import re
        prog = re.compile(pattern, flags)
        list = self.findfiles(dir, file_pat, recursive)
        list.sort()
        print "Searching %r in %s/%s..." % (pattern, dir, file_pat)
        hits = 0
        for fn in list:
            try:
                f = open(fn)
            except IOError, msg:
                print msg
                continue
            lineno = 0
            while 1:
                block = f.readlines(100000)
                if not block:
                    break
                for line in block:
                    lineno = lineno + 1
                    if line[-1:] == '\n':
                        line = line[:-1]
                    if prog.search(line):
                        sys.stdout.write("  File \"%s\", line %d, %s\"\n" % 
                                         (fn, lineno, line.strip()))
                        hits = hits + 1
        if hits:
            if hits == 1:
                s = ""
            else:
                s = "es"
            print "Found", hits, "match%s. Right-click file to open." % s
        else:
            print "No matches."

    def findfiles(self, dir, base, recursive=True):
        # Based on code from idlelib
        import fnmatch
        try:
            names = os.listdir(dir or os.curdir)
        except os.error, msg:
            print msg
            return []
        list = []
        subdirs = []
        for name in names:
            fn = os.path.join(dir, name)
            if os.path.isdir(fn):
                subdirs.append(fn)
            else:
                if fnmatch.fnmatch(name, base):
                    list.append(fn)
        if recursive:
            for subdir in subdirs:
                list.extend(self.findfiles(subdir, base, recursive))
        return list

# Let's start!
Gtk.Application.Init()
#------------------------------
try:
    pw = PyjamaProject(sys.argv[1:] or 
                       list(System.Environment.GetCommandLineArgs())[1:])
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
Gtk.Application.Run()


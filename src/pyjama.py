# Setup environment:
import sys, os, traceback
sys.path.append(os.path.abspath("modules"))

# Add some paths for Windows:
import Microsoft
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
            doc.execute(text, "Loading file...")
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

# Let's start!
Gtk.Application.Init()
#------------------------------
try:
    pw = PyjamaProject(sys.argv[1:])
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
Gtk.Application.Run()


#
# Pyjama - Educational Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

# Setup environment:
import sys
import os
# First, let's find out where this file is: src/pyjama.py
pyjama_fullpath = os.path.abspath(__file__)
# Next, let's add the absolute paths of what we need:
fullpath, basename = os.path.split(pyjama_fullpath)
for dir in ['.', './bin/Lib', './bin/DLLs', './modules', './src']:
    path = os.path.join(fullpath, dir)
    sys.path.append(path)
import os, traceback
sys.path.append(os.path.abspath("modules"))

# Add some paths for Windows:
import Microsoft
import System
mono_version = "?.?.?"
try:
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\")
except:
    registry = None
# Get Mono Path    
if registry:
    mono_version = registry.GetValue("DefaultCLR") # '2.8', '2.8.1', etc
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\%s\\" % mono_version)
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
import Pango

# Import pure-Python modules:
import traceback

# Pyjama imports:
from engine import EngineManager
from config import config
from utils import _

# Setup Runtime environment:
def handle_exception(arg):
    #print >> sys.stderr, e.__class__.__name__
    print dir(arg)
    sys.exit(1)

# Turn on Unhandled Exception Handled:
# EXCEPTION HANDLER
#GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes


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
            print >> sys.stderr, "Cannot load language file '%s'" % filename
    return results

class PyjamaProject(object):
    def __init__(self, argv):
        self.shell = None
        self.editor = None
        self.version = version           # from global variable
        self.config = config             # from global variable
        self.mono_version = mono_version # from global variable
        self.system = System.Environment.OSVersion.VersionString
        self.gui = True
        self.standalone = False
        self.indent_string = "    "
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
            elif arg == "--nogui":
                self.gui = False
                request_shell = True
            elif arg == "--exec":
                self.standalone = True
                request_shell = True
            else:
                files.append(arg)
                request_editor = True
        if files == [] and not request_editor:
            request_shell = True
        if request_editor:
            if not self.standalone:
                from editor import EditorWindow
                self.editor = EditorWindow(self, files)
        if request_shell:
            if self.standalone:
                from shell import Shell
                self.shell = Shell(self, files)
            else:
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
        self.shell.message("Searching %r in %s/%s...\n" % (pattern, dir, file_pat))
        hits = 0
        for fn in list:
            try:
                f = open(fn)
            except IOError, msg:
                self.shell.message(msg + "\n")
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
                        self.shell.message("  File \"%s\", line %d, %s\"\n" % 
                                         (fn, lineno, line.strip()))
                        hits = hits + 1
        if hits:
            if hits == 1:
                s = ""
            else:
                s = "es"
            self.shell.message("Found %d match%s. Right-click file to open.\n" % (hits, s))
        else:
            self.shell.message("No matches.\n")

    def findfiles(self, dir, base, recursive=True):
        # Based on code from idlelib
        import fnmatch
        try:
            names = os.listdir(dir or os.curdir)
        except os.error, msg:
            self.shell.message(msg + "\n")
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

    def get_fontname(self):
        fontsize = self.config.get("pyjama.fontsize")
        font = self.config.get("pyjama.font")
        pangofont = Pango.FontDescription.FromString("%s %d" % (font, fontsize))
        return pangofont

    def update_fonts(self, sender, args):
        pangofont = self.get_fontname()
        if self.shell:
            self.shell.modify_font(pangofont)
        if self.editor:
            self.editor.modify_font(pangofont)

    def increase_fontsize(self, obj, event):
        self.config.set("pyjama.fontsize", 
                        min(self.config.get("pyjama.fontsize") + 1, 36))
        Gtk.Application.Invoke(self.update_fonts)

    def decrease_fontsize(self, obj, event):
        self.config.set("pyjama.fontsize", 
                        max(self.config.get("pyjama.fontsize") - 1, 5))
        Gtk.Application.Invoke(self.update_fonts)

    def about(self, obj, event):
        def invoke(sender, args):
            aboutDialog = Gtk.AboutDialog()
            aboutDialog.DefaultResponse = Gtk.ResponseType.Close
            #aboutDialog.Close += lambda o, e: aboutDialog.Destroy()
            #aboutDialog.SetEmailHook(lambda dialog, email: self.message(email))
            #aboutDialog.SetUrlHook(lambda dialog, link: Gnome.Url.Show(link))
            # 
            #aboutDialog.Artists =""
            aboutDialog.Authors = System.Array[str](["Douglas Blank <dblank@cs.brynmawr.edu>"])
            aboutDialog.Comments = "Scripting Environment\n\nRunning on %s\nMono %s\n" % (self.system, 
                                                                                          self.mono_version)
            aboutDialog.Copyright = "(c) 2011, Institute for Personal Robots in Education"
            #aboutDialog.Documenters
            #aboutDialog.License
            #aboutDialog.Logo
            #aboutDialog.LogoIconName
            aboutDialog.Name = "Pyjama Project"
            #aboutDialog.TranslatorCredits
            aboutDialog.Version = self.version
            aboutDialog.Website = "http://PyjamaProject.org/"
            #aboutDialog.WebsiteLabel
            aboutDialog.WrapLicense = True
            aboutDialog.Run()
            aboutDialog.Destroy()
        Gtk.Application.Invoke(invoke)
    
# Let's start!
version = "0.2.2"
args = sys.argv[1:] or list(System.Environment.GetCommandLineArgs())[1:]
if "--help" in args:
    print
    print "Pyjama Project, Version %s, on %s" % (version, 
                                                 System.Environment.OSVersion.VersionString)
    print "----------------------------------------------------------------------------"
    print "Start pyjama with the following options:"
    print "  pyjama                            Defaults to shell"
    print "  pyjama FILENAMES                  Edits FILENAMES"
    print "  pyjama --shell                    Brings up shell"
    print "  pyjama --editor                   Brings up editor"
    print "  pyjama --exec FILENAMES           Runs FILENAMES standalone, with graphics"
    print "  pyjama --exec --nogui FILENAMES   Runs FILENAMES standalone, no graphics"
    print "  pyjama --version                  Displays the version number (%s)" % version
    print "  pyjama --help                     Displays this message"
    print
    sys.exit(0)
elif "--version" in args:
    print version
    sys.exit(0)
if "--nogui" not in args:
    Gtk.Application.Init()
#------------------------------
try:
    pw = PyjamaProject(args)
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
if "--nogui" not in args:
    Gtk.Application.Run()
    config.save()


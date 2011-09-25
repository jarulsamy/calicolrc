#
# Calico - Scripting Environment
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
# $Id: calico.py -1   $

# Setup environment:
from __future__ import print_function
import sys
import os
import traceback
# First, let's save where we are:
startpath = os.path.abspath(".")
# Next, let's find out where this file is: src/calico.py
calico_fullpath = os.path.abspath(__file__)
# Next, let's add the absolute paths of what we need:
fullpath, basename = os.path.split(calico_fullpath)
user = os.path.expanduser("~/")
# make a .calico directory in user's home dir:
calico_user = os.path.join(user, ".calico")
if not os.path.isdir(calico_user):
    os.path.os.mkdir(calico_user)
calicopath = os.path.join(fullpath, "..") # /src/..
# change here to start
os.chdir(calicopath)
for dir in ['.', './bin/Lib', './bin/DLLs', './modules', './src']:
    path = os.path.abspath(dir)
    sys.path.append(path)

# Needed for adding references to gtk, gdk, pango, and glib:
# Starting with (at least) 2.10.3, gtk is in own directory:
sys.path.append("C:\\Program Files (x86)\\GtkSharp\\2.12\\lib\\gtk-sharp-2.0")

# Get mono_runtime version:
import Microsoft
import System
mtype = System.Type.GetType("Mono.Runtime")
method = mtype.GetMethod("GetDisplayName", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)
mono_runtime = method.Invoke(None, None)

# Bring in DLLs to import from:
import clr
# Mono graphics:
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")
clr.AddReference("pango-sharp")
clr.AddReference("glib-sharp")
#clr.AddReference("gnome-sharp")

# Other DLLs:
clr.AddReference("Microsoft.Scripting")
clr.AddReference("Mono.TextEditor")

# Bring .NET References into IronPython scope:
import Gtk
import Pango
import GLib

from Mono.TextEditor import Highlighting
# Send paths to DLLs:
clr.AddReference("Myro.dll")
import Myro
Myro.initialize_module(calicopath, os.name)

# Initialize text highlighting
path, filename = os.path.split(__file__)
# path = "/.../Calico/src/"
Highlighting.SyntaxModeService.LoadStylesAndModes(
    os.path.join(path, "..", "bin", "SyntaxModes"))

# Import pure-Python modules:
import tempfile
import re
import glob

# Calico imports:
from engine import EngineManager
from config import config
from utils import _, Chat

# Setup Runtime environment:
def handle_exception(arg):
    if pw.shell:
        Gtk.Application.Invoke(pw.shell.stop_running)
        Gtk.Application.Invoke(lambda s,a: pw.shell.message(
                str(arg.ExceptionObject).split("\n")[0]))
        Gtk.Application.Invoke(lambda s,a: pw.shell.message("[Script has stopped------]"))

args = sys.argv[1:] or list(System.Environment.GetCommandLineArgs())[1:]
# Turn on Unhandled Exception Handled:
# EXCEPTION HANDLER
if "--debug" not in args:
    GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes

class CalicoProject(object):
    """
    This class is meant to be created as a singleton instance
    to hold all of the components of one user together.
    """
    def __init__(self, argv):
        """
        Constructor for the singleton Calico instance. argv is the
        command-line files and flags.
        """
        self.last_error = ""
        self.actionHandlers = []
        self.debug = False
        self.trace = False
        self.shell = None
        self.editor = None
        self.chat = None
        self.connection = None
        self.language = "python"         # will be overridden below
        self.version = version           # from global variable
        self.config = config             # from global variable
        self.mono_runtime = mono_runtime # from global variable
        self.startpath = startpath       # from global variable
        self.calico_user = calico_user   # from global variable
        self.calico_root = os.path.abspath(".")
        self.system = System.Environment.OSVersion.VersionString
        self.gui = True
        self.standalone = False
        self.indent_string = "    "
        self.languages = self.get_registered_languages()
        self.engine = EngineManager(self)
        for lang in self.languages:
            language = self.languages[lang]
            lclass = language.get_engine_class()
            if lclass:
                self.engine.register(lclass)
        # Set up all of the engines:
        self.engine.setup()
        self.engine.start()
        # Create the plugin handlers:
        sys.path.append(os.path.abspath("plugins"))
        self.plugins = {}
        for filename in glob.glob("plugins/*.py"):
            path, basename = os.path.split(filename)
            base, ext = os.path.splitext(basename)
            if base == "__init__":
                continue
            try:
                module = __import__(base)
                plugin = module.make_plugin(self)
            except:
                plugin = None
                traceback.print_exc()
                print("Could not load plugin '%s'; skipping" % filename)
            if plugin:
                self.plugins[base] = plugin
        # Ok, done with initialization, let's go back to where we came
        os.chdir(self.startpath)
        request_shell = True # needed because running would hang otherwise; and will be one window soon
        request_editor = False
        request_chat = False
        files = []
        # Handle command line args:
        for arg in argv:
            if arg == "--shell":
                request_shell = True
            elif arg == "--editor":
                request_editor = True
            elif arg == "--chat":
                request_chat = True
            elif arg == "--nogui":
                self.gui = False
                request_shell = True
            elif arg == "--exec":
                self.standalone = True
                request_shell = True
            elif arg.startswith("--lang") or arg.startswith("--language"):
                flag, lang = arg.split("=", 1)
                self.language = lang.strip().lower()
            elif arg == "--debug":
                self.debug = True
            elif arg == "--trace":
                self.trace = True
            else:
                files.append(os.path.abspath(arg))
                request_editor = True
        if files == [] and not request_editor and not request_chat:
            # a default action, given nothing else requested
            request_shell = True
        # Open requested windows
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
        if request_chat:
            if self.standalone:
                pass # no chat in standalone mode
            else:
                from chat import ChatWindow
                self.chat = ChatWindow(self)

    def get_registered_languages(self):
        """
        Return a dictionary of Language objects for each language
        currently available. The language name (lowercase) is the
        key in the dictionary pointing to a Language object.
        """
        results = {}
        languages = glob.glob("languages/*/*.py")
        for filename in languages: 
            path, basename = os.path.split(filename)
            base, ext = os.path.splitext(basename)
            if (base in self.config.get("calico.languages") or
                "All" in self.config.get("calico.languages")):
                if base in self.config.get("calico.languages_ignore"):
                    continue
                sys.path.append(os.path.abspath(path))
                try:
                    import_name, ext = os.path.basename(filename).rsplit(".")
                    exec("import %s as LanguageModule" % import_name)
                    lang = LanguageModule.register_language()
                    results[lang.language] = lang
                except:
                    traceback.print_exc()
                    self.error_message(_("Cannot load language file '%s'") % filename)
        return results

    def error_message(self, message):
        print(message, file=sys.stderr)

    def load(self, filename):
        """
        Load a program. The filename extension will be used to find
        the appropriate language engine.
        """
        # force into a Python string:
        filename = str(filename.ToString())
        base, ext = filename.rsplit(".")
        for name in self.languages:
            if ext in self.languages[name].extensions:
                if self.shell:
                    self.shell.change_to_lang(name)
                if name in self.engine:
                    return self.engine[name].execute_file(filename)
        raise AttributeError(_("unknown file extension: '%s'") % filename)

    def blast(self, mfrom, type, filename, code):
        """
        Run a program.
        """
        language = self.get_language_from_filename(filename)
        q = None
        if type == "execute":
            q = _("Do you want to run the script?")
        elif type == "edit":
            q = _("Do you want to open the script?")
        md = Gtk.MessageDialog(self.get_window(),
                              Gtk.DialogFlags.DestroyWithParent,
                              Gtk.MessageType.Question,
                              Gtk.ButtonsType.YesNo,
                              _("You have received a blast from '%(mfrom)s'.\n%(q)s")
                              % {"mfrom": mfrom, "q": q})
        def invoke(sender, args):
            result = md.Run()
            md.Destroy()
            if result == int(Gtk.ResponseType.Yes):
                if type == "execute":
                    self.setup_shell()
                    # FIXME: execute in a locked-down, secure environment
                    self.shell.execute(code, language)
                elif type == "edit":
                    self.setup_editor()
                    path = tempfile.gettempdir()
                    temp = os.path.abspath(os.path.join(path, filename))
                    fp = open(temp, "w")
                    fp.write(code)
                    fp.close()
                    self.editor.select_or_open(temp, language=language)
        Gtk.Application.Invoke(invoke)

    def get_language_from_filename(self, filename):
        """
        Get the language string (lower-case) based on a filename
        extension.
        """
        base, ext = filename.rsplit(".")
        for name in self.languages:
            if ext in self.languages[name].extensions:
                return name
        return "python" # FIXME: default language come from config

    def Print(self, *args, **kwargs):
        """
        Short-hand for message, but makes sure that the shell is up
        and running.
        """
        message = " ".join([str(m) for m in args])
        end = kwargs.get("end", "\n")
        file = kwargs.get("file", None)
        tag = kwargs.get("tag", "black")
        if self.shell:
            self.shell.message(message, tag, end=end)
        else:
            print(message, end=end) # FIXME: file=file?

    def get_window(self):
        if self.chat:
            return self.chat.window
        elif self.shell:
            return self.shell.window
        elif self.editor:
            return self.editor.window
        else:
            return None

    def on_close(self, what):
        if what in ["shell", "all"]:
            if self.shell:
                self.shell.window.Destroy()
                self.shell = None 
        if what in ["editor", "all"]:
            if self.editor:
                self.editor.window.Destroy()
                self.editor = None 
        if what in ["chat", "all"]:
            if self.chat:
                self.chat.window.Destroy()
                self.chat = None
        if ((self.editor is None) and
            (self.shell is None) and
            (self.chat is None)):
            # Clean up, save settings:
            self.config.get("calico.recent_files")[:] = self.config.get("calico.recent_files")[-10:]
            config.save()
            # Close connections:
            if pw and pw.connection:
                pw.connection.close()
            for engine in self.engine.get_languages():
                self.engine[engine].stop()
            Gtk.Application.Quit()

    def setup_chat(self, *args, **kwargs):
        if self.chat is None:
            from chat import ChatWindow
            self.chat = ChatWindow(self)
        def invoke(sender, args):
            self.chat.window.Present()
        Gtk.Application.Invoke(invoke)

    def setup_shell(self, *args, **kwargs):
        if self.shell is None:
            from shell import ShellWindow
            self.shell = ShellWindow(self)
        def invoke(sender, args):
            self.shell.window.Present()
        Gtk.Application.Invoke(invoke)

    def setup_editor(self, *args, **kwargs):
        if self.editor is None:
            from editor import EditorWindow
            self.editor = EditorWindow(self)
        def invoke(sender, args):
            self.editor.window.Present()
        Gtk.Application.Invoke(invoke)

    def update_status(self):
        def invoke(sender, args):
            if self.shell:
                self.shell.update_status()
            if self.editor:
                self.editor.update_status()
            if self.chat:
                self.chat.update_status()
        Gtk.Application.Invoke(invoke)

    def grep(self, pattern, file_pat="*.py", dir=".", 
             flags=re.IGNORECASE, recursive=True):
        """
        grep("FIXME")
        grep("import sys", "*.py", ".", recursive=True)
        flags = 0 # re.IGNORECASE
        """
        # Based on code from idlelib
        self.shell.message(_("Searching for %r in %s/%s...") %
                    (pattern, os.path.abspath(dir), file_pat), "black")
        prog = re.compile(pattern, flags)
        list = self.findfiles(dir, file_pat, recursive)
        list.sort()
        hits = 0
        for fn in list:
            try:
                f = open(fn)
            except:
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
                        self.shell.message(_("  File \"%s\", line %d, %s\"") %
                                         (fn, lineno, line.strip()))
                        hits = hits + 1
        if hits:
            if hits == 1:
                s = ""
            else:
                s = "es"
            self.shell.message(_("Found %d match%s. Right-click file to open.") % (hits, s), "black")
        else:
            self.shell.message(_("No matches found."), "black")

    def findfiles(self, dir, base, recursive=True):
        # Based on code from idlelib
        import fnmatch
        try:
            names = os.listdir(dir or os.curdir)
        except os.error, msg:
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
        fontsize = self.config.get("calico.fontsize")
        font = self.config.get("calico.font")
        pangofont = Pango.FontDescription.FromString("%s %d" % (font, fontsize))
        return pangofont

    def select_font(self, obj, event):
        def invoke(s, e):
            d = Gtk.FontSelectionDialog("Select Calico Font")
            d.SetFontName(str(self.get_fontname()))
            response = d.Run()
            if response == int(Gtk.ResponseType.Ok):
                fontName, fontSize = d.FontName.rsplit(" ", 1)
                self.config.set("calico.font", fontName)
                self.config.set("calico.fontsize", int(fontSize))
                if self.shell:
                    self.shell.set_font()
                if self.editor:
                    self.editor.set_font()
                if self.chat:
                    self.chat.set_font()
            d.Destroy()
        Gtk.Application.Invoke(invoke)

    def increase_fontsize(self, obj, event):
        def invoke(sender, args):
            pangofont = self.get_fontname()
            fontName, fontSize = pangofont(" ", 1)
            self.config.set("calico.font", fontName)
            self.config.set("calico.fontsize", min(int(fontSize) + 1, 36))
            if self.shell:
                self.shell.set_font()
            if self.editor:
                self.editor.set_font()
            if self.chat:
                self.chat.set_font()
        Gtk.Application.Invoke(invoke)

    def decrease_fontsize(self, obj, event):
        def invoke(sender, args):
            pangofont = self.get_fontname()
            fontName, fontSize = pangofont(" ", 1)
            self.config.set("calico.font", fontName)
            self.config.set("calico.fontsize", max(int(fontSize) - 1, 5))
            if self.shell:
                self.shell.set_font()
            if self.editor:
                self.editor.set_font()
            if self.chat:
                self.chat.set_font()
        Gtk.Application.Invoke(invoke)

    def about(self, obj, event):
        def invoke(sender, args):
            aboutDialog = Gtk.AboutDialog()
            aboutDialog.DefaultResponse = Gtk.ResponseType.Close
            #aboutDialog.SetEmailHook(lambda dialog, email: self.message(email))
            #aboutDialog.SetUrlHook(lambda dialog, link: Gnome.Url.Show(link))
            #aboutDialog.Artists =""
            aboutDialog.Authors = System.Array[str](["Douglas Blank <dblank@cs.brynmawr.edu>"])
            aboutDialog.Comments = (_("Scripting Environment") + "\n\n" + _("Running on %s") + "\nMono %s\n") % (self.system,
                                                                                          self.mono_runtime)
            aboutDialog.Copyright = _("(c) 2011, Institute for Personal Robots in Education")
            #aboutDialog.Documenters
            #aboutDialog.License
            #aboutDialog.Logo
            #aboutDialog.LogoIconName
            aboutDialog.Name = _("Calico Project")
            #aboutDialog.TranslatorCredits
            aboutDialog.Version = self.version
            aboutDialog.Website = "http://CalicoProject.org/"
            #aboutDialog.WebsiteLabel
            aboutDialog.WrapLicense = True
            aboutDialog.Run()
            aboutDialog.Destroy()
        Gtk.Application.Invoke(invoke)

    def login(self, user=None, password=None, debug=False):
        self.connection = Chat(self, user, password, debug)

    def register(self, user, email, password, keyword, debug=False):
        # Use a restricted acces account, to talk to admin
        ch = Chat(self, "testname", "password", debug)
        System.Threading.Thread.Sleep(5000)
        ch.send("admin", """register
    email: %s
    username: %s
    password: %s
    keyword: %s
    """ % (email, user, password, keyword))
        # send a special message to create account
        # wait for response:
        messages = ch.receive()
        count = 0
        while len(messages) == 0 and count < 10:
            messages = ch.receive()
            System.Threading.Thread.Sleep(1000)
            print(_("   waiting for confirmation..."))
            count += 1
        print(_("received messages:"))
        for message in messages:
            print(message[1])
            print()
        ch.close()

    def login_dialog(self, window):
        def invoke(sender, args):
            dialog = Gtk.Dialog(_("Calico Login"), window,
                                Gtk.DialogFlags.DestroyWithParent)
            dialog.Modal = True
            items = [(_("Username"), False), (_("Password"), True)]
            table = Gtk.Table(len(items), 2, False)
            row = 0
            data = {}
            for item, password in items:
                label = Gtk.Label("%s:" % item)
                label.Justify = Gtk.Justification.Right
                entry = Gtk.Entry()
                if password:
                    entry.Visibility = False
                data[item] = entry
                table.Attach(label, 0, 1, row, row + 1,
                    Gtk.AttachOptions.Expand, Gtk.AttachOptions.Expand, 0, 0)
                table.Attach(entry, 1, 2, row, row + 1)
                row += 1
            expand, fill, padding = True, True, 0
            dialog.VBox.PackStart(table, expand, fill, padding)
            dialog.AddButton(_("Login"), Gtk.ResponseType.Apply)
            dialog.AddButton(_("Cancel"), Gtk.ResponseType.Cancel)
            dialog.ShowAll()
            response = dialog.Run()
            if response == int(Gtk.ResponseType.Apply):
                self.login(data[_("Username")].Text,
                           data[_("Password")].Text)
            dialog.Destroy()
        Gtk.Application.Invoke(invoke)

    def register_dialog(self, window):
        def invoke(sender, args):
            dialog = Gtk.Dialog(_("Calico Registration"), window,
                                Gtk.DialogFlags.DestroyWithParent)
            dialog.Modal = True
            items = [(_("Username"), False),
                     (_("Email"), False),
                     (_("Password"), True),
                     (_("Password again"), True),
                     (_("Course keyword"), False)]
            table = Gtk.Table(len(items), 2, False)
            row = 0
            data = {}
            for item, password in items:
                label = Gtk.Label("%s: " % item)
                label.Justify = Gtk.Justification.Right
                entry = Gtk.Entry()
                if password:
                    entry.Visibility = False
                data[item] = entry
                table.Attach(label, 0, 1, row, row + 1,
                    Gtk.AttachOptions.Expand, Gtk.AttachOptions.Expand, 0, 0)
                table.Attach(entry, 1, 2, row, row + 1)
                row += 1
            expand, fill, padding = True, True, 0
            dialog.VBox.PackStart(table, expand, fill, padding)
            dialog.AddButton(_("Register"), Gtk.ResponseType.Apply)
            dialog.AddButton(_("Cancel"), Gtk.ResponseType.Cancel)
            dialog.ShowAll()
            response = dialog.Run()
            if response == int(Gtk.ResponseType.Apply):
                # FIXME: check passwords, report and repeat if necessary
                self.register(data[_("Username")].Text,
                              data[_("Email")].Text,
                              data[_("Password")].Text,
                              data[_("Course keyword")].Text)
            dialog.Destroy()
            # FIXME: report results
        Gtk.Application.Invoke(invoke)

    def alert(self, message):
        def invoke(sender, args):
            md = Gtk.MessageDialog(self.get_window(),
                                  Gtk.DialogFlags.DestroyWithParent,
                                  Gtk.MessageType.Info,
                                  Gtk.ButtonsType.Ok,
                                  message)
            md.Run()
            md.Destroy()
        Gtk.Application.Invoke(invoke)

    def on_action(self, action, **data):
        """
        Fires on actions throughout Calico.
        """
        for actionHandler in self.actionHandlers:
            actionHandler(action, **data)

# Let's start!
version = "1.1.0beta"
if "--help" in args:
    print()
    print(_("Calico Project, Version %s, on %s") % (version,
                                                 System.Environment.OSVersion.VersionString))
    print("  " + _("Using Mono runtime version %s" % mono_runtime))
    print("----------------------------------------------------------------------------")
    print(_("Start calico with the following options:"))
    print(_("  StartCalico                            Defaults to shell"))
    print(_("  StartCalico FILENAME:LINE ...          Edits FILENAMEs, positioned on LINEs"))
    print(_("  StartCalico --shell                    Brings up shell window"))
    print(_("  StartCalico --editor                   Brings up editor window"))
    print(_("  StartCalico --lang=LANGUAGE            Sets default language (python, etc.)"))
    print(_("  StartCalico --chat                     Brings up chat window"))
    print(_("  StartCalico --exec FILENAMEs           Run FILENAMEs standalone with graphics"))
    print(_("  StartCalico --exec --nogui FILENAMEs   Run FILENAMEs standalone no graphics"))
    print(_("  StartCalico --version                  Displays the version number (%s)" % version))
    print(_("  StartCalico --help                     Displays this message"))
    print(_("  StartCalico --debug                    Puts Calico in debugging mode"))
    print(_("  StartCalico --trace                    Puts Calico in tracing mode"))
    print()
    sys.exit(0)
elif "--version" in args:
    print(version)
    sys.exit(0)

messagesLocked = False

def handleMessages(sender, args):
    """
    Callback for handling additional calico starts. This is run by the
    main Calico program to read the messages sent by other attempts to
    start.
    """
    global messagesLocked
    if not messagesLocked:
        messagesLocked = True
        messages = os.path.join(calico_user, "messages")
        os.chdir(startpath)
        execute = False
        for word in file(messages, "r"):
            word = word.strip()
            if word.startswith("--"):
                if word == "--exec":
                    execute = True
                elif word == "--chat":
                    Gtk.Application.Invoke(lambda s,a: pw.setup_chat())
                elif word == "--editor":
                    execute = False
                    Gtk.Application.Invoke(lambda s,a: pw.setup_editor())
                elif word == "--shell":
                    Gtk.Application.Invoke(lambda s,a: pw.setup_shell())
                continue
            if word:
                filename = os.path.abspath(word)
                if execute:
                    import time
                    Gtk.Application.Invoke(lambda s,a: pw.setup_shell())
                    Gtk.Application.Invoke(lambda s,a: pw.load(filename))
                else:
                    Gtk.Application.Invoke(lambda s,a: pw.setup_editor())
                    Gtk.Application.Invoke(lambda s,a: pw.editor.select_or_open(filename))
        fp = file(messages, "w")
        fp.close()
        messagesLocked = False

if "--trace" in args:
    from debugger import Debugger
    Debugger(None, interactive=False, show_trace=True).set_trace()

#################################################
# Single Instance Application
current = System.Diagnostics.Process.GetCurrentProcess()
alreadyRunning = False
for process in System.Diagnostics.Process.GetProcessesByName("mono"):
    if process.Id == current.Id:
        continue
    for module in process.Modules:
        if module.ModuleName in ["Myro.dll", "mono"]:
            alreadyRunning = True
messages = os.path.join(calico_user, "messages")
if alreadyRunning:
    # Not allowed! We'll send command line to running Calico through
    # message file. Append args to command line:
    print("Calico is already running...")
    fp = file(messages, "a")
    fp.write("\n".join(args) + "\n")
    fp.close()
    # Exit; message has been sent
    sys.exit(0)
else:
    # We are the "server"; clean the messages file:
    fp = file(messages, "w")
    fp.close()
    # set up a watcher to check for messages
    watcher = System.IO.FileSystemWatcher()
    watcher.Path = calico_user
    watcher.Filter = "messages"
    watcher.NotifyFilter = System.IO.NotifyFilters.LastWrite
    watcher.Changed += handleMessages
    watcher.EnableRaisingEvents = True
    # and continue loading...
# end of Single Instance logic
#################################################

if "--nogui" not in args:
    # FIXME: thread safety:
    # http://developer.gnome.org/gtk-faq/stable/x481.html
    #g_thread_init(NULL);
    #gdk_threads_init();
    Gtk.Application.Init()
#------------------------------
try:
    pw = CalicoProject(args)
except:
    traceback.print_exc()
    sys.exit()
#------------------------------
if "--nogui" not in args:
    Gtk.Application.Run()
sys.exit(0)

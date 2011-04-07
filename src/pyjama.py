#
# Pyjama - Scripting Environment
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
# $Id$

# Setup environment:
import sys
import os
import traceback
# First, let's save where we are:
startpath = os.path.abspath(".")
# Next, let's find out where this file is: src/pyjama.py
pyjama_fullpath = os.path.abspath(__file__)
# Next, let's add the absolute paths of what we need:
fullpath, basename = os.path.split(pyjama_fullpath)
user = os.path.expanduser("~/")
# make a .pyjama directory in user's home dir:
pyjama_user = os.path.join(user, ".pyjama")
if not os.path.isdir(pyjama_user):
    os.path.os.mkdir(pyjama_user)
pyjamapath = os.path.join(fullpath, "..") # /src/..
# change here to start
os.chdir(pyjamapath)
for dir in ['.', './bin/Lib', './bin/DLLs', './modules', './src']:
    path = os.path.join(fullpath, dir)
    sys.path.append(path)
sys.path.append(os.path.abspath("modules"))

# Add some paths for Windows:
import Microsoft
import System
mono_version = "?.?.?"
mtype = System.Type.GetType("Mono.Runtime")
method = mtype.GetMethod("GetDisplayName", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)
mono_runtime = method.Invoke(None, None)
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

# Initialize text highlighting
path, filename = os.path.split(__file__)
# path = "/.../Pyjama/src/"
Highlighting.SyntaxModeService.LoadStylesAndModes(
    os.path.join(path, "..", "bin", "SyntaxModes"))

# Import pure-Python modules:
import tempfile
import re
import glob

# Pyjama imports:
from engine import EngineManager
from config import config
from utils import _, Chat

# Setup Runtime environment:
def handle_exception(arg):
    if pw.shell:
        Gtk.Application.Invoke(pw.shell.stop_running)
        Gtk.Application.Invoke(lambda s,a: pw.shell.message("Crashed!"))

# Turn on Unhandled Exception Handled:
# EXCEPTION HANDLER
GLib.ExceptionManager.UnhandledException += handle_exception

# Define local functions and classes

class PyjamaProject(object):
    """
    This class is meant to be created as a singleton instance
    to hold all of the components of one user together.
    """
    def __init__(self, argv):
        """
        Constructor for the singleton Pyjama instance. argv is the
        command-line files and flags.
        """
        self.last_error = ""
        self.actionHandlers = []
        self.debug = False
        self.shell = None
        self.editor = None
        self.chat = None
        self.connection = None
        self.version = version           # from global variable
        self.config = config             # from global variable
        self.mono_runtime = mono_runtime # from global variable
        self.startpath = startpath       # from global variable
        self.pyjama_user = pyjama_user   # from global variable
        self.pyjama_root = os.path.abspath(".")
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
                print "Could not load plugin '%s'; skipping" % filename
            if plugin:
                self.plugins[base] = plugin
        # Ok, done with initialization, let's go back to where we came
        os.chdir(self.startpath)
        request_shell = False
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
            elif arg == "--debug":
                self.debug = True
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
        sys.path.append(os.path.abspath("languages"))
        results = {}
        languages = glob.glob("languages/*.py")
        for filename in languages: 
            path, basename = os.path.split(filename)
            base, ext = os.path.splitext(basename)
            if (base in self.config.get("pyjama.languages") or
                "All" in self.config.get("pyjama.languages")):
                if base in self.config.get("pyjama.languages_ignore"):
                    continue
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
        print >> sys.stderr, message

    def load(self, filename):
        """
        Load a program. The filename extension will be used to find
        the appropriate language engine.
        """
        # force into a Python string:
        filename = str(filename.ToString())
        for name in self.languages:
            if filename.endswith("." + self.languages[name].extension):
                if self.shell:
                    self.shell.change_to_lang(name)
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
                              _("You have received a blast from '%s'.\n%s")
                              % (mfrom, q))
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
        for name in self.languages:
            if filename.endswith("." + self.languages[name].extension):
                return name
        return "python" # FIXME: default language come from config

    def Print(self, message, tag="green"):
        """
        Short-hand for message, but makes sure that the shell is up
        and running.
        """
        if self.shell:
            self.shell.message(message, tag)
        else:
            print message

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
            self.config.get("pyjama.recent_files")[:] = self.config.get("pyjama.recent_files")[-10:]
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
        fontsize = self.config.get("pyjama.fontsize")
        font = self.config.get("pyjama.font")
        pangofont = Pango.FontDescription.FromString("%s %d" % (font, fontsize))
        return pangofont

    def increase_fontsize(self, obj, event):
        self.config.set("pyjama.fontsize",
                        min(self.config.get("pyjama.fontsize") + 1, 36))
        def invoke(sender, args):
            pangofont = self.get_fontname()
            if self.shell:
                self.shell.increase_font_size(pangofont)
            if self.editor:
                self.editor.increase_font_size(pangofont)
            if self.chat:
                self.chat.increase_font_size(pangofont)
        Gtk.Application.Invoke(invoke)

    def decrease_fontsize(self, obj, event):
        self.config.set("pyjama.fontsize",
                        max(self.config.get("pyjama.fontsize") - 1, 5))
        def invoke(sender, args):
            pangofont = self.get_fontname()
            if self.shell:
                self.shell.decrease_font_size(pangofont)
            if self.editor:
                self.editor.decrease_font_size(pangofont)
            if self.chat:
                self.chat.decrease_font_size(pangofont)
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
            aboutDialog.Name = _("Pyjama Project")
            #aboutDialog.TranslatorCredits
            aboutDialog.Version = self.version
            aboutDialog.Website = "http://PyjamaProject.org/"
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
            print _("   waiting for confirmation...")
            count += 1
        print _("received messages:")
        for message in messages:
            print message[1]
            print
        ch.close()

    def login_dialog(self, window):
        def invoke(sender, args):
            dialog = Gtk.Dialog(_("Pyjama Login"), window,
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
            dialog = Gtk.Dialog(_("Pyjama Registration"), window,
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
        Fires on actions throughout Pyjama.
        """
        for actionHandler in self.actionHandlers:
            actionHandler(action, **data)


# Let's start!
version = "0.4.5"
args = sys.argv[1:] or list(System.Environment.GetCommandLineArgs())[1:]
if "--help" in args:
    print
    print _("Pyjama Project, Version %s, on %s") % (version,
                                                 System.Environment.OSVersion.VersionString)
    print "----------------------------------------------------------------------------"
    print _("Start pyjama with the following options:")
    print _("  pyjama                            Defaults to shell")
    print _("  pyjama FILENAME:LINE ...          Edits FILENAMEs, positioned on LINEs")
    print _("  pyjama --shell                    Brings up shell window")
    print _("  pyjama --editor                   Brings up editor window")
    print _("  pyjama --chat                     Brings up chat window")
    print _("  pyjama --exec FILENAMEs           Runs FILENAMEs standalone, with graphics")
    print _("  pyjama --exec --nogui FILENAMEs   Runs FILENAMEs standalone, no graphics")
    print _("  pyjama --version                  Displays the version number (%s)" % version)
    print _("  pyjama --help                     Displays this message")
    print _("  pyjama --debug                    Puts Pyjama in debugging mode")
    print
    sys.exit(0)
elif "--version" in args:
    print version
    sys.exit(0)

messagesLocked = False

def handleMessages(sender, args):
    """
    Callback for handling additional pyjama starts. This is run by the
    main Pyjama program to read the messages sent by other attempts to
    start.
    """
    global messagesLocked
    if not messagesLocked:
        messagesLocked = True
        messages = os.path.join(pyjama_user, "messages")
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

#################################################
# Single Instance Application
messages = os.path.join(pyjama_user, "messages")
(mutex, locked) = System.Threading.Mutex(True, "PyjamaProject/%s" % System.Environment.UserName, None)
if locked:
    # We are the "server"; clean the messages file:
    fp = file(messages, "w")
    fp.close()
    # set up a watcher to check for messages
    watcher = System.IO.FileSystemWatcher()
    watcher.Path = pyjama_user
    watcher.Filter = "messages"
    watcher.NotifyFilter = System.IO.NotifyFilters.LastWrite
    watcher.Changed += handleMessages
    watcher.EnableRaisingEvents = True
    # and continue loading...
else:
    # Not allowed! We'll send command line to running Pyjama through
    # message file. Append args to command line:
    fp = file(messages, "a")
    fp.write("\n".join(args) + "\n")
    fp.close()
    # Exit; message has been sent
    sys.exit(0)
# end of Single Instance logic
#################################################

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
sys.exit(0)

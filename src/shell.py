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
# $Id: $

# Bring .NET References into IronPython scope:
from Mono.TextEditor import TextEditor, TextEditorOptions
import Gtk, Gdk, Pango, GLib
import System
import System.Threading

# Pyjama modules:
from window import Window, MyWindow
from utils import _, CustomStream, MUTEX, ConsoleStream

# Pure-Python modules:
import traceback
import sys, os
import re

# Local classes:
class History(object):
    def __init__(self):
        self.history = [""]
        self.position = 0

    def up(self):
        if self.position > 0:
            self.position -= 1
        return self.history[self.position]

    def down(self):
        if self.position < len(self.history) - 1:
            self.position += 1
        return self.history[self.position]

    def update(self, text):
        self.history[self.position] = text

    def add(self, text):
        if self.history[-1] != text: # different
            self.history.append(text)
        self.position = len(self.history) - 1

    def last(self, text):
        # turns space into last command
        if len(self.history) > 1 and self.history[-2] == text:
            pass # same, skip it!
        else:
            self.history[-1] = text
        self.position = len(self.history) - 1

class Shell(object):
    def __init__(self, pyjama, files):
        self.pyjama = pyjama
        self.pyjama.engine.set_redirects(ConsoleStream(), 
                                         ConsoleStream("red"), None)
        for file in files:
            self.execute_file(file, self.pyjama.get_language_from_filename(file))

    def message(self, text):
        print text,

    def execute_file(self, filename, language):
        self.message("Loading file '%s'...\n" % filename)
        self.pyjama.engine[language].execute_file(filename)
        self.message("Done loading file.\n")

class ShellWindow(Window):
    def __init__(self, pyjama):
        self.pyjama = pyjama
        self.executeThread = None
        self.language = "python"
        self.lang_manager = None
        self.window = MyWindow(_("Pyjama Shell"))
        self.window.set_on_key_press(self.on_key_press)
        self.window.SetDefaultSize(700, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.history_textview = Gtk.TextView()
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File",
                 [("Open Script...", Gtk.Stock.Open, 
                   None, self.on_open_file),
                  None,
                  ] +
                  self.make_new_file_menu() +
                  [None,
                  ("Close", Gtk.Stock.Close,
                   None, self.on_close),
                  ("Quit", Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                ("_Edit", [
                    ("_Copy", None, None, None),
                    ("_Paste", None, None, None),
                    ("Cut", None, None, None),
                    ("Select all", Gtk.Stock.SelectAll, None, None),
                          ]),
                ("She_ll", self.make_language_menu()),
                ("Windows", [
                    ("Editor", None, "F6", self.pyjama.setup_editor),
                    ("Shell", None, "F7", self.pyjama.setup_shell),
                    ]),
                ("O_ptions", [
                    ("Make font larger", None, None, self.pyjama.increase_fontsize),
                    ("Make font smaller", None, None, self.pyjama.decrease_fontsize),
                    ]),
                ("_Help", [
                    ("About the Pyjama Project", Gtk.Stock.About, None, self.pyjama.about),
                    ]),
                ]
        toolbar = [(Gtk.Stock.New, self.on_new_file),
                   (Gtk.Stock.Open, self.on_open_file),
                   (Gtk.Stock.Apply, self.on_run),
                   (Gtk.Stock.Stop, self.on_stop),
                   ]
        self.make_gui(menu, toolbar)
        Gtk.Application.Invoke(self.stop_running)
        self.history = History()
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Show()
        self.statusbar.Push(0, "Language: Python")
        self.command_area = Gtk.HBox()
        alignment = Gtk.Alignment( 0.5, 0.0, 0, 0)
        self.prompt = Gtk.Label("python>")
        alignment.Add(self.prompt)
        self.command_area.PackStart(alignment, False, False, 0)
        self.scrolled_window = Gtk.ScrolledWindow()
        self.command_area.PackStart(self.scrolled_window, True, True, 0)
        self.scrolled_window.ShadowType = Gtk.ShadowType.Out
        self.scrolled_window.HeightRequest = 20

        options = TextEditorOptions()
        self.textview = TextEditor(Options=options)
        self.textview.Options.ShowFoldMargin = False
        self.textview.Options.ShowIconMargin = False
        self.textview.Options.ShowInvalidLines = False
        self.textview.Options.ShowLineNumberMargin = True
        self.textview.Options.TabsToSpaces = True
        self.textview.Options.HighlightMatchingBracket = True
        self.textview.Document.MimeType = "text/x-%s" % self.language

        self.textview.Show()
        #self.textview.ModifyFont(self.pyjama.get_fontname())
        self.scrolled_window.AddWithViewport(self.textview)
        self.results = Gtk.ScrolledWindow()
        for color in ["red", "blue", "purple", "black"]:
            tag = Gtk.TextTag(color)
            if color in ["red"]:
                tag.Weight = Pango.Weight.Bold
            tag.Foreground = color 
            self.history_textview.Buffer.TagTable.Add(tag)
        self.history_textview.ModifyFont(self.pyjama.get_fontname())
        self.history_textview.PopulatePopup += self.popup
        self.history_textview.WrapMode = Gtk.WrapMode.Char
        self.history_textview.Editable = False
        self.results.Add(self.history_textview)
        self.results.Show()
        self.vpane = Gtk.VPaned()
        self.vpane.Pack1(self.command_area, True, True)
        self.vpane.Pack2(self.results, True, True)
        # initialize
        self.window.Add(self.vbox)
        self.vbox.PackStart(self.menubar, False, False, 0)
        self.vbox.PackStart(self.toolbar, False, False, 0)
        self.vbox.PackStart(self.vpane, True, True, 0)
        self.vbox.PackEnd(self.statusbar, False, False, 0)
        self.window.ShowAll()
        # Set this Python's stderr:
        # EXCEPTION HANDLER
        stdout = CustomStream(self.history_textview, "black")
        sys.stderr = CustomStream(self.history_textview, "red")
        self.pyjama.engine.set_redirects(stdout, sys.stderr, None)
        self.textview.GrabFocus()
        self.change_to_lang(self.language)
        # Setup clipboard stuff:
        self.clipboard = Gtk.Clipboard.Get(
              Gdk.Atom.Intern("CLIPBOARD", True))
        self.message("Pyjama Project %s\n" % self.pyjama.version)
        self.message(("-" * 50) + "\n")
    
    def modify_font(self, font):
        #self.textview.ModifyFont(font)
        self.history_textview.ModifyFont(font)

    def on_copy(self, obj, event):
        focused = self.window.Focus
        if focused:
            focused.CopyClipboard(self.clipboard)

    def on_cut(self, obj):
        focused = self.window.Focus
        if focused:
            focused.CutClipboard(self.clipboard, True) # FIXME: editable?

    def on_paste(self, obj):
        focused = self.window.Focus
        if focused:
            focused.PasteClipboard(self.clipboard, None, True) # FIXME: editable?
    
    def make_language_menu(self):
        languages = []
        num = 1
        for lang in sorted(self.pyjama.engine.get_languages()):
            if self.pyjama.engine[lang].text_based:
                languages.append(["Change to %s" % lang.title(), 
                    None, "<control>%d" % num, 
                    lambda obj, event, lang=lang: self.change_to_lang(lang)])
                num += 1
        return ([("Run", Gtk.Stock.Apply, "F5", self.on_run),
                 ("Clear", None, "<control>Delete", self.clear),
                 ("Restart shell", None, "<control>r", self.reset_shell),
                 ("Stop script", None, "Escape", self.on_stop),
                 None] + 
                languages)

    def update_gui(self):
        self.set_title(_("%s - Pyjama Shell") % self.language.title())
        self.prompt.Text = "%-6s>" % self.language
        self.statusbar.Pop(0)
        self.statusbar.Push(0, _("Language: %s") % self.language.title())

    def on_key_press(self, event, force=False):
        # FIXME: this should be handled in textview, but haven't
        # figured out how to overload just it.
        # So, this currently handles the keys for the whole window.
        #if event is not None:
        #    self.message(str(event.Key))
        if event is None or str(event.Key) == "Return":
            # if cursor in middle, insert a Return
            caret = self.textview.Caret
            line = caret.Line
            line_count = self.textview.Document.LineCount
            if line != line_count - 1 and not force:
                return False
            # else, execute text
            # extra line at end signals ready_to_execute:
            text = self.textview.Document.Text
            if text == "":
                return True # nothing to do, but handled
            elif self.ready_for_execute(text) or force:
                self.history.last(text.rstrip())
                self.history.add("")
                self.execute(text.rstrip(), self.language)
                def invoke(sender, args):
                    self.textview.Document.Text = ''
                    self.textview.GrabFocus()
                    self.textview.Caret.Line = 0
                    self.textview.Caret.Column = 0
                Gtk.Application.Invoke(invoke)
                return True
        elif str(event.Key) == "Up":
            text = self.textview.Document.Text
            caret = self.textview.Caret
            line = caret.Line
            if line == 0:
                self.history.update(text.rstrip())
                text = self.history.up()
                def invoke(sender, args):
                    self.textview.Document.Text = text
                    self.textview.GrabFocus()
                    self.textview.Caret.Line = 0
                    self.textview.Caret.Column = 0
                Gtk.Application.Invoke(invoke)
        elif str(event.Key) == "Down":
            text = self.textview.Document.Text
            caret = self.textview.Caret
            line = caret.Line
            line_count = self.textview.Document.LineCount
            if line == line_count - 1:
                self.history.update(text.rstrip())
                text = self.history.down()
                def invoke(sender, args):
                    self.textview.Document.Text = text
                    self.textview.GrabFocus()
                    self.textview.Caret.Line = self.textview.Document.LineCount - 1
                    self.textview.Caret.Column = 0
                Gtk.Application.Invoke(invoke)
        elif str(event.Key) == "Tab":
            return False
            mark = self.textview.Buffer.InsertMark
            current = self.textview.Buffer.GetIterAtMark(mark)
            pos = current.LineOffset
            line = current.Line
            start = self.textview.Buffer.GetIterAtLine(line)
            text = self.textview.Buffer.GetText(start, current, True)
            if text.strip():
                help_text = self.completion(text)
                if help_text:
                    self.message(help_text)
                return True
        return False

    def completion(self, text):
        variable = self.find_variable(text)
        items = []
        if variable:
            parts = variable.split(".", 1)
            root = parts[0]
            if len(parts) == 1:
                items = [x for x in self.pyjama.engine.scope.GetVariableNames() if x.startswith(root)]
            else:
                partial = parts[-1]
                (found, value) = self.pyjama.engine.scope.TryGetVariable(root)
                if found:
                    for part in parts[1:-1]:
                        if hasattr(value, part):
                            value = getattr(value, part)
                        else:
                            value = None
                            break
                    if value:
                        items = [x for x in dir(value) if x.startswith(partial) and not x.startswith("_")]
        if items:           
            return "Possible completions:\n   " + ("\n   ".join(items)) + "\n"
        else:
            return "No completions found for '%s'\n" % text

    def find_variable(self, text):
        """
        Finds variable-like characters in a text.
        """
        candidate = ""
        for char in reversed(text):
            if char.isalnum() or char in ["_", "."]:
                candidate += char
            else:
                break
        candidate = "".join(reversed(candidate))
        if candidate.isdecimal() or candidate.isdigit() or candidate.isnumeric():
            return None
        return candidate

    def change_to_lang(self, language):
        self.language = language
        if self.lang_manager:
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
        self.update_gui()

    def on_save_file_as(self, obj, event):
        pass

    def clear(self, obj, event):
        def invoke_clear(sender, args):
            MUTEX.WaitOne()
            self.history_textview.Buffer.Text = ""
            MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke_clear)

    def on_quit(self, obj, event):
        Gtk.Application.Quit()

    def on_close(self, obj, event):
        self.pyjama.on_close("shell")
        return True

    def on_run(self, obj, event):
        self.on_key_press(None, force=True)

    def on_stop(self, obj, event):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            self.message("Stopping...\n")
            self.executeThread.Abort()
        Gtk.Application.Invoke(self.stop_running)

    def on_new_file(self, obj, event, language="python"):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_new_file(obj, event, language)

    def on_open_file(self, obj, event):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_open_file(obj, event)

    def select_or_open(self, filename, lineno=0, language="python"):
        self.pyjama.setup_editor()
        self.pyjama.editor.select_or_open(filename, lineno, language)

    def reset_shell(self, obj, event):
        self.pyjama.engine.reset()
        self.message("-----------\n")
        self.message("Reset shell\n")
        self.message("-----------\n")

    def message(self, message, tag="purple"):
        # DO NOT PUT the ev, WaitOne stuff here!
        def invoke(sender, args):
            MUTEX.WaitOne()
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, message, tag)
            MUTEX.ReleaseMutex()
            GLib.Timeout.Add(100, self.goto_end)
        Gtk.Application.Invoke(invoke)

    def goto_end(self):
        MUTEX.WaitOne()
        end = self.history_textview.Buffer.EndIter
        self.history_textview.ScrollToIter(end, 0.4, True, 0, 1.0)
        MUTEX.ReleaseMutex()

    def set_title(self, text):
        self.window.Title = text

    def execute_file(self, filename, language):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            return

        def background():
            self.message("Loading file '%s'...\n" % filename)
            Gtk.Application.Invoke(self.start_running)
            self.pyjama.engine[language].execute_file(filename)
            Gtk.Application.Invoke(self.stop_running)
            self.message("Done loading file.\n")

        self.executeThread = System.Threading.Thread(
                                System.Threading.ThreadStart(background))
        self.executeThread.Start()

    def load_text(self, text, language):
        self.language = language
        self.textview.Document.Text = self.undent_text(text.rstrip())
        self.update_gui()

    def execute(self, text, language):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            return
        prompt = "%-6s> " % language
        MUTEX.WaitOne()
        count = 2
        for line in text.split("\n"):
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, 
                             "%s" % prompt,
                             "black")
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, 
                             "%s\n" % line,
                             "blue")
            prompt = ((".....%d" % count)[-6:]) + "> "
            count += 1
        MUTEX.ReleaseMutex()

        # pragma/meta commands, start with #;
        if text == "":
            return False
        elif text and text[0:2] == "#;":
            text = text[2:].strip()
            command = text.lower()
            if command in self.pyjama.engine.get_languages():
                self.language = command
                self.update_gui()
                return True
            else:
                exec(text)
                return True
        self.language = language
        self.update_gui()
        self.execute_in_background(text)

    def start_running(self, sender, args):
        self.toolbar_buttons[Gtk.Stock.Stop].Sensitive = True

    def stop_running(self, sender, args):
        self.toolbar_buttons[Gtk.Stock.Stop].Sensitive = False

    def execute_in_background(self, text):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            return

        def background():
            Gtk.Application.Invoke(self.start_running)
            self.pyjama.engine[self.language].execute(text)
            Gtk.Application.Invoke(self.stop_running)

        self.executeThread = System.Threading.Thread(
                                System.Threading.ThreadStart(background))
        self.executeThread.Start()

    def ready_for_execute(self, text):
        return self.pyjama.engine[self.language].ready_for_execute(text)

    def goto_file(self, filename, lineno):
        self.pyjama.setup_editor()
        self.pyjama.editor.select_or_open(filename, lineno)

    def popup(self, textview, popup_args):
        mark = textview.Buffer.InsertMark
        iter = textview.Buffer.GetIterAtMark(mark)
        start = textview.Buffer.GetIterAtLine(iter.Line)
        iter.Offset = start.Offset + start.CharsInLine
        text = textview.Buffer.GetText(start, iter, False) # invisible chars

        match = re.search('File \"(.*)\", line (\d*)', text)
        if match: # 'File "<string>", line 167'
            filename, lineno = match.groups()
            lineno = int(lineno)
            basename = os.path.basename(filename)
            filename = os.path.abspath(filename)
            menuitem = Gtk.MenuItem("Edit %s" % basename)
            Gtk.Application.Invoke(lambda s, a: menuitem.Show())
            menuitem.Activated += lambda w, e: self.goto_file(filename, lineno)
            popup_args.Menu.Append(menuitem)

    def undent_text(self, text):
        """
        Removes same number of spaces from each line, if all indented.
        """
        # FIXME: could also remove "......>" from history_text window.
        spaces = re.match("\s*", text).group()
        if spaces:
            lines = []
            for line in text.split("\n"):
                if not line.startswith(spaces):
                    return text
                else:
                    lines.append(line[len(spaces):])
            return "\n".join(lines)
        return text
        

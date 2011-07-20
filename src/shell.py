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
# $Id$

# Pure-Python modules:
from __future__ import print_function
import traceback
import sys, os
import re

# Bring .NET References into IronPython scope:
from Mono.TextEditor import TextEditor, TextEditorOptions, Highlighting
path, filename = os.path.split(__file__)
# /.../Calico/src/
Highlighting.SyntaxModeService.LoadStylesAndModes(
                os.path.join(path, "..", "bin", "SyntaxModes"))

import Gtk, Gdk, Pango, GLib
import System
import System.Threading

# Calico modules:
from window import Window, MyWindow
from utils import (_, CustomStream, MUTEX, ConsoleStream, StatusBar,
                   SearchInFilesBar, OpenUrl)

def exec_invoke(text):
    # FIXME: if this fails, it crashes calico; why?
    exec(text)

# Local classes:
class History(object):
    def __init__(self, config):
        self.config = config
        self.history = self.config.get("shell.history")
        if len(self.history) == 0 or self.history[-1] != "":
            self.history.append("")
        self.position = len(self.history) - 1

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
    def __init__(self, calico, files):
        self.calico = calico
        self.calico.engine.set_redirects(ConsoleStream(), 
                                         ConsoleStream("red"), None)
        for file in files:
            self.execute_file(file, self.calico.get_language_from_filename(file))

    def message(self, text, newline=True):
        if newline:
            print(text)
        else:
            print(text, end="")

    def execute_file(self, filename, language):
        self.message(_("Loading file '%s'...") % filename)
        self.calico.engine[language].execute_file(filename)
        self.message(_("Done loading file."))

class ShellWindow(Window):
    def __init__(self, calico):
        self.calico = calico
        self.executeThread = None
        self.language = "python"
        self.window = MyWindow(_("Calico Shell - %s") % System.Environment.UserName)
        self.window.add_key_press_handler(self.on_key_press)
        self.window.SetDefaultSize(700, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.history_textview = Gtk.TextView()
        self.vbox = Gtk.VBox()
        self.searchbar = SearchInFilesBar()
        self.searchbar.set_shell(self)
        # ---------------------
        # make menu:
        menu = [(_("File"),
                 [(_("Open Script..."), Gtk.Stock.Open, None, self.on_open_file),
                  ] +
                 self.make_recents_menu() +
                 self.make_examples_menu() +
                 [None,
                  (_("Search in files..."), None, "<control>f", self.searchbar.open),
                  None] +
                 self.make_new_file_menu() +
                 [None,
                  (_("Register..."), None, None, lambda o, e: self.calico.register_dialog(self.window)),
                  (_("Login..."), None, "<control>l", lambda o, e: self.calico.login_dialog(self.window)),
                  None,
                  (_("Close"), Gtk.Stock.Close,
                   None, self.on_close),
                  (_("Quit"), Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                (_("Edit"), [
                    (_("Copy"), None, None, None),
                    (_("Paste"), None, None, None),
                    (_("Cut"), None, None, None),
                    (_("Select all"), Gtk.Stock.SelectAll, None, None),
                    ]),
                (_("Shell"), self.make_language_menu()),
                (_("Windows"), [
                    (_("Editor"), None, "F6", self.calico.setup_editor),
                    (_("Shell"), None, "F7", self.calico.setup_shell),
                    (_("Chat"), None, "F8", self.calico.setup_chat),
                    ]),
                (_("Options"), [
                    (_("Make font larger"), None, "<control>equal", self.calico.increase_fontsize),
                    (_("Make font smaller"), None, "<control>minus", self.calico.decrease_fontsize),
                    ]),
                (_("Help"), [
                    (_("About the Calico Project"), Gtk.Stock.About, None, self.calico.about),
                    ]),
                ]
        toolbar = [(Gtk.Stock.New, self.on_new_file, _("Create a new script")),
                   (Gtk.Stock.Open, self.on_open_file, _("Open an existing script")),
                   (Gtk.Stock.Apply, self.on_run, _("Run script")),
                   (Gtk.Stock.Stop, self.on_stop, _("Stop script")),
                   (Gtk.Stock.GotoBottom, self.swap_panes, _("Swap script and history areas")),
                   ]
        self.prompt_at_top = True
        self.make_gui(menu, toolbar)
        Gtk.Application.Invoke(self.stop_running)
        self.history = History(self.calico.config)
        self.statusbar = StatusBar()
        self.statusbar.init(_("Language"), _("Status"))
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
        self.textview.Options.ShowLineNumberMargin = False # option
        self.textview.Options.TabsToSpaces = True
        self.textview.Options.HighlightMatchingBracket = True
        try:
            self.textview.Document.MimeType = "text/x-%s" % self.language
        except:
            pass

        self.textview.Show()
        #self.textview.ModifyFont(self.calico.get_fontname())
        self.scrolled_window.Add(self.textview)
        self.results = Gtk.ScrolledWindow()
        for color in ["red", "blue", "purple", "black", "green"]:
            tag = Gtk.TextTag(color)
            if color in ["red"]:
                tag.Weight = Pango.Weight.Bold
            tag.Foreground = color 
            self.history_textview.Buffer.TagTable.Add(tag)
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
        self.vbox.PackStart(self.searchbar, False, False, 0)
        self.vbox.PackStart(self.vpane, True, True, 0)
        self.vbox.PackEnd(self.statusbar, False, False, 0)
        self.vbox.Show()
        self.menubar.ShowAll()
        self.toolbar.ShowAll()
        self.vpane.ShowAll()
        self.statusbar.ShowAll()
        self.window.Show()
        # Set this Python's stderr:
        # EXCEPTION HANDLER
        stdout = CustomStream(self.calico, self.history_textview, "black")
        sys.stderr = CustomStream(self.calico, self.history_textview, "red")
        self.calico.engine.set_redirects(stdout, sys.stderr, None)
        self.textview.GrabFocus()
        self.change_to_lang(self.language)
        # Setup clipboard stuff:
        self.clipboard = Gtk.Clipboard.Get(
              Gdk.Atom.Intern("CLIPBOARD", True))
        self.message(_("Calico Project %s") % self.calico.version)
        self.message(("-" * 50))
        self.set_font()
        self.show_icon()
        self.message("")
        # Setup plugins
        Window.__init__(self, calico)

    def show_widget(self, widget):
        def invoke(sender, args):
            MUTEX.WaitOne()
            widget.Show()
            anchor_iter = self.history_textview.Buffer.CreateChildAnchor(self.history_textview.Buffer.EndIter)
            self.history_textview.AddChildAtAnchor(widget, anchor_iter[0])
            MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke)

    def show_icon(self):
        def invoke(sender, args):
            MUTEX.WaitOne()
            image = Gtk.Image(os.path.join(self.calico.calico_root, "examples", "images", "abstract-butterfly-sm.gif"))
            image.Show()
            anchor_iter = self.history_textview.Buffer.CreateChildAnchor(self.history_textview.Buffer.EndIter)
            self.history_textview.AddChildAtAnchor(image, anchor_iter[0])
            MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke)

    def set_font(self, font=None):
        if font is None:
            font = self.calico.get_fontname()
        def invoke(sender, args):
            # FIXME: set textview font, too
            fontname = self.calico.config.get("calico.font")
            fontsize = self.calico.config.get("calico.fontsize")
            self.textview.Options.FontName = str(fontname) + " " + str(fontsize)
            self.history_textview.ModifyFont(font)
        Gtk.Application.Invoke(invoke)

    def decrease_font_size(self, font):
        self.history_textview.ModifyFont(font)
        self.textview.Options.ZoomOut()

    def increase_font_size(self, font):
        self.history_textview.ModifyFont(font)
        self.textview.Options.ZoomIn()

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
        for lang in sorted(self.calico.engine.get_languages()):
            if self.calico.engine[lang].text_based:
                languages.append([_("Change to %s") % lang.title(),
                    None, "<control>%d" % num, 
                    lambda obj, event, lang=lang: self.change_to_lang(lang)])
                num += 1
        return ([(_("Run"), Gtk.Stock.Apply, "F5", self.on_run),
                 (_("Clear"), None, "<control>Delete", self.clear),
                 (_("Restart shell"), None, "<control>r", self.reset_shell),
                 (_("Stop script"), None, "Escape", self.on_stop),
                 None] + 
                languages)

    def update_gui(self):
        try:
            self.textview.Document.MimeType = "text/x-%s" % self.language
        except:
            pass
        self.set_title(_("%s - Calico Shell - %s") % (self.language.title(), System.Environment.UserName))
        self.prompt.Text = "%s>" % (self.language + "------")[:6]
        self.statusbar.set(_("Language"), self.language.title())
        self.statusbar.set(_("Status"), self.get_status())

    def update_status(self):
        self.statusbar.set(_("Status"), self.get_status())

    def get_status(self):
        if self.calico.connection:
            return self.calico.connection.status
        else:
            return _("offline")

    def swap_panes(self, obj, event):
        if self.prompt_at_top:
            self.toolbar_buttons[Gtk.Stock.GotoBottom].StockId = Gtk.Stock.GotoTop
        else:
            self.toolbar_buttons[Gtk.Stock.GotoBottom].StockId = Gtk.Stock.GotoBottom
        self.prompt_at_top = not self.prompt_at_top
        def invoke(sender, args):
            c1 = self.vpane.Child1
            c2 = self.vpane.Child2
            self.vpane.Remove(c1)
            self.vpane.Remove(c2)
            self.vpane.Add1(c2)
            self.vpane.Add2(c1)
        Gtk.Application.Invoke(invoke)

    def on_key_press(self, widget, event, force=False):
        # FIXME: this should be handled in textview, but if we subclass
        # TextEditor, then we mess up the signals on scrolling window
        # So, this currently handles the keys for the whole window.
        #if event is not None:
        #    self.message(str(event.Key))
        # These are only if we are inside textview
        if self.window.Focus == self.textview:
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
                        col = self.textview.Document.GetLine(0).Length
                        self.textview.Caret.Column = col
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
                        self.textview.Caret.Column = self.textview.Document.GetLine(0).Length
                    Gtk.Application.Invoke(invoke)
            elif str(event.Key) == "Tab":
                return False
            return False
        else:
            return False

    def completion(self, text):
        variable = self.find_variable(text)
        items = []
        if variable:
            parts = variable.split(".", 1)
            root = parts[0]
            if len(parts) == 1:
                items = [x for x in self.calico.engine.scope.GetVariableNames() if x.startswith(root)]
            else:
                partial = parts[-1]
                (found, value) = self.calico.engine.scope.TryGetVariable(root)
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
            return _("Possible completions:\n   ") + ("\n   ".join(items)) + "\n"
        else:
            return _("No completions found for '%s'\n") % text

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
        self.update_gui()
        #self.show_prompt()

    def on_save_file_as(self, obj, event):
        pass

    def clear(self, obj, event):
        def invoke_clear(sender, args):
            MUTEX.WaitOne()
            self.history_textview.Buffer.Text = ""
            MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke_clear)

    def on_quit(self, obj, event):
        self.clean_up()
        self.calico.on_close("all")
        return True

    def clean_up(self):
        # Let's not let this get too big:
        self.calico.config.set("shell.history",
                        self.calico.config.get("shell.history")[-30:])

    def on_close(self, obj, event):
        self.clean_up()
        self.calico.on_close("shell")
        return True

    def on_run(self, obj, event):
        self.on_key_press(widget=self, event=None, force=True)

    def on_stop(self, obj, event):
        if (self.executeThread):
            self.message(_("Stopping..."))
            self.executeThread.Abort()
            self.executeThread = None
            Gtk.Application.Invoke(self.stop_running)
        else:
            self.searchbar.search_off()

    def on_new_file(self, obj, event, language="python"):
        self.calico.setup_editor()
        self.calico.editor.on_new_file(obj, event, language)

    def on_open_file(self, obj, event):
        self.calico.setup_editor()
        self.calico.editor.on_open_file(obj, event)

    def select_or_open(self, filename, lineno=0, language="python"):
        self.calico.setup_editor()
        self.calico.editor.select_or_open(filename, lineno, language)

    def reset_shell(self, obj, event):
        self.calico.engine.reset()
        self.message("-----------")
        self.message(_("Reset shell"))
        self.message("-----------")

    def message(self, message, tag="purple", newline=True):
        # DO NOT PUT the ev, WaitOne stuff here!
        message = str(message)
        if newline:
            message += "\n"
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

    def show_prompt(self):
        self.message(self.prompt.Text, tag="purple")

    def execute_file(self, filename, language):
        if (self.executeThread):
            return

        def background():
            self.message(_("Loading file '%s'...") % filename)
            Gtk.Application.Invoke(self.start_running)
            self.calico.engine[language].execute_file(filename)
            Gtk.Application.Invoke(self.stop_running)
            self.message(_("Done loading file."))
            #self.show_prompt()

        self.executeThread = System.Threading.Thread(
                                System.Threading.ThreadStart(background))
        self.executeThread.IsBackground = True
        self.executeThread.Start()

    def load_text(self, text, language):
        self.language = language
        self.textview.Document.Text = self.undent_text(text.rstrip())
        self.update_gui()

    def execute(self, text, language):
        if (self.executeThread):
            return
        prompt = "%s> " % (language + "------")[:6]
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
        self.goto_end()
        # pragma/meta commands, start with #;
        if text == "":
            return False
        elif text and text[0:2] == "#;":
            text = text[2:].strip()
            command = text.lower()
            if command in self.calico.engine.get_languages():
                self.language = command
                self.update_gui()
                return True
            else:
                Gtk.Application.Invoke(lambda s, a: exec_invoke(text))
                return True
        self.language = language
        self.update_gui()
        self.execute_in_background(text)

    def start_running(self, sender, args):
        self.toolbar_buttons[Gtk.Stock.Stop].Sensitive = True
        self.toolbar_buttons[Gtk.Stock.Apply].Sensitive = False
        if self.calico.editor:
            self.calico.editor.toolbar_buttons[Gtk.Stock.Apply].Sensitive = False

    def make_error_url(self, language, message):
        # FIXME: need to format url based on language
        error = ""
        for line in reversed(message.split("\n")):
            if line.strip() != "":
                error = line.strip()
                break
        if ":" in error:
            error, details = error.rsplit(":", 1)
        error = error.replace("<", "")
        error = error.replace(">", "")
        error = error.replace("'", "")
        return "http://wiki.roboteducation.org/Error:%s:%s" % (language.title(), error.strip())

    def stop_running(self, sender, args):
        self.executeThread = None
        self.toolbar_buttons[Gtk.Stock.Stop].Sensitive = False
        self.toolbar_buttons[Gtk.Stock.Apply].Sensitive = True
        if self.calico.editor:
            self.calico.editor.toolbar_buttons[Gtk.Stock.Apply].Sensitive = True
        if self.calico.last_error != "":
            url = self.make_error_url(self.language, self.calico.last_error)
            def invoke(sender, args):
                MUTEX.WaitOne()
                button = Gtk.Button(_("Get help on error"))
                button.Clicked += lambda o, e: OpenUrl(url)
                button.Show()
                anchor_iter = self.history_textview.Buffer.CreateChildAnchor(self.history_textview.Buffer.EndIter)
                self.history_textview.AddChildAtAnchor(button, anchor_iter[0])
                MUTEX.ReleaseMutex()
            Gtk.Application.Invoke(invoke)
            self.message("")
            self.calico.last_error = ""

    def execute_in_background(self, text):
        if (self.executeThread):
            return

        def background():
            Gtk.Application.Invoke(self.start_running)
            self.calico.engine[self.language].execute(text)
            ##self.show_prompt()
            Gtk.Application.Invoke(self.stop_running)

        self.executeThread = System.Threading.Thread(
                                System.Threading.ThreadStart(background))
        self.executeThread.Start()

    def ready_for_execute(self, text):
        return self.calico.engine[self.language].ready_for_execute(text)

    def goto_file(self, filename, lineno):
        self.calico.setup_editor()
        self.calico.editor.select_or_open(filename, lineno)

    def popup(self, textview, popup_args):
        mark = textview.Buffer.InsertMark
        iter = textview.Buffer.GetIterAtMark(mark)
        start = textview.Buffer.GetIterAtLine(iter.Line)
        iter.Offset = start.Offset + start.CharsInLine
        text = textview.Buffer.GetText(start, iter, False) # invisible chars

        match = re.search(_('File \"(.*)\", line (\d*)'), text)
        if match: # 'File "<string>", line 167'
            filename, lineno = match.groups()
            lineno = int(lineno)
            basename = os.path.basename(filename)
            filename = os.path.abspath(filename)
            menuitem = Gtk.MenuItem(_("Edit %s") % basename)
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
        

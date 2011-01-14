# Bring .NET References into IronPython scope:
import Gtk, Pango
import System
import re

from window import Window
from utils import _, CustomStream

import traceback
import sys, os

import System.Threading
from System.Threading import ManualResetEvent

DEBUG = False

class History(object):
    def __init__(self):
        self.history = []
        self.position = None
        self.dirty = False

    def up(self):
        #print "up", self.position, self.history
        if self.position is not None and 0 <= self.position - 1 < len(self.history):
            self.position -= 1
            #print "ok"
            return self.history[self.position]
        return None

    def down(self):
        #print "down", self.position, self.history
        if self.position is not None and 0 <= self.position + 1 < len(self.history):
            self.position += 1
            #print "ok"
            return self.history[self.position]
        return None

    def replace(self, text):
        self.history[-1] = text

    def add(self, text):
        self.dirty = False
        self.history.append(text)
        self.position = len(self.history)
        #print "add", self.position, self.history

    def last(self):
        return self.position == len(self.history)

    def nextlast(self):
        return self.position == len(self.history) - 1

class MyWindow(Gtk.Window):
    def set_on_key_press(self, on_key_press):
        self.on_key_press = on_key_press

    def OnKeyPressEvent(self, event):
        return (self.on_key_press(event) or 
                Gtk.Window.OnKeyPressEvent(self, event))

class ShellWindow(Window):
    def __init__(self, pyjama):
        self.pyjama = pyjama
        self.executeThread = None
        self.language = "python"
        self.lang_manager = None
        self.window = MyWindow(_("Pyjama Shell"))
        self.window.set_on_key_press(self.on_key_press)
        self.window.SetDefaultSize(600, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.history_textview = Gtk.TextView()
        # Set up all of the engines:
        self.pyjama.engine.set_redirects(CustomStream(self.history_textview), 
                                         CustomStream(self.history_textview,
                                                      "red"), 
                                         None)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Open Script...", Gtk.Stock.Open,
                   None, self.on_open_file),
                  ("New Script", Gtk.Stock.New,
                   None, self.on_new_file),
                  None,
                  ("Close", Gtk.Stock.Close,
                   None, self.on_close),
                  ("Quit", Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                ("_Edit", []),
                ("She_ll", self.make_language_menu()),
                ("Windows", [
                    ("Editor", None, "F6", self.pyjama.setup_editor),
                    ("Shell", None, "F7", self.pyjama.setup_shell),
                    ]),
                ("O_ptions", []),
                ("_Help", []),
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

        try:
            import clr
            clr.AddReference("gtksourceview2-sharp")
            import GtkSourceView
            self.lang_manager = GtkSourceView.SourceLanguageManager()
            self.textview = GtkSourceView.SourceView()
            self.textview.ShowLineNumbers = False
            self.textview.InsertSpacesInsteadOfTabs = True
            self.textview.IndentWidth = 4
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
            self.textview.Editable = True
            self.textview.WrapMode = Gtk.WrapMode.Word
            self.textview.AcceptsTab = True
        except:
            self.textview = Gtk.TextView()
        self.textview.Show()
        self.textview.ModifyFont(Pango.FontDescription.FromString("Monospace 10"))
        self.scrolled_window.AddWithViewport(self.textview)
        self.results = Gtk.ScrolledWindow()
        for color in ["red", "blue", "purple", "black"]:
            tag = Gtk.TextTag(color)
            if color in ["red"]:
                tag.Weight = Pango.Weight.Bold
            tag.Foreground = color 
            self.history_textview.Buffer.TagTable.Add(tag)
        self.history_textview.ModifyFont(Pango.FontDescription.FromString("Monospace 10"))
        self.history_textview.PopulatePopup += self.popup
        self.history_textview.WrapMode = Gtk.WrapMode.Word
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
        # FIXME EXCEPTION HANDLER
        sys.stdout = CustomStream(self.history_textview, "black")
        sys.stderr = CustomStream(self.history_textview, "red")
        self.pyjama.engine.set_redirects(sys.stdout, sys.stderr, None)
        self.textview.GrabFocus()
        self.change_to_lang(self.language)

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
                 ("Restart shell", None, "<control>r", self.reset_shell),
                 ("Stop script", None, "Escape", self.on_stop),
                 None] + 
                languages)

    def update_gui(self):
        self.set_title(_("%s - Pyjama Shell") % self.language.title())
        self.prompt.Text = "%-6s>" % self.language
        self.statusbar.Pop(0)
        self.statusbar.Push(0, _("Language: %s") % self.language.title())

    def on_key_press(self, event):
        #if event is not None:
        #    self.message(str(event.Key))
        if event is None or str(event.Key) == "Return":
            end = self.textview.Buffer.EndIter
            start = self.textview.Buffer.StartIter
            text = self.textview.Buffer.GetText(start, end, False)
            if text.strip() == "":
                return True # nothing to do, but handled
            elif self.ready_for_execute(text):
                if self.history.dirty and self.history.nextlast():
                    self.history.replace(text)
                else:
                    self.history.add(text)
                self.execute(text, self.language)
                return True
        elif str(event.Key) == "Up":
            mark = self.textview.Buffer.InsertMark
            itermark = self.textview.Buffer.GetIterAtMark(mark)
            line = itermark.Line
            start = self.textview.Buffer.StartIter
            end = self.textview.Buffer.EndIter
            alltext = self.textview.Buffer.GetText(start, end, False)
            #print "line:", line
            if line == 0:
                # if on a new line, save it
                if not self.history.dirty:
                    self.history.add(alltext)
                    self.history.dirty = True
                    self.history.up()
                elif self.history.nextlast():
                    self.history.replace(alltext)
                #else: abandon any changes
                text = self.history.up()
                if text is not None:
                    self.textview.Buffer.Text = text
                    return True
        elif str(event.Key) == "Down":
            mark = self.textview.Buffer.InsertMark
            itermark = self.textview.Buffer.GetIterAtMark(mark)
            line = itermark.Line
            if line == self.textview.Buffer.LineCount - 1:
                text = self.history.down()
                if text is not None:
                    self.textview.Buffer.Text = text
                    return True
        return False

    def change_to_lang(self, language):
        self.language = language
        if self.lang_manager:
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
        self.update_gui()

    def on_save_file_as(self, obj, event):
        pass

    def on_quit(self, obj, event):
        Gtk.Application.Quit()

    def on_close(self, obj, event):
        self.pyjama.on_close("shell")
        return True

    def on_run(self, obj, event):
        self.on_key_press(None)

    def on_stop(self, obj, event):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            self.message("Stopping...\n")
            self.executeThread.Abort()
        Gtk.Application.Invoke(self.stop_running)

    # these aren't needed?
    def on_new_file(self, obj, event):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_new_file(obj, event)

    def on_open_file(self, obj, event):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_open_file(obj, event)

    def reset_shell(self, obj, event):
        self.pyjama.engine.reset()
        self.message("-----------\n")
        self.message("Reset shell\n")
        self.message("-----------\n")

    def message(self, message, tag="purple"):
        # DO NOT PUT the ev, WaitOne stuff here!
        def invoke(sender, args):
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, message, tag)
        Gtk.Application.Invoke(invoke)

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

    def execute(self, text, language):
        if (self.executeThread and 
            self.executeThread.ThreadState == System.Threading.ThreadState.Running):
            return
        self.textview.Buffer.Clear()
        prompt = "%-6s> " % language
        for line in text.split("\n"):
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, 
                             "%s" % prompt,
                             "black")
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, 
                             "%s\n" % line,
                             "blue")
            prompt = "......>"

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

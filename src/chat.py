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

import Gtk
import Gdk
import Pango
import GLib
import System

from utils import _, Mutex, StatusBar, get_colors
from window import Window, MyWindow
from random import choice

class ChatWindow(Window):
    def __init__(self, pyjama):
        self.MUTEX = Mutex(False, "PyjamaChatMutex")
        self.room = "General"
        self.pyjama = pyjama
        self.colormap = {}
        self.window = MyWindow(_("Pyjama Chat - %s") % System.Environment.UserName)
        self.window.add_key_press_handler(self.on_key_press)
        self.window.SetDefaultSize(350, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.textview = Gtk.TextView()
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [(_("File"),
                 [(_("Open Script..."), Gtk.Stock.Open, None, self.on_open_file),
                  ] +
                  self.make_recents_menu() +
                  self.make_examples_menu() +
                  [None] + 
                  self.make_new_file_menu() +
                  [None,
                  (_("Register..."), None, None, lambda o, e: self.pyjama.register_dialog(self.window)),
                  (_("Login..."), None, "<control>l", lambda o, e: self.pyjama.login_dialog(self.window)),
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
                (_("Chat"), []),
                (_("Windows"), [
                    (_("Editor"), None, "F6", self.pyjama.setup_editor),
                    (_("Shell"), None, "F7", self.pyjama.setup_shell),
                    (_("Chat"), None, "F8", self.pyjama.setup_chat),
                    ]),
                (_("Options"), [
                    (_("Make font larger"), None, "<control>equal", self.pyjama.increase_fontsize),
                    (_("Make font smaller"), None, "<control>minus", self.pyjama.decrease_fontsize),
                    ]),
                (_("Help"), [
                    (_("About the Pyjama Project"), Gtk.Stock.About, None, self.pyjama.about),
                    ]),
                ]
        toolbar = [(Gtk.Stock.New, self.on_new_file, _("Create a new script")),
                   (Gtk.Stock.Open, self.on_open_file, _("Open an existing script")),
                   ]
        self.make_gui(menu, toolbar)
        self.statusbar = StatusBar()
        self.statusbar.init(_("Status"))

        # Enter messages:
        self.vbox = Gtk.VBox()
        self.command_area = Gtk.HBox()
        self.prompt = Gtk.Label(_("Chat: "))
        self.entry = Gtk.Entry()
        self.send_button = Gtk.Button(_("Send"))
        self.send_button.Clicked += self.send_clicked 
        self.command_area.PackStart(self.prompt, False, False, 0)
        self.command_area.PackStart(self.entry, True, True, 0)
        self.command_area.PackStart(self.send_button, False, False, 0)

        self.textview = Gtk.TextView()
        self.results = Gtk.ScrolledWindow()
        for color in get_colors():
            tag = Gtk.TextTag(color)
            if color in ["red"]:
                tag.Weight = Pango.Weight.Bold
            tag.Foreground = color
            self.textview.Buffer.TagTable.Add(tag)
        self.textview.ModifyFont(self.pyjama.get_fontname())
        self.textview.PopulatePopup += self.popup
        self.textview.WrapMode = Gtk.WrapMode.Word
        self.textview.Editable = False
        self.results.Add(self.textview)
        # initialize
        self.window.Add(self.vbox)
        self.vbox.PackStart(self.menubar, False, False, 0)
        self.vbox.PackStart(self.toolbar, False, False, 0)
        self.vbox.PackStart(self.results, True, True, 0)
        self.vbox.PackStart(self.command_area, False, False, 0)
        self.vbox.PackEnd(self.statusbar, False, False, 0)
        # Set this Python's stderr:
        # EXCEPTION HANDLER
        self.entry.GrabFocus()
        # Setup clipboard stuff:
        self.clipboard = Gtk.Clipboard.Get(
              Gdk.Atom.Intern("CLIPBOARD", True))
        # Setup plugins
        Window.__init__(self, pyjama)
        def invoke(sender, args):
            self.window.ShowAll()
            self.update_status()
            self.message(_("Pyjama Chat Window") + "\n" +
                         "-------------------\n" +
                         _("Enter /help for details") + "\n")
        Gtk.Application.Invoke(invoke)

    def send_clicked(self, obj=None, event=None):
        text = str(self.entry.Text)
        if text.startswith("/help"):
            self.message("/help", "black")
            self.message(_("""
Chat commands:
   MESSGE        - send a message to
                   all in conference
   @USER MESSAGE - send a message to
                   USER
   /join CONF    - join a conference
   /list         - list conferences
   /create CONF  - create conference
   /help         - this help message

"""))
            self.entry.Text = ""
        elif self.pyjama.connection:
            if self.pyjama.connection.status == "online":
                # FIXME: escape text to make XML body appropriate (it may do that automatically)
                # FIXME: add /create and /list
                if str(self.entry.Text).startswith("/join "):
                    self.room = str(self.entry.Text)[6:].strip()
                    self.pyjama.connection.send("admin", "[join]\nroom: %s" % self.room)
                elif str(self.entry.Text).startswith("@"):
                    user, message = str(self.entry.Text).split(" ", 1)
                    self.pyjama.connection.send("admin", "[broadcast]\nto: %s\n%s" % (user[1:], message))
                else:
                    self.pyjama.connection.send("admin", "[broadcast]\nroom: %s\n%s" % (self.room, self.entry.Text))
                self.entry.Text = ""
            else:
                self.message(_("You are not currently online"))
        else:
            self.message(_("You need to login first"))

    def update_status(self):
        self.statusbar.set(_("Status"), self.get_status())

    def get_status(self):
        if self.pyjama.connection:
            return self.pyjama.connection.status
        else:
            return _("offline")

    def decrease_font_size(self, font):
        self.textview.ModifyFont(font)

    def increase_font_size(self, font):
        self.textview.ModifyFont(font)

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

    def update_gui(self):
        #self.statusbar.Pop(0)
        #self.statusbar.Push(0, _("Language: %s") % self.language.title())
        pass

    def on_key_press(self, widget, event, force=False):
        if str(event.Key) == "Return":
            # if cursor in middle, insert a Return
            self.send_clicked()
            return True
        return False

    def clear(self, obj, event):
        def invoke_clear(sender, args):
            self.MUTEX.WaitOne()
            self.textview.Buffer.Text = ""
            self.MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke_clear)

    def on_quit(self, obj, event):
        self.clean_up()
        self.pyjama.on_close("all")
        return True

    def clean_up(self):
        pass

    def on_close(self, obj, event):
        self.clean_up()
        self.pyjama.on_close("chat")
        return True

    def on_new_file(self, obj, event, language="python"):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_new_file(obj, event, language)

    def on_open_file(self, obj, event):
        self.pyjama.setup_editor()
        self.pyjama.editor.on_open_file(obj, event)

    def select_or_open(self, filename, lineno=0, language="python"):
        self.pyjama.setup_editor()
        self.pyjama.editor.select_or_open(filename, lineno, language)

    def display_message(self, name, message):
        if name in self.colormap:
            color = self.colormap[name]
        else:
            color = choice(get_colors())
            self.colormap[name] = color
        self.pyjama.chat.message("%s: " % name, color, newline=False)
        self.pyjama.chat.message(message, "black")

    def message(self, message, tag="purple", newline=True):
        if newline:
            message += "\n"
        # DO NOT PUT the ev, WaitOne stuff here!
        def invoke(sender, args):
            self.MUTEX.WaitOne()
            end = self.textview.Buffer.EndIter
            self.textview.Buffer.InsertWithTagsByName(end, message, tag)
            self.MUTEX.ReleaseMutex()
            GLib.Timeout.Add(100, self.goto_end)
        Gtk.Application.Invoke(invoke)

    def goto_end(self):
        def invoke(sender, args):
            self.MUTEX.WaitOne()
            end = self.textview.Buffer.EndIter
            self.textview.ScrollToIter(end, 0.0, True, 0, 0.5)
            self.MUTEX.ReleaseMutex()
        Gtk.Application.Invoke(invoke)

    def set_title(self, text):
        self.window.Title = text

    def popup(self, textview, popup_args):
        pass


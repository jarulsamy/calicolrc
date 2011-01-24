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

import Gtk
import GLib
import System

from window import Window, MyWindow
from utils import _
import os

class EditorWindow(Window):
    def __init__(self, pyjama, files=None):
        self.pyjama = pyjama
        # create the parts
        self.window = MyWindow(_("Pyjama Editor"))
        self.window.set_on_key_press(self.on_key_press)
        self.window.SetDefaultSize(700, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Open Script...", Gtk.Stock.Open, 
                   None, self.on_open_file),
                  None,
                  ] + 
                  self.make_new_file_menu() +
                 [
                  None,
                  ("Save...", Gtk.Stock.Save, 
                   None, self.on_save_file),
                  ("Save as...", Gtk.Stock.SaveAs,
                   None, self.on_save_file_as),
                  None,
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
                    None,
                    ("Indent", None, "<control>bracketright", self.indent_region),
                    ("Unindent", None, "<control>bracketleft", self.unindent_region),
                          ]),
                ("Script", [
                    ("Run", Gtk.Stock.Apply, "F5", self.on_run),
                    ("Reset and run", None, "<control>F5", self.on_reset_run),
                           ]),
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
                   (Gtk.Stock.Save, self.on_save_file), 
                   (Gtk.Stock.Apply, self.on_run),
                   ]
        self.make_gui(menu, toolbar)
        self.notebook = Gtk.Notebook()
        self.notebook.Scrollable = True
        self.notebook.TabHborder = 5
        self.notebook.TabBorder = 1
        self.notebook.TabVborder = 1
        self.notebook.SwitchPage += self.changed_page
        self.notebook.PageRemoved += self.changed_page
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Push(0, "Language: Python")
        self.statusbar.HasResizeGrip = True
        self.statusbar.Show()
        # initialize
        self.window.Add(self.vbox)
        self.vbox.PackStart(self.menubar, False, False, 0)
        self.vbox.PackStart(self.toolbar, False, False, 0)
        self.vbox.PackStart(self.notebook, True, True, 0)
        self.vbox.PackStart(self.statusbar, False, False, 0)
        self.window.ShowAll()

        # Open files on command line, or just a New Script:
        if files:
            for file in files:
                filename = os.path.abspath(file)
                self.select_or_open(filename)
        else:
            page = self.make_document(None)
            self.notebook.AppendPage(page.widget, page.tab)
            self.notebook.SetTabReorderable(page.widget, True)
        doc = self.get_current_doc()
        if doc:
            doc.grab_focus()

    def on_key_press(self, eventkey):
        """
        Handles key press events for the entire window. If handled
        here, return True.
        """
        if str(eventkey.Key) == "Tab":
            doc = self.get_current_doc()
            if doc:
                doc.insert_at_cursor(self.pyjama.indent_string)
                return True
        return False

    def changed_page(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            self.statusbar.Pop(0)
            self.statusbar.Push(0, _("Language: %s") % doc.language.title())
            self.window.Title = "%s - %s" % (doc.title,  _("Pyjama Editor"))
            if doc.filename:
                path, filename = os.path.split(doc.filename)
                try:
                    os.chdir(path)
                except:
                    pass # Fail silently
        else:
            self.statusbar.Pop(0)
            self.statusbar.Push(0, _("Language: "))
            self.window.Title = _("Pyjama Editor")

    def select_or_open(self, filename, lineno=0, language="python"):
        """
        Open, or select a file if already opened.
        lineno == 0 means don't care, otherwise go to
        a specific line number.
        """
        page = None
        # if already open, select it
        if filename is not None:
            for page_num in range(self.notebook.NPages):
                npage = self.notebook.GetNthPage(page_num)
                if npage.document.filename == filename:
                    self.notebook.CurrentPage = page_num
                    page = npage # reselect opened filename
                    break
            if page is None:
                page = self.make_document(filename) # make a new document with filename
        else: # make a no-named document of type language
            page = self.make_document(None, language)
        if page:
            page_num = self.notebook.AppendPage(page.widget, page.tab)
            self.notebook.SetTabReorderable(page.widget, True)
            self.notebook.CurrentPage = page_num
            # FIXME: also add to live Recent files menu:
            if filename not in self.pyjama.config.get("pyjama.recent_files"):
                self.pyjama.config.get("pyjama.recent_files").append(filename)
            if len(self.pyjama.config.get("pyjama.recent_files")) > 10:
                self.pyjama.config.get("pyjama.recent_files").pop()
        ###########################################################
        # Remove temp page, if one, and not same kind as one added:
        if self.notebook.NPages == 2:
            doc0 = self.notebook.GetNthPage(0).document
            if doc0.filename is None and not doc0.get_dirty():
                self.notebook.RemovePage(0)
        if page and lineno != 0:
            self.goto_line(lineno)

    def goto_line(self, lineno):
        """
        Go to a line number in the current document. Call with [1, n]
        inclusive.
        """
        doc = self.get_current_doc()
        if doc:
            doc.goto_line(lineno)

    def on_open_file(self, obj, event):
        retval = False
        fc = Gtk.FileChooserDialog("Select the file to open",
                                   self.window,
                                   Gtk.FileChooserAction.Open,
                                   "Cancel", Gtk.ResponseType.Cancel,
                                   "Open", Gtk.ResponseType.Accept)
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            self.select_or_open(fc.Filename)
            path, base = os.path.split(fc.Filename)
            os.chdir(path)
            retval = True
        fc.Destroy()
        return retval

    def on_close_tab(self, page):
        page_num = self.notebook.PageNum(page)
        self.notebook.RemovePage(page_num)

    def on_new_file(self, obj, event, language="python"):
        self.select_or_open(None, language=language)

    def make_new_file_menu(self):
        retval = []
        for lang in self.pyjama.languages:
            retval.append(
                ("New %s Script" % lang.title(), None, 
                 None, lambda o,e,lang=lang: self.on_new_file(o, e, lang))
                )
        retval.append(None) # separator
        retval.append("Recent files") # submenu
        for file in sorted(self.pyjama.config.get("pyjama.recent_files")):
            retval.append(("Recent files", (file, None, None, lambda o,e,file=file: self.select_or_open(file))))
        return retval

    # FIXME: get default type of file from config 
    def make_document(self, filename, language="python"):
        """
        Provide language if filename is not given.
        """
        # FIXME: allow TXT types files, just to edit
        if filename:
            if "." in filename:
                pathname, extension = filename.rsplit(".", 1)
            else:
                pathname, extension = filename, ""
            for lang in self.pyjama.languages:
                if self.pyjama.languages[lang].extension == extension:
                    page = self.pyjama.languages[lang].get_document_class()(filename, self.pyjama, lang)
                    return page
        page = self.pyjama.languages[language].get_document_class()(filename, self.pyjama, language)
        return page

    def on_save_file(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            doc.save()

    def on_save_file_as(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            doc.save_as()

    def get_current_doc(self):
        if self.notebook.CurrentPage >= 0:
            return self.notebook.GetNthPage(self.notebook.CurrentPage).document
        else:
            return None

    document = property(get_current_doc)

    def get_docs(self):
        return [self.notebook.GetNthPage(i).document 
                for i in range(self.notebook.NPages)]

    def on_reset_run(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            if doc.save():
                self.pyjama.setup_shell()
                self.pyjama.shell.reset_shell(None, None)
                self.pyjama.shell.execute_file(doc.filename, doc.language)

    def on_run(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            self.pyjama.setup_shell()
            # if text selected, use that
            text = doc.get_selected_text()
            if text:
                self.pyjama.shell.load_text(text, doc.language)
            else:
                # else, load file
                if doc.save():
                    self.pyjama.shell.execute_file(doc.filename, doc.language)

    def on_close(self, obj, event):
        self.pyjama.on_close("editor")
        return True

    def on_quit(self, obj, event):
        Gtk.Application.Quit()

    def modify_font(self, font):
        for doc in self.get_docs():
            doc.modify_font(font)


import Gtk
import GLib
import System

from window import Window
from utils import _
import os

class EditorWindow(Window):
    def __init__(self, pyjama, files=None):
        self.pyjama = pyjama
        # create the parts
        self.window = Gtk.Window(_("Pyjama Editor"))
        self.window.SetDefaultSize(600, 550)
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
                ("O_ptions", []),
                ("_Help", []),
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
                page = self.make_document(filename)
                self.notebook.AppendPage(page.widget, page.tab)
                self.notebook.SetTabReorderable(page.widget, True)
        else:
            page = self.make_document(None)
            self.notebook.AppendPage(page.widget, page.tab)
            self.notebook.SetTabReorderable(page.widget, True)
        doc = self.get_current_doc()
        if doc:
            doc.grab_focus()

    def changed_page(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            self.statusbar.Pop(0)
            self.statusbar.Push(0, _("Language: %s") % doc.language.title())
            self.window.Title = "%s - %s" % (doc.title,  _("Pyjama Editor"))
            if doc.filename:
                path, filename = os.path.split(doc.filename)
                os.chdir(path)
        else:
            self.statusbar.Pop(0)
            self.statusbar.Push(0, _("Language: "))
            self.window.Title = _("Pyjama Editor")

    def select_or_open(self, filename, lineno=0):
        """
        Open, or select a file if already opened.
        lineno == 0 means don't care, otherwise go to
        a specific line number.
        """
        page = None
        # if already open, select it
        for page_num in range(self.notebook.NPages):
            npage = self.notebook.GetNthPage(page_num)
            if npage.document.filename == filename:
                self.notebook.CurrentPage = page_num
                page = npage
                break
        if page is None:
            page = self.make_document(filename)
            page_num = self.notebook.AppendPage(page.widget, page.tab)
            self.notebook.SetTabReorderable(page.widget, True)
            self.notebook.CurrentPage = page_num
        # Remove temp page, if one:
        if self.notebook.NPages == 2:
            doc0 = self.notebook.GetNthPage(0).document
            if doc0.filename is None and not doc0.get_dirty():
                self.notebook.RemovePage(0)
        if page and lineno != 0:
            self.goto_line(lineno)

    def goto_line(self, lineno):
        """
        Go to a line number in the current document. Call with [1, n]. Internally,
        linenumbers are zero-based.
        """
        def invoke(sender, args):
            doc = self.get_current_doc()
            line = doc.textview.Buffer.GetIterAtLine(lineno - 1)
            doc.textview.Buffer.PlaceCursor(line)
            GLib.Timeout.Add(200, lambda: doc.textview.ScrollToIter(line, 0.4, True, 0, 1.0))
        Gtk.Application.Invoke(invoke)

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
        page = self.pyjama.languages[language].get_document_class()(None, self.pyjama, language)
        page_num = self.notebook.AppendPage(page.widget, page.tab)
        self.notebook.SetTabReorderable(page.widget, True)
        self.notebook.CurrentPage = page_num
        doc0 = self.notebook.GetNthPage(0).document
        if doc0.filename == None and not doc0.get_dirty():
            self.notebook.RemovePage(0)

    def make_new_file_menu(self):
        retval = []
        for lang in self.pyjama.languages:
            retval.append(
                ("New %s Script" % lang.title(), None, 
                 None, lambda o,e,lang=lang: self.on_new_file(o, e, lang))
                )
        return retval

    def make_document(self, filename):
        # FIXME: allow TXT types files, just to edit
        if filename:
            if "." in filename:
                pathname, extension = filename.rsplit(".")
            else:
                pathname, extension = filename, ""
            for lang in self.pyjama.languages:
                if self.pyjama.languages[lang].extension == extension:
                    page = self.pyjama.languages[lang].get_document_class()(filename, self.pyjama, lang)
                    return page
        # FIXME: get default type of file from config 
        page = self.pyjama.languages["python"].get_document_class()(filename, self.pyjama, "python")
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
            (selected, start, end) = doc.textview.Buffer.GetSelectionBounds()
            if selected:
                text = doc.textview.Buffer.GetText(start, end, True)
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

    def get_textview(self):
        """
        Overloaded to give the current doc's textview.
        """
        return self.get_current_doc().textview

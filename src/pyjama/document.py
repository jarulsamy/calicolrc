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

from __future__ import print_function
import Gtk
import GLib
import Pango
import System
import os
from Mono.TextEditor import TextEditor, Highlighting, TextEditorOptions
path, filename = os.path.split(__file__)
# /.../Calico/src/
Highlighting.SyntaxModeService.LoadStylesAndModes(
                os.path.join(path, "..", "bin", "SyntaxModes"))

from utils import _

# Local classes:
class MyScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of (and access) additional
    items.
    """

class Document(object):
    """
    Base Document for all languages.
    """
    def __init__(self, filename, calico, language="python"):
        if filename:
            filename = os.path.abspath(filename)
        self.filename = filename
        self.calico = calico
        self.language = language
        if self.filename:
            self.title = os.path.basename(self.filename)
        else:
            self.title = _("New %s Script") % self.language.title()
        self.make_tab()
        self.make_widget()
        self.configure()
        self.begin_not_undoable()
        if self.filename and os.path.exists(self.filename):
            self.open()
        self.end_not_undoable()

    def make_tab(self):
        self.tab = Gtk.HBox()
        self.label = Gtk.Label(self.title)
        tooltips = Gtk.Tooltips()
        tooltips.SetTip(self.label, self.filename, None)
        #self.tab.WidthRequest = 150
        #label.Ellipsize = Pango.EllipsizeMode.End
        self.label.Show()
        self.tab.PackStart(self.label)
        button = Gtk.Button()
        button.Relief = Gtk.ReliefStyle.None
        img = Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu)
        img.Show()
        button.Add(img)
        button.Show()
        button.Clicked += lambda obj, event: \
            self.calico.editor.on_close_tab(self.widget)
        self.tab.PackEnd(button)

    def on_modified(self, obj, event):
        if self.get_dirty():
            self.label.Text = "*%s" % self.title
        else:
            self.label.Text = self.title

    def make_widget(self):
        """
        Side-effect creates self.widget with a .document property to
        self. Hook up any signals to text editing widget.
        """
        self.widget = MyScrolledWindow()
        self.widget.document = self

    def save(self):
        """
        """
        if not self.filename:
            return self.save_as()
        if self.filename:
            # first, let's make sure the directory is writable
            proposed_dir, basename = os.path.split(os.path.abspath(self.filename))
            if not self.is_writable(proposed_dir):
                # if not, let's change dirs
                personal = System.Environment.GetFolderPath(
                    System.Environment.SpecialFolder.Personal)
                self.filename = os.path.join(personal, basename)
                return self.save_as()
            # if exists, make backup
            if os.path.isfile(self.filename):
                try:
                    fp_out = open(self.filename + "~", "w")
                    oldtext = "".join(file(self.filename).xreadlines())
                    fp_out.write(oldtext)
                    fp_out.close()
                except:
                    print("Cannot write file: '%s~'" % self.filename)
            # now save the new:
            try:
                fp = open(self.filename, "w")
                fp.write(self.get_text())
                fp.close()
            except:
                print("Cannot write file: '%s'" % self.filename)
            self.set_clean()
            return True
        return False

    def is_writable(self, directory):
        tempfile = os.path.join(directory, "tempfile.tmp")
        retval = True
        try:
            fp = open(tempfile, "w")
            fp.close()
            os.remove(tempfile)
        except:
            retval = False
        return retval

    def save_as(self):
        if self.filename:
            proposed_dir, basename = os.path.split(os.path.abspath(self.filename))
        else:
            proposed_dir = os.getcwd()
            basename = "Untitled." + self.calico.languages[self.language].extensions[0]
        # first, let's make sure the directory is writable
        if not self.is_writable(proposed_dir):
            # if not, let's change dirs
            proposed_dir = System.Environment.GetFolderPath(
                System.Environment.SpecialFolder.Personal)
        if os.getcwd() != proposed_dir:
            os.chdir(proposed_dir)
        retval = False
        fc = Gtk.FileChooserDialog(_("Enter the file to save"),
                                   self.calico.editor.window,
                                   Gtk.FileChooserAction.Save,
                                   _("Cancel"), Gtk.ResponseType.Cancel,
                                   _("Save"), Gtk.ResponseType.Accept)
        fc.CurrentName = basename      # the file: entry text box
        fc.SelectFilename(basename)    # the file selection, if it exists
        fc.KeepAbove = True
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            self.filename = fc.Filename
            tooltips = Gtk.Tooltips()
            tooltips.SetTip(self.label, self.filename, None)
            self.save()
            self.title = os.path.basename(self.filename)
            self.label.Text = self.title
            self.on_change_file()
            retval = True
        fc.Destroy()
        return retval

    def set_clean(self):
        pass

    def get_dirty(self):
        pass

    def begin_not_undoable(self):
        pass

    def end_not_undoable(self):
        pass

    def on_change_file(self):
        self.language = self.calico.get_language_from_filename(self.filename)

    def begin_not_undoable(self):
        pass

    def end_not_undoable(self):
        pass

    def configure(self):
        pass

    def grab_focus(self):
        pass

    def search(self):
        pass
        
    def text_has_focus(self):
        pass

    def on_key_press(self, obj, event):
        return False

    def modify_font(self, font):
        pass

    def get_text(self):
        pass

    def get_selected_text(self):
        pass

    def insert_at_cursor(self, text):
        pass

    def open(self):
        pass

    def goto_line(self, lineno):
        pass

    def get_line(self):
        return 0

    def set_font(self, font=None):
        pass

    def update_language(self):
        pass

class TextEditorDocument(Document):
    def __init__(self, filename, calico, language="python"):
        Document.__init__(self, filename, calico, language)

    def make_widget(self):
        Document.make_widget(self)
        options = TextEditorOptions()
        self.texteditor = TextEditor(Options=options)
        #self.modify_font()
        # FIXME: not quite right: should reset on undo and doesn't 
        # set on space
        self.texteditor.Document.DocumentUpdated += self.on_modified
        try:
            self.texteditor.Document.MimeType = "text/x-%s" % self.language
        except:
            pass
        self.widget.Add(self.texteditor)
        self.texteditor.Show()
        self.widget.Show()
        self.grab_focus()

    def configure(self):
        #self.texteditor.Options.AllowTabsAfterNonTabs
        #self.texteditor.Options.AutoIndent
        #self.texteditor.Options.CanResetZoom
        #self.texteditor.Options.IndentationSize
        #self.texteditor.Options.IndentationString
        #self.texteditor.Options.RemoveTrailingWhitespaces
        #self.texteditor.Options.RulerColumn
        #self.texteditor.Options.ShowEolMarkers
        #self.texteditor.Options.ShowFoldMargin = True
        #self.texteditor.Options.ShowIconMargin = False
        self.texteditor.Options.ShowInvalidLines = False
        self.texteditor.Options.ShowLineNumberMargin = True
        self.texteditor.Options.TabsToSpaces = True
        self.texteditor.Options.HighlightCaretLine = True
        self.texteditor.Options.HighlightMatchingBracket = True
        self.texteditor.Options.OverrideDocumentEolMarker = True
        self.texteditor.Options.DefaultEolMarker = "\n"
        #self.texteditor.Options.ShowRuler
        #self.texteditor.Options.ShowSpaces
        #self.texteditor.Options.ShowTabs
        #self.texteditor.Options.TabSize
        #self.texteditor.Options.WordFindStrategy
        #self.texteditor.Options.Zoom
        #self.texteditor.Options.ZoomIn
        #self.texteditor.Options.ZoomOut
        #self.texteditor.Options.ZoomReset

    def on_change_file(self):
        Document.on_change_file(self)
        try:
            self.texteditor.Document.MimeType = "text/x-%s" % self.language
        except:
            pass

    def update_language(self):
        self.texteditor.Document.MimeType = "text/x-%s" % self.language

    def begin_not_undoable(self):
        pass

    def end_not_undoable(self):
        pass

    def grab_focus(self):
        self.texteditor.GrabFocus()

    def search(self):
        pass

    def text_has_focus(self):
        pass

    def increase_font_size(self, font):
        self.texteditor.Options.ZoomIn()

    def decrease_font_size(self, font):
        self.texteditor.Options.ZoomOut()

    def set_font(self, font = None):
        if font is None:
            fontname = self.calico.config.get("calico.font")
            fontsize = self.calico.config.get("calico.fontsize")
            self.texteditor.Options.FontName = str(fontname) + " " + str(fontsize)

    def get_text(self):
        return self.texteditor.Document.Text

    def get_selected_text(self):
        return self.texteditor.SelectedText

    def insert_at_cursor(self, text):
        pass

    def open(self):
        if os.path.isfile(self.filename):
            self.texteditor.Document.Text = "".join(file(self.filename).xreadlines())
        self.grab_focus()
        
    def goto_line(self, lineno):
        def invoke(sender, args):
            self.texteditor.Caret.Line = lineno - 1
            self.texteditor.Caret.Column = 0
            self.texteditor.GrabFocus()
            GLib.Timeout.Add(100, self.texteditor.CenterToCaret)
        self.calico.Invoke(invoke)

    def scroll_to_line(self, lineno):
        def invoke(sender, args):
            self.texteditor.Caret.Line = lineno - 1
            self.texteditor.Caret.Column = 0
            self.texteditor.GrabFocus()
            GLib.Timeout.Add(100, self.texteditor.ScrollToCaret)
        self.calico.Invoke(invoke)

    def get_dirty(self):
        return self.texteditor.Document.IsDirty

    def set_clean(self):
        self.texteditor.Document.SetNotDirtyState()

    def get_line(self):
        return self.texteditor.Caret.Line + 1

def MakeDocument(*args, **kwargs):
    return TextEditorDocument(*args, **kwargs)

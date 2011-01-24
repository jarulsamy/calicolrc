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

import Gtk
import GLib
import Pango
import os

class MyScrolledWindow(Gtk.ScrolledWindow):
    """
    Wrapper so that we can keep track of (and access) additional
    items.
    """

class Document(object):
    """
    Base Document for all languages.
    """
    def __init__(self, filename, pyjama, language="python"):
        if filename:
            filename = os.path.abspath(filename)
        self.filename = filename
        self.pyjama = pyjama
        self.language = language
        if self.filename:
            self.title = os.path.basename(self.filename)
        else:
            self.title = "New %s Script" % self.language.title()
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
            self.pyjama.editor.on_close_tab(self.widget)
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
            self.save_as()
        if self.filename:
            fp = open(self.filename, "w")
            fp.write(self.get_text())
            fp.close()
            self.set_clean()
            return True
        return False

    def save_as(self):
        retval = False
        fc = Gtk.FileChooserDialog("Enter the file to save",
                                   self.pyjama.editor.window,
                                   Gtk.FileChooserAction.Save,
                                   "Cancel", Gtk.ResponseType.Cancel,
                                   "Save", Gtk.ResponseType.Accept)
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            self.filename = fc.Filename
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
        self.language = self.pyjama.get_language_from_filename(self.filename)

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

class TextViewDocument(Document):
    def configure(self):
        self.textview.Editable = True
        self.textview.WrapMode = Gtk.WrapMode.Char
        self.textview.AcceptsTab = True

    def grab_focus(self):
        self.textview.GrabFocus()

    def get_dirty(self):
        return self.textview.Buffer.Modified

    def set_clean(self):
        self.textview.Buffer.Modified = False

    def search(self):
        pass
        # start = self.textview.Buffer.StartIter
        # end = self.textview.Buffer.EndIter
        # (found,  mstart, mend) = start.ForwardSearch("def ", 
        #    Gtk.TextSearchFlags.TextOnly, end)
        # if found:
        #     self.textview.Buffer.SelectRange(mstart, mend)
        
    def make_widget(self):
        Document.make_widget(self)
        # FIXME: need to handle keys in editor:
        self.textview = Gtk.TextView() 
        self.widget.Add(self.textview)
        self.textview.Buffer.ModifiedChanged += self.on_modified
        self.modify_font()
        self.textview.Show()
        self.widget.Show()
        self.grab_focus()

    def modify_font(self, font=None):
        if font is None:
            font = self.pyjama.get_fontname()
        self.textview.ModifyFont(font)

    def get_text(self):
        return str(self.textview.Buffer.Text)

    def insert_at_cursor(self, text):
        self.textview.Buffer.InsertAtCursor(text)

    def get_selected_text(self):
        (selected, start, end) = self.textview.Buffer.GetSelectionBounds()
        if selected:
            return self.textview.Buffer.GetText(start, end, True)
        return None

    def goto_line(self, lineno):
        """
        Go to a line number in the current document. Call with [1, n]. Internally,
        linenumbers are zero-based.
        """
        def invoke(sender, args):
            line = self.textview.Buffer.GetIterAtLine(lineno - 1)
            self.textview.Buffer.PlaceCursor(line)
            GLib.Timeout.Add(200, lambda: self.textview.ScrollToIter(line, 0.4, True, 0, 1.0))
        Gtk.Application.Invoke(invoke)

    def open(self):
        self.textview.Buffer.Text = "".join(file(self.filename).xreadlines())
        self.grab_focus()

    def text_has_focus(self):
        return self.textview.HasFocus

try:
    import clr
    clr.AddReference("gtksourceview2-sharp")
    import GtkSourceView

    class SourceViewDocument(TextViewDocument):
        def begin_not_undoable(self):
            self.textview.Buffer.BeginNotUndoableAction()

        def end_not_undoable(self):
            self.textview.Buffer.EndNotUndoableAction()
            self.set_clean()

        def make_widget(self):
            TextViewDocument.make_widget(self)
            self.lang_manager = GtkSourceView.SourceLanguageManager()
            # FIXME: need to handle keys in editor:
            self.textview = GtkSourceView.SourceView()
            self.modify_font()
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
            self.widget.Add(self.textview)
            self.textview.Show()
            self.widget.Show()
            self.grab_focus()

        def configure(self):
            TextViewDocument.configure(self)
            self.textview.ShowLineNumbers = True
            self.textview.HighlightCurrentLine = True

        def on_change_file(self):
            TextViewDocument.on_change_file(self)
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
except:
    SourceViewDocument = None

try:
    import clr
    clr.AddReference("Mono.TextEditor")
    from Mono.TextEditor import TextEditor, Highlighting
    path, filename = os.path.split(__file__)
    # /.../Pyjama/src/
    Highlighting.SyntaxModeService.LoadStylesAndModes(
        os.path.join(path, "..", "bin", "SyntaxModes"))

    class TextEditorDocument(Document):
        def __init__(self, filename, pyjama, language="python"):
            Document.__init__(self, filename, pyjama, language)

        def make_widget(self):
            Document.make_widget(self)
            # FIXME: need to handle keys in editor:
            self.texteditor = TextEditor()
            #self.modify_font()
            # FIXME: not quite right: should reset on undo and doesn't 
            # set on space
            self.texteditor.Document.DocumentUpdated += self.on_modified
            self.texteditor.Document.MimeType = "text/x-%s" % self.language
            self.widget.Add(self.texteditor)
            self.texteditor.Show()
            self.widget.Show()
            self.grab_focus()

        def configure(self):
            #self.texteditor.Options.AllowTabsAfterNonTabs = 
            #self.texteditor.Options.AutoIndent
            #self.texteditor.Options.CanResetZoom
            #self.texteditor.Options.HighlightCaretLine
            #self.texteditor.Options.HighlightMatchingBracket
            #self.texteditor.Options.IndentationSize
            #self.texteditor.Options.IndentationString
            #self.texteditor.Options.RemoveTrailingWhitespaces
            #self.texteditor.Options.RulerColumn
            #self.texteditor.Options.ShowEolMarkers
            #self.texteditor.Options.ShowFoldMargin
            #self.texteditor.Options.ShowIconMargin
            self.texteditor.Options.ShowInvalidLines = False
            self.texteditor.Options.ShowLineNumberMargin = True
            #self.texteditor.Options.ShowRuler
            #self.texteditor.Options.ShowSpaces
            #self.texteditor.Options.ShowTabs
            #self.texteditor.Options.TabSize
            #self.texteditor.Options.TabsToSpaces
            #self.texteditor.Options.WordFindStrategy
            #self.texteditor.Options.Zoom
            #self.texteditor.Options.ZoomIn
            #self.texteditor.Options.ZoomOut
            #self.texteditor.Options.ZoomReset

        def on_change_file(self):
            Document.on_change_file(self)
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

        def modify_font(self, font):
            pass

        def get_text(self):
            return self.texteditor.Document.Text

        def get_selected_text(self):
            return self.texteditor.SelectedText

        def insert_at_cursor(self, text):
            pass

        def open(self):
            self.texteditor.Document.Text = "".join(file(self.filename).xreadlines())
            self.grab_focus()
            
        def goto_line(self, lineno):
            def invoke(sender, args):
                self.texteditor.Caret.Line = lineno - 1
                self.texteditor.Caret.Column = 0
                self.texteditor.ScrollToCaret()
            Gtk.Application.Invoke(invoke)

        def get_dirty(self):
            return self.texteditor.Document.IsDirty

        def set_clean(self):
            self.texteditor.Document.SetNotDirtyState()

except:
    TextEditorDocument = None

def MakeDocument(*args, **kwargs):
    # In order of Preference:
    if TextEditorDocument:
        return TextEditorDocument(*args, **kwargs)
    elif SourceViewDocument:
        return SourceViewDocument(*args, **kwargs)
    else:
        return TextViewDocument(*args, **kwargs)


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
        self.filename = filename
        if filename:
            filename = os.path.abspath(filename)
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

    def get_dirty(self):
        pass

    def make_widget(self):
        """
        Side-effect creates self.widget with a .document property to
        self. Hook up any signals to text editing widget.
        """
        self.widget = MyScrolledWindow()
        self.widget.document = self

    def on_key_press(self, obj, event):
	print event
	return False

    def modify_font(self, font):
        pass

    def get_text(self):
        pass

    def insert_at_cursor(self, text):
        pass

    def open(self):
        pass

    def save(self):
        """
        """
        if not self.filename:
            self.save_as()
        if self.filename:
            fp = open(self.filename, "w")
            fp.write(self.get_text())
            fp.close()
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

    def on_change_file(self):
        self.language = self.pyjama.get_language_from_filename(self.filename)

class TextViewDocument(Document):
    def begin_not_undoable(self):
        if (hasattr(self, "textview") and 
            hasattr(self.textview.Buffer, "BeginNotUndoableAction")):
            self.textview.Buffer.BeginNotUndoableAction()

    def end_not_undoable(self):
        if hasattr(self, "textview"): 
            if hasattr(self.textview.Buffer, "EndNotUndoableAction"):
                self.textview.Buffer.EndNotUndoableAction()
            self.textview.Buffer.Modified = False

    def configure(self):
        self.textview.Editable = True
        self.textview.WrapMode = Gtk.WrapMode.Char
        self.textview.AcceptsTab = True

    def grab_focus(self):
        self.textview.GrabFocus()

    def get_dirty(self):
        return self.textview.Buffer.Modified

    def search(self):
        pass
        # start = self.textview.Buffer.StartIter
        # end = self.textview.Buffer.EndIter
        # (found,  mstart, mend) = start.ForwardSearch("def ", 
        #    Gtk.TextSearchFlags.TextOnly, end)
        # if found:
        #     self.textview.Buffer.SelectRange(mstart, mend)
        
    def make_widget(self):
        super(TextViewDocument, self).make_widget()
        # FIXME: need to handle keys in editor:
        self.textview = Gtk.TextView() 
        self.widget.Add(self.textview)
        self.textview.Buffer.ModifiedChanged += self.on_modified
        self.modify_font()
        self.textview.Show()
        self.widget.Show()
        self.textview.GrabFocus()

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
        self.textview.Buffer.Text = "".join(file(self.filename).readlines())
        self.textview.GrabFocus()

    def save(self):
        """
        """
        retval = super(TextViewDocument, self).save()
        if retval:
            self.textview.Buffer.Modified = False
        return retval

    def text_has_focus(self):
        return self.textview.HasFocus

try:
    import clr
    clr.AddReference("gtksourceview2-sharp")
    import GtkSourceView

    class SourceViewDocument(TextViewDocument):
        def make_widget(self):
            super(TextViewDocument, self).make_widget()
            self.lang_manager = GtkSourceView.SourceLanguageManager()
            # FIXME: need to handle keys in editor:
            self.textview = GtkSourceView.SourceView()
            self.modify_font()
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)
            self.widget.Add(self.textview)
            self.textview.Show()
            self.widget.Show()
            self.textview.GrabFocus()

        def configure(self):
            super(SourceViewDocument, self).configure()
            self.textview.ShowLineNumbers = True
            self.textview.HighlightCurrentLine = True

        def on_change_file(self):
            super(Document, self).on_change_file()
            self.textview.Buffer.Language = self.lang_manager.GetLanguage(
                self.language)

except:
    SourceViewDocument = None

def MakeDocument(*args, **kwargs):
    if SourceViewDocument:
        return SourceViewDocument(*args, **kwargs)
    else:
        return TextViewDocument(*args, **kwargs)


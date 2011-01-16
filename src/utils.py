import Gtk
import System
from System.Threading import ManualResetEvent

DEBUG = False

def _(text): return text

def Array(*list):
    Type = type(list[0])
    dimensions = [len(list)]
    return System.Array.CreateInstance(Type, *dimensions)

class Language(object):
    def __init__(self, language, extension):
        self.language = language
        self.extension = extension

class CustomStream(System.IO.Stream):
    def __init__(self, textview, tag=None):
        self.textview = textview
        self.tag = tag

    def set_output(self, textview):
        self.textview = textview

    def write(self, text):
        #if DEBUG:
        #    print text,
        #    return

        #ev = ManualResetEvent(False)
        #def invoke(sender, args):
        #    end = self.textview.Buffer.EndIter
        #    self.textview.Buffer.InsertWithTagsByName(end, text, "red")
        #    self.goto_end()
        #    ev.Set()
        #Gtk.Application.Invoke(invoke)
        #ev.WaitOne()
        if self.tag == "red":
            System.Console.Error.Write(text)
        else:
            System.Console.Write(text)

    def goto_end(self):
        end = self.textview.Buffer.EndIter
        insert_mark = self.textview.Buffer.InsertMark 
        self.textview.Buffer.PlaceCursor(end)
        self.textview.ScrollToMark(insert_mark, 0.0, True, 0, 1.0)

    def Write(self, bytes, offset, count):
        if DEBUG:
            text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
            print text,
            return

        ev = ManualResetEvent(False)
        def invoke(sender, args):
            text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
            if self.tag:
                end = self.textview.Buffer.EndIter
                self.textview.Buffer.InsertWithTagsByName(end, text, self.tag)
            else:
                self.textview.Buffer.InsertAtCursor(text)
            self.goto_end()
            ev.Set()
        Gtk.Application.Invoke(invoke)
        ev.WaitOne()

    @property
    def CanRead(self):
        return False

    @property
    def CanSeek(self):
        return False

    @property
    def CanWrite(self):
        return True

    def Flush(self):
        pass

    def Close(self):
        pass

    @property
    def Position(self):
        return 0

def pick_file():
    global retval
    retval = None
    Gtk.Application.Init()
    fc = Gtk.FileChooserDialog("Select a file",
                               None,
                               Gtk.FileChooserAction.Open,
                               "Cancel", Gtk.ResponseType.Cancel,
                               "Open", Gtk.ResponseType.Accept)
    Gtk.Application.Invoke(lambda obj, args: fc.ShowAll())
    ev = ManualResetEvent(False)
    def get_filename_cb(obj, args):
        global retval
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            retval = fc.Filename
        fc.Destroy()
        ev.Set()
    Gtk.Application.Invoke(get_filename_cb)
    ev.WaitOne()
    return retval



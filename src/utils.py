import Gtk
import System
from System.Threading import Mutex, ManualResetEvent

MUTEX = Mutex(False, "PyjamaMutex")

def _(text): return text

def Array(*list):
    Type = type(list[0])
    dimensions = [len(list)]
    return System.Array.CreateInstance(Type, *dimensions)

class Language(object):
    def __init__(self, language, extension):
        self.language = language
        self.extension = extension

class ConsoleStream(System.IO.Stream):
    def __init__(self, tag=None):
        self.tag = tag

    def write(self, text):
        if self.tag == "red":
            System.Console.Error.Write(text)
        else:
            System.Console.Write(text)

    def goto_end(self):
        pass

    def Write(self, bytes, offset, count):
        text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
        print text,

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

class CustomStream(System.IO.Stream):
    def __init__(self, textview, tag=None):
        self.textview = textview
        self.tag = tag

    def set_output(self, textview):
        self.textview = textview

    def write(self, text):
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
        ev = ManualResetEvent(False)
        def invoke(sender, args):
            MUTEX.WaitOne()
            text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
            if self.tag:
                end = self.textview.Buffer.EndIter
                self.textview.Buffer.InsertWithTagsByName(end, text, self.tag)
            else:
                self.textview.Buffer.InsertAtCursor(text)
            self.goto_end()
            ev.Set()
            MUTEX.ReleaseMutex()
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


def zipfiles(files_and_dirs, filename):
    import clr
    clr.AddReference("ICSharpCode.SharpZipLib")
    import ICSharpCode
    ZipFile = ICSharpCode.SharpZipLib.Zip.ZipFile
    # first, make sure all files are files. if dir, expand to all files
    # in it, recursively:
    files = get_files(files_and_dirs)
    zf = ZipFile.Create(filename)
    zf.BeginUpdate()
    path, basename = os.path.split(filename)
    base, ext = os.path.splitext(basename)
    # Make zip file be in a directory called same name as zip (minus .zip)
    for (file, newfile) in rename_files_for_zip_package(files, base):
        zf.Add(file, newfile)
    zf.CommitUpdate()
    zf.Close()

def get_files(items):
    import glob
    files = []
    for item in items:
        if os.path.isfile(item):
            files.append(item)
        elif os.path.isdir(item):
            files.extend(get_files(glob.glob(item + os.path.sep + "*")))
        else:
            pass # ignore things that don't exist, etc.
    return files

def rename_files_for_zip_package(files, package_name):
    """
    Finds the longest common pathname among files.
    Assumes all file paths are absolute.
    """
    prefix = ""
    done = False
    original_files = files[:]
    while not done:
        candidate = None
        # Go through files to get next candidate
        for file in files:
            if os.path.sep not in file:
                done = True
                break
            path, filename = os.path.split(file)
            path_parts = path.split(os.path.sep)
            if candidate is None:
                # first, get a candidate prefix that they all might share
                candidate = path_parts[0]
            if len(path_parts) > 0 and path_parts[0] == candidate:
                pass # ok so far
            else:
                done = True
        if candidate is None:
            done = True
        if not done:
            prefix += (candidate + os.path.sep)
            # go through files and remove candidate:
            newfiles = []
            for file in files:
                path, filename = os.path.split(file)
                path_parts = path.split(os.path.sep)
                path_parts.pop(0)
                if len(path_parts) == 0:
                    newfiles.append(filename)
                else:
                    newfiles.append(os.path.sep.join(path_parts) + os.path.sep + filename)
            files = newfiles
    return zip(original_files, 
               [file.replace(prefix, package_name + os.path.sep) 
                for file in original_files])

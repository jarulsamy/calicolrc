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
import clr
clr.AddReference("agsXMPP")
clr.AddReference("Mono.Posix")

import Gtk
import Gdk
import System
from System.Threading import Mutex, ManualResetEvent
import Mono
import agsXMPP

import os
import traceback

MUTEX = Mutex(False, "CalicoMutex")

_ = Mono.Unix.Catalog.GetString

def Array(*list):
    Type = type(list[0])
    dimensions = [len(list)]
    return System.Array.CreateInstance(Type, *dimensions)

class Language(object):
    def __init__(self, language, extensions):
        self.language = language
        self.extensions = extensions

    def get_document_type(self):
        return "Script"

    def get_document_class(self):
        from document import MakeDocument
        return MakeDocument

    def get_engine_class(self):
        raise NotImplementedError()

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
        print(text, end="")

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
    def __init__(self, calico, textview, tag=None):
        self.calico = calico
        self.textview = textview
        self.tag = tag

    def set_output(self, textview):
        self.textview = textview

    def write(self, text):
        if self.tag == "red":
            # send it to Write method:
            System.Console.Error.Write(text)
        else:
            System.Console.Write(text)

    def goto_end(self):
        end = self.textview.Buffer.EndIter
        insert_mark = self.textview.Buffer.InsertMark 
        self.textview.Buffer.PlaceCursor(end)
        self.textview.ScrollToMark(insert_mark, 0.0, True, 0, 0.5)

    def Write(self, bytes, offset, count):
        """
        Method for use by the lower-level CLR language.
        """
        def invoke(sender, args):
            text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
            if self.tag:
                end = self.textview.Buffer.EndIter
                self.textview.Buffer.InsertWithTagsByName(end, text, self.tag)
                if self.tag == "red":
                    self.calico.last_error += text
            else:
                self.textview.Buffer.InsertAtCursor(text)
            insert_mark = self.textview.Buffer.InsertMark 
            end = self.textview.Buffer.EndIter
            self.textview.Buffer.PlaceCursor(end)
            self.textview.ScrollToMark(insert_mark, 0.0, True, 0, 0.5)
        self.calico.Invoke(invoke, wait=True)

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

class Chat:
    def __init__(self, calico, user, password, debug=False):
        self.status = _("offline")
        self.calico = calico
        self.user = user
        self.password = password
        self.debug = debug
        self.alert = True
        self.server = "myro.roboteducation.org"
        self.port = 5222
        self.messages = []

        self.client = agsXMPP.XmppClientConnection(self.server, self.port)
        self.client.UseSSL = False

        self.client.OnReadXml += agsXMPP.XmlHandler(self.OnReadXml)
        self.client.OnWriteXml += agsXMPP.XmlHandler(self.OnWriteXml)
        self.client.OnLogin += self.OnLogin
        self.client.OnMessage += agsXMPP.protocol.client.MessageHandler(self.OnMessage)
        self.client.OnError += agsXMPP.ErrorHandler(self.OnError)
        self.client.OnAuthError += agsXMPP.XmppElementHandler(self.OnAuthError)
        self.client.OnClose += self.close

        try:
            self.client.Open(self.user, self.password, "CalicoClient", 5)
        except Exception, exp:
            traceback.print_exp()

    def send(self, to, text):
        self.client.Send(
            agsXMPP.protocol.client.Message("%s@%s" % (to, self.server),
                                            agsXMPP.protocol.client.MessageType.chat,
                                            text))

    def messages_waiting(self):
        return len(self.messages) > 0

    def receive(self):
        retval, self.messages[:] = self.messages[:], []
        return retval

    def close(self, sender=None):
        self.status = _("offline")
        self.client.Close()

    def OnLogin(self, sender):
        if self.user == "testname":
            return
        self.status = _("online")
        self.send("", "2") # make this my only login
        if self.alert:
            self.calico.Print(_("You are now logged in as '%s'.") % self.user)
            self.send("admin", "[broadcast]\nroom: %s\n%s" % ("General",
                "%s has joined the discussion" % self.user))
        if self.debug:
            print("LOGIN:", self.user)
            #self.client.SendMyPresence()

    def OnError(self, sender, exp):
        print("ERROR in Chat:", self.user, exp)

    def OnAuthError(self, sender, xml):
        self.status = _("rejected")
        if self.alert:
            self.calico.Print(_("You were not allowed to log in.") + "\n" +
                              _("Please check your ID and password."))
        if self.debug:
            print("AUTHERROR:", self.user, xml)

    def OnReadXml(self, sender, xml):
        if self.debug:
            print("READXML:", self.user, xml)

    def OnWriteXml(self, sender, xml):
        #self.calico.update_status()
        if self.debug:
            print("WRITEXML:", self.user, xml)

    def OnMessage(self, sender, msg):
        """
        msg.From = "id@server/resource"
        """
        mfrom = "%s@%s" % (msg.From.User, msg.From.Server)
        if self.debug:
            print("MESSAGE:", self.user, msg)
        if str(msg.Body).startswith("[broadcast]"):
            line0, rest = str(msg.Body).split("\n", 1) # [broadcast]
            if "\n" in rest:
                line1, message = rest.split("\n", 1) # from
                fromheader, address = [item.strip() for item in line1.split(":")]
                name, domain = address.split("@")
                #if self.calico.chat:
                #    self.calico.chat.display_message(name, message)
                self.calico.Print("%s -> %s" % (name, message))
                return
        elif str(msg.Body).startswith("[blast]"):
            line0, rest = str(msg.Body).split("\n", 1) # [blast]
            if "\n" in rest:
                line1, rest = rest.split("\n", 1) # from:
                fromheader, address = [item.strip() for item in line1.split(":", 1)]
                line2, rest = rest.split("\n", 1) # type:
                typeheader, type = [item.strip() for item in line2.split(":", 1)]
                line3, code = rest.split("\n", 1) # filename:
                fileheader, filename = [item.strip() for item in line3.split(":", 1)]
                self.calico.blast(address, type, filename, code)
                return
        elif str(msg.Body).startswith("[result]"):
            line0, rest = str(msg.Body).split("\n", 1) # [blast]
            self.calico.Print(rest)
            return
        elif str(msg.Body).startswith("[update]"):
            if True:
                line0, rest = str(msg.Body).split("\n", 1) # [update]
                line1, rest = rest.split("\n", 1) # room:
                header, value = [item.strip() for item in line1.split(":", 1)]
                #if header == "room":
                #    self.calico.chat.room = value
            return
        elif str(msg.Body).startswith("[info]"):
            # ignore
            return
        # otherwise, receive for user's own use
        self.messages.append((mfrom, msg.Body))

class MySearchInFilesEntry(Gtk.Entry):
    def set_calico(self, calico):
        self.calico = calico

    def OnKeyPressEvent(self, event):
        if event.Key == Gdk.Key.Return:
            self.calico.grep(self.Text)
            return True
        elif event.Key == Gdk.Key.Escape:
            def invoke(sender, args):
                self.searchbar.Hide()
                if self.searchbar.shell:
                    self.searchbar.shell.GrabFocus()
            self.calico.Invoke(invoke)
            return True
        elif event.Key == Gdk.Key.Up:
            return True
        elif event.Key == Gdk.Key.Down:
            return True
        else:
            retval = Gtk.Entry.OnKeyPressEvent(self, event)
            return retval

class MyEntry(Gtk.Entry):
    def set_calico(self, calico):
        self.calico = calico

    def set_searchbar(self, searchbar):
        self.searchbar = searchbar

    def OnKeyPressEvent(self, event):
        if event.Key == Gdk.Key.Return and bool(event.State & Gdk.ModifierType.ShiftMask):
            self.searchbar.prev()
            return True
        elif event.Key == Gdk.Key.Return:
            self.searchbar.next()
            return True
        elif event.Key == Gdk.Key.Up:
            return True
        elif event.Key == Gdk.Key.Down:
            return True
        elif event.Key == Gdk.Key.Escape:
            def invoke(sender, args):
                self.searchbar.Hide()
                if self.searchbar.editor.document:
                    self.searchbar.editor.document.texteditor.GrabFocus()
            self.calico.Invoke(invoke)
            return True
        else:
            retval = Gtk.Entry.OnKeyPressEvent(self, event)
            if retval:
                self.searchbar.next(from_selection_start=True)
            return retval

class SearchBar(Gtk.HBox):
    def set_calico(self, calico):
        self.calico = calico
        self.entry.set_calico(self.calico)

    def __init__(self, *args, **kwargs):
        self.editor = None
        self.label = Gtk.Label(_("Search: "))
        self.entry = MyEntry()
        self.entry.set_searchbar(self)
        # Previous:
        prev_button = Gtk.Button()
        img = Gtk.Image(Gtk.Stock.GoUp, Gtk.IconSize.Menu)
        prev_button.Add(img)
        prev_button.Clicked += self.prev
        # Next:
        next_button = Gtk.Button()
        next_button.Clicked += self.next
        img = Gtk.Image(Gtk.Stock.GoDown, Gtk.IconSize.Menu)
        next_button.Add(img)
        # Close:
        close_button = Gtk.Button()
        img = Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu)
        close_button.Add(img)
        close_button.Clicked += self.close
        # Add them
        self.PackStart(self.label, False, False, 0)
        self.PackStart(self.entry, True, True, 0)
        self.PackStart(prev_button, False, False, 0)
        self.PackStart(next_button, False, False, 0)
        self.PackStart(close_button, False, False, 0)

    def set_editor(self, editor):
        self.editor = editor

    def next(self, obj=None, event=None, from_selection_start=False):
        def invoke(sender, args):
            if self.editor.document:
                self.Show()
                self.editor.document.texteditor.SearchPattern = self.entry.Text
                selection_range = self.editor.document.texteditor.SelectionRange
                if selection_range:
                    if from_selection_start:
                        offset = selection_range.Offset
                    else:
                        offset = selection_range.Offset + selection_range.Length
                else:
                    offset = self.editor.document.texteditor.Caret.Offset
                search_result = self.editor.document.texteditor.SearchForward(offset)
                if search_result:
                    offset = search_result.Offset
                    length = search_result.Length
                    self.editor.document.texteditor.Caret.Offset = offset + length
                    self.editor.document.texteditor.SetSelection(offset, 
                                                                 offset + length)
                    self.editor.document.texteditor.ScrollToCaret()
        self.calico.Invoke(invoke)

    def prev(self, obj=None, event=None):
        def invoke(sender, args):
            if self.editor.document:
                self.Show()
                self.editor.document.texteditor.SearchPattern = self.entry.Text
                selection_range = self.editor.document.texteditor.SelectionRange
                if selection_range:
                    offset = selection_range.Offset
                else:
                    offset = self.editor.document.texteditor.Caret.Offset
                search_result = self.editor.document.texteditor.SearchBackward(offset)
                if search_result:
                    offset = search_result.Offset
                    length = search_result.Length
                    self.editor.document.texteditor.Caret.Offset = offset + length
                    self.editor.document.texteditor.SetSelection(offset, 
                                                                 offset + length)
                    self.editor.document.texteditor.ScrollToCaret()
        self.calico.Invoke(invoke)

    def open(self, obj, event):
        self.search_on()

    def search_on(self):
        def invoke(sener, args):
            self.entry.GrabFocus()
            self.entry.SelectRegion(0, len(self.entry.Text))
            self.ShowAll()
        self.calico.Invoke(invoke)

    def close(self, obj, event):
        self.search_off()

    def search_off(self):
        def invoke(sender, args):
            self.Hide()
            if self.editor.document:
                self.editor.document.texteditor.GrabFocus()
        self.calico.Invoke(invoke)

class SearchInFilesBar(Gtk.HBox):
    def set_calico(self, calico):
        self.calico = calico
        self.entry.set_calico(self.calico)

    def __init__(self, *args, **kwargs):
        self.shell = None
        self.label = Gtk.Label(_("Search in files:"))
        self.entry = MySearchInFilesEntry()
        # Close:
        close_button = Gtk.Button()
        img = Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu)
        close_button.Add(img)
        close_button.Clicked += self.close
        # Add them
        self.PackStart(self.label, False, False, 0)
        self.PackStart(self.entry, True, True, 0)
        self.PackStart(close_button, False, False, 0)

    def set_shell(self, shell):
        self.shell = shell
        self.entry.set_calico(self.shell.calico)

    def open(self, obj, event):
        self.search_on()

    def search_on(self):
        def invoke(sener, args):
            self.entry.GrabFocus()
            self.entry.SelectRegion(0, len(self.entry.Text))
            self.ShowAll()
        self.calico.Invoke(invoke)

    def close(self, obj, event):
        self.search_off()

    def search_off(self):
        def invoke(sender, args):
            self.Hide()
            if self.shell.textview:
                self.shell.textview.GrabFocus()
        self.calico.Invoke(invoke)

class StatusBar(Gtk.VBox):
    def init(self, *labels):
        self.labels = labels
        self.hbox = Gtk.HBox()
        self.statusbars = {}
        for label in labels:
            self.statusbars[label] = Gtk.Statusbar()
            self.statusbars[label].Push(0, "%s: " % label)
            self.statusbars[label].HasResizeGrip = (label == labels[-1])
            self.hbox.PackStart(self.statusbars[label])
            self.statusbars[label].Show()
        # Add other items, hidden here
        ###############################################
        self.hbox.Show()
        self.PackStart(self.hbox)
        self.Show()

    def set(self, label, value):
        self.statusbars[label].Pop(0)
        self.statusbars[label].Push(0, _("%s: %s") % (label, value))

def get_colors():
    return ["red", "blue", "purple", "black", "green"]

class Plugin:
    """
    A base class for plugins.
    """
    def __init__(self, calico):
        self.calico = calico
        self.init()

    def init(self):
        pass

    def log_action(self, action, **data):
        pass

    def on_switch_page(self, notebook, args):
        pass

    def on_key_press(self, widget, *data):
        return False

    def on_select_page(self, *data):
        return False

    def on_create_window(self, window):
        from shell import ShellWindow
        from chat import ChatWindow
        from editor import EditorWindow
        print("Plugin setup for window '%s'" % window)
        self.window = window                  # Main window object
        self.gtk_window = self.window.window  # Gtk.Window
        if isinstance(self.window, ShellWindow):
            self.window_type = "shell"
        elif isinstance(self.window, EditorWindow):
            self.window_type = "editor"
            self.window.notebook.SwitchPage += self.on_switch_page
        elif isinstance(self.window, ChatWindow):
            self.window_type = "chat"
        # Handle keystrokes:
        self.gtk_window.insert_key_press_handler(self.on_key_press, 0)
        # Errors:
        # Running files:
        
def OpenUrl(url):
    System.Diagnostics.Process.Start(url)


import System

def _(text): return text

def Array(*list):
    Type = type(list[0])
    dimensions = [len(list)]
    return System.Array.CreateInstance(Type, *dimensions)

class CustomStream(System.IO.Stream):
    def __init__(self, textview, tag=None):
        self.textview = textview
        self.tag = tag

    def set_output(self, textview):
        self.textview = textview

    def write(self, text):
        end = self.textview.Buffer.EndIter
        self.textview.Buffer.InsertWithTagsByName(end, text, "red")
        self.goto_end()

    def goto_end(self):
        end = self.textview.Buffer.EndIter
        insert_mark = self.textview.Buffer.InsertMark 
        self.textview.Buffer.PlaceCursor(end)
        self.textview.ScrollToMark(insert_mark, 0.0, True, 0, 1.0)

    def Write(self, bytes, offset, count):
        # Turn the byte-array back into a string
        text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
        #print "write: %s" % repr(text)
        #if not text.endswith("\n"):
        #    text += "\n"
        if self.tag:
            end = self.textview.Buffer.EndIter
            self.textview.Buffer.InsertWithTagsByName(end, text, self.tag)
        else:
            self.textview.Buffer.InsertAtCursor(text)
        self.goto_end()

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


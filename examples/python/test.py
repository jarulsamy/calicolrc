import Gtk

x = None

def goto_file(filename, lineno):
    global x
    x = filename, lineno
    print x

def popup(textview, popup_args):
    mark = textview.Buffer.InsertMark
    iter = textview.Buffer.GetIterAtMark(mark)
    start = textview.Buffer.GetIterAtLine(iter.Line)
    iter.Offset = start.Offset + start.CharsInLine
    text = textview.Buffer.GetText(start, iter, False) # invisible chars

    match = re.search('File \"(.*)\", line (\d*)', text)
    if match: # 'File "<string>", line 167'
        filename, lineno = match.groups()
        lineno = int(lineno)
        menuitem = Gtk.MenuItem("Goto")
        menuitem.Show()
        menuitem.Activated += lambda sender, args: goto_file(filename, lineno)
        popup_args.Menu.Append(menuitem)

window = Gtk.Window("Sample")
textview = Gtk.TextView()
window.Add(textview)
textview.PopulatePopup += popup
window.ShowAll()

#textview = pyjama.editor.notebook.GetNthPage(1).Children[0]

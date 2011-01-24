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
import Gdk
import System

class MyWindow(Gtk.Window):
    def on_key_press(self, eventkey):
        """
        Handle the key press for the entire window. Return True if
        handled.  This is generally overloaded.
        """
        return False

    def set_on_key_press(self, on_key_press):
        """
        Set the key press handler method:
        """
        self.on_key_press = on_key_press

    def OnKeyPressEvent(self, eventkey):
        """
        Override the main event handler to insert ours beforehand.
        """
        return (self.on_key_press(eventkey) or 
                Gtk.Window.OnKeyPressEvent(self, eventkey))

class Window(object):
    def make_gui(self, menu, toolbar):
        self.menubar = Gtk.MenuBar()
        self.submenu = {}
        self.accel_group = {}
        for text, items in menu:
            submenu = Gtk.Menu()
            accel_group = Gtk.AccelGroup()
            self.window.AddAccelGroup(accel_group)
            for row in items:
                if row is None: # separator
                    menuitem = Gtk.SeparatorMenuItem()
                elif isinstance(row, str): # submenu
                    self.submenu[row] = Gtk.Menu()
                    menuitem = Gtk.MenuItem(row)
                    menuitem.Submenu = self.submenu[row]
                    submenu.Append(menuitem)
                    continue
                elif len(row) == 2: # submenu, entry
                    menuname, row = row
                    menuitem = self.make_menuitem(row, accel_group)
                    self.submenu[menuname].Append(menuitem)
                    self.accel_group[menuname] = accel_group
                    continue
                else:
                    menuitem = self.make_menuitem(row, accel_group)
                submenu.Append(menuitem)
            menuitem = Gtk.MenuItem(text)
            menuitem.Submenu = submenu
            self.menubar.Append(menuitem)
        # ---------------------
        self.toolbar = Gtk.Toolbar()
        self.toolbar.ToolbarStyle = Gtk.ToolbarStyle.Icons
        i = 0
        self.toolbar_buttons = {}
        for (img, function) in toolbar:
            if img is None:
                tool_item = Gtk.SeparatorToolItem()
            else:
                tool_item = Gtk.ToolButton(img)
                self.toolbar_buttons[img] = tool_item
            if function:
                tool_item.Clicked += function
            self.toolbar.Insert(tool_item, i)
            i += 1

    def make_menuitem(self, row, accel_group):
        subtext, img, accel, function = row
        if img is None:
            menuitem = Gtk.MenuItem(subtext)
        else:
            menuitem = Gtk.ImageMenuItem(img, accel_group)
        if function:
            menuitem.Activated += function
        if accel:
            key, mod = Gtk.Accelerator.Parse(accel)
            menuitem.AddAccelerator("activate", accel_group,
                                    key, mod,
                                    Gtk.AccelFlags.Visible)
        return menuitem

    def print_view(self):
        papersize = Gtk.PaperSize(Gtk.PaperSize.Default)
        setup = Gtk.PageSetup()
        setup.PaperSize = papersize
        po = Gtk.PrintOperation()
        po.DefaultPageSetup = setup
        po.BeginPrint += self.begin_print
        po.DrawPage += self.draw_page
        po.ExportFilename = "output.pdf"
        response = po.Run(Gtk.PrintOperationAction.PrintDialog, 
                          self.window)
        # or Preview

    def begin_print(self, operation, context):
        return
        width = context.get_width()
        height = context.get_height()
        self.layout = context.create_pango_layout()

        self.layout.set_font_description(
            pango.FontDescription("Sans " + str(self.font_size)) )
        self.layout.set_width(int(width*pango.SCALE))
        self.layout.set_text(self.text)
    
        num_lines = self.layout.get_line_count()
        self.lines_per_page = math.floor( 
            context.get_height() / (self.font_size/2) )
        pages = ( int(math.ceil( float(num_lines) /
                                 float(self.lines_per_page) ) ) )
        operation.set_n_pages(pages)

    def draw_page (self, operation, context, page_number):
        return
        cr = context.get_cairo_context()
        cr.set_source_rgb(0, 0, 0)
        start_line = page_number * self.lines_per_page
        if page_number + 1 != operation.props.n_pages:
            end_line = start_line + self.lines_per_page
        else:
            end_line = self.layout.get_line_count()

        cr.move_to(0, 0)
        iter = self.layout.get_iter()
        i=0
        while 1:
            if i > start_line:
                line = iter.get_line()
                cr.rel_move_to(0, self.font_size/2)
                cr.show_layout_line(line)
            i += 1
            if not (i < end_line and iter.next_line()):
                break

        if page_number + 1 != operation.props.n_pages:
            end_line = start_line + self.lines_per_page
        else:
            end_line = self.layout.get_line_count()

    def print_view2(self):
        pj = Gtk.PrintJob(Gnome.PrintConfig.Default())
        dialog = Gnome.PrintDialog(pj, "Print Test", 0)
        response = dialog.Run()
        if (response == Gtk.PrintButtons.Cancel):
            dialog.Hide()
            dialog.Dispose()
	    return
        gpc = pj.Context
        self.print_view_to_gc(gpc)
        pj.Close()
	if response == Gtk.PrintButtons.Print: 
            pj.Print()
        elif response == Gtk.PrintButtons.Preview:
            Gtk.PrintJobPreview(pj, "Print Test").Show()
        dialog.Hide()
	dialog.Dispose()

    def print_view3(self):
        pc = Gnome.PrintConfig.Default()
        Gtk.Print.RunPageSetupDialog(self.window, None, pc)

    def print_view_to_gc(self, gpc):
        Gnome.Print.Beginpage(gpc, "Pyjama")
        Gnome.Print.Moveto(gpc, 1, 700)
        Gnome.Print.Show(gpc, self.textview.Buffer.Text)
        Gnome.Print.Showpage(gpc)

    def get_textview(self):
        """
        Return the default textview for this window.
        """
        return self.textview

    def indent_region(self, obj, event):
        """
        Indent the selected region of text.
        """
        (selected, start, end) = self.get_textview().Buffer.GetSelectionBounds()
        if selected:
            text = self.get_textview().Buffer.GetText(start, end, True)
            retval = ""
            for line in text.split("\n"):
                retval += "    " + line + "\n"
            def invoke(sender, args):
                self.get_textview().Buffer.DeleteSelection(True, True)
                self.get_textview().Buffer.InsertAtCursor(retval)
            Gtk.Application.Invoke(invoke)
            # FIXME: reselect this text
            # FIXME: slight bug in replace (too many newlines?)
            return True
        return False

    def unindent_region(self, obj, event):
        """
        Unindent the selected region of text.
        """
        (selected, start, end) = self.get_textview().Buffer.GetSelectionBounds()
        if selected:
            text = self.get_textview().Buffer.GetText(start, end, True)
            retval = ""
            for line in text.split("\n"):
                retval += line[4:] + "\n"
            def invoke(sender, args):
                self.get_textview().Buffer.DeleteSelection(True, True)
                self.get_textview().Buffer.InsertAtCursor(retval)
            Gtk.Application.Invoke(invoke)
            # FIXME: reselect this text
            # FIXME: slight bug in replace (too many newlines?)
            return True
        return False

    def save_as_filename(self, filename):
        """
        Save the contents of the current window into a file.
        Filename can be at least a jpg, or png.
        """
        def invoke(sender, args):
            drawable = self.window.GdkWindow
            colormap = drawable.Colormap 
            size = drawable.GetSize()
            pixbuf = Gdk.Pixbuf.FromDrawable(drawable, colormap, 0, 0, 0, 0, size[0], size[1])
            pixbuf.Save(filename, filename.rsplit(".", 1)[-1])
        Gtk.Application.Invoke(invoke)


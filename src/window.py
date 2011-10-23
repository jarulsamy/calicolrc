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
import Gdk

import os
import glob

# Calico modules:
import Graphics
from utils import _

class MyWindow(Gtk.Window):

    def insert_key_press_handler(self, on_key_press, position):
        """
        Set the key press handler method:
        """
        if hasattr(self, "key_press_handlers"):
            self.key_press_handlers.insert(0, on_key_press)
        else:
            self.key_press_handlers = [on_key_press]

    def add_key_press_handler(self, on_key_press):
        """
        Set the key press handler method:
        """
        if hasattr(self, "key_press_handlers"):
            self.key_press_handlers.append(on_key_press)
        else:
            self.key_press_handlers = [on_key_press]

    def OnKeyPressEvent(self, eventkey):
        """
        Override the main event handler to insert ours beforehand.
        """
        if hasattr(self, "key_press_handlers"):
            for key_press_handler in self.key_press_handlers:
                result = key_press_handler(self, eventkey)
                if result:
                    return result
        Gtk.Window.OnKeyPressEvent(self, eventkey)

class Window(object):
    def __init__(self, calico):
        self.calico = calico
        for plugin in self.calico.plugins:
            self.calico.plugins[plugin].on_create_window(self)
        
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
                    if "/" in row:
                        path = row.split("/")
                        path_so_far = ""
                        parent = submenu
                        for part in path:
                            if path_so_far:
                                path_so_far = path_so_far + "/" + part
                            else:
                                path_so_far = part
                            if path_so_far not in self.submenu:
                                self.submenu[path_so_far] = Gtk.Menu()
                                self.accel_group[path_so_far] = accel_group
                                menuitem = Gtk.MenuItem(part)
                                menuitem.Submenu = self.submenu[path_so_far]
                                parent.Append(menuitem)
                            parent = self.submenu[path_so_far]
                    else:
                        self.submenu[row] = Gtk.Menu()
                        self.accel_group[row] = accel_group
                        menuitem = Gtk.MenuItem(row)
                        menuitem.Submenu = self.submenu[row]
                        submenu.Append(menuitem)
                    continue
                elif len(row) == 2: # submenu, entry
                    menuname, row = row
                    menuitem = self.make_menuitem(row, accel_group)
                    self.submenu[menuname].Append(menuitem)
                    continue
                else:
                    menuitem = self.make_menuitem(row, accel_group)
                submenu.Append(menuitem)
            # Now, add menu to hang the submenu
            # if "/" in text:
            #     path = row.split("/")
            #     path_so_far = ""
            #     for part in path:
            #         if path_so_far:
            #             path_so_far = "/" + part
            #         else:
            #             path_so_far = part
            #         menuitem = Gtk.MenuItem(text)
            #         menuitem.Submenu = submenu
            #         self.menubar.Append(menuitem)
            # else:
            menuitem = Gtk.MenuItem(text)
            menuitem.Submenu = submenu
            self.menubar.Append(menuitem)
        # ---------------------
        self.toolbar = Gtk.Toolbar()
        self.toolbar.ToolbarStyle = Gtk.ToolbarStyle.Icons
        i = 0
        self.toolbar_buttons = {}
        for (img, function, tooltip) in toolbar:
            if img is None:
                tool_item = Gtk.SeparatorToolItem()
            else:
                tool_item = Gtk.ToolButton(img)
                self.toolbar_buttons[img] = tool_item
            if function:
                tool_item.Clicked += function
            tooltips = Gtk.Tooltips()
            tooltips.SetTip(tool_item, tooltip, None)
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

    def make_new_file_menu(self):
        retval = []
        for lang in sorted(self.calico.languages):
            retval.append(
                (_("New %s %s") % (
                        lang.title(), 
                        self.calico.languages[lang].get_document_type()), None,
                 None, lambda o,e,lang=lang: self.on_new_file(o, e, lang))
                )
        return retval

    def make_recents_menu(self):
        retval = []
        retval.append(_("Recent files")) # submenu
        for file in self.calico.config.get("calico.recent_files"):
            if file and os.path.isfile(file):
                menufile = file.replace("_", "__")
                retval.append((_("Recent files"),
                               (menufile, None, None,
                                lambda o,e,file=file: self.select_or_open(file))))
        return retval

    def make_examples_menu(self):
        retval = []
        for lang in self.calico.engine.get_languages():
            menulang = lang.title()
            retval.append(_("Examples") + "/" + menulang) # submenu
            path = os.path.join(self.calico.calico_root, "examples", lang, "*")
            files = glob.glob(path)
            files.sort()
            for file in files:
                if os.path.isdir(file):
                    continue
                path, menufile = os.path.split(file)
                menufile = menufile.replace("_", "__")
                retval.append((_("Examples") + "/" + menulang,
                               (menufile, None, None,
                                lambda o,e,file=file: self.select_or_open(file))))
        return retval

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
        self.calico.Invoke(invoke)

def make_barchart(title, labels, data):
    win = Graphics.Window(title, 600, 300)
    maxvalue = max(data)
    for text in labels:
        # put labels across
        label = Label(Graphics.Point(x, y), text)
        label.draw(win)
    for value in data:
        # put bar in place
        # x,y is bottom, right-hand point
        height = 34
        bar = Rectangle(Graphics.Point(x, y - height),
                        Graphics.Point(x + width, y))
        bar.draw(win)
    win.ShowAll()
    return win


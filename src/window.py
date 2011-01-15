import Gtk
import Gnome

class Window(object):
    def make_gui(self, menu, toolbar):
        self.menubar = Gtk.MenuBar()
        for text, items in menu:
            submenu = Gtk.Menu()
            accel_group = Gtk.AccelGroup()
            self.window.AddAccelGroup(accel_group)
            for row in items:
                if row is None:
                    menuitem = Gtk.SeparatorMenuItem()
                else:
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

    def print_view(self):
        pj = Gnome.PrintJob(Gnome.PrintConfig.Default())
        dialog = Gnome.PrintDialog(pj, "Print Test", 0)
        response = dialog.Run()
        if (response == Gnome.PrintButtons.Cancel):
            dialog.Hide()
            dialog.Dispose()
	    return
        gpc = pj.Context
        self.print_view_to_gc(gpc)
        pj.Close()
	if response == Gnome.PrintButtons.Print: 
            pj.Print()
        elif response == Gnome.PrintButtons.Preview:
            Gnome.PrintJobPreview(pj, "Print Test").Show()
        dialog.Hide()
	dialog.Dispose()

    def print_view_to_gc(self, gpc):
        Gnome.Print.Beginpage(gpc, "Pyjama")
        Gnome.Print.Moveto(gpc, 1, 700)
        Gnome.Print.Show(gpc, self.textview.Buffer.Text)
        Gnome.Print.Showpage(gpc)

import Gtk

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
        pj = Gtk.PrintJob(Gtk.PrintConfig.Default())
        dialog = Gtk.PrintDialog(pj, "Print Test", 0)
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

    def print_view_to_gc(self, gpc):
        Gtk.Print.Beginpage(gpc, "Pyjama")
        Gtk.Print.Moveto(gpc, 1, 700)
        Gtk.Print.Show(gpc, self.textview.Buffer.Text)
        Gtk.Print.Showpage(gpc)

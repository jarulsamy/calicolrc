import Gtk, Pango

class MyWindow(Gtk.Window):
    def __init__(self, title):
        self.SetDefaultSize(600, 400)
        #this.DeleteEvent += new DeleteEventHandler(OnMyWindowDelete);

        self.da = Gtk.DrawingArea()
        self.da.SetSizeRequest(600, 400)
        self.da.ExposeEvent += self.Expose_Event

        self.layout = Pango.Layout(self.PangoContext)
        self.layout.Width = Pango.Units.FromPixels(600)
        self.layout.Wrap = Pango.WrapMode.Word
        self.layout.Alignment = Pango.Alignment.Left
        self.layout.FontDescription = Pango.FontDescription.FromString("Ahafoni CLM Bold 100")
        self.layout.SetMarkup("<span color=\"blue\">Hello World</span>")
        self.Add(self.da)

        def invoke(sender, args):
            self.ShowAll()

        Gtk.Application.Invoke(invoke)

    def Expose_Event(self, obj, args):
        self.da.GdkWindow.DrawLayout(self.da.Style.TextGC(Gtk.StateType.Normal),
                                5, 5, self.layout)


mywin = MyWindow("My Window")
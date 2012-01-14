using System;
using System.Xml;
using System.Text;
using System.Collections.Generic;
using System.Reflection;

public class JigsawWidget : Gtk.ScrolledWindow
{
  public Jigsaw.Canvas cvs = null;
  //Gtk.Fixed cvsFixed = null;
  public object document = null;

  public JigsawWidget(string modulePath) {
	this.CanFocus = true;
	cvs = new Jigsaw.Canvas(modulePath, 900, 600, 3000, 2000);
	this.Add (cvs);
  }

//  public JigsawWidget() {
//    this.CanFocus = true;
//	this.VscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//	this.HscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//	
//	// Add the Canvas and other utility widgets
//	cvsFixed = new Gtk.Fixed();
//	cvs = new Jigsaw.Canvas(900, 600);
//	cvsFixed.Put(cvs, 0,0);
//	this.AddWithViewport(cvsFixed);
//  }
}

// -----------------------------------------------------------------------
public class MainWindow : Gtk.Window
{
	// The main jigsaw canvas
	Jigsaw.Canvas js;
	
	// All the various UI widgets whose properties change at runtime depending upon app mode
	Gtk.Button tbRunRun = null;
	Gtk.Button tbRunPause = null;
	Gtk.Button tbRunStep = null;
	Gtk.Button tbRunStop = null;
	Gtk.MenuItem miRunRun = null;
	Gtk.MenuItem miRunPause = null;
	Gtk.MenuItem miRunStep = null;
	Gtk.MenuItem miRunStop = null;

	public MainWindow () : base(Gtk.WindowType.Toplevel)
	{
		// Main window
		this.Title = "Jigsaw";
		this.Icon = new global::Gdk.Pixbuf (global::System.IO.Path.Combine (global::System.AppDomain.CurrentDomain.BaseDirectory, "plugin.png"));
		this.WindowPosition = ((global::Gtk.WindowPosition)(4));
		this.DefaultWidth = 1000;
		this.DefaultHeight = 700;
		this.DeleteEvent += new Gtk.DeleteEventHandler (this.OnDeleteEvent);

		// - - - VBox
		Gtk.VBox vb = new Gtk.VBox(false, 0);
		
		Gtk.AccelGroup agrp = new Gtk.AccelGroup();
		this.AddAccelGroup(agrp);
		
		// - - - File menu
		Gtk.Menu muFile = new Gtk.Menu();

		Gtk.MenuItem miNew = new Gtk.MenuItem("_New");
		miNew.Activated += new EventHandler(OnFileNew);
		muFile.Append(miNew);
		miNew.AddAccelerator("activate", agrp, (int)'N', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		Gtk.MenuItem miOpen = new Gtk.MenuItem("_Open");
		miOpen.Activated += new EventHandler(OnFileOpen);
		muFile.Append(miOpen);
		miOpen.AddAccelerator("activate", agrp, (int)'O', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);
		
		Gtk.MenuItem miFileSep1 = new Gtk.MenuItem();
		muFile.Append(miFileSep1);
		
		Gtk.MenuItem miSave = new Gtk.MenuItem("_Save");
		miSave.Activated += new EventHandler(OnFileSave);
		muFile.Append(miSave);
		miSave.AddAccelerator("activate", agrp, (int)'S', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);
		
		Gtk.MenuItem miSaveAs = new Gtk.MenuItem("Save _As...");
		miSaveAs.Activated += new EventHandler(OnFileSaveAs);
		muFile.Append(miSaveAs);
		miSaveAs.AddAccelerator("activate", agrp, (int)'S', Gdk.ModifierType.ControlMask | Gdk.ModifierType.ShiftMask, Gtk.AccelFlags.Visible);
		
		Gtk.MenuItem miSaveAsPython = new Gtk.MenuItem("Save As _Python...");
		miSaveAsPython.Activated += new EventHandler(OnFileSaveAsPython);
		muFile.Append(miSaveAsPython);
		
		Gtk.MenuItem miSep2 = new Gtk.MenuItem();
		muFile.Append(miSep2);

		Gtk.MenuItem miUse = new Gtk.MenuItem("_Use Library...");
		miUse.Activated += new EventHandler(OnFileUseLibrary);
		muFile.Append(miUse);
		
		Gtk.MenuItem miMap = new Gtk.MenuItem("_Generate Library Map File...");
		miMap.Activated += new EventHandler(OnFileGenLibMap);
		muFile.Append(miMap);
		
		Gtk.MenuItem miSep3 = new Gtk.MenuItem();
		muFile.Append(miSep3);
		
		Gtk.MenuItem miExit = new Gtk.MenuItem("_Quit");
		miExit.Activated += new EventHandler(OnFileExit);
		muFile.Append(miExit);
		miExit.AddAccelerator("activate", agrp, (int)'Q', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		// - - - View Menu
		Gtk.Menu muView = new Gtk.Menu();
		
		Gtk.MenuItem miViewZoomIn = new Gtk.MenuItem("_Zoom In");
		miViewZoomIn.Activated += new EventHandler(OnViewZoomIn);
		muView.Append(miViewZoomIn);
		miViewZoomIn.AddAccelerator("activate", agrp, (int)'+', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		Gtk.MenuItem miViewZoomOut = new Gtk.MenuItem("Zoom _Out");
		miViewZoomOut.Activated += new EventHandler(OnViewZoomOut);
		muView.Append(miViewZoomOut);
		miViewZoomOut.AddAccelerator("activate", agrp, (int)'-', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		Gtk.MenuItem miViewZoom100 = new Gtk.MenuItem("_Normal Size");
		miViewZoom100.Activated += new EventHandler(OnViewZoom100);
		muView.Append(miViewZoom100);
		miViewZoom100.AddAccelerator("activate", agrp, (int)'0', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);
		
		Gtk.MenuItem miViewSep1 = new Gtk.MenuItem();
		muView.Append(miViewSep1);
		
		Gtk.MenuItem miViewToggleInset = new Gtk.MenuItem("_Toggle Inset");
		miViewToggleInset.Activated += new EventHandler(OnViewToggleInset);
		muView.Append(miViewToggleInset);
		miViewToggleInset.AddAccelerator("activate", agrp, (int)'T', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		Gtk.MenuItem miViewProperties = new Gtk.MenuItem("_Properties");
		miViewProperties.Activated += new EventHandler(OnViewProperties);
		muView.Append(miViewProperties);
		miViewProperties.AddAccelerator("activate", agrp, (int)'P', Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);
		
		// Run Menu
		Gtk.Menu muRun = new Gtk.Menu();
		
		miRunRun = new Gtk.MenuItem("_Run");
		miRunRun.Activated += new EventHandler(OnEngineRun);
		muRun.Append(miRunRun);		
		miRunRun.AddAccelerator("activate", agrp, (int)Gdk.Key.F5, Gdk.ModifierType.None, Gtk.AccelFlags.Visible);

		miRunPause = new Gtk.MenuItem("_Pause");
		miRunPause.Activated += new EventHandler(OnEnginePause);
		muRun.Append(miRunPause);
		miRunPause.Sensitive = false;
		miRunPause.AddAccelerator("activate", agrp, (int)Gdk.Key.Break, Gdk.ModifierType.ControlMask, Gtk.AccelFlags.Visible);

		miRunStep = new Gtk.MenuItem("St_ep");
		miRunStep.Activated += new EventHandler(OnEngineStep);
		muRun.Append(miRunStep);
		miRunStep.AddAccelerator("activate", agrp, (int)Gdk.Key.F11, Gdk.ModifierType.ShiftMask, Gtk.AccelFlags.Visible);

		miRunStop = new Gtk.MenuItem("_Stop");
		miRunStop.Activated += new EventHandler(OnEngineStop);
		muRun.Append(miRunStop);
		miRunStop.Sensitive = false;
		miRunStop.AddAccelerator("activate", agrp, (int)Gdk.Key.F5, Gdk.ModifierType.ShiftMask, Gtk.AccelFlags.Visible);

		// Build menu Bar
		Gtk.MenuBar mb = new Gtk.MenuBar();
		
		Gtk.MenuItem miFile = new Gtk.MenuItem("_File");
		miFile.Submenu = muFile;
		mb.Append(miFile);
		
		Gtk.MenuItem miView = new Gtk.MenuItem("_View");
		miView.Submenu = muView;
		mb.Append(miView);

		Gtk.MenuItem miRun = new Gtk.MenuItem("_Run");
		miRun.Submenu = muRun;
		mb.Append(miRun);
		
		vb.PackStart(mb, false, false, 0);
		
		// --- Toolbar
		Gtk.Toolbar tb = new Gtk.Toolbar();
		
		Gtk.Image imNew = new Gtk.Image(Gtk.Stock.New, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbNew = new Gtk.Button(imNew);
		tbNew.Clicked += new EventHandler(OnFileNew);
		tbNew.TooltipText = "Start a new program";
		tb.Add(tbNew);
		
		Gtk.Image imOpen = new Gtk.Image(Gtk.Stock.Open, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbOpen = new Gtk.Button(imOpen);
		tbOpen.Clicked += new EventHandler(OnFileOpen);
		tbOpen.TooltipText = "Open a program from a file";
		tb.Add(tbOpen);
		
		Gtk.Image imSave = new Gtk.Image(Gtk.Stock.Save, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbSave = new Gtk.Button(imSave);
		tbSave.Clicked += new EventHandler(OnFileSave);
		tbSave.TooltipText = "Save the program";
		tb.Add(tbSave);
		
		Gtk.VSeparator tbSep1 = new Gtk.VSeparator();
		tb.Add(tbSep1);
		
		Gtk.Image imZoomIn = new Gtk.Image(Gtk.Stock.ZoomIn, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbZoomIn = new Gtk.Button(imZoomIn);
		tbZoomIn.Clicked += new EventHandler(OnViewZoomIn);
		tbZoomIn.TooltipText = "Zoom in";
		tb.Add(tbZoomIn);
		
		Gtk.Image imZoomOut = new Gtk.Image(Gtk.Stock.ZoomOut, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbZoomOut = new Gtk.Button(imZoomOut);
		tbZoomOut.Clicked += new EventHandler(OnViewZoomOut);
		tbZoomOut.TooltipText = "Zoom out";
		tb.Add(tbZoomOut);
		
		Gtk.Image imZoom100 = new Gtk.Image(Gtk.Stock.Zoom100, Gtk.IconSize.SmallToolbar);
		Gtk.Button tbZoom100 = new Gtk.Button(imZoom100);
		tbZoom100.Clicked += new EventHandler(OnViewZoom100);
		tbZoom100.TooltipText = "Reset zoom to 100%";
		tb.Add(tbZoom100);

		Gtk.VSeparator tbSep2 = new Gtk.VSeparator();
		tb.Add(tbSep2);
		
		Gtk.Image imRunRun = new Gtk.Image(Gtk.Stock.MediaPlay, Gtk.IconSize.SmallToolbar);
		tbRunRun = new Gtk.Button(imRunRun);
		tbRunRun.Clicked += new EventHandler(OnEngineRun);
		tbRunRun.TooltipText = "Run the program from the beginning";
		tb.Add(tbRunRun);

		Gtk.Image imRunPause = new Gtk.Image(Gtk.Stock.MediaPause, Gtk.IconSize.SmallToolbar);
		tbRunPause = new Gtk.Button(imRunPause);
		tbRunPause.Clicked += new EventHandler(OnEnginePause);
		tbRunPause.TooltipText = "Pause the program";
		tbRunPause.Sensitive = false;
		tb.Add(tbRunPause);
		
		Gtk.Image imRunStep = new Gtk.Image(Gtk.Stock.Execute, Gtk.IconSize.SmallToolbar);
		tbRunStep = new Gtk.Button(imRunStep);
		tbRunStep.Clicked += new EventHandler(OnEngineStep);
		tbRunStep.TooltipText = "Advance program one step";
		tb.Add(tbRunStep);
		
		Gtk.Image imRunStop = new Gtk.Image(Gtk.Stock.MediaStop, Gtk.IconSize.SmallToolbar);
		tbRunStop = new Gtk.Button(imRunStop);
		tbRunStop.Clicked += new EventHandler(OnEngineStop);
		tbRunStop.TooltipText = "Stop and reset the program";
		tbRunStop.Sensitive = false;
		tb.Add(tbRunStop);
		
		vb.PackStart (tb, false, false, 0);
		
		// Embedded scrolled window
//		Gtk.ScrolledWindow sw = new Gtk.ScrolledWindow ();
//		sw.CanFocus = true;
//		sw.VscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//		sw.HscrollbarPolicy = Gtk.PolicyType.Always; //Gtk.PolicyType.Never; //((global::Gtk.PolicyType)(0));
//		this.Add(sw);
		
		// Add the Canvas and other utility widgets
//		Jigsaw.Canvas cvs = new Jigsaw.Canvas(1200, 900);
//		Gtk.Fixed cvsFixed = new Gtk.Fixed();
//		cvsFixed.Put(cvs, 0,0);
//		sw.AddWithViewport(cvsFixed);
		
		// Create core Jigsaw Canvas
		// "/Programs/Mono/Calico-dev/modules"
		// For running directly from Calico/languages/Jigsaw outside of Calico:
		//js = new Jigsaw.Canvas(System.IO.Path.Combine("..", "..", "modules"), 
		//                       900, 600, 3000, 2000);	
		
		string modulePath = null;
		modulePath = "../../modules"; // @"C:\Programs\Mono\Calico-dev\modules\";
		js = new Jigsaw.Canvas(modulePath, 900, 600, 3000, 2000);		
		js.JigsawRun += new EventHandler(OnJigsawRun);
		js.JigsawStop += new EventHandler(OnJigsawStop);
		js.JigsawStep += new EventHandler(OnJigsawStep);
		js.JigsawPause += new EventHandler(OnJigsawPause);
		
		vb.Add(js);

		this.Add(vb);

		// Let 'er rip
		this.ShowAll();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnJigsawRun(object sender, EventArgs a)
	{	
		// Set toolbar sensitive status
		tbRunPause.Sensitive = true;
		tbRunStop.Sensitive = true;
		tbRunStep.Sensitive = false;
		tbRunRun.Sensitive = false;
		
		// Copy to menu items
		miRunRun.Sensitive = tbRunRun.Sensitive;
		miRunPause.Sensitive = tbRunPause.Sensitive;
		miRunStep.Sensitive = tbRunStep.Sensitive;
		miRunStop.Sensitive = tbRunStop.Sensitive;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnJigsawStop(object sender, EventArgs a)
	{	
		tbRunRun.Sensitive = true;
		tbRunStep.Sensitive = true;
		tbRunStop.Sensitive = false;
		tbRunPause.Sensitive = false;
		
		// Copy to menu items
		miRunRun.Sensitive = tbRunRun.Sensitive;
		miRunPause.Sensitive = tbRunPause.Sensitive;
		miRunStep.Sensitive = tbRunStep.Sensitive;
		miRunStop.Sensitive = tbRunStop.Sensitive;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnJigsawStep(object sender, EventArgs a)
	{	
		tbRunStep.Sensitive = true;
		tbRunStop.Sensitive = true;
		tbRunRun.Sensitive = true;
		tbRunPause.Sensitive = false;
		
		// Copy to menu items
		miRunRun.Sensitive = tbRunRun.Sensitive;
		miRunPause.Sensitive = tbRunPause.Sensitive;
		miRunStep.Sensitive = tbRunStep.Sensitive;
		miRunStop.Sensitive = tbRunStop.Sensitive;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnJigsawPause(object sender, EventArgs a)
	{	
		Console.WriteLine ("OnJigsawPause");
		tbRunStep.Sensitive = true;
		tbRunStop.Sensitive = true;
		tbRunRun.Sensitive = true;
		tbRunPause.Sensitive = false;
		
		// Copy to menu items
		miRunRun.Sensitive = tbRunRun.Sensitive;
		miRunPause.Sensitive = tbRunPause.Sensitive;
		miRunStep.Sensitive = tbRunStep.Sensitive;
		miRunStop.Sensitive = tbRunStop.Sensitive;
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{	// Close the application when the main window is closed 
		
		if (js.ResolveUnsavedChanges() == false) {
			a.RetVal = true;
			return;
		}
		
		a.RetVal = false;
		Gtk.Application.Quit ();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileNew(object sender, EventArgs a)
	{	
		js.Stop();
		js.OnFileNew(sender, a);
		this.UpdateWindowTitle ();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileOpen(object sender, EventArgs a)
	{	
		js.Stop();
		js.OnFileOpen(sender, a);
		this.UpdateWindowTitle ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileSave(object sender, EventArgs a)
	{	
		js.Stop();
		js.OnFileSave(sender, a);
		this.UpdateWindowTitle ();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileSaveAs(object sender, EventArgs a)
	{	
		js.Stop();
		js.OnFileSaveAs(sender, a);
		this.UpdateWindowTitle ();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileSaveAsPython(object sender, EventArgs a)
	{	
		js.Stop();
		js.OnFileSaveAsPython(sender, a);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	private void UpdateWindowTitle() {
		this.Title = "Jigsaw";
		string filepath = js.CurrentPath;
		if (filepath != null && filepath != "") this.Title += " - " + System.IO.Path.GetFileName(filepath);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileExit (object sender, EventArgs a)
	{	// Close the application when the main window is closed
		js.Stop();
		Gtk.Application.Quit();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileUseLibrary(object sender, EventArgs a)
	{
		js.Stop();
		
		// Get map file path
		Gtk.FileChooserDialog fc = null;
		fc = new Gtk.FileChooserDialog("Library to load", 
		                               this,
		                               Gtk.FileChooserAction.Open,
		                               "Cancel", Gtk.ResponseType.Cancel,
		                               "Load",   Gtk.ResponseType.Accept);
		
		Gtk.FileFilter f1 = new Gtk.FileFilter();
		f1.Name = "Library map files";
		f1.AddPattern("*.map");
		fc.AddFilter(f1);
		
		Gtk.FileFilter f3 = new Gtk.FileFilter();
		f3.Name = "All files";
		f3.AddPattern("*.*");
		fc.AddFilter(f3);
		
		int response = fc.Run ();

		// If file not selected, exit
		if (response == (int)Gtk.ResponseType.Cancel) {
			fc.Destroy();
			return;
		}
		
		// Get map file name
		string mapfile = fc.Filename;
		fc.Destroy();
		
		// Check if file does not exist
		if (!System.IO.File.Exists(mapfile)) {
			Gtk.MessageDialog dlg = new Gtk.MessageDialog(
				this,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Warning,
				Gtk.ButtonsType.Ok,
				"Map file not found");
			dlg.Title = "File not found";
			
			Gtk.ResponseType rsp = (Gtk.ResponseType)dlg.Run ();
			dlg.Destroy();
			return;
		}
		
		js.UseLibrary(mapfile);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnFileGenLibMap(object sender, EventArgs a)
	{
		js.Stop();
		
		// Get DLL file path
		Gtk.FileChooserDialog fc = null;
		fc = new Gtk.FileChooserDialog("Library to map", 
		                               this,
		                               Gtk.FileChooserAction.Open,
		                               "Cancel", Gtk.ResponseType.Cancel,
		                               "Open",   Gtk.ResponseType.Accept);
		
		Gtk.FileFilter f1 = new Gtk.FileFilter();
		f1.Name = "DLL files";
		f1.AddPattern("*.dll");
		fc.AddFilter(f1);
		
		Gtk.FileFilter f3 = new Gtk.FileFilter();
		f3.Name = "All files";
		f3.AddPattern("*.*");
		fc.AddFilter(f3);
		
		int response = fc.Run ();

		// If file not selected, exit
		if (response == (int)Gtk.ResponseType.Cancel) {
			fc.Destroy();
			return;
		}
		
		// Get assembly info
		string assembly_name = fc.Filename;
		fc.Destroy();
		
		// Create map file path
		string dir_name = System.IO.Path.GetDirectoryName(assembly_name);
		string base_file = System.IO.Path.GetFileNameWithoutExtension(assembly_name);
		string filename = String.Format ("{0}{1}{2}.map", dir_name, System.IO.Path.DirectorySeparatorChar, base_file);
		
		// Check if to overwrite
		if (System.IO.File.Exists(filename)) {
			Gtk.MessageDialog dlg = new Gtk.MessageDialog(
				this,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Warning,
				Gtk.ButtonsType.YesNo,
				"Map file already exists. Overwrite it?");
			dlg.Title = "Overwrite?";
			
			Gtk.ResponseType rsp = (Gtk.ResponseType)dlg.Run ();
			dlg.Destroy();
			if (rsp == Gtk.ResponseType.No) return;
		}
		
		// Create XML map file -- temp modified from Reflection.Utils.Mapping.Save()
		XmlWriterSettings settings = new XmlWriterSettings();
		settings.Indent = true;
		settings.IndentChars = "    ";
		settings.Encoding = Encoding.ASCII;
		
		try
		{
			using (XmlWriter xw = XmlWriter.Create(filename, settings)) {
				xw.WriteStartElement("map");
				xw.WriteAttributeString("path", assembly_name);
				xw.WriteElementString("docstring", "");
				
			  	foreach (string type_name in Reflection.Utils.getTypeNames(assembly_name))
				{	//Console.WriteLine (type_name);
					Type type = Reflection.Utils.getType(assembly_name, type_name);
					
					// Write constructors
					foreach (ConstructorInfo ci in Reflection.Utils.getConstructors(type))
					{	// write it
						xw.WriteStartElement("constructor");
				  	    xw.WriteAttributeString("assembly_name", base_file);
					    xw.WriteAttributeString("type_name", type_name);
					    xw.WriteAttributeString("constructor_name", ci.Name);
						xw.WriteElementString("docstring", "");
						
					    foreach (ParameterInfo pi in ci.GetParameters()) {
					      xw.WriteStartElement("parameter");
					      xw.WriteAttributeString("name", pi.Name);
					      xw.WriteAttributeString("type", pi.ParameterType.FullName);
					      xw.WriteAttributeString("default", pi.DefaultValue.ToString());
					      xw.WriteEndElement();
					    }
						
					  	xw.WriteEndElement();
					}
					
					// Write static methods
					foreach (MethodInfo mi in Reflection.Utils.getStaticMethods(type))
					{	// write it
						xw.WriteStartElement("method");
				  	    xw.WriteAttributeString("assembly_name", base_file);
					    xw.WriteAttributeString("type_name", type_name);
					    xw.WriteAttributeString("method_name", mi.Name);
						xw.WriteAttributeString("return_type", mi.ReturnType.FullName);
						xw.WriteElementString("docstring", "");
						
					    foreach (ParameterInfo pi in mi.GetParameters()) {
					      xw.WriteStartElement("parameter");
					      xw.WriteAttributeString("name", pi.Name);
					      xw.WriteAttributeString("type", pi.ParameterType.FullName);
					      xw.WriteAttributeString("default", pi.DefaultValue.ToString());
					      xw.WriteEndElement();
					    }
					  	xw.WriteEndElement();
					}
			  	}
				
				xw.WriteEndElement();	// close map
			}
			
			// Inform user where map file written
			Gtk.MessageDialog dlg2 = new Gtk.MessageDialog(
				this,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Info,
				Gtk.ButtonsType.Ok,
				String.Format ("Map file created at {0}", filename));
			dlg2.Title = "Map File Created";
			
			Gtk.ResponseType rsp2 = (Gtk.ResponseType)dlg2.Run ();
			dlg2.Destroy();
			
		} catch (Exception ex) {
			// Inform user of error
			Gtk.MessageDialog dlg2 = new Gtk.MessageDialog(
				this,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Error,
				Gtk.ButtonsType.Ok,
				String.Format ("Map file creation Failed:\n\n{0}", ex.Message));
			dlg2.Title = "Map File Creation Failed";
			
			Gtk.ResponseType rsp2 = (Gtk.ResponseType)dlg2.Run ();
			dlg2.Destroy();
		}
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnViewZoomIn(object sender, EventArgs a)
	{	
		js.OnViewZoomIn(sender, a);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnViewZoomOut(object sender, EventArgs a)
	{	
		js.OnViewZoomOut(sender, a);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnViewZoom100(object sender, EventArgs a)
	{	
		js.OnViewZoom100(sender, a);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnViewToggleInset(object sender, EventArgs a)
	{	
		js.OnViewToggleInset(sender, a);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnViewProperties(object sender, EventArgs a)
	{	
		js.ShowPropertiesWindow();
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnEngineRun(object sender, EventArgs a)
	{	
		js.Run();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnEnginePause(object sender, EventArgs a)
	{	
		js.Pause();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnEngineStop(object sender, EventArgs a)
	{	
		js.Stop();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	protected void OnEngineStep(object sender, EventArgs a)
	{	
		js.Step();
	}
}
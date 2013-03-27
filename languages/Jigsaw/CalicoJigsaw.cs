//
//  CalicoJigsaw.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
using System;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using Calico;

public class CalicoJigsawEngine : Engine
{
	public class MyTextWriter : TextWriter
	{
		Calico.CustomStream custom;

		public MyTextWriter (Calico.CustomStream custom) : base()
		{
			this.custom = custom;
		}
		
		public override System.Text.Encoding Encoding {
			get { return System.Text.Encoding.UTF8; }
		}
		
		public override void Write (char[] buffer, int index, int count)
		{
			byte [] bytes = new byte[buffer.Length];
			for (int i=0; i<buffer.Length; i++)
				bytes [i] = (byte)buffer [i];
			custom.Write (bytes, index, count);
		}
	}
	
	public CalicoJigsawEngine (LanguageManager manager) : base(manager)
	{
	}

	public override void SetRedirects (CustomStream stdout, 
                      CustomStream stderr)
	{
		System.Console.SetOut (new MyTextWriter (stdout));
		System.Console.SetError (new MyTextWriter (stderr));
	}
}

public class CalicoJigsawDocument : Document
{
	public Jigsaw.Canvas cvs = null;
    
	public CalicoJigsawDocument (Calico.MainWindow calico, string filename) : base(calico, filename, "jigsaw")
	{
		cvs = new Jigsaw.Canvas (
	               System.IO.Path.Combine (calico.path, "../modules"), 100, 100, 3000, 2000);
		this.focus_widget = cvs;
		this.preferredNotebook = "main";
		widget.AddWithViewport (cvs);
		if (filename != null)
			cvs.ReadFile (filename);
		cvs.JigsawRun += new EventHandler(OnJigsawRun);		
		cvs.JigsawStop += new EventHandler(OnJigsawStop);
		cvs.JigsawStep += new EventHandler(OnJigsawStep);
		cvs.JigsawPause += new EventHandler(OnJigsawPause);
		cvs.JigsawError += new EventHandler(OnJigsawError);
		//cvs.JigsawRunBlockStack += new EventHandler(OnJigsawRunBlockStack);
		/*
		widget.Focused += delegate {
		    calico.updateControls(this); 
		};
		*/
		/*
		widget.ButtonPressEvent += delegate (object o, Gtk.ButtonPressEventArgs e) {
		    calico.updateControls(this);
		    cvs.ProcessEvent(e.Event); // HACK: only way to get event passed to canvas!
		};
		*/
		cvs.CanvasChanged += new EventHandler(OnJigsawCanvasChanged);
		/*
		cvs.CanvasChanged += delegate {
		    calico.updateControls(this);
		};
		*/
		cvs.Modified = false;
		cvs.AutoProperties = true;
		calico.ProgramSpeed.Value = cvs.TimeOut;
		widget.ShowAll ();
	}

	public override Gtk.Widget GetPropertyNotebookWidget() {
		// Return Property Notebook Widget
		Jigsaw.PropertyWindow p = new Jigsaw.PropertyWindow(cvs);
		return (Gtk.Widget)p;
	}

    public override bool IsDirty {
        get { return cvs.Modified; }
        set { 
			cvs.Modified = true;
		}
    }

	protected void OnJigsawCanvasChanged(object sender, EventArgs a) {
		Gtk.Application.Invoke( delegate {
			UpdateDocument();
		});
	}
	
	public override string GetText() {
		return cvs.ToXml();
	}
	
	protected void OnJigsawRun(object sender, EventArgs a) {
		calico.OnStartRunning();
	}
	
	protected void OnJigsawStop(object sender, EventArgs a)
	{
		//if (!cvs.IsRunning) {
		//if (cvs.State != Jigsaw.RunningState.Running) {
			if (calico.CurrentDocument == this)
				// FIXME: only do this when this is the toplevel doc which was running
				calico.OnStopRunning();
		//}
	}
	
	protected void OnJigsawStep(object sender, EventArgs a)
	{
		// This is fired when Jigsaw Steps
	}
	
	protected void OnJigsawPause(object sender, EventArgs a)
	{
		// This is fired when Jigsaw pauses, such as hits a breakpoint
		calico.PlayButton.Sensitive = true;
	}
	
	protected void OnJigsawError(object sender, EventArgs a)
	{
		// This is fired when Jigsaw has an error
		calico.OnStopRunning();		
	}
	
	public override double SpeedValue {
		get {return cvs.TimeOut;}
		set {cvs.TimeOut = value;}
	}	
	
	public override void ExecuteFileInBackground ()
	{
		calico.Print (Calico.Tag.Info, "Running Jigsaw script...\n");
		Gtk.Application.Invoke (delegate {
			cvs.Reset ();
			if (cvs.TimeOut <= 1.0) {
				calico.PauseButton.Sensitive = false;
				cvs.Step ();
			} else {
				calico.PauseButton.Sensitive = true;
				cvs.Run ();
			}
		});
	}
	/*
	void override Print ()
	{
		new Printing("base", "filename");
	}	
	*/
	public override void ZoomIn ()
	{
		cvs.DoZoom (1.05);
	}

	public override void ZoomOut ()
	{
		cvs.DoZoom (1.0 / 1.05);
	}

	public override void DefaultZoom()
	{
    	cvs.DoResetZoom();
	}

	public override void ToggleBreakpoint ()
	{
		cvs.ToggleBreakPoint ();
	}

	public override bool HasBreakpointSet {
		get { return cvs.HasBreakPointSet (); }
	}
	
    public override bool AlwaysAllowSpeedAdjustment {
        get { return true; }
    }
	
	public override bool CanSaveAsPython() {
		return true;
	}
	
	public override void Export(Calico.MainWindow calico)
	{
		cvs.Stop();
		string filename = cvs.OnFileSaveAsPython(null, null);
		if (filename != null) {
			calico.FileSavedAs(this, filename); // already opened, updated
			calico.Open(filename);	
		}
	}
       
	public override bool SaveDocument ()
	{
		bool retval = cvs.SaveDocument (filename);
		if (retval) 
			cvs.Modified = false;
		return retval;
	}

	public override bool SearchMore (string s)
	{
		return cvs.SearchMore (s);
	}

	public override bool SearchNext (string s)
	{
		return cvs.SearchNext (s);
	}

	public override bool SearchPrevious (string s)
	{
		return cvs.SearchPrevious (s);
	}
	
	public override void UseLibrary (string filename)
	{
		cvs.UseLibrary (filename);
	}
	
	public override object Selection {
		get { 
			if (cvs.HasSelection()) {
				return ((Jigsaw.CBlock)cvs.GetSelection()).ToXml(cvs);
			} else
				return null;
		}
	}
	
	public override bool Paste(object obj) {
		if (obj is string) {
			Jigsaw.XmlWrapper xr = new Jigsaw.XmlWrapper(obj.ToString(), true);
			bool retval = cvs.ProcessXml(xr, 10, 10);
			cvs.Reset(); // invalidates
			return retval;
		}
		return false;
	}
	
	public override bool HasSelection {
		get { return cvs.HasSelection(); }
	}
	
	public override void Stop ()
	{
		cvs.Stop ();
		cvs.Reset();
		calico.OnStopRunning();
	}
	
	public override void OnPlayButton() {
		if (calico.ProgramSpeed.Value == 0)
			cvs.Step();
		else
			cvs.Run();
	}
	
	public override void OnPauseButton() {
		cvs.Pause();
	}
	
	public override bool Close() {
		// cleanup, return true if no problems
		cvs.HidePropertiesWindow();
		cvs.Stop();
		return true;
	}

	public override string [] GetAuthors() 
	{
	    return new string[] {
		"Mark Russo <russomf@gmail.com>",
		"Doug Blank <dblank@cs.brynmawr.edu>"
	    };
	}

	public override void SetOptionsMenu(Gtk.MenuItem options_menu) {
	    options_menu.Submenu = new Gtk.Menu();
	    // Toggle inset:
	    Gtk.MenuItem menu = new Gtk.MenuItem("Toggle Inset");
	    menu.Activated += new EventHandler(cvs.OnViewToggleInset);
	    ((Gtk.Menu)options_menu.Submenu).Add(menu);
	    menu.Show();
	    // // Show properties:
	    // menu = new Gtk.MenuItem("View Properties");
	    // menu.Activated += delegate { cvs.ShowPropertiesWindow(); };
	    // ((Gtk.Menu)options_menu.Submenu).Add(menu);
	    // menu.Show();
	    // // Auto-view properties:
	    // Gtk.CheckMenuItem miViewAutoProps = new Gtk.CheckMenuItem("Auto-view Properties");
	    // miViewAutoProps.Active = true;
	    // miViewAutoProps.Activated += (sender, a) => { cvs.AutoProperties = (sender as Gtk.CheckMenuItem).Active; };
	    // ((Gtk.Menu)options_menu.Submenu).Add(miViewAutoProps);
	    // miViewAutoProps.Show();
    }
}

public class CalicoJigsawLanguage : Language
{
	public CalicoJigsawLanguage () : 
        base("jigsaw",  "Jigsaw", new string[] { "jig"}, null)
	{
		IsTextLanguage = false;
	}
    
	public override void MakeEngine (LanguageManager manager)
	{
		engine = new CalicoJigsawEngine (manager);
	}

	public override Document MakeDocument (Calico.MainWindow calico, string filename)
	{
		return new CalicoJigsawDocument (calico, filename);
	}

	public static new Language MakeLanguage ()
	{
		return new CalicoJigsawLanguage ();
	}

}


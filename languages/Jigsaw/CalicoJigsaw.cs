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
    
	public CalicoJigsawDocument (Calico.MainWindow calico, string filename) : 
	base(calico, filename, "jigsaw")
	{
		cvs = new Jigsaw.Canvas (
			System.IO.Path.Combine (calico.path, "../modules"), 100, 100, 3000, 2000);
		widget.AddWithViewport (cvs);
		if (filename != null)
			cvs.ReadFile (filename);
		calico.ProgramSpeed.ChangeValue += ChangeTimeOut;
		cvs.JigsawStop += new EventHandler(OnJigsawStop);
		cvs.JigsawStep += new EventHandler(OnJigsawStep);
		cvs.JigsawPause += new EventHandler(OnJigsawPause);
		cvs.JigsawError += new EventHandler(OnJigsawError);
		cvs.CanvasChanged += new EventHandler(OnJigsawCanvasChanged);
		cvs.Modified = false;
		widget.ShowAll ();
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
	
	protected void OnJigsawStop(object sender, EventArgs a)
	{
		calico.OnStopRunning();
	}
	
	protected void OnJigsawStep(object sender, EventArgs a)
	{
	}
	
	protected void OnJigsawPause(object sender, EventArgs a)
	{
	}
	
	protected void OnJigsawError(object sender, EventArgs a)
	{
		calico.OnStopRunning();		
	}
	
	public void ChangeTimeOut(object sender, EventArgs args) {
		Gtk.HScale s = (Gtk.HScale)sender;
		cvs.TimeOut = s.Value;
	}
	
	public override void ExecuteFileInBackground ()
	{
		calico.Print (Calico.Tag.Info, "Running Jigsaw script...\n");
		Gtk.Application.Invoke (delegate {
			cvs.Reset ();
			cvs.Run ();
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

	public override void ToggleBreakpoint ()
	{
		cvs.ToggleBreakPoint ();
	}

	public override bool HasBreakpointSet {
		get { return cvs.HasBreakPointSet (); }
	}
	
	public override bool CanSaveAsPython() {
		return true;
	}
	
	public override void Export(Calico.MainWindow calico)
	{
		cvs.Stop();
		string filename = cvs.OnFileSaveAsPython(null, null);
		if (filename != null)
			calico.Open(filename);
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
	
	public override void Stop ()
	{
		cvs.Stop ();
		calico.OnStopRunning();
	}
	public override string [] GetAuthors() 
	{
        return new string[] {"Mark Russo <russomf@gmail.com>"};
    }
    public override void SetOptionsMenu(Gtk.MenuItem options_menu) {
        options_menu.Submenu = new Gtk.Menu();
		// Toggle inset:
		Gtk.MenuItem menu = new Gtk.MenuItem("Toggle Inset");
		menu.Activated += new EventHandler(cvs.OnViewToggleInset);
		((Gtk.Menu)options_menu.Submenu).Add(menu);
		menu.Show();
		// Show properties:
		menu = new Gtk.MenuItem("View Properties");
		menu.Activated += delegate { cvs.ShowPropertiesWindow(); };
		((Gtk.Menu)options_menu.Submenu).Add(menu);
		menu.Show();
		// Auto-view properties:
		Gtk.CheckMenuItem miViewAutoProps = new Gtk.CheckMenuItem("Auto-view Properties");
		miViewAutoProps.Activated += (sender, a) => { cvs.AutoProperties = (sender as Gtk.CheckMenuItem).Active; };
		((Gtk.Menu)options_menu.Submenu).Add(miViewAutoProps);
		miViewAutoProps.Show();
    }
}

public class CalicoJigsawLanguage : Language
{
	public CalicoJigsawLanguage () : 
        base("jigsaw",  "Jigsaw", new string[] { "jig", "xml" }, null)
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

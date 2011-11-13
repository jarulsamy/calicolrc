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
	public CalicoJigsawEngine (LanguageManager manager) : base(manager)
	{
	}
}

public class CalicoJigsawDocument : Document
{
	public Jigsaw.Canvas cvs = null;
    
	public CalicoJigsawDocument (Calico.MainWindow calico, string filename) : 
	base(calico, filename, "jigsaw")
	{
		cvs = new Jigsaw.Canvas (900, 600, 3000, 2000);
		widget.AddWithViewport (cvs);
		if (filename != null)
			cvs.ReadFile (filename);
		widget.ShowAll ();
	}
	
	public override void ExecuteFileInBackground ()
	{
		calico.Print("Running Jigsaw script...");
		cvs.Run();
		calico.Print("Done");
	}

	public override void ZoomIn ()
	{
		cvs.DoZoom (1.05);
	}

	public override void ZoomOut ()
	{
		cvs.DoZoom (1.0 / 1.05);
	}

        public override void Export(Calico.MainWindow calico) {
	  string filename = cvs.Export();
	  calico.SelectOrOpen(filename);
	}

}

public class CalicoJigsawLanguage : Language
{
	public CalicoJigsawLanguage () : 
        base("jigsaw",  "Jigsaw", new string[] { "jig", "xml" }, null)
	{
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

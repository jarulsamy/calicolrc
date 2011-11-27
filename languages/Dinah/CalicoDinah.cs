//
//  CalicoDinah.cs
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

public class CalicoDinahEngine : Engine
{
	
	public class MyTextWriter : TextWriter {
		Calico.CustomStream custom;
		public MyTextWriter(Calico.CustomStream custom) : base() {
			this.custom = custom;
		}
		
		public override System.Text.Encoding Encoding  {
			get { return System.Text.Encoding.UTF8; }
		}
		
		public override void Write (char[] buffer, int index, int count)
		{
			byte [] bytes = new byte[buffer.Length];
			for (int i=0; i<buffer.Length; i++)
				bytes[i] = (byte)buffer[i];
			custom.Write(bytes, index, count);
		}
	}
	
	public CalicoDinahEngine (LanguageManager manager) : base(manager)
	{
	}
    public override void SetRedirects(CustomStream stdout, 
                      CustomStream stderr) {
		System.Console.SetOut(new MyTextWriter(stdout));
	  	System.Console.SetError(new MyTextWriter(stderr));
    }
}

public class CalicoDinahDocument : Document
{
	public Dinah.DinahWidget dinah = null;
    
	public CalicoDinahDocument (Calico.MainWindow calico, string filename) : 
	base(calico, filename, "dinah")
	{
		dinah = new Dinah.DinahWidget();
		widget.AddWithViewport(dinah);
		if (filename != null)
			dinah.ReadFile (filename);
		widget.ShowAll ();
	}
	
	public override void ExecuteFileInBackground ()
	{
		calico.Print(Calico.Tag.Info, "Running Dinah script...\n");
		dinah.Run();
	}

        public override void Export(Calico.MainWindow calico) {
	  string filename = dinah.Export();
	  calico.SelectOrOpen(filename);
	}

}

public class CalicoDinahLanguage : Language
{
	public CalicoDinahLanguage () : 
        base("dinah",  "Dinah", new string[] { "din"}, null)
	{
	}
    
	public override void MakeEngine (LanguageManager manager)
	{
		engine = new CalicoDinahEngine (manager);
	}

	public override Document MakeDocument (Calico.MainWindow calico, string filename)
	{
		return new CalicoDinahDocument (calico, filename);
	}

	public static new Language MakeLanguage ()
	{
		return new CalicoDinahLanguage ();
	}
}

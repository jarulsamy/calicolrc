//
//  Spreadsheet.cs
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

public class SpreadsheetWidget : Gtk.TreeView {
	
	Gtk.ListStore liststore;

	public SpreadsheetWidget() : base() {
		// Environment table:
		Gtk.CellRendererText renderer = new Gtk.CellRendererText();
		renderer.Editable = true;
		renderer.Edited += (o, args) => OnRendererEdited(o, args, 0);
        AppendColumn("Column 1", renderer, "text", 0);
		renderer = new Gtk.CellRendererText();
		renderer.Editable = true;
		renderer.Edited += (o, args) => OnRendererEdited(o, args, 1);
        AppendColumn("Column 2", renderer, "text", 1);
		Columns[0].SortIndicator = true;
		Columns[1].SortIndicator = true;
		Columns[0].Clickable = true;
		Columns[1].Clickable = true;
		Columns[0].SortColumnId = 0;
		Columns[1].SortColumnId = 1;
		
		
        // Create a ListStore as the Model
        liststore = new Gtk.ListStore(typeof(string), typeof(string));
        Model = liststore;
        ShowAll();
		
		liststore.AppendValues(new object [] {"row1-1", "0"});
		liststore.AppendValues(new object [] {"row2-1", "1"});
		liststore.AppendValues(new object [] {"row3-1", "2"});
		liststore.AppendValues(new object [] {"row4-1", "3"});
	}

	void OnRendererEdited (object o, Gtk.EditedArgs args, int column)
	{
		Gtk.TreeIter iter;
		liststore.GetIter(out iter, new Gtk.TreePath(args.Path));
		liststore.SetValue(iter, column, args.NewText);
	}
}

public class CalicoSpreadsheetDocument : Document
{
	public CalicoSpreadsheetDocument(Calico.MainWindow calico, string filename) : 
	base(calico, filename, "spreadsheet")
	{
		SpreadsheetWidget sheet = new SpreadsheetWidget();
		widget.AddWithViewport (sheet);
		//if (filename != null)
		//	cvs.ReadFile (filename);
		widget.ShowAll ();
	}
}

public class CalicoSpreadsheetLanguage : Language {
	public CalicoSpreadsheetLanguage () : 
        base("spreadsheet",  "Spreadsheet", new string[] { "csv"}, "text/csv")
	{
	  IsTextLanguage = false;
	}
    
	public override Document MakeDocument (Calico.MainWindow calico, string filename)
	{
		return new CalicoSpreadsheetDocument (calico, filename);
	}
	
	public static new Language MakeLanguage ()
	{
		return new CalicoSpreadsheetLanguage ();
	}
}

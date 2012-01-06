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
using IronPython.Runtime; // Operations, List, Tuple, Dict, ...

public class SpreadsheetWidget : Gtk.TreeView {
	
	public Gtk.ListStore liststore;
	Document document;

	public SpreadsheetWidget(Document doc) : base() {
		document = doc;
		EnableGridLines = Gtk.TreeViewGridLines.Both;
		RulesHint = true;
		// Environment table:
		makeColumn();
		int index = 0;
		foreach(string col in new string [] {"A", "B", "C", "D", "E", "F", "G", "H", "I",
											"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
											"T", "U", "V", "W", "X", "Y", "Z"}) {
			makeColumn(col, index + 1);
			index++;
		}
		
        // Create a ListStore as the Model
        liststore = new Gtk.ListStore(makeTypes()); 
        Model = liststore;
        ShowAll();
		
		//liststore.Append();
		for (int i = 0; i < 100; i++) {
			liststore.AppendValues(makeRow());
		}
	}
	
	Type [] makeTypes() {
		Type [] types = new Type[27];
		types[0] = typeof(int);
		for (int i = 1; i < 27; i++) {
			types[i] = typeof(string);
		}
		return types;
	}
	
	object [] makeRow() {
		object [] row = new object[27];
		row[0] = Model.IterNChildren() + 1;
		for (int i = 1; i < 27; i++) {
			row[i] = "";
		}
		return row;
	}
	
	public void makeColumn() {
		// Inital column
		Gtk.CellRendererText renderer = new Gtk.CellRendererText();
		renderer.Background = "lightgray";
		renderer.Editable = false;
		Gtk.TreeViewColumn column = new Gtk.TreeViewColumn("", renderer, "text", 0);
		column.SortIndicator = true;
		column.Clickable = true;
		column.SortColumnId = 0;
        AppendColumn(column);
	}	

	public void makeColumn(string text, int index) {
		Gtk.CellRendererText renderer = new Gtk.CellRendererText();
		renderer.Editable = true;
		renderer.Edited += makeHandler(index);
		Gtk.TreeViewColumn column = new Gtk.TreeViewColumn(text, renderer, "text", index);
		column.Resizable = true;
		column.SortIndicator = true;
		column.Clickable = true;
		column.SortColumnId = index;
		renderer.WidthChars = 15;
		renderer.Ellipsize = Pango.EllipsizeMode.End;
		column.Alignment = .5f;
        AppendColumn(column);
	}	
	
	Gtk.EditedHandler makeHandler(int index) {
		return (o, a) => OnRendererEdited(o, a, index);
	}
	
	void OnRendererEdited (object o, Gtk.EditedArgs args, int column)
	{
		Gtk.TreeIter iter;
		liststore.GetIter(out iter, new Gtk.TreePath(args.Path));
		liststore.SetValue(iter, column, args.NewText);
		document.IsDirty = true;
		document.UpdateDocument();
	}
}

public class CalicoSpreadsheetDocument : Document
{
	public SpreadsheetWidget sheet;	
	public CalicoSpreadsheetDocument(Calico.MainWindow calico, string filename) : 
	base(calico, filename, "spreadsheet")
	{
		DocumentType = "Sheet";
		sheet = new SpreadsheetWidget(this);
		if (filename != null) {
			int row = 0;
			Gtk.TreeIter iter;
			foreach(List line in new Csv.reader(filename).readLines()) {
				int col = 1;
				foreach(string item in line) {
					sheet.liststore.GetIterFromString(out iter, String.Format("{0}:{1}", row, col));
					sheet.liststore.SetValue(iter, col, item);
					col++;
				}
				row++;
			}
		}
		widget.AddWithViewport (sheet);
		widget.ShowAll ();
	}
	
	bool SaveRow(Csv.writer writer, Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
		object [] content = new object[26];
		for (int i = 1; i < 27; i++) {
			content[i-1] = model.GetValue(iter, i);
		}
		writer.WriteFields(content);
		return false;
	}

	bool GetData(List<List<string>> list, Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
		List<string> row = new List<string>();
		for (int i = 1; i < 27; i++) {
			row.Add((string)model.GetValue(iter, i));
		}
		list.Add(row);
		return false;
	}

	public List<List<string>> GetData() {
		List<List<string>> list = new List<List<string>>();
		sheet.liststore.Foreach((model, path, iter) => GetData(list, model, path, iter));
		return list;
	}
	
	public override bool Save ()
	{
		Csv.writer writer = new Csv.writer(filename);
		sheet.liststore.Foreach((model, path, iter) => SaveRow(writer, model, path, iter));
		writer.Close();
		IsDirty = false;
		UpdateDocument();	
		return true;
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

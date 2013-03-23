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

public class Column : IEnumerable<object> {
	Gtk.ListStore liststore;
	string column = null;
	CalicoSpreadsheetDocument document;
	int x;

	public Column(CalicoSpreadsheetDocument doc, Gtk.ListStore liststore, string column) {
		this.document = doc;
		this.liststore = liststore;
		this.column = column;
	}
	public Column(CalicoSpreadsheetDocument doc, Gtk.ListStore liststore, int x) {
		this.document = doc;
		this.liststore = liststore;
		this.x = x;
	}
	public object this[int row_or_y] {
		get {
			Gtk.TreeIter iter;
			if (column == null) {
				liststore.GetIterFromString(out iter, row_or_y.ToString());
				return liststore.GetValue(iter, x + 1);		
			} else {
				char c;
				Char.TryParse(column.ToUpper(), out c);
				int offset = c - 'A' + 1;
				liststore.GetIterFromString(out iter, (row_or_y - 1).ToString());
				return liststore.GetValue(iter, offset);
			}
		}
		set {
			Gtk.TreeIter iter;
			if (column == null) {
				liststore.GetIterFromString(out iter, row_or_y.ToString());
				liststore.SetValue(iter, x + 1, value.ToString());		
			} else {
				char c;
				Char.TryParse(column.ToUpper(), out c);
				int offset = c - 'A' + 1;
				liststore.GetIterFromString(out iter, (row_or_y - 1).ToString());
				liststore.SetValue(iter, offset, value.ToString());
			}
			document.IsDirty = true;
			document.UpdateDocument();
		}
	}
	
	public IEnumerator<object> GetEnumerator ()
	{
	  Gtk.TreeIter iter;
	  if (column == null) {
	    for (int row_or_y = 0; row_or_y < document.Rows; row_or_y++) {
	      liststore.GetIterFromString(out iter, row_or_y.ToString());
	      yield return liststore.GetValue(iter, x + 1);		
	    }
	  } else {
	    char c;
	    Char.TryParse(column.ToUpper(), out c);
	    int offset = c - 'A' + 1;
	    for (int row_or_y = 0; row_or_y < document.Rows; row_or_y++) {
	      liststore.GetIterFromString(out iter, row_or_y.ToString());
	      yield return liststore.GetValue(iter, offset);
	    }
	  }
	}

	System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
	{
		return GetEnumerator();
	}
	
}

public class SpreadsheetWidget : Gtk.TreeView {
	
	public Gtk.ListStore liststore;
	CalicoSpreadsheetDocument document;

	public SpreadsheetWidget(CalicoSpreadsheetDocument doc) : base() {
		document = doc;
		EnableGridLines = Gtk.TreeViewGridLines.Both;
		EnableSearch = true;
		RulesHint = true;
		// Environment table:
		makeColumn();
		for(int index = 0; index < document.Cols; index++) {
			makeColumn(makeColumnName(index), index + 1);
		}
		
        // Create a ListStore as the Model
        liststore = new Gtk.ListStore(makeTypes()); 
        Model = liststore;
        ShowAll();
		
		//liststore.Append();
		for (int i = 0; i < document.Rows; i++) {
			liststore.AppendValues(makeRow());
		}
	}

	public string makeColumnName(int index) {
		string indexes = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		string retval = "";
		retval += indexes.Substring(index % document.Cols, 1);
		return retval;
	}
	
	Type [] makeTypes() {
		Type [] types = new Type[document.Cols + 1];
		types[0] = typeof(int);
		for (int i = 1; i < document.Cols + 1; i++) {
			types[i] = typeof(string);
		}
		return types;
	}
	
	object [] makeRow() {
		object [] row = new object[document.Cols + 1];
		row[0] = Model.IterNChildren() + 1;
		for (int i = 1; i < document.Cols + 1; i++) {
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

public class CalicoSpreadsheetDocument : Document, IEnumerable<object>
{
	public SpreadsheetWidget sheet;	
	public int maxrow;
	public CalicoSpreadsheetDocument(Calico.MainWindow calico, string filename) : 
	base(calico, filename, "spreadsheet")
	{
		DocumentType = "Sheet";
		Rows = 100;
		Cols = 26;
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
		focus_widget = sheet;
		widget.AddWithViewport (sheet);
		widget.ShowAll ();
	}
	
	public int Rows {
		get;
		set;
	}

	public int Cols {
		get;
		set;
	}

	public Column this[int x] {
		get {
			return new Column(this, sheet.liststore, x);
		}
	}
		
	public IEnumerator<object> GetEnumerator ()
	{
	  for (int x = 0; x < Cols; x++) {
	    yield return new Column(this, sheet.liststore, x);
	  }
	}

	System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
	{
		return GetEnumerator();
	}
	
	public Column this[string column] {
		get {
			return new Column(this, sheet.liststore, column);
		}
	}

	bool SaveRow(Csv.writer writer, Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
		int maxcol = 1;
		for (int i = 1; i < Cols + 1; i++) {
		    if (((string)model.GetValue(iter, i)) != "")
			maxcol = i;
		}
		object [] content = new object[maxcol];
		for (int i = 1; i <= maxcol; i++) {
			content[i-1] = model.GetValue(iter, i);
		}
		writer.WriteFields(content);
		return false;
	}

	bool GetData(List<List<string>> list, Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
		List<string> row = new List<string>();
		for (int i = 1; i < Cols + 1; i++) {
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
	
	public object GetData(string column, int row) {
		// column is "A", "B", row is 1 based
		Gtk.TreeIter iter;
		char c;
		Char.TryParse(column.ToUpper(), out c);
		int offset = c - 'A' + 1;
		sheet.liststore.GetIterFromString(out iter, (row - 1).ToString());
		return sheet.liststore.GetValue(iter, offset);
	}

	public object GetData(int x, int y) {
		// Matrix, 0-based
		Gtk.TreeIter iter;
		sheet.liststore.GetIterFromString(out iter, x.ToString());
		return sheet.liststore.GetValue(iter, y + 1);
	}

	public void FindMaxRow() {
	    maxrow = 0;
	    int currow = 0;
	    sheet.liststore.Foreach((model, path, iter) => 
		{
		    currow++;
		    if (ContainsData(model, path, iter)) {
			maxrow = currow;
		    }
		    return false;
		}
				    );
	}

	public bool ContainsData(Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
	    for (int i = 1; i < Cols + 1; i++) {
		if (((string)model.GetValue(iter, i)) != "") 
		    return true;
	    }
	    return false;
	}

	public override bool SaveDocument()
	{
		try {
			Csv.writer writer = new Csv.writer(filename);
			FindMaxRow();
			int currow = 0;
			sheet.liststore.Foreach((model, path, iter) => {
				currow++;
				if (currow <= maxrow)
				    SaveRow(writer, model, path, iter);
				return false;
			    }
			    );
			writer.Close();
			IsDirty = false;
			UpdateDocument();	
			return true;
		} catch {
			return false;
		}
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

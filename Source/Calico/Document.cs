//  
//  Document.cs
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

namespace Calico
{
	public class Document
	{
		public string language;
		public string filename;
		public Document document;
		public Gtk.ScrolledWindow widget;
		public Gtk.Widget tab_label; // label for notebook page
		public Gtk.Button close_button; // tab close button
		
		public Document (string filename, string language) : base() 
		{
			this.filename = filename;
			this.language = language;
			widget = new Gtk.ScrolledWindow();
			string name = System.IO.Path.GetFileName(filename);
			tab_label = new Gtk.HBox();			
			Gtk.Label myLabel = new Gtk.Label(name);
			((Gtk.HBox)tab_label).Add(myLabel);
			close_button = new Gtk.Button();
			Gtk.Image img = new Gtk.Image();
	        close_button.Relief = Gtk.ReliefStyle.None;
        	img = new Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu);
			close_button.Add(img);
			((Gtk.HBox)tab_label).Add(close_button);
			myLabel.TooltipText = filename;
			tab_label.ShowAll();
		}		
		public bool GotoLine(int lineno) {
			return true;
		}
		
		public bool IsDirty {
			get {return true;}
			set {}
		}
		
		public virtual void Configure () {
			// For setting defaults
		}
		
		public virtual bool Close() {
			// Close document, and return successful close status
			return true;
		}
	}
	
	public class TextDocument : Document {
		public Mono.TextEditor.TextEditor texteditor;
		
		public TextDocument(string filename, string language) : base(filename, language) 
		{

			//Mono.TextEditor.TextEditorOptions options = new Mono.TextEditor.TextEditorOptions();
			texteditor = new Mono.TextEditor.TextEditor();
			texteditor.Document.MimeType = String.Format("text/x-{0}", language);
			widget.Add(texteditor);
			widget.ShowAll();
		}
		
		public override void Configure() {
			// FIXME: take into account user's defaults
			texteditor.Options.ShowInvalidLines = false;
        	texteditor.Options.ShowLineNumberMargin = true;
        	texteditor.Options.TabsToSpaces = true;
        	texteditor.Options.HighlightCaretLine = true;
        	texteditor.Options.HighlightMatchingBracket = true;
        	texteditor.Options.OverrideDocumentEolMarker = true;
        	texteditor.Options.DefaultEolMarker = "\n";
		}
		
		public override bool Close() {
			return true;
		}	
	}
}


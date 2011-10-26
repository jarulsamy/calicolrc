using System;

namespace Calico
{
	public class Document
	{
		public Gtk.Widget widget;
		public Gtk.Widget tab; // label
		public string Language;
		public string Filename;
		
		public Document (string filename)
		{
			Filename = filename;
		}
		
		public bool GoToLine(int lineno) {
			return true;
		}
		
		public bool IsDirty {
			get {return true;}
			set {}
		}
		
	}
	
	public class TextDocument : Document {
		public Mono.TextEditor.TextEditor texteditor;
		
		public TextDocument(string filename) : base(filename) {
			//Mono.TextEditor.TextEditorOptions options = new Mono.TextEditor.TextEditorOptions();
			texteditor = new Mono.TextEditor.TextEditor();
			texteditor.ShowAll();
			widget = texteditor;
			//texteditor.Document.MimeType = "text/x-sql";
		}
	}
	
	
}


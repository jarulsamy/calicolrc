using System;

namespace Calico
{
	public class Document
	{
		public Gtk.Widget Widget; // widget to add to notebook
		public Gtk.Widget Label; // label for notebook page
		public string Language;
		public string Filename;
		
		public Document (string filename)
		{
			Filename = filename;
		}
		
		public bool GotoLine(int lineno) {
			return true;
		}
		
		public bool IsDirty {
			get {return true;}
			set {}
		}
		
		public virtual void Configure () {
		}
		
	}
	
	public class TextDocument : Document {
		public Mono.TextEditor.TextEditor texteditor;
		
		public TextDocument(string filename) : base(filename) {
			//Mono.TextEditor.TextEditorOptions options = new Mono.TextEditor.TextEditorOptions();
			texteditor = new Mono.TextEditor.TextEditor();
			Widget = new Gtk.ScrolledWindow();
			((Gtk.ScrolledWindow)Widget).Add(texteditor);
			string name = System.IO.Path.GetFileName(filename);
			Widget.ShowAll();
			Label = new Gtk.Label(name);
			Label.TooltipText = filename;
			texteditor.Document.MimeType = "text/x-python";
		}
		
		public override void Configure() {
			texteditor.Options.ShowInvalidLines = false;
        	texteditor.Options.ShowLineNumberMargin = true;
        	texteditor.Options.TabsToSpaces = true;
        	texteditor.Options.HighlightCaretLine = true;
        	texteditor.Options.HighlightMatchingBracket = true;
        	texteditor.Options.OverrideDocumentEolMarker = true;
        	texteditor.Options.DefaultEolMarker = "\n";
		}
		
	}
	
	
}


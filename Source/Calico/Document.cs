using System;

namespace Calico
{
	public class Document
	{
		public string Language;
		public string Filename;
		public Document document;		
		public Gtk.ScrolledWindow Widget;
		public Gtk.Widget Tab; // label for notebook page
		public Gtk.Button CloseButton; // tab close button
		
		public Document (string filename, string language) : base() 
		{
			Filename = filename;
			Language = language;
			Widget = new Gtk.ScrolledWindow();
			string name = System.IO.Path.GetFileName(Filename);
			Tab = new Gtk.HBox();			
			Gtk.Label myLabel = new Gtk.Label(name);
			((Gtk.HBox)Tab).Add(myLabel);
			CloseButton = new Gtk.Button();
			Gtk.Image img = new Gtk.Image();
	        CloseButton.Relief = Gtk.ReliefStyle.None;
        	img = new Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu);
			CloseButton.Add(img);
			((Gtk.HBox)Tab).Add(CloseButton);
			myLabel.TooltipText = Filename;
			Tab.ShowAll();
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
			texteditor.Document.MimeType = String.Format("text/x-{0}", Language);
			Widget.Add(texteditor);
			Widget.ShowAll();
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


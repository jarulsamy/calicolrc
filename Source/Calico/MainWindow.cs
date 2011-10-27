using System;
using System.IO; // Path
using Gtk;
using System.Collections.Generic;
using Calico;

public static class Extensions 
{
	public static string ToTitleCase(string aString) 
	{ 
		try { 
			return System.Threading.Thread.CurrentThread.CurrentCulture.TextInfo.ToTitleCase(aString); 
		} catch { 
			return aString; 
		} 
	}
}


public partial class MainWindow: Gtk.Window
{	
	public Clipboard clipboard;
	private Mono.TextEditor.TextEditor _shell;
	public Dictionary<Gtk.Widget,Document> DocumentMap = new Dictionary<Gtk.Widget,Document>();
	public Document CurrentDocument {
		get {
			int page_num = DocumentNotebook.Page;
			if (page_num > 1) {
				Gtk.Widget widget = DocumentNotebook.GetNthPage(page_num);
				if (DocumentMap.ContainsKey(widget))
					return DocumentMap[widget];
				else
					return null;
			} else
				return null;
		}
	}
	public Gtk.ScrolledWindow ScrolledWindow {
		get {return scrolledwindow1;}
		set {scrolledwindow1 = value;}
	}
	public Mono.TextEditor.TextEditor Shell {
		get {return _shell;}
	}
	
	public MainWindow (string [] args): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		// Had to add this here, as Setic didn't like it
		_shell = new Mono.TextEditor.TextEditor();
		ScrolledWindow.Add(_shell);
		ScrolledWindow.ShowAll();
		// Setup clipboard, and Gui:
		clipboard = Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", false));
		DocumentNotebook.CurrentPage = 0;
		// Set optional items of TextArea
        Shell.Options.ShowFoldMargin = false;
        Shell.Options.ShowIconMargin = false;
        Shell.Options.ShowInvalidLines = false;
        Shell.Options.ShowLineNumberMargin = false; // option
        Shell.Options.TabsToSpaces = true;
        Shell.Options.HighlightMatchingBracket = true;
        
        try {
            Shell.Document.MimeType = String.Format("text/x-{0}", "python");
		} catch {
            // pass
		}
		
		List<string> files = new List<string>();
		foreach (string arg in args) {
			if (arg.StartsWith("--")) {
			} else {
				files.Add(System.IO.Path.GetFullPath(arg));
			}
		}
        foreach (string filename in files) {
            SelectOrOpen(filename);
		}
	}

	public bool SelectOrOpen() {
		return SelectOrOpen(null, null);
	}

	public bool SelectOrOpen(string filename) {
		return SelectOrOpen(filename, null);
	}

	public static string _(string message) {
		return global::Mono.Unix.Catalog.GetString(message);
	}
	
	public bool SelectOrOpen(string filename, string language) {
        // First, check for filename:N format:
		int lineno = 0;
        if (filename != null) {
			 System.Text.RegularExpressions.Match match = System.Text.RegularExpressions.Regex.Match(filename, "(.*)\\:(\\d+)$");
            if (match.Success) {
				filename = (string)match.Groups[0].Captures[0].Value;
				lineno = Convert.ToInt32(match.Groups[0].Captures[1].Value);
			}
		}
        // FIXME: can attempt to open bogus path/filename
        // but this is useful for file creation
        Document page = null;
        // if already open, select it
        bool add_it = true;
        if (filename != null) {
			// Page 0 is the Help page; Page 1 is Shell
            for (int page_num = 2; page_num < DocumentNotebook.NPages; page_num++) {
                Gtk.Widget npage = DocumentNotebook.GetNthPage(page_num);
				if (DocumentMap.ContainsKey(npage)) {
					Document npage_document = DocumentMap[npage];
	                if (npage_document.Filename == filename) {
	                    DocumentNotebook.CurrentPage = page_num;
	                    add_it = false;
	                    break;
					}
				}
			}
            if (page == null) {
                page = MakeDocument(filename);  // make a new document with filename
			}
		} else { // make a no-named document of type language
            if (language == null) {
                // FIXME: issue with getting document, before it is ready
                language = "python";
                if (CurrentDocument != null) {
                    if (CurrentDocument.Language != null) {
                        language = CurrentDocument.Language;
					}
				}
			}
            page = MakeDocument(null, language);
		}
        if (add_it) {
            //self.calico.on_action("opened-document", filename=filename);
            int page_num = DocumentNotebook.AppendPage(page.Widget, page.Tab);
            DocumentNotebook.SetTabReorderable(page.Widget, true);
            DocumentNotebook.CurrentPage = page_num;
			page.CloseButton.Clicked += delegate {TryToClose(page);};
            if (filename != null) {
                UpdateRecentFiles(filename);
			}
		}
        if (page != null && lineno != 0) {
            return page.GotoLine(lineno);
		}
		return false;
	}
	
	public void TryToClose(Document document) {
		if (document.Close()) {
			int page_num = DocumentNotebook.PageNum(document.Widget);
			DocumentNotebook.RemovePage(page_num);
		}
	}
	
	public void UpdateRecentFiles(string filename) {
	}
		
	public Document MakeDocument(string filename) {
		// FIXME: get language from defaults
		string language = "python";
		if (filename == null) {
			filename = String.Format("New {0} Script", Extensions.ToTitleCase(language));
		}
		return new TextDocument(filename, language);
	}

	public Document MakeDocument(string filename, string language) {
		if (language == null) {
			language = "python";
		}
		if (filename == null) {
			filename = String.Format("New {0} Script", Extensions.ToTitleCase(language));
		}
		return new TextDocument(filename, language);
	}

	public bool Close() {
		// Delete Window
		// FIXME: ask to save files, or cancel
		return true;
	}
	
	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
		if (Close()) {
			Application.Quit ();
			a.RetVal = true;
		}
	}
	
	protected virtual void OnNewAction1Activated (object sender, System.EventArgs e)
	{
		SelectOrOpen();
	}
	
	protected virtual void OnOpenAction1Activated (object sender, System.EventArgs e)
	{
        Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog(_("Select the file to open"),
                                   							this,
						                                   	Gtk.FileChooserAction.Open,
						                                   _("Cancel"), Gtk.ResponseType.Cancel,
						                                   _("Open"), Gtk.ResponseType.Accept);
	    fc.KeepAbove = true;
        if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
            SelectOrOpen(fc.Filename);
            //path, base = os.path.split(fc.Filename)
            //os.chdir(path)
		}
        fc.Destroy();
	}
	
	protected virtual void OnNewActionActivated (object sender, System.EventArgs e)
	{
		SelectOrOpen();
	}
	
	protected virtual void OnOpenActionActivated (object sender, System.EventArgs e)
	{
		OnOpenAction1Activated(sender, e);
	}
	
	protected virtual void OnSaveActionActivated (object sender, System.EventArgs e)
	{
	}
	
	protected virtual void OnSaveAsActionActivated (object sender, System.EventArgs e)
	{
	}
	
	protected virtual void OnQuitActionActivated (object sender, System.EventArgs e)
	{
		if (Close()) {
			Application.Quit ();
		}
	}
	
	protected virtual void OnNotebookDocsSwitchPage (object o, Gtk.SwitchPageArgs args)
	{
		if (CurrentDocument is Document) {
			prompt.Text = CurrentDocument.Language;
			status_langauge.Text = Extensions.ToTitleCase(prompt.Text);
		} else { // Home, and Shell
			Console.WriteLine(((Gtk.Notebook)o).Page);
		}
	}
	
	protected virtual void OnButton2Clicked (object sender, System.EventArgs e)
	{
		OnOpenAction1Activated(sender, e);
	}
	
	protected virtual void OnButton3Clicked (object sender, System.EventArgs e)
	{
		SelectOrOpen();
	}
	
	protected virtual void OnCopyActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
			if (text != null)
				clipboard.Text = text;
		} else if (Focus is Gtk.TextView) {
			((Gtk.TextView)Focus).Buffer.CopyClipboard(clipboard);
		}
	}
	
	protected virtual void OnPasteActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
			if (text != null)
				((Mono.TextEditor.TextEditor)Focus).DeleteSelectedText();
			((Mono.TextEditor.TextEditor)Focus).InsertAtCaret(clipboard.WaitForText());
		} else if (Focus is Gtk.TextView) {
			((Gtk.TextView)Focus).Buffer.PasteClipboard(clipboard);
		}
	}
	
	protected virtual void OnCutActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
			if (text != null)
				clipboard.Text = text;
			((Mono.TextEditor.TextEditor)Focus).DeleteSelectedText();
		} else if (Focus is Gtk.TextView) {
			((Gtk.TextView)Focus).Buffer.CutClipboard(clipboard, true);
		}
	}
	
	protected virtual void OnUndoActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			Mono.TextEditor.MiscActions.Undo(Shell.GetTextEditorData());
		}
	}
	
	protected virtual void OnRedoActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			Mono.TextEditor.MiscActions.Redo(Shell.GetTextEditorData());
		}
	}
	
	protected virtual void OnSelectAllActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
		} else if (Focus is Gtk.TextView) {
			Gtk.TextView textview = (Gtk.TextView)Focus;
			textview.Buffer.SelectRange(textview.Buffer.StartIter, textview.Buffer.EndIter);
		}
	}
	
	protected virtual void OnIndentActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			Mono.TextEditor.MiscActions.IndentSelection(Shell.GetTextEditorData());
		} 
	}
	
	protected virtual void OnUnindentActionActivated (object sender, System.EventArgs e)
	{
		if (Focus is Mono.TextEditor.TextEditor) {
			Mono.TextEditor.MiscActions.RemoveIndentSelection(Shell.GetTextEditorData());
		} 
	}
	
	protected virtual void OnCommentRegionActionActivated (object sender, System.EventArgs e)
	{
	}
	
	protected virtual void OnUncommentRegionActionActivated (object sender, System.EventArgs e)
	{
	}
	
	protected virtual void OnButton4Clicked (object sender, System.EventArgs e)
	{
		System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Getting_Started");
	}
	
	protected virtual void OnKeyPressEvent (object o, Gtk.KeyPressEventArgs args)
	{
		// FIXME: on escape, if running stop; clear command entry if focus
		// FIXME: if in command entry shell
	}
	
	public Gtk.Notebook DocumentNotebook {
		get {return notebook_docs; }
	}
	
}

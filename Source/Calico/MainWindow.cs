using System;
using System.IO; // Path
using Gtk;
using System.Collections.Generic;


public partial class MainWindow: Gtk.Window
{	
	public Clipboard clipboard; 
	public Dictionary<Gtk.Widget,Calico.Document> DocumentMap = new Dictionary<Gtk.Widget,Calico.Document>();
	private Calico.Document current_document = null;
	public Calico.Document CurrentDocument {
		get {return current_document;}
		set {current_document = value;}
	}
	public Mono.TextEditor.TextEditor CommandTextArea {
		get {return texteditor2;}
	}
	
	public MainWindow (string [] args): base (Gtk.WindowType.Toplevel)
	{
		Build ();
		clipboard = Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", false));
		DocumentNotebook.CurrentPage = 0;
        //options = TextEditorOptions()
        //self.textview = TextEditor(Options=options)
        CommandTextArea.Options.ShowFoldMargin = false;
        CommandTextArea.Options.ShowIconMargin = false;
        CommandTextArea.Options.ShowInvalidLines = false;
        CommandTextArea.Options.ShowLineNumberMargin = false; // option
        CommandTextArea.Options.TabsToSpaces = true;
        CommandTextArea.Options.HighlightMatchingBracket = true;
        try {
            CommandTextArea.Document.MimeType = String.Format("text/x-{0}", "python");
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

	public bool SelectOrOpen(string filename) {
		return SelectOrOpen(filename, "");
	}

	public static string _(string message) {
		return global::Mono.Unix.Catalog.GetString(message);
	}
	
	public bool SelectOrOpen(string filename, string language) {
        // First, check for filename:N format:
		int lineno = 0;
        if (filename != "") {
			 System.Text.RegularExpressions.Match match = System.Text.RegularExpressions.Regex.Match(filename, "(.*)\\:(\\d+)$");
            if (match.Success) {
				filename = (string)match.Groups[0].Captures[0].Value;
				lineno = Convert.ToInt32(match.Groups[0].Captures[1].Value);
			}
		}
        // FIXME: can attempt to open bogus path/filename
        // but this is useful for file creation
        Calico.Document page = null;
        // if already open, select it
        bool add_it = true;
        if (filename != "") {
			// Page 0 is the Help page; Page 1 is Shell
            for (int page_num = 2; page_num < DocumentNotebook.NPages; page_num++) {
                Gtk.Widget npage = DocumentNotebook.GetNthPage(page_num);
				Calico.Document npage_document = DocumentMap[npage];
                if (npage_document.Filename == filename) {
                    DocumentNotebook.CurrentPage = page_num;
                    page = npage_document; // reselect opened filename
                    add_it = false;
                    break;
				}
			}
            if (page == null) {
                page = MakeDocument(filename);  // make a new document with filename
			}
		} else { // make a no-named document of type language
            if (language == "") {
                // FIXME: issue with getting document, before it is ready
                language = "python";
                if (CurrentDocument != null) {
                    if (CurrentDocument.Language != "") {
                        language = CurrentDocument.Language;
					}
				}
			}
            page = MakeDocument("", language);
		}
        if (add_it) {
            //self.calico.on_action("opened-document", filename=filename);
			DocumentMap[page.Widget] = page;
            int page_num = DocumentNotebook.AppendPage(page.Widget, page.Label);
            DocumentNotebook.SetTabReorderable(page.Widget, true);
            DocumentNotebook.CurrentPage = page_num;
            if (filename != "") {
                UpdateRecentFiles(filename);
			}
		}
        //###########################################################
        // Remove temp page, if one, and not same kind as one added:
        if (DocumentNotebook.NPages == 4) {
			if (DocumentMap.ContainsKey(DocumentNotebook.GetNthPage(2))) {
	            Calico.Document doc0 = DocumentMap[DocumentNotebook.GetNthPage(2)];
	            if (doc0.Filename == "" && ! doc0.IsDirty) {
	                DocumentNotebook.RemovePage(1);
				}
			}
		}
        if (page != null && lineno != 0) {
            return page.GotoLine(lineno);
		}
		return false;
	}
	
	public void UpdateRecentFiles(string filename) {
	}
	
	public Calico.Document MakeDocument(string filename) {
		return new Calico.TextDocument(filename);
	}

	public Calico.Document MakeDocument(string filename, string language) {
		return new Calico.TextDocument(filename);
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
		SelectOrOpen("");
	}
	
	protected virtual void OnOpenAction1Activated (object sender, System.EventArgs e)
	{
		bool retval = false;
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
            retval = true;
		}
        fc.Destroy();
        //e.retval;
	}
	
	protected virtual void OnNewActionActivated (object sender, System.EventArgs e)
	{
		SelectOrOpen("");
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
		Console.WriteLine("SwitchPage fired!");
	}
	
	protected virtual void OnButton2Clicked (object sender, System.EventArgs e)
	{
		OnOpenAction1Activated(sender, e);
	}
	
	protected virtual void OnButton3Clicked (object sender, System.EventArgs e)
	{
		SelectOrOpen("");
	}
	
	protected virtual void OnCopyActionActivated (object sender, System.EventArgs e)
	{
		Focus.GetClipboard(Gdk.Atom.Intern("CLIPBOARD", false)).WaitForText();
	}
	
	protected virtual void OnPasteActionActivated (object sender, System.EventArgs e)
	{
		//if (Focus is Gtk.TextView) 
			//((Gtk.TextView)Focus) = clipboard.WaitForText();
	}
	
	protected virtual void OnSelectAllActionActivated (object sender, System.EventArgs e)
	{
		//
	}
	
	
	
	
	public Gtk.Notebook DocumentNotebook {
		get {return notebook_docs; }
	}
	
}

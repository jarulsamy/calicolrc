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
	public Dictionary<string,Language> LanguageMap;
	public EngineManager manager;
	public string CurrentLanguage = "python";
	public bool Debug = false;
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
	public Gtk.TextView Output {
		get {return textview1;}
	}
	
	public MainWindow (string [] args, Dictionary<string,Language> LanguageMap, bool Debug): base (Gtk.WindowType.Toplevel)
	{
		this.Debug = Debug;
		this.LanguageMap = LanguageMap;
		Build();
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
            Shell.Document.MimeType = String.Format("text/x-{0}", CurrentLanguage);
		} catch {
            // pass
		}
		
		// Now, let's load engines
		manager = new EngineManager(new Project());
		foreach (string name in LanguageMap.Keys) {
			manager.register(LanguageMap[name]);
			manager.engines[name].set_manager(manager);
		}
		manager.setup();
		manager.start();
		
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
	                if (npage_document.filename == filename) {
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
                language = CurrentLanguage;
                if (CurrentDocument != null) {
                    if (CurrentDocument.language != null) {
                        language = CurrentDocument.language;
					}
				}
			}
            page = MakeDocument(null, language);
		}
        if (add_it) {
            //self.calico.on_action("opened-document", filename=filename);
            int page_num = DocumentNotebook.AppendPage(page.widget, page.tab_label);
			DocumentMap[page.widget] = page;
            DocumentNotebook.SetTabReorderable(page.widget, true);
            DocumentNotebook.CurrentPage = page_num;
			page.close_button.Clicked += delegate {TryToClose(page);};
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
			int page_num = DocumentNotebook.PageNum(document.widget);
			DocumentNotebook.RemovePage(page_num);
		}
	}
	
	public void UpdateRecentFiles(string filename) {
	}
		
	public Document MakeDocument(string filename) {
		// FIXME: get language from defaults
		string language = CurrentLanguage;
		if (filename == null) {
			filename = String.Format("New {0} Script", Extensions.ToTitleCase(language));
		}
		return new TextDocument(filename, language);
	}

	public Document MakeDocument(string filename, string language) {
		if (language == null) {
			language = CurrentLanguage;
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
	
	[GLib.ConnectBeforeAttribute]
	protected virtual void OnKeyPressEvent (object o, Gtk.KeyPressEventArgs args)
	{
		if (Focus == Shell) { // Shell handler
			// control+c, if nothing is selected, else it is a copy
			if (args.Event.Key == Gdk.Key.c && (args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
				string text = Shell.SelectedText;
				if (text == null) {
					Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
					Shell.DeleteSelectedText();
					args.RetVal = true;
				}
			} else if (args.Event.Key == Gdk.Key.Escape) { // escape with selected, delete; else delete all
				string text = Shell.SelectedText;
				if (text == null) {
					Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
				}
				Shell.DeleteSelectedText();
				args.RetVal = true;
			} else if (args.Event.Key == Gdk.Key.Return) {
                // if control key is down, too, just insert a return
				bool force = false;
				string completion = "";
                if ((args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
                    Shell.InsertAtCaret("\n");
                    args.RetVal = true;
					return;
				}
                // if cursor in middle, insert a Return
                Mono.TextEditor.Caret caret = Shell.Caret;
                int line = caret.Line;
                int line_count = Shell.Document.LineCount;
                if (line != line_count - 1 && ! force) {
                    completion = null;
                    args.RetVal = false;
				}
                // else, execute text
                // extra line at end signals ready_to_execute:
                string text = Shell.Document.Text;
                if (text == "") {
                    completion = null;
                    args.RetVal = true; // nothing to do, but handled
				} else if (manager.engines[CurrentLanguage].ReadyToExecute(text) || force) {
                    //history.last(text.rstrip());
                    //history.add("");
                    bool results = Execute(text.TrimEnd(), CurrentLanguage);
                    if (results) {
						Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
                        Mono.TextEditor.DeleteActions.DeleteSelection(Shell.GetTextEditorData());
                        Shell.GrabFocus();
                        Shell.Caret.Line = 0;
                        Shell.Caret.Column = 0;
					}
                    completion = null;
                    args.RetVal = true;
				}
			}
		} else if (Focus is Mono.TextEditor.TextEditor) { // Editor, handle : on end of line
		}
	}
	
	public bool Execute(string text, string language) {
        //if (executeThread) {
        //    return false;
		//}
        string prompt = String.Format("{0}> ", CurrentLanguage);
        int count = 2;
        foreach (string line in text.Split('\n')) {
            Write("{0}", prompt); // black
            WriteLine("{0}", line); // blue
            prompt = String.Format(".....{0}>", count);
            count += 1;
		}
        // pragma/meta commands, start with #;
        if (text == "") {
            return false;
		} 
        this.CurrentLanguage = language;
        //self.execute_in_background(text);
        return true;
	}
	
	public void Write(string format, params object [] args) {
		if (Debug) {
			Console.Write(format, args);
		} else {
			Output.Buffer.InsertAtCursor(String.Format(format, args));
			ScrollToEnd();
		}
	}
	
	public void WriteLine(string format, params object [] args) {
		if (Debug) {
			Console.WriteLine(format, args);
		} else {
			Output.Buffer.InsertAtCursor(String.Format(format, args) + "\n");
			ScrollToEnd();
		}
	}
	
	public void ScrollToEnd() {
		Gtk.TextIter end = Output.Buffer.EndIter;
		Gtk.TextMark mark = Output.Buffer.GetMark("insert");
		Output.Buffer.MoveMark(mark, end);
		Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
	}
	
	protected virtual void OnNotebookDocsSwitchPage (object o, Gtk.SwitchPageArgs args)
	{
		if (CurrentDocument != null) {
			// FIXME: Turn some things on
			prompt.Text = CurrentDocument.language;
			status_langauge.Text = Extensions.ToTitleCase(prompt.Text);
			CurrentDocument.widget.Child.GrabFocus();
		} else if (DocumentNotebook.Page == 0) { // Home
			// FIXME: Turn some things off
		} else if (DocumentNotebook.Page == 1) { // Shell
			// FIXME: Turn some things off
			Shell.GrabFocus();
		} else { // Some other page
			// FIXME: Turn some things off
		}
	}
	
	public Gtk.Notebook DocumentNotebook {
		get {return notebook_docs; }
	}
	
}

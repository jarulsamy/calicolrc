using System;
using System.IO; // Path
using Gtk;
using System.Collections.Generic;

public partial class MainWindow: Gtk.Window
{	
	
	public Dictionary<Gtk.Widget,Calico.Document> DocumentMap= new Dictionary<Gtk.Widget,Calico.Document>();

	
	public MainWindow (string [] args): base (Gtk.WindowType.Toplevel)
	{
		Build ();
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
            for (int page_num = 0; page_num < DocumentNotebook.NPages; page_num++) {
                Gtk.Widget npage = DocumentNotebook.GetNthPage(page_num);
                if (npage.Document.Filename == filename) {
                    DocumentNotebook.CurrentPage = page_num;
                    page = npage; // reselect opened filename
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
			DocumentMap[page.widget] = page;
            page_num = DocumentNotebook.AppendPage(page.widget, page.tab);
            DocumentNotebook.SetTabReorderable(page.widget, true);
            DocumentNotebook.CurrentPage = page_num;
            if (filename != "") {
                UpdateRecentFiles(filename);
			}
		}
        //###########################################################
        // Remove temp page, if one, and not same kind as one added:
        if (DocuementNotebook.NPages == 2) {
            Docuemnt doc0 = DocumentNotebook.GetNthPage(0).document;
            if (doc0.Filename == "" && ! doc0.IsDirty) {
                DocumentNotebook.RemovePage(0);
			}
		}
        if (page != null && lineno != 0) {
            return Document.GotoLine(lineno);
		}
		return false;
	}
	
	public void UpdateRecentFiles(string filename) {
	}
	
	public Calico.Document MakeDocument(string filename) {
		return new TextDocument(filename);
	}
	
	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
		Application.Quit ();
		a.RetVal = true;
	}
	
	public Gtk.Notebook DocumentNotebook {
		get {return notebook_docs; }
	}
	
}

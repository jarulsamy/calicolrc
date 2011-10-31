//
//  MainWindow.cs
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
using System.IO; // Path
using System.Threading;
using System.Collections.Generic;
using Calico;

namespace Calico {

public partial class MainWindow : Gtk.Window {
    public Gtk.Clipboard clipboard;
    private Mono.TextEditor.TextEditor _shell;
    public Dictionary<Gtk.Widget, Document> documents = new Dictionary<Gtk.Widget, Document>();
    public Dictionary<int, Language> languages_by_count = new Dictionary<int, Language>();
    public Dictionary<string, Language> languages;
    public EngineManager manager;
    public string CurrentLanguage = "python"; // FIXME: get from defaults
    public string ShellLanguage = "python";
    public bool Debug = false;
    public static int gui_thread_id = -1;
    public static int SHELL = 1;
    public static int HOME = 0;
    public Document CurrentDocument {
        get {
            int page_num = DocumentNotebook.Page;
            if (page_num > 1) {
                Gtk.Widget widget = DocumentNotebook.GetNthPage(page_num);
                if (documents.ContainsKey(widget))
                    return documents[widget];
                else
                    return null;
            } else
                return null;
        }
    }
    public Gtk.ScrolledWindow ScrolledWindow {
        get { return scrolledwindow1; }
        set { scrolledwindow1 = value; }
    }
    public Mono.TextEditor.TextEditor Shell {
        get { return _shell; }
    }
    public Gtk.TextView Output {
        get { return textview1; }
    }
    public string CurrentProperLanguage {
        get {
            if (CurrentLanguage != null && languages.ContainsKey(CurrentLanguage))
                return languages[CurrentLanguage].proper_name;
            return null;
        }
    }

    public MainWindow(string[] args, Dictionary<string, Language> LanguageMap, bool Debug) : base(Gtk.WindowType.Toplevel) {
        this.Debug = Debug;
        this.languages = LanguageMap;
        Build();
        Gtk.Application.Invoke(delegate {
                gui_thread_id = Thread.CurrentThread.ManagedThreadId;
        });
        // Had to add this here, as Stetic didn't like it
        _shell = new Mono.TextEditor.TextEditor();
        ScrolledWindow.Add(_shell);
        ScrolledWindow.ShowAll();
        // Setup clipboard, and Gui:
        clipboard = Gtk.Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", false));
        DocumentNotebook.CurrentPage = 0;
        Title = String.Format("Calico - {0}", System.Environment.UserName);
	    // New file menu:
        Gtk.MenuItem file_menu = (Gtk.MenuItem) UIManager.GetWidget("/menubar2/FileAction/NewAction");
        file_menu.Submenu = new Gtk.Menu();
        foreach (KeyValuePair<string,Language> pair in LanguageMap) {
            Language language = pair.Value;
            Gtk.MenuItem menu = new Gtk.MenuItem(language.proper_name);
            ((Gtk.Menu)file_menu.Submenu).Add(menu);
            menu.Activated += delegate { SelectOrOpen(null, language.name); };
        }
        file_menu.Submenu.ShowAll();

        // Languages to menu items:
        Gtk.MenuItem switch_menu = (Gtk.MenuItem) UIManager.GetWidget("/menubar2/ScriptAction/LanguageAction");
        switch_menu.Submenu = new Gtk.Menu();
    	int count = 0;
        Gtk.RadioMenuItem group = null;
        Gtk.RadioMenuItem radioitem;
        foreach (KeyValuePair<string,Language> pair in LanguageMap) {
            Language language = pair.Value;
	        // FIXME: select default language initially
    	    // unique name, label, mnemonic, accel, tooltip, user data
	        string name = String.Format("Switch to {0}", language.proper_name);
            if (count == 0) {
    	        radioitem = new Gtk.RadioMenuItem(name);
                group = radioitem;
            } else {
                radioitem = new Gtk.RadioMenuItem(group, name);
            }
            ((Gtk.Menu)switch_menu.Submenu).Add(radioitem);
            uint key;
            Gdk.ModifierType mod;
            Gtk.Accelerator.Parse(String.Format("<control>{0}", count + 1), out key, out mod);
            radioitem.AddAccelerator("activate",
                                     UIManager.AccelGroup,
                                     new Gtk.AccelKey((Gdk.Key)key, mod, Gtk.AccelFlags.Visible));
            languages_by_count[count + 1] = language;
            radioitem.ButtonReleaseEvent += OnSwitchLanguage;
            radioitem.Data["id"] = count + 1;
	        count++;
    	}
        switch_menu.Submenu.ShowAll();

        // Set optional items of TextArea
        Shell.Options.ShowFoldMargin = false;
        Shell.Options.ShowIconMargin = false;
        Shell.Options.ShowInvalidLines = false;
        Shell.Options.ShowLineNumberMargin = false;
        // option
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
        }
        manager.setup();
        manager.start();
        manager.set_redirects(new CustomStream(this, "black"),
                              new CustomStream(this, "red"));
        SetLanguage(CurrentLanguage);
        
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

    public static bool needInvoke() {
        //Console.WriteLine("gui_thread_id: {0}", Myro.gui_thread_id);
        if (MainWindow.gui_thread_id == -1) {
          return false; // in another thread
        } else if (MainWindow.gui_thread_id == Thread.CurrentThread.ManagedThreadId) {
          return false; // you are already in the GUI thread
        } else {
          return true; // need to invoke!
        }
    }

    public delegate void InvokeDelegate();
    public static void Invoke(InvokeDelegate invoke) {
        if (needInvoke())
          Gtk.Application.Invoke(delegate {invoke();});
        else
          invoke();
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

    public void makeNewFile(string lang_name) {
        Console.WriteLine("makeNewFile: {0}", lang_name);
    }

    public void OnSwitchLanguage(object obj, Gtk.ButtonReleaseEventArgs args) {
        Gtk.RadioMenuItem radioitem = (Gtk.RadioMenuItem)obj;
        OnSwitchLanguage((int)radioitem.Data["id"]);
    }

    public void SetLanguage(string language) {
        CurrentLanguage = language;
        Invoke(delegate {
            prompt.Text = String.Format("{0}>", CurrentLanguage);
            status_langauge.Text = String.Format("<i>{0}</i> ", CurrentProperLanguage);
            status_langauge.UseMarkup = true;
        });
    }

    public void OnSwitchLanguage(int lang_count) {
    if (languages_by_count.ContainsKey(lang_count)) {
        Language language = languages_by_count[lang_count];
        SetLanguage(language.name);
        ShellLanguage = language.name;
        DocumentNotebook.Page = SHELL;
        }
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
                if (documents.ContainsKey(npage)) {
                    Document npage_document = documents[npage];
                    if (npage_document.filename == filename) {
                        DocumentNotebook.CurrentPage = page_num;
                        add_it = false;
                        break;
                    }
                }
            }
            if (page == null) {
                page = MakeDocument(filename);
                // make a new document with filename
            }
        } else {
            // make a no-named document of type language
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
            documents[page.widget] = page;
            DocumentNotebook.SetTabReorderable(page.widget, true);
            DocumentNotebook.CurrentPage = page_num;
            page.close_button.Clicked += delegate { TryToClose(page); };
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
        string language = CurrentLanguage;
        return MakeDocument(filename, language);
    }

    public Document MakeDocument(string filename, string language) {
        if (language == null) {
            language = CurrentLanguage;
        }
	// FIXME: get proper name from language:
        if (filename == null) {
            filename = String.Format("New {0} Script", languages[language].proper_name);
        }
	// FIXME: get document from language
        return new TextDocument(filename, language);
    }

    public bool Close() {
        // Delete Window
        // FIXME: ask to save files, or cancel
        return true;
    }

    protected void OnDeleteEvent(object sender, Gtk.DeleteEventArgs a) {
        if (Close()) {
            Gtk.Application.Quit();
            a.RetVal = true;
        }
    }

    protected virtual void OnNewAction1Activated(object sender, System.EventArgs e) {
        SelectOrOpen();
    }

    protected virtual void OnOpenAction1Activated(object sender, System.EventArgs e) {
        Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog(_("Select the file to open"), this, Gtk.FileChooserAction.Open, _("Cancel"), Gtk.ResponseType.Cancel, _("Open"), Gtk.ResponseType.Accept);
        fc.KeepAbove = true;
        if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
            SelectOrOpen(fc.Filename);
            //path, base = os.path.split(fc.Filename)
            //os.chdir(path)
        }
        fc.Destroy();
    }

    protected virtual void OnNewActionActivated(object sender, System.EventArgs e) {
        SelectOrOpen();
    }

    protected virtual void OnOpenActionActivated(object sender, System.EventArgs e) {
        OnOpenAction1Activated(sender, e);
    }

    protected virtual void OnSaveActionActivated(object sender, System.EventArgs e) {
    }

    protected virtual void OnSaveAsActionActivated(object sender, System.EventArgs e) {
    }

    protected virtual void OnQuitActionActivated(object sender, System.EventArgs e) {
        if (Close()) {
            Gtk.Application.Quit();
        }
    }

    protected virtual void OnButton2Clicked(object sender, System.EventArgs e) {
        OnOpenAction1Activated(sender, e);
    }

    protected virtual void OnButton3Clicked(object sender, System.EventArgs e) {
        SelectOrOpen();
    }

    protected virtual void OnCopyActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            string text = editor.SelectedText;
            if (text != null)
                clipboard.Text = text;
        } else if (Focus is Gtk.TextView) {
            ((Gtk.TextView)Focus).Buffer.CopyClipboard(clipboard);
        }
    }

    protected virtual void OnPasteActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
            if (text != null)
                ((Mono.TextEditor.TextEditor)Focus).DeleteSelectedText();
            ((Mono.TextEditor.TextEditor)Focus).InsertAtCaret(clipboard.WaitForText());
        } else if (Focus is Gtk.TextView) {
            ((Gtk.TextView)Focus).Buffer.PasteClipboard(clipboard);
        }
    }

    protected virtual void OnCutActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
            if (text != null)
                clipboard.Text = text;
            ((Mono.TextEditor.TextEditor)Focus).DeleteSelectedText();
        } else if (Focus is Gtk.TextView) {
            ((Gtk.TextView)Focus).Buffer.CutClipboard(clipboard, true);
        }
    }

    protected virtual void OnUndoActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            Mono.TextEditor.MiscActions.Undo(editor.GetTextEditorData());
        }
    }

    protected virtual void OnRedoActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            Mono.TextEditor.MiscActions.Redo(editor.GetTextEditorData());
        }
    }

    protected virtual void OnSelectAllActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            
            Mono.TextEditor.SelectionActions.SelectAll(editor.GetTextEditorData());
        } else if (Focus is Gtk.TextView) {
            Gtk.TextView textview = (Gtk.TextView)Focus;
            textview.Buffer.SelectRange(textview.Buffer.StartIter, textview.Buffer.EndIter);
        }
    }

    protected virtual void OnIndentActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            
            Mono.TextEditor.MiscActions.IndentSelection(editor.GetTextEditorData());
        }
    }

    protected virtual void OnUnindentActionActivated(object sender, System.EventArgs e) {
        if (Focus is Mono.TextEditor.TextEditor) {
            Mono.TextEditor.TextEditor editor = (Mono.TextEditor.TextEditor)Focus;
            Mono.TextEditor.MiscActions.RemoveIndentSelection(editor.GetTextEditorData());
        }
    }

    protected virtual void OnCommentRegionActionActivated(object sender, System.EventArgs e) {
    }

    protected virtual void OnUncommentRegionActionActivated(object sender, System.EventArgs e) {
    }

    protected virtual void OnButton4Clicked(object sender, System.EventArgs e) {
        System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Getting_Started");
    }

    [GLib.ConnectBeforeAttribute]
    protected virtual void OnKeyPressEvent(object o, Gtk.KeyPressEventArgs args) {
        // FIXME: handle control+1 language switch here
        // not sure why accelerators didn't work
        int key_0 = (int)Gdk.Key.Key_0;
        if (((((int)args.Event.Key) > key_0) &&
              ((int)args.Event.Key) < (key_0 + 10))
            && (args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
            OnSwitchLanguage(((int)args.Event.Key) - key_0);
            args.RetVal = true;
            return;
        }
        if (Focus == Shell) {
            // Shell handler
            // control+c, if nothing is selected, else it is a copy
            if (args.Event.Key == Gdk.Key.c && (args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
                string text = Shell.SelectedText;
                if (text == null) {
                    Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
                    Shell.DeleteSelectedText();
                    args.RetVal = true;
                }
            } else if (args.Event.Key == Gdk.Key.Escape) {
                // escape with selected, delete; else delete all
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
                if (line != line_count - 1 && !force) {
                    completion = null;
                    args.RetVal = false;
                }
                // else, execute text
                // extra line at end signals ready_to_execute:
                string text = Shell.Document.Text;
                if (text == "") {
                    completion = null;
                    args.RetVal = true;
                    // nothing to do, but handled
                } else if (manager[CurrentLanguage].ReadyToExecute(text) || force) {
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
        } else if (Focus is Mono.TextEditor.TextEditor) { // not shell
            // Editor, handle : on end of line
        }
    }

    public bool Execute(string text, string language) {
        //if (executeThread) {
        //    return false;
        //}
        string prompt = String.Format("{0}> ", CurrentLanguage);
        int count = 2;
        foreach (string line in text.Split('\n')) {
            Write("{0}", prompt);
            // black
            WriteLine("{0}", line);
            // blue
            prompt = String.Format(".....{0}>", count);
            count += 1;
        }
        if (text == "") {
            return false;
        }
        this.CurrentLanguage = language;
        ExecuteInBackground(text);
        return true;
    }

    public void ExecuteInBackground(string text) {
        // This is the only approved method of running code
        manager[CurrentLanguage].execute(text);
    }

    public void Write(string format, params object[] args) {
        // These Write functions are the only approved methods of output
        if (Debug) {
            Console.Write(format, args);
        } else {
            Invoke( delegate {
                lock(Output) {
                    Output.Buffer.InsertAtCursor(String.Format(format, args));
                    Gtk.TextIter end = Output.Buffer.EndIter;
                    Gtk.TextMark mark = Output.Buffer.GetMark("insert");
                    Output.Buffer.MoveMark(mark, end);
                    Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
                    }
                });
        }
    }

    public void WriteLine(string format, params object[] args) {
        if (Debug) {
            Console.WriteLine(format, args);
        } else {
            Invoke( delegate {
                lock(Output) {
                    Output.Buffer.InsertAtCursor(String.Format(format, args) + "\n");
                    Gtk.TextIter end = Output.Buffer.EndIter;
                    Gtk.TextMark mark = Output.Buffer.GetMark("insert");
                    Output.Buffer.MoveMark(mark, end);
                    Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
                    }
                });
        }
    }

    public void ScrollToEnd() {
        Invoke(delegate {
            Gtk.TextIter end = Output.Buffer.EndIter;
            Gtk.TextMark mark = Output.Buffer.GetMark("insert");
            Output.Buffer.MoveMark(mark, end);
            Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
        });
    }

    protected virtual void OnNotebookDocsSwitchPage(object o, Gtk.SwitchPageArgs args) {
        if (CurrentDocument != null) {
    	  // FIXME: Turn some things on
            SetLanguage(CurrentDocument.language);
            CurrentDocument.widget.Child.GrabFocus();
            Title = String.Format("{0} - Calico Editor - {1}", CurrentDocument.filename, System.Environment.UserName);
        } else if (DocumentNotebook.Page == HOME) {
            // Home
            // FIXME: Turn some things off
            Title = String.Format("Calico - {0}", System.Environment.UserName);
        } else if (DocumentNotebook.Page == SHELL) {
            // Shell
            // FIXME: Turn some things off
            SetLanguage(ShellLanguage);
            Shell.GrabFocus();
            Title = String.Format("{0} - Calico Shell - {1}", CurrentProperLanguage, System.Environment.UserName);
        } else {
            // Some other page
            // FIXME: Turn some things off
            Title = String.Format("Calico - {0}", System.Environment.UserName);
        }
    }

    protected virtual void OnShellActionActivated(object sender, System.EventArgs e) {
        DocumentNotebook.Page = SHELL; // Shell
    }

    public Gtk.Notebook DocumentNotebook {
        get { return notebook_docs; }
    }
    
  }
}
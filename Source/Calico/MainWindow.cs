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
using System.Text;
using System.Collections.Generic;
using Calico;

namespace Calico {

    public partial class MainWindow : Gtk.Window {
        public Config config;
        public Gtk.Clipboard clipboard;
        private Mono.TextEditor.TextEditor _shell;
        public Dictionary<Gtk.Widget, Document> documents = new Dictionary<Gtk.Widget, Document>();
        public Dictionary<int, Language> languages_by_count = new Dictionary<int, Language>();
        public static Dictionary<Tag, Gtk.TextTag> tags = new Dictionary<Tag, Gtk.TextTag>();
        public static Dictionary<Tag, string> tagnames = new Dictionary<Tag, string>();
        public LanguageManager manager;
        public string CurrentLanguage = null;
        public string ShellLanguage = null;
        public bool Debug = false;
        public static int gui_thread_id = -1;
        public static int SHELL = 1;
        public static int HOME = 0;
        public string path;
        bool IsUpdateEnvironment = false; // FIXME: get from user options
        Dictionary<string,bool> EnvironmentVariables = new Dictionary<string, bool>();
        Dictionary<string,bool> LocalVariables = new Dictionary<string, bool>();
        Gtk.ListStore EnvironmentList;
        Gtk.ListStore LocalList;
        public System.Threading.Thread executeThread = null;
        public History history;
        public TabCompletion completion = null;
        static string dialogResponse;
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
        public Gtk.HScale ProgramSpeed {
            get { return hscale1;}
        }
        public Gtk.Notebook.NotebookChild EnvironmentPage {
            get { return (Gtk.Notebook.NotebookChild)(this.notebook_tools[this.GtkScrolledWindow1]);}
        }
        public Gtk.Notebook DocumentNotebook {
            get { return notebook_docs; }
        }
        public Gtk.Notebook ToolNotebook {
            get { return notebook_tools; }
        }
        public Document this[int page_num] {
            get {
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

        public string OS {
            get {
                string retval = System.Environment.OSVersion.Platform.ToString();
                if (retval.StartsWith("Win"))
                    retval = "Windows";
                return retval;
                // "Unix" or "Windows" are two common retvals
            }
        }
        public Gtk.TreeView EnvironmentTreeView {
            get { return treeview1; }
        }
        public Gtk.TreeView LocalTreeView {
            get { return treeview2; }
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
        public Gtk.Button StartButton {
            get { return _startButton; }
        }
        public Gtk.Action StartAction {
            get { return yesAction1; }
        }
        public Gtk.Button StopButton {
            get { return _stopButton; }
        }
        public Gtk.Button PauseButton {
            get { return _pauseButton; }
        }
        public Gtk.Button PlayButton {
            get { return _playButton; }
        }
        public string CurrentProperLanguage {
            get {
                if (CurrentLanguage != null && manager.languages.ContainsKey(CurrentLanguage))
                    return manager.languages[CurrentLanguage].proper_name;
                return null;
            }
            set {
                foreach (string lang in manager.getLanguages()) {
                    string lang_proper = manager.languages[lang].proper_name;
                    if (lang_proper == value) {
                        CurrentLanguage = lang;
                        break;
                    }
                }
            }
        }

        public static bool Contains(string ext, string[] extensions) {
            foreach (string lext in extensions) {
                if (lext == ext)
                    return true;
            }
            return false;
        }

        public MainWindow(string[] args, LanguageManager manager, bool Debug, Config config) :
                base(Gtk.WindowType.Toplevel) {
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            completion = null;
            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }
            // Colors by name:
            // FIXME: load from defaults
            tagnames[Tag.Error] = "error";
            tags[Tag.Error] = new Gtk.TextTag("error");
            tags[Tag.Error].Foreground = "red";
            tags[Tag.Error].Weight = Pango.Weight.Bold;
            
            tagnames[Tag.Warning] = "warning";
            tags[Tag.Warning] = new Gtk.TextTag("warning");
            tags[Tag.Warning].Foreground = "orange";
            
            tagnames[Tag.Info] = "info";
            tags[Tag.Info] = new Gtk.TextTag("info");
            tags[Tag.Info].Foreground = "purple";
            
            tagnames[Tag.Normal] = "normal";
            tags[Tag.Normal] = new Gtk.TextTag("normal");
            tags[Tag.Normal].Foreground = "black";
            // Build the GUI:
            Build();
            Gtk.Application.Invoke(delegate { gui_thread_id = Thread.CurrentThread.ManagedThreadId; });
            PostBuild();
            foreach (Gtk.TextTag tag in tags.Values) {
                Output.Buffer.TagTable.Add(tag);
                // TextView
            }
            Output.WrapMode = Gtk.WrapMode.Char; // FIXME: config
            //Output.CopyClipboard += OutputCopiedText;
            // Setup clipboard, and Gui:
            clipboard = Gtk.Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", false));
            DocumentNotebook.CurrentPage = 0;
            Title = String.Format("Calico - {0}", System.Environment.UserName);
            
            manager.SetCalico(this);
            // FIXME: move to Python language
            manager["python"].engine.Execute("from __future__ import division, with_statement, print_function;" +
                "del division, with_statement, print_function", false);

            manager.SetRedirects(new CustomStream(this, Tag.Normal), new CustomStream(this, Tag.Error));
            // Run this in in the GUI thread, after we start:
            Gtk.Application.Invoke(delegate { manager.PostSetup(this); });
            
            // Examples menu:
            Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
            examples_menu.Submenu = new Gtk.Menu();
            foreach (string lang in manager.getLanguages()) {
                Language language = manager[lang];
                DirectoryInfo dir = new DirectoryInfo(System.IO.Path.Combine(path, System.IO.Path.Combine("..", System.IO.Path.Combine("examples", language.name))));
                Gtk.MenuItem menu = new Gtk.MenuItem(language.proper_name);
                ((Gtk.Menu)examples_menu.Submenu).Add(menu);
                menu.Submenu = new Gtk.Menu();
                foreach (FileInfo f in dir.GetFiles("*.*")) {
                    if (!f.Name.EndsWith("~") && Contains(System.IO.Path.GetExtension(f.Name).Substring(1), language.extensions) && !f.Name.StartsWith("_")) {
                        Gtk.MenuItem fmenu = new Gtk.MenuItem(f.Name.Replace("_", "__"));
                        ((Gtk.Menu)menu.Submenu).Add(fmenu);
                        var fullname = f.FullName;
                        fmenu.Activated += delegate { SelectOrOpen(fullname); };
                    }
                }
                menu.Submenu.ShowAll();
            }
            examples_menu.Submenu.ShowAll();
            
            // New file menu:
            Gtk.MenuItem file_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/NewAction");
            file_menu.Submenu = new Gtk.Menu();
            foreach (KeyValuePair<string, Language> pair in manager.languages) {
                Language language = pair.Value;
                Gtk.MenuItem menu = new Gtk.MenuItem(language.proper_name);
                ((Gtk.Menu)file_menu.Submenu).Add(menu);
                menu.Activated += delegate { SelectOrOpen(null, language.name); };
            }
            file_menu.Submenu.ShowAll();
            
            // Languages to menu items:
            Gtk.MenuItem switch_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ScriptAction/LanguageAction");
            switch_menu.Submenu = new Gtk.Menu();
            int count = 0;
            Gtk.RadioMenuItem @group = null;
            Gtk.RadioMenuItem radioitem;
            foreach (KeyValuePair<string, Language> pair in manager.languages) {
                Language language = pair.Value;
                if (! language.IsTextLanguage)
                    continue;
                if (language.name == "python") {
                    // FIXME: get from defaults, preferred lang
                    CurrentLanguage = language.name;
                    ShellLanguage = language.name;
                } else if (CurrentLanguage == null) {
                    CurrentLanguage = language.name;
                    ShellLanguage = language.name;
                }
                // FIXME: select default language initially
                // unique name, label, mnemonic, accel, tooltip, user data
                string name = String.Format("Switch to {0}", language.proper_name);
                if (count == 0) {
                    radioitem = new Gtk.RadioMenuItem(name);
                    @group = radioitem;
                } else {
                    radioitem = new Gtk.RadioMenuItem(@group, name);
                }
                ((Gtk.Menu)switch_menu.Submenu).Add(radioitem);
                uint key;
                Gdk.ModifierType mod;
                Gtk.Accelerator.Parse(String.Format("<control>{0}", count + 1), out key, out mod);
                radioitem.AddAccelerator("activate", UIManager.AccelGroup, new Gtk.AccelKey((Gdk.Key)key, mod, Gtk.AccelFlags.Visible));
                languages_by_count[count + 1] = language;
                radioitem.ButtonReleaseEvent += OnSwitchLanguage;
                //DragDataReceived += SelectionReceived;
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

            Print(Tag.Info, String.Format("The Calico Project, Version {0}\n", MainClass.Version));
            SetLanguage(CurrentLanguage);
            // Load files:
            bool debug_handler = true;
            foreach (string arg in args) {
                if (arg.StartsWith("--")) {
                    if (arg == "--debug-handler") {
                        debug_handler = false;
                    }
                } else {
                    SelectOrOpen(System.IO.Path.GetFullPath(arg));
                }
            }
            if (debug_handler)
                GLib.ExceptionManager.UnhandledException += HandleException;
            // Set EnvironmentPage to match displayed state:
            IsUpdateEnvironment = true;
            EnvironmentTabAction.Active = true;
            history = new History((List<string>)this.config.values["shell"]["history"]);
            UpdateUpDownArrows();
            addToRecentsMenu(null);
            // End of GUI setup
            GLib.Timeout.Add(100, new GLib.TimeoutHandler( UpdateEnvironment ));
        }

        public void HandleException(GLib.UnhandledExceptionArgs args) {
            Print(Tag.Error, String.Format("Exception: {0}", args.ExceptionObject.ToString()));
        }

        public void PostBuild() {
            // Had to add this here, as Stetic didn't like it
            _shell = new Mono.TextEditor.TextEditor();
            _shell.KeyReleaseEvent += ShellKeyReleaseEvent;
            ScrolledWindow.Add(_shell);
            ScrolledWindow.ShowAll();
            // Environment table:
            EnvironmentTreeView.AppendColumn("Variable", new Gtk.CellRendererText(), "text", 0);
            EnvironmentTreeView.AppendColumn("Value", new Gtk.CellRendererText(), "text", 1);
            // Create a ListStore as the Model
            EnvironmentList = new Gtk.ListStore(typeof(string), typeof(string));
            EnvironmentTreeView.Model = EnvironmentList;
            EnvironmentTreeView.ShowAll();
            // Local environment:
            LocalTreeView.AppendColumn("Variable", new Gtk.CellRendererText(), "text", 0);
            LocalTreeView.AppendColumn("Value", new Gtk.CellRendererText(), "text", 1);
            LocalList = new Gtk.ListStore(typeof(string), typeof(string));
            LocalTreeView.Model = LocalList;
            LocalTreeView.ShowAll();
        }

        [GLib.ConnectBeforeAttribute]
        public void ShellKeyReleaseEvent(object obj, System.EventArgs args) {
            // This code was interacting with OnKeyPress
            if (_shell.Document.Text == "") {
                StartButton.Sensitive = false;
                StartAction.Sensitive = false;
            } else {
                StartButton.Sensitive = true;
                StartAction.Sensitive = true;
            }
        }

        public static bool needInvoke() {
            //Console.WriteLine("gui_thread_id: {0}", Myro.gui_thread_id);
            if (MainWindow.gui_thread_id == -1) {
                return false;
                // in another thread
            } else if (MainWindow.gui_thread_id == Thread.CurrentThread.ManagedThreadId) {
                return false;
                // you are already in the GUI thread
            } else {
                return true;
                // need to invoke!
            }
        }

        public delegate void InvokeDelegate();
        public static void Invoke(InvokeDelegate invoke) {
            if (needInvoke())
                Gtk.Application.Invoke(delegate { invoke(); });
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

        public void OnSwitchLanguage(object obj, Gtk.ButtonReleaseEventArgs args) {
            Gtk.RadioMenuItem radioitem = (Gtk.RadioMenuItem)obj;
            OnSwitchLanguage((int)radioitem.Data["id"]);
        }

        public void SetLanguage(string language) {
            CurrentLanguage = language;
            if (CurrentLanguage != null) {
                if (Shell.Document.MimeType != String.Format("text/x-{0}", CurrentLanguage)) {
                    try {
                        Shell.Document.MimeType = String.Format("text/x-{0}", CurrentLanguage);
                        // Force an update; works nicely as it keeps cursor in same place:
                        // FIXME: Interaction with ShellOnKeyPress?
                        Shell.Document.Text = Shell.Document.Text;
                    } catch {
                        // pass
                    }
                }
            }
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
                    // Groups[0] is entire string
                    filename = (string)match.Groups[1].Captures[0].Value;
                    lineno = Convert.ToInt32(match.Groups[2].Captures[0].Value);
                }
            }
            // Special case for quick goto:
            if (CurrentDocument != null && CurrentDocument.filename == filename && filename != null) {
                if (lineno != 0) {
                    return CurrentDocument.GotoLine(lineno);
                } else {
                    return true;
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
                    if (language == null)
                        language = manager.GetLanguageFromExtension(filename);
                    page = MakeDocument(filename, language);
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
                int page_num = DocumentNotebook.AppendPage(page.widget, page.tab_widget);
                documents[page.widget] = page;
                DocumentNotebook.SetTabReorderable(page.widget, true);
                DocumentNotebook.CurrentPage = page_num;
                page.close_button.Clicked += delegate { TryToClose(page); };
            }
            if (filename != null) {
                string dir = System.IO.Path.GetDirectoryName(filename);
                System.IO.Directory.SetCurrentDirectory(dir);
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

        public Document MakeDocument(string filename) {
            string language = CurrentLanguage;
            return MakeDocument(filename, language);
        }

        public void addToRecentsMenu (string filename)
        {
            List<string> filenames = (List<string>)config.GetValue("config", "recent-files");
            if (filename != null) {
                // Last is most recent:
                if (filenames.Contains(filename)) {
                    filenames.Remove(filename);
                }
                filenames.Insert(0, filename);
            }
            // Trim down to appropriate size:
            int max = (int)config.GetValue("config", "recent-files-size");
            if (filenames.Count > max) {
                filenames.RemoveRange(max, filenames.Count - max);
            }
            // Now update menu:
            Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/RecentScriptsAction");
            examples_menu.Submenu = new Gtk.Menu();
            foreach (string file in filenames) {
                Gtk.MenuItem fmenu = new Gtk.MenuItem(file.Replace("_", "__"));
                ((Gtk.Menu)examples_menu.Submenu).Add(fmenu);
                string fullname = file; // because C# doesn't make closures on values
                fmenu.Activated += delegate { SelectOrOpen(fullname); };
            }
            examples_menu.Submenu.ShowAll();
        }

        public Document MakeDocument(string filename, string language) {
            if (language == null) {
                language = CurrentLanguage;
            }
            Document document = manager.languages[language].MakeDocument(this, filename);
            document.Configure();
            addToRecentsMenu(filename);
            return document;
        }

        public void Cleanup() {
            // Any additions before saving:
            int max = (int)config.GetValue("shell", "history-size");
            if (max != -1) { // no limit
                List<string> history = (List<string>)config.GetValue("shell", "history");
                if (history.Count > max) {
                    history.RemoveRange(0, history.Count - max);
                }
            }
            config.Save();
        }

        public bool Close() {
            // Delete Window
            // FIXME: ask to save files, or cancel
            return true;
        }

        protected void OnDeleteEvent(object sender, Gtk.DeleteEventArgs a) {
            if (Close()) {
                Cleanup();
                Gtk.Application.Quit();
                a.RetVal = true;
            }
        }

        public void PickNew() {
            string [] prop_languages = manager.getLanguagesProper();
            if (prop_languages.Length > 1) {
                ManualResetEvent ev = new ManualResetEvent(false);
                dialogResponse = null;
                Invoke(delegate {
                    Gtk.Dialog fc = new Gtk.Dialog("Select Language", this, 0);
                    fc.VBox.PackStart(new Gtk.Label("Create a New Script"));
                    foreach (string choice in manager.getLanguagesProper()) {
                        Gtk.Button button = new Gtk.Button(choice);
                        button.Clicked += new System.EventHandler(DialogHandler);
                        fc.AddActionWidget(button, Gtk.ResponseType.Ok);
                    }
                    fc.ShowAll();
                    fc.Run();
                    fc.Destroy();
                    ev.Set();
                    });
                ev.WaitOne();
                if (dialogResponse != null) {
                    CurrentProperLanguage = dialogResponse;
                    SelectOrOpen(null, CurrentLanguage);
                }
            } else {
                SelectOrOpen();
            }
        }

        public static void DialogHandler(object obj, System.EventArgs args) {
            dialogResponse = ((Gtk.Button)obj).Label;
        }

        protected virtual void OnNewActionActivated(object sender, System.EventArgs e) {
            PickNew();
        }

        protected virtual void OnButton3Clicked(object sender, System.EventArgs e) {
            PickNew();

        }
        protected void OnNewButtonClicked (object sender, System.EventArgs e)
        {
            PickNew();
        }

        protected virtual void OnOpenAction1Activated(object sender, System.EventArgs e) {
            Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog(_("Select the file to open"), this, Gtk.FileChooserAction.Open, _("Cancel"), Gtk.ResponseType.Cancel, _("Open"), Gtk.ResponseType.Accept);
            fc.KeepAbove = true;
            if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
                SelectOrOpen(fc.Filename);
            }
            fc.Destroy();
        }

        protected virtual void OnOpenActionActivated(object sender, System.EventArgs e) {
            OnOpenAction1Activated(sender, e);
        }

        protected virtual void OnSaveActionActivated(object sender, System.EventArgs e) {
            if (CurrentDocument != null) {
                CurrentDocument.Save();
                SetLanguage(CurrentLanguage);
                addToRecentsMenu(CurrentDocument.filename); // needed, if didn't have filename before
            }
        }

        protected virtual void OnSaveAsActionActivated(object sender, System.EventArgs e) {
            if (CurrentDocument != null) {
                CurrentDocument.SaveAs();
                SetLanguage(CurrentLanguage);
                addToRecentsMenu(CurrentDocument.filename);
            }
        }

        protected virtual void OnQuitActionActivated(object sender, System.EventArgs e) {
            if (Close()) {
                Cleanup();
                Gtk.Application.Quit();
            }
        }

        protected virtual void OnButton2Clicked(object sender, System.EventArgs e) {
            OnOpenAction1Activated(sender, e);
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

        protected virtual string CommentText(string text) {
            // FIXME: this isn't right... need to get entire lines
            text = "## " + text.Replace("\n", "\n## ");
            return text;
        }

        protected virtual string UnCommentText(string text) {
            return text;
        }

        protected virtual string CleanUpText(string text) {
            // Prevent weird characters like Word smart quotes.
            // First, replace all of those that we know about:
            text = text.Replace('\u2013', '-');
            text = text.Replace('\u2014', '-');
            text = text.Replace('\u2015', '-');
            text = text.Replace('\u2017', '_');
            text = text.Replace('\u2018', '\'');
            text = text.Replace('\u2019', '\'');
            text = text.Replace('\u201a', ',');
            text = text.Replace('\u201b', '\'');
            text = text.Replace('\u201c', '\"');
            text = text.Replace('\u201d', '\"');
            text = text.Replace('\u201e', '\"');
            text = text.Replace("\u2026", "...");
            text = text.Replace('\u2032', '\'');
            text = text.Replace('\u2033', '\"');
            // And then replace the rest with '':
            byte[] bytes = Encoding.Convert(Encoding.UTF8, Encoding.GetEncoding(Encoding.ASCII.EncodingName, new EncoderReplacementFallback(string.Empty), new DecoderExceptionFallback()), Encoding.UTF8.GetBytes(text));
            return Encoding.ASCII.GetString(bytes);
        }

        protected virtual void OnPasteActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextEditor) {
                string text = ((Mono.TextEditor.TextEditor)Focus).SelectedText;
                if (text != null)
                    ((Mono.TextEditor.TextEditor)Focus).DeleteSelectedText();
                ((Mono.TextEditor.TextEditor)Focus).InsertAtCaret(CleanUpText(clipboard.WaitForText()));
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

        public static int CommentLine(Mono.TextEditor.TextEditorData data, Mono.TextEditor.LineSegment line) {
            // FIXME: get comment string from language
            data.Insert(line.Offset, "## ");
            return 1;
        }

        public static int UnCommentLine(Mono.TextEditor.TextEditorData data, Mono.TextEditor.LineSegment line) {
            // FIXME: get comment string from language
            if (data.Document.GetCharAt(line.Offset) == '#' && data.Document.GetCharAt(line.Offset + 1) == '#' && data.Document.GetCharAt(line.Offset + 2) == ' ') {
                data.Remove(line.Offset, 3);
                return 1;
            } else {
                return 0;
            }
        }

        static void SelectLineBlock(Mono.TextEditor.TextEditorData data, int endLineNr, int startLineNr) {
            Mono.TextEditor.LineSegment endLine = data.Document.GetLine(endLineNr);
            data.MainSelection = new Mono.TextEditor.Selection(startLineNr, 0, endLineNr, endLine.Length);
        }

        protected virtual void OnCommentRegionActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextEditor) {
                Mono.TextEditor.TextEditor texteditor = (Mono.TextEditor.TextEditor)Focus;
                Mono.TextEditor.TextEditorData data = texteditor.GetTextEditorData();
                int startLineNr = data.IsSomethingSelected ? data.MainSelection.MinLine : data.Caret.Line;
                int endLineNr = data.IsSomethingSelected ? data.MainSelection.MaxLine : data.Caret.Line;
                data.Document.BeginAtomicUndo();
                int first = -1;
                int last = 0;
                foreach (Mono.TextEditor.LineSegment line in data.SelectedLines) {
                    last = CommentLine(data, line);
                    if (first < 0)
                        first = last;
                }
                if (data.IsSomethingSelected)
                    SelectLineBlock(data, endLineNr, startLineNr);
                if (data.Caret.Column != 0) {
                    data.Caret.PreserveSelection = true;
                    data.Caret.Column = System.Math.Max(0, data.Caret.Column - last);
                    data.Caret.PreserveSelection = false;
                }
                data.Document.EndAtomicUndo();
                data.Document.RequestUpdate(new Mono.TextEditor.MultipleLineUpdate(startLineNr, endLineNr));
                data.Document.CommitDocumentUpdate();
            }
        }

        protected virtual void OnUncommentRegionActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextEditor) {
                Mono.TextEditor.TextEditor texteditor = (Mono.TextEditor.TextEditor)Focus;
                Mono.TextEditor.TextEditorData data = texteditor.GetTextEditorData();
                int startLineNr = data.IsSomethingSelected ? data.MainSelection.MinLine : data.Caret.Line;
                int endLineNr = data.IsSomethingSelected ? data.MainSelection.MaxLine : data.Caret.Line;
                data.Document.BeginAtomicUndo();
                int first = -1;
                int last = 0;
                foreach (Mono.TextEditor.LineSegment line in data.SelectedLines) {
                    last = UnCommentLine(data, line);
                    if (first < 0)
                        first = last;
                }
                if (data.IsSomethingSelected)
                    SelectLineBlock(data, endLineNr, startLineNr);
                if (data.Caret.Column != 0) {
                    data.Caret.PreserveSelection = true;
                    data.Caret.Column = System.Math.Max(0, data.Caret.Column - last);
                    data.Caret.PreserveSelection = false;
                }
                data.Document.EndAtomicUndo();
                data.Document.RequestUpdate(new Mono.TextEditor.MultipleLineUpdate(startLineNr, endLineNr));
                data.Document.CommitDocumentUpdate();
            }
        }

        protected virtual void OnButton4Clicked(object sender, System.EventArgs e) {
            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Getting_Started");
        }

        [GLib.ConnectBeforeAttribute]
        protected virtual void OnKeyPressEvent(object o, Gtk.KeyPressEventArgs args) {
            int key_0 = (int)Gdk.Key.Key_0;
            if (((((int)args.Event.Key) > key_0) && ((int)args.Event.Key) < (key_0 + 10)) && (args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
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
                    if ((args.Event.State & Gdk.ModifierType.ControlMask) != 0) {
                        Shell.InsertAtCaret("\n");
                        args.RetVal = true;
                        return;
                    }
                    // if cursor in middle, insert a Return
                    Mono.TextEditor.Caret caret = Shell.Caret;
                    int line = caret.Line;
                    int line_count = Shell.Document.LineCount;
                    if (line != line_count - 1) {
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
                    } else if (manager[CurrentLanguage].engine.ReadyToExecute(text)) {
                        history.last(text.TrimEnd());
                        history.add("");
                        ExecuteShell();
                        completion = null;
                        args.RetVal = true;
                    }
                    UpdateUpDownArrows();
                } else if (args.Event.Key == Gdk.Key.Up) {
                    KeyUp(false); // look at cursor (don't force)
                } else if (args.Event.Key == Gdk.Key.Down) {
                    KeyDown(false); // look at cursor (don't force)
                } else if (args.Event.Key == Gdk.Key.Tab) {
                    // where are we?
                    int lineNo = Shell.Caret.Line;
                    string [] lines = Shell.Document.Text.Split('\n');
                    string line = lines[lineNo];
                    string text = line.Substring(0, Shell.Caret.Column);
                    if (text.Trim() != "") { // something there!
                        if (completion == null) {
                            completion = new TabCompletion(this, Shell, text);
                            if (completion.items != null) { // first time:
                                Print(completion.format());
                            }
                        }
                        if (completion.items != null && completion.items.Count > 0) {
                            completion.insertText();
                        }
                        args.RetVal = true; // don't put in tab
                    }
                    completion = null;
                } else {
                    completion = null;
                }
            } else if (Focus is Mono.TextEditor.TextEditor) {
                // Handle not dirty
                // not shell
                // Editor, handle : on end of line
            }
        }

        public void KeyDown(bool force) {
            string text = Shell.Document.Text;
            var caret = Shell.Caret;
            int line = caret.Line;
            int line_count = Shell.Document.LineCount;
            if (force || line == (line_count - 1)) {
                history.update(text.TrimEnd());
                text = history.down();
                Shell.Document.Text = text;
                Shell.GrabFocus();
                Shell.Caret.Line = Shell.Document.LineCount - 1;
                Shell.Caret.Column = Shell.Document.GetLine(0).Length;
             }
            UpdateUpDownArrows();
        }

        public void KeyUp(bool force) {
            string text = Shell.Document.Text;
            var caret = Shell.Caret;
            int line = caret.Line;
            if (force || line == 0) {
                history.update(text.TrimEnd());
                text = history.up();
                Shell.Document.Text = text;
                Shell.GrabFocus();
                Shell.Caret.Line = 0;
                int col = Shell.Document.GetLine(0).Length;
                Shell.Caret.Column = col;
            }
            UpdateUpDownArrows();
        }

        public void UpdateUpDownArrows() {
            if (history.Position == 0) {
                history_up.Sensitive = false;
            } else {
                history_up.Sensitive = true;
            }
            if (history.Position == history.Last) {
                history_down.Sensitive = false;
            } else {
                history_down.Sensitive = true;
            }
            if (spinbutton1.Value != history.Position + 1) {
                spinbutton1.Value = history.Position + 1;
            }
        }

        public bool ExecuteShell() {
            string text = Shell.Document.Text;
            bool results = Execute(text.TrimEnd(), CurrentLanguage);
            if (results) {
                Mono.TextEditor.SelectionActions.SelectAll(Shell.GetTextEditorData());
                Mono.TextEditor.DeleteActions.DeleteSelection(Shell.GetTextEditorData());
                Shell.GrabFocus();
                Shell.Caret.Line = 0;
                Shell.Caret.Column = 0;
            }
            return true;
        }

        public bool Execute(string text, string language) {
            if (executeThread != null) {
                return false;
            }
            string prompt = String.Format("{0}> ", CurrentLanguage);
            int count = 2;
            foreach (string line in text.Split('\n')) {
                Print(Tag.Info, prompt);
                // black
                Print(line + "\n");
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
            executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                Invoke(OnStartRunning);
                manager[CurrentLanguage].engine.Execute(text);
                Invoke(OnStopRunning);
            }));
            executeThread.IsBackground = true;
            executeThread.Start();
        }

        public void OnStartRunning() {
            StartButton.Sensitive = false;
            StartAction.Sensitive = false;
            PlayButton.Sensitive = false;
            PauseButton.Sensitive = false;
            StopButton.Sensitive = true;
            noAction.Sensitive = true;
        }

        public void OnStopRunning() {
            StopButton.Sensitive = false;
            noAction.Sensitive = false;
            if (CurrentDocument != null) { // Editor
                if (CurrentDocument.HasContent) {
                    StartButton.Sensitive = true; // need something to execute
                    StartAction.Sensitive = true;
                    ProgramSpeed.Sensitive = true;
                    PlayButton.Sensitive = true;
                    PauseButton.Sensitive = true;
                } else {
                    StartButton.Sensitive = false; // need something to execute
                    StartAction.Sensitive = false;
                    ProgramSpeed.Sensitive = false;
                    PlayButton.Sensitive = false;
                    PauseButton.Sensitive = false;
                }
            } else { // Shell
                StartButton.Sensitive = false; // need something to execute
                StartAction.Sensitive = false;
                ProgramSpeed.Sensitive = false;
                PlayButton.Sensitive = false;
                PauseButton.Sensitive = false;
            }
            executeThread = null;
            /*
       if self.calico.last_error != "":
            url = self.make_error_url(self.language, self.calico.last_error)
            error_button_text = None
            text = self.get_error_text(self.calico.last_error)
            match = re.search(_('File \"(.*)\", line (\d*)'), text)
            if match: # 'File "<string>", line 167'
                filename, lineno = match.groups()
                lineno = int(lineno)
                basename = os.path.basename(filename)
                filename = os.path.abspath(filename)
                if basename != "<string>":
                    error_button_text = _("Go to error in %s") % basename
                    error_button_func = lambda w, e: self.goto_file(filename, lineno)
            def invoke(sender, args):
                button = Gtk.Button(_("Get help on error"))
                button.Clicked += lambda o, e: OpenUrl(url)
                button.Show()
                anchor_iter = self.history_textview.Buffer.CreateChildAnchor(self.history_textview.Buffer.EndIter)
                self.history_textview.AddChildAtAnchor(button, anchor_iter[0])
                if error_button_text:
                    button = Gtk.Button(error_button_text)
                    button.Clicked += error_button_func
                    button.Show()
                    anchor_iter = self.history_textview.Buffer.CreateChildAnchor(self.history_textview.Buffer.EndIter)
                    self.history_textview.AddChildAtAnchor(button, anchor_iter[0])
            self.calico.Invoke(invoke)
             */
        }

        public void AbortThread() {
            // FIXME: import Myro
            if (executeThread != null) {
                Print(Tag.Warning, _("Stopping...\n"));
                executeThread.Abort();
                executeThread.Join();
                executeThread = null;
                //if Myro.robot:
                //    Myro.robot.flush()
                //    Myro.robot.stop()
                Invoke(OnStopRunning);
            }
        }

        public void ExecuteFileInBackground(string filename, string language) {
            // This is the only approved method of running code
            Print(Tag.Info, String.Format("Running '{0}'...\n", filename));
            executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                Invoke(OnStartRunning);
                CurrentLanguage = language;
                manager[CurrentLanguage].engine.ExecuteFile(filename); // not in GUI thread
                Invoke(OnStopRunning);
            }));
            executeThread.IsBackground = true;
            executeThread.Start();
        }

        public static Gtk.TextTag TagColor(Tag tag) {
            return tags[tag];
        }

        public void Print(string format) {
            Print(Tag.Normal, format);
        }

        public void Print(Tag tag, string format) {
            // These Write functions are the only approved methods of output
            Thread.Sleep(1); // Force a context switch, even for an instant
            if (Debug) {
                Console.Write(format);
            } else {
                Invoke(delegate {
                    lock (Output) {
                        if (tag == Tag.Error)
                            ToolNotebook.Page = 0; // show output page
                        Gtk.TextIter end = Output.Buffer.EndIter;
                        string colorname = MainWindow.tagnames[tag];
                        Output.Buffer.InsertWithTagsByName(ref end, format, colorname);
                        end = Output.Buffer.EndIter;
                        Gtk.TextMark mark = Output.Buffer.GetMark("insert");
                        Output.Buffer.MoveMark(mark, end);
                        Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
                    }
                });
            }
        }

        public void ScrollToEnd() { // Made to be called by itself, from anywhere
            Invoke(delegate {
                Gtk.TextIter end = Output.Buffer.EndIter;
                Gtk.TextMark mark = Output.Buffer.GetMark("insert");
                Output.Buffer.MoveMark(mark, end);
                Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
            });
        }

        protected virtual void OnNotebookDocsSwitchPage(object o, Gtk.SwitchPageArgs args) {
            if (executeThread != null) {
                StopButton.Sensitive = true;
                noAction.Sensitive = true;
            } else {
                StopButton.Sensitive = false;
                noAction.Sensitive = false;
            }
            if (CurrentDocument != null) {
                if (CurrentDocument.HasContent) {
                    StartButton.Sensitive = true;
                    StartAction.Sensitive = true;
                    ProgramSpeed.Sensitive = true;
                    PauseButton.Sensitive = true;
                    PlayButton.Sensitive = true;
                } else {
                    StartButton.Sensitive = false;
                    StartAction.Sensitive = false;
                    ProgramSpeed.Sensitive = false;
                    PauseButton.Sensitive = false;
                    PlayButton.Sensitive = false;
                }
                SetLanguage(CurrentDocument.language);
                CurrentDocument.widget.Child.GrabFocus();
                Title = String.Format("{0} - Calico Editor - {1}", CurrentDocument.basename, System.Environment.UserName);
            } else if (DocumentNotebook.Page == HOME) {
                StartButton.Sensitive = false;
                StartAction.Sensitive = false;
                ProgramSpeed.Sensitive = false;
                PauseButton.Sensitive = false;
                PlayButton.Sensitive = false;
                Title = String.Format("Calico - {0}", System.Environment.UserName);
            } else if (DocumentNotebook.Page == SHELL) {
                if (Shell.Document.Text != "") {
                    ProgramSpeed.Sensitive = false;
                    StartAction.Sensitive = true;
                    StartButton.Sensitive = true;
                    PauseButton.Sensitive = false;
                    PlayButton.Sensitive = false;
                } else {
                    StartButton.Sensitive = false;
                    StartAction.Sensitive = false;
                    ProgramSpeed.Sensitive = false;
                    PauseButton.Sensitive = false;
                    PlayButton.Sensitive = false;
                }
                SetLanguage(ShellLanguage);
                // Workaround: had to add this for notebook page selection:
                GLib.Timeout.Add(0, delegate {
                    Shell.GrabFocus();
                    return false;
                });
                Title = String.Format("{0} - Calico Shell - {1}", CurrentProperLanguage, System.Environment.UserName);
            } else {
                StartButton.Sensitive = false;
                StartAction.Sensitive = false;
                ProgramSpeed.Sensitive = false;
                PauseButton.Sensitive = false;
                PlayButton.Sensitive = false;
                // Some other page
                Title = String.Format("Calico - {0}", System.Environment.UserName);
            }
        }

        protected virtual void OnShellActionActivated(object sender, System.EventArgs e) {
            DocumentNotebook.Page = SHELL;
            // Shell
        }

        protected virtual void OnYesAction1Activated(object sender, System.EventArgs e) {
            if (CurrentDocument != null) {
                bool retval = CurrentDocument.Save();
                if (retval) {
                    if (ProgramSpeed.Value < 100) {
                        // FIXME: Let Document handle tracing, call Document.ExecuteFileWithTrace
                        SetLanguage(CurrentLanguage);
                        manager[CurrentLanguage].engine.SetTraceOn(this);
                        ExecuteFileInBackground(CurrentDocument.filename, CurrentDocument.language);
                    } else {
                        ProgramSpeed.Sensitive = false;
                        SetLanguage(CurrentLanguage);
                        manager[CurrentLanguage].engine.SetTraceOff();
                        CurrentDocument.ExecuteFileInBackground();
                    }
                }
            } else if (DocumentNotebook.Page == SHELL) {
                string text = Shell.Document.Text;
                history.last(text.TrimEnd());
                history.add("");
                ExecuteShell();
            }
        }

        public bool EnvModelForeachFunc (Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
            string vname = (string)model.GetValue(iter, 0);
            try {
                model.SetValue(iter, 1, Repr(manager.scope.GetVariable(vname)));
            } catch {
                model.SetValue(iter, 1, "<undefined>");
            }
            return false;
        }

        public bool LocalModelForeachFunc (Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter,
            string variable, string value) {
            string vname = (string)model.GetValue(iter, 0);
            if (variable == vname) {
                model.SetValue(iter, 1, value);
            } else {
                model.SetValue(iter, 1, "<undefined>");
            }
            return false;
        }

        public static string ArrayToString(object[] args) {
            string retval = "";
            if (args != null) {
              int count = ((Array)args).Length;
              for (int i = 0; i < count; i++) {
                      if (args[i] is object[]) {
                            retval += ArrayToString((object[])args[i]);
                      } else {
                            if (retval != "")
                              retval += ", ";
                            retval += args[i];
                      }
              }
            }
            return "[" + retval + "]";
        }

        public static bool HasMethod(object obj, string methodName) {
            if (obj != null) {
	            Type type = obj.GetType();
	            return type.GetMethod(methodName) != null;
            } else {
                return false;
            }
	    }

        public static string InvokeMethod(object obj, string methodName, params object [] args) {
	        var type = obj.GetType();
            try {
                System.Reflection.MethodInfo mi = type.GetMethod(methodName);
	            return (string)mi.Invoke(obj, args);
            } catch {
                return null;
            }
	    }

        public string Repr(object obj) {
            string repr = null;
    	    if (HasMethod(obj, "__repr__")) {
                // FIXME: make sure there is a python
                repr = InvokeMethod(obj, "__repr__", manager["python"].engine.GetDefaultContext());
    	    } else if (HasMethod(obj, "to_s")) {
                repr = InvokeMethod(obj, "to_s");
            } else if (obj is Array) {
                repr = (string)ArrayToString((object[]) obj);
            }
            if (repr == null) {
                if (obj != null)
                    repr = obj.ToString();
                else
                    repr = "None";
            }
            return repr;
        }

        public void ClearLocal() {
            LocalList.Clear();
            LocalVariables.Clear();
        }

        public void UpdateLocal(string vname, string repr) {
            LocalList.Foreach((a, b, c) => LocalModelForeachFunc(a, b, c,
                vname, repr));
            if (! LocalVariables.ContainsKey(vname)) {
                Gtk.TreeIter iter = LocalList.AppendValues(vname, repr);
                Gtk.TreePath treepath = LocalList.GetPath(iter);
                LocalTreeView.SetCursor(treepath, null, false);
                LocalVariables[vname] = true;
            }
        }

        public bool UpdateEnvironment() {
            // update tree in Environment tab
            // update the ones that already exist:
            EnvironmentList.Foreach(EnvModelForeachFunc);
            // now, add those that aren't in it:
            foreach (string vname in manager.scope.GetVariableNames()) {
                if (vname.StartsWith("_") || vname == "calico")
                    continue;
                if (! EnvironmentVariables.ContainsKey(vname)) {
                    string repr = Repr(manager.scope.GetVariable(vname));
                    Gtk.TreeIter iter = EnvironmentList.AppendValues(vname, repr);
                    Gtk.TreePath treepath = EnvironmentList.GetPath(iter);
                    EnvironmentTreeView.SetCursor(treepath, null, false);
                    EnvironmentVariables[vname] = true;
                }
            }
            return IsUpdateEnvironment; // keep doing
        }

        protected virtual void OnNoActionActivated (object sender, System.EventArgs e)
        {
            AbortThread();
        }

        protected virtual void OnEnvironmentTabActionActivated (object sender, System.EventArgs e)
        {
            // show environment tab if active
            if (! EnvironmentTabAction.Active) {
                IsUpdateEnvironment = false;
                EnvironmentPage.Child.Hide();
            } else {
                IsUpdateEnvironment = true;
                GLib.Timeout.Add(250, new GLib.TimeoutHandler( UpdateEnvironment ));
                EnvironmentPage.Child.Show();
                ToolNotebook.Page = 1;
            }
        }
        
        protected virtual void OnHistoryUpClicked (object sender, System.EventArgs e)
        {
            KeyUp(true); // force it regardless of cursor location
        }

        protected virtual void OnHistoryDownClicked (object sender, System.EventArgs e)
        {
            KeyDown(true);
        }

        protected void OnZoomInActionActivated (object sender, System.EventArgs e)
        {
            if (CurrentDocument != null)
                CurrentDocument.ZoomIn();
        }

        protected void OnZoomOutActionActivated (object sender, System.EventArgs e)
        {
            if (CurrentDocument != null)
                CurrentDocument.ZoomOut();
        }

        protected void OnExportActionActivated (object sender, System.EventArgs e)
        {
            if (CurrentDocument != null)
                CurrentDocument.Export(this);

        }

        protected void OnMediaPauseActionActivated (object sender, System.EventArgs e)
        {
            throw new System.NotImplementedException ();
        }

        protected void OnOpenButtonClicked (object sender, System.EventArgs e)
        {
            OnOpenAction1Activated(sender, e);
        }

        protected void OnStopButtonClicked (object sender, System.EventArgs e)
        {
            AbortThread();
        }

        protected void OnStartButtonClicked (object sender, System.EventArgs e)
        {
            OnYesAction1Activated(sender, e);
        }

        protected void OnPauseButtonClicked (object sender, System.EventArgs e)
        {
            throw new System.NotImplementedException ();
        }

        protected void OnPlayButtonClicked (object sender, System.EventArgs e)
        {
            throw new System.NotImplementedException ();
        }

        protected void OnPrintActionActivated (object sender, System.EventArgs e)
        {
            if (CurrentDocument != null) {
                CurrentDocument.Print();
            }
        }

        protected void OnSpinbutton1ChangeValue (object o, Gtk.ChangeValueArgs args)
        {
            //Console.WriteLine("ChangeValue: {0}", args);
        }

        protected void OnSpinbutton1Input (object o, Gtk.InputArgs args)
        {
            //Console.WriteLine("Input: {0}", args);
        }
    }
}

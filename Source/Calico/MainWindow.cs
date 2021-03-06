//
//  MainWindow.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011-2014 The Calico Project
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

using Mono.Unix;
using System;
using System.IO;

// Path
using System.Threading;
using System.Text;
using System.Collections.Generic;
using Calico;

using System.Collections;

namespace Calico {
    public static class Extensions
    {
        public static T[] Slice<T> (this T[] source, int start, int end)
        {
            if (start == 0 && end == source.Length)
                return source;
            // Handles negative ends.
            if (end < 0) {
                end = source.Length + end;
            }
            int len = end - start;
            // Return new array.
            T[] res = new T[len];
            for (int i = 0; i < len; i++) {
                res [i] = source [i + start];
            }
            return res;
        }

        public static T[] Slice<T> (this T[] source, int start)
        {
            int end = source.Length;
            if (start == 0 && end == source.Length)
                return source;
            int len = end - start;
            // Return new array.
            T[] res = new T[len];
            for (int i = 0; i < len; i++) {
                res [i] = source [i + start];
            }
            return res;
        }

        public static object[] Slice (this IList source, int start, int end)
        {
            // Handles negative ends.
            if (end < 0) {
                end = source.Count + end;
            }
            int len = end - start;
            // Return new array.
            object[] res = new object[len];
            for (int i = 0; i < len; i++) {
                res [i] = source [i + start];
            }
            return res;
        }
    }

    public partial class MainWindow : Gtk.Window {
        internal string environment = "gtk";
        public bool serverMode = false;
        public Config config;
        public Gtk.Clipboard clipboard;
        private Mono.TextEditor.TextEditor _shell;
        public Dictionary<Gtk.Widget, Document> documents = new Dictionary<Gtk.Widget, Document>();
        public Dictionary<int, Language> languages_by_count = new Dictionary<int, Language>();
        public static Dictionary<Tag, Gtk.TextTag> tags = new Dictionary<Tag, Gtk.TextTag>();
        public static Dictionary<Tag, string> tagnames = new Dictionary<Tag, string>();
        public static Dictionary<string,Func<object,Gtk.KeyPressEventArgs,Gtk.Widget,bool>> onKey = 
            new Dictionary<string,Func<object,Gtk.KeyPressEventArgs,Gtk.Widget,bool>>();
        public ManualResetEvent playResetEvent = new ManualResetEvent(false);
        public Gtk.RadioMenuItem language_group;
        public LanguageManager manager;
        public string CurrentLanguage = null;
        public string ShellLanguage = null;
        public bool Debug = false;
        public static int gui_thread_id = -1;
        public string path;
        bool IsUpdateEnvironment = false; // FIXME: get from user options
        Dictionary<string,bool> EnvironmentVariables = new Dictionary<string, bool>();
        Dictionary<string,bool> LocalVariables = new Dictionary<string, bool>();
        Gtk.ListStore EnvironmentList;
        Gtk.ListStore LocalList;
        public System.Threading.Thread executeThread = null;
        public bool isRunning = false;
        public History history;
        public TabCompletion completion = null;
        static string dialogResponse;
        public Chat connection;
        int animationSequence = 0;
        Gtk.Image[] animationImages = new Gtk.Image [2];
        System.Threading.Thread signal_thread;
        public static MainWindow _mainWindow = null;
        Gtk.Widget _lastSelectedPage = null;
        private bool searchMode; // true: search-only; false: replace
        public string[] args = null;
        public System.IO.TextWriter stdout = System.Console.Out;
        public System.IO.TextWriter stderr = System.Console.Error;
        public CustomStream cstdout;
        public CustomStream cstderr;
        public IList<string> persistentPaths = new List<string>();
        public string onReset = null;

	public void addPath(string path, string file=""){
		if(file != ""){
			string dir = System.IO.Path.GetDirectoryName(file);
			path = dir + "/" + path; 
		}
		path = new FileInfo(path).FullName;
		if(!persistentPaths.Contains(path)){
			persistentPaths.Add(path);
			manager.AddPath(path);
		}
	}

	public object getGlobals(){
		if(manager.ContainsKey("python")){
			return manager["python"].engine.getPersistentVariables();
		} else {
			return null;
		}
	}

	public void clearGlobals(){
		if(manager.ContainsKey("python")){
			manager["python"].engine.clearPersistentVariables();
		}
	}

        public bool toolSwapped {
            get {
                return (bool) config.GetValue("config", "toolSwapped");
            }
            set {
                config.SetValue("config", "toolSwapped", value);
            }
        }
        public bool verticalSwapped  {
            get {
                return (bool) config.GetValue("config", "verticalSwapped");
            }
            set {
                config.SetValue("config", "verticalSwapped", value);
            }
        }
        public bool horizontalSwapped  {
            get {
                return (bool) config.GetValue("config", "horizontalSwapped");
            }
            set {
                config.SetValue("config", "horizontalSwapped", value);
            }
        }

        public double ProgramSpeedValue {
            get {
                ManualResetEvent ev = new ManualResetEvent(false);
                double retval = 0;
                Invoke(delegate {
                    retval = ProgramSpeed.Value;
                    ev.Set();
                });
                ev.WaitOne();
                return retval;
            }
        }

        public Gtk.Widget lastSelectedPage {
            get { return _lastSelectedPage;}
            set { 
                //System.Console.WriteLine("setting lastSelectedPage = " + value);
                _lastSelectedPage = value;
            }
        }

        enum TargetType {
            String,
            RootWindow,
            Filename,
        };

        private static Gtk.TargetEntry[] target_table = new Gtk.TargetEntry [] {
            //new Gtk.TargetEntry ("STRING", 0, (uint) TargetType.String ),
            //new Gtk.TargetEntry ("text/plain", 0, (uint) TargetType.String),
            new Gtk.TargetEntry("text/uri-list", 0, (uint)TargetType.Filename),
                //new Gtk.TargetEntry ("application/x-rootwindow-drop", 0, (uint) TargetType.RootWindow)
        };

        public MainWindow(): base(Gtk.WindowType.Toplevel) {
        }

        public MainWindow(string[] args, LanguageManager manager, bool Debug, Config config, 
                System.Threading.Thread signal_thread, int gui_thread_id) :
            base(Gtk.WindowType.Toplevel) {
                MainWindow.gui_thread_id = gui_thread_id;
                this.args = args;
                this.signal_thread = signal_thread;
                _mainWindow = this;
                this.Icon = new Gdk.Pixbuf(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "abstract-butterfly-icon.gif"));
                this.config = config;
                this.Debug = Debug;
                this.manager = manager;
                completion = null;
                path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
                if (path.StartsWith("\\")) {
                    path = path.Substring(1);
                }
                if ((bool)config.GetValue("config", "remember-window-position")) {
                    int x = (int)config.GetValue("config", "window-x");
                    int y = (int)config.GetValue("config", "window-y");
                    int w = (int)config.GetValue("config", "window-width");
                    int h = (int)config.GetValue("config", "window-height");
                    if (x > 0 && y > 0) {
                        Move(x, y);
                    }
                    if (w != -1 || h != -1) {
                        Resize(w, h);
                    }
                }
                // FIXME: URI to path
                // string path = Uri.UnescapeDataString(uri.Path);
                // Path.GetDirectoryName(path);
                Mono.TextEditor.Highlighting.SyntaxModeService.LoadStylesAndModes(
                        System.IO.Path.Combine(path, "SyntaxModes"));
                // Colors by name:
                // FIXME: load from defaults
                tagnames [Tag.Error] = "error";
                tags [Tag.Error] = new Gtk.TextTag("error");
                tags [Tag.Error].Foreground = "red";
                tags [Tag.Error].Weight = Pango.Weight.Bold;

                tagnames [Tag.Warning] = "warning";
                tags [Tag.Warning] = new Gtk.TextTag("warning");
                tags [Tag.Warning].Foreground = "orange";

                tagnames [Tag.Info] = "info";
                tags [Tag.Info] = new Gtk.TextTag("info");
                tags [Tag.Info].Foreground = "purple";

                tagnames [Tag.Normal] = "normal";
                tags [Tag.Normal] = new Gtk.TextTag("normal");
                tags [Tag.Normal].Foreground = "black";


                //this.TypeHint = Gdk.WindowTypeHint.Normal;
                // Build the GUI:
                Build();
                this.KeepAbove = true;

                PostBuild();
                foreach (Gtk.TextTag tag in tags.Values) {
                    Output.Buffer.TagTable.Add(tag);
                    //ChatOutput.Buffer.TagTable.Add(tag);
                    // TextView
                }
                Output.WrapMode = Gtk.WrapMode.Char; // FIXME: config
                //Output.ButtonPressEvent += HandleOutputButtonPressEvent;
                Output.PopulatePopup += HandlePopulatePopup;
                //Output.CopyClipboard += OutputCopiedText;
                // Setup clipboard, and Gui:
                clipboard = Gtk.Clipboard.Get(Gdk.Atom.Intern("CLIPBOARD", false));
                Title = String.Format("Calico - {0}", System.Environment.UserName);

                manager.SetCalico(this);
                configureIO();

                // Run this in in the GUI thread, after we start:
                if (!((IList<string>)args).Contains("--nomodules")) {
                    InvokeBlocking(delegate {
                        manager.PostSetup(this);
                    });
                }

                // Examples menu:
                Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
                if (examples_menu == null) {
                    throw new Exception("/menubar2/FileAction/ExamplesAction");
                }
                examples_menu.Submenu = new Gtk.Menu();
                // Menu items for Examples:
                foreach (string lang in manager.getLanguages()) {
                    Language language = manager [lang];
                    // Skip if not visible:
                    if (! ((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name)) {
                        continue;
                    }
                    string examples_path = language.getExamplesPath(path);
                    if (examples_path != null) {
                        DirectoryInfo dir = new DirectoryInfo(examples_path);
                        if (dir.Exists) {
                            process_example_dir(examples_menu, language.proper_name, dir, language);
                        }
                    }
                }
                examples_menu.Submenu.ShowAll();

                // New file menu:
                Gtk.MenuItem file_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/NewAction");
                if (file_menu == null) {
                    throw new Exception("/menubar2/FileAction/NewAction");
                }
                file_menu.Submenu = new Gtk.Menu();
                // Menu item for New file:
                foreach (string language_name in manager.getLanguages()) {
                    Language language = manager [language_name];
                    // Skip if not a visible language:
                    if (! ((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name)) {
                        continue;
                    }
                    Gtk.MenuItem menu = new Gtk.MenuItem(language.proper_name);
                    ((Gtk.Menu)file_menu.Submenu).Add(menu);
                    menu.Activated += delegate {
                        Open(null, language.name);
                    };
                }
                file_menu.Submenu.ShowAll();

                // Use Library menu:
                Gtk.MenuItem library_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/EditAction/UseALibraryAction");
                if (library_menu == null) {
                    throw new Exception("/menubar2/EditAction/UseALibraryAction");
                }
                library_menu.Submenu = new Gtk.Menu();
                // Scan for DLLs:
                DirectoryInfo ldir = new DirectoryInfo(System.IO.Path.Combine(path, System.IO.Path.Combine("..", "modules")));
                if (ldir.Exists) {
                    foreach (FileInfo f in ldir.GetFiles("*.dll")) {
                        Gtk.MenuItem fmenu = new Gtk.MenuItem(f.Name.Substring(0,
                                    f.Name.Length - 4).Replace("_", "__")
                                );
                        ((Gtk.Menu)library_menu.Submenu).Add(fmenu);
                        var fullname = f.FullName;
                        fmenu.Activated += delegate {
                            UseLibrary(fullname);
                        };
                    }
                }
                library_menu.Submenu.ShowAll();

                // Remove Module menu:
                Gtk.MenuItem removemodule_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/EditAction/RemoveModuleAction");
                if (removemodule_menu == null) {
                    throw new Exception("/menubar2/EditAction/RemoveModuleAction");
                }
                // Languages to menu items:
                Gtk.MenuItem switch_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ShellAction1/LanguageAction");
                if (switch_menu == null) {
                    throw new Exception("/menubar2/ShellAction1/LanguageAction");
                }
                switch_menu.Submenu = new Gtk.Menu();
                initialize_switch_menu(switch_menu);

                // Languages to config items:
                Gtk.MenuItem languages_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/CalicoAction/LanguagesAction");
                if (languages_menu == null) {
                    throw new Exception("/menubar2/CalicoAction/LanguagesAction");
                }
                languages_menu.Submenu = new Gtk.Menu();
                // Menu item for marking Visible languages:
                foreach (string lang in manager.getLanguages()) {
                    Language language = manager [lang];
                    Gtk.CheckMenuItem menu_item = new Gtk.CheckMenuItem(language.proper_name);
                    if (((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name)) {
                        menu_item.Active = true;
                    } else {
                        menu_item.Active = false;
                    }
                    menu_item.Activated += OnChangeActiveLanguages;
                    ((Gtk.Menu)languages_menu.Submenu).Add(menu_item);
                }
                languages_menu.Submenu.ShowAll();

                // Setup config Calico options:
                Gtk.MenuItem options_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/CalicoAction/OptionsAction");
                if (options_menu == null) {
                    throw new Exception("/menubar2/CalicoAction/OptionsAction");
                }
                options_menu.Submenu = new Gtk.Menu();
                bool value = (bool)config.GetValue("config", "remember-window-position");
                Gtk.CheckMenuItem options_menu_item = new Gtk.CheckMenuItem(_("Remember Window Position"));
                options_menu_item.Active = value;
                options_menu_item.Activated += delegate {
                    config.SetValue("config", "remember-window-position", options_menu_item.Active);
                };
                ((Gtk.Menu)options_menu.Submenu).Add(options_menu_item);
                options_menu.ShowAll();

                // Set optional items of TextArea
                ShellEditor.Options.ShowFoldMargin = false;
                ShellEditor.Options.ShowIconMargin = false;
                //ShellEditor.Options.ShowInvalidLines = false;
                ShellEditor.Options.ShowLineNumberMargin = false;
                // option
                ShellEditor.Options.TabsToSpaces = true;
                ShellEditor.Options.HighlightMatchingBracket = true;
                //ShellEditor.Document.DocumentUpdated += updateShellControls;
                ShellEditor.TextArea.Focused +=   delegate(object o, Gtk.FocusedArgs fargs) {
                    updateShellControls(ShellEditor.Document, null);
                };

                PrintLine(Tag.Info, String.Format(_("The Calico Project, Version {0}"), MainClass.Version));
                // Handle all flags, in case something crashes:
                bool debug_handler = true;
                foreach (string arg1 in args) {
                    if (arg1 == "--debug-handler") {
                        debug_handler = false;
                    }
                }
                if (debug_handler) {
                    GLib.ExceptionManager.UnhandledException += HandleException;
                }
                // Load files:
                bool openedFile = false;
                bool runFile = false;
                string lastFile = "";
                foreach (string arg2 in args) {
                    if (arg2.StartsWith("--")) {
                        if (arg2.StartsWith("--lang=")) {
                            string [] parts = arg2.Split('=');
                            CurrentLanguage = parts[1];
                        }
                        if (arg2 == "--run") {
                            runFile = true;
                        }
                    } else {
                        lastFile = arg2;
                        Open(System.IO.Path.GetFullPath(arg2));
                        openedFile = true;
                    }
                }
                SetLanguage(CurrentLanguage);
                if (!openedFile) {
                    Open(null, CurrentLanguage); // FIXME: open a file of DEFAULT type
                }
                // Hide things that shouldn't be seen yet:
                searchboxHide();
                vpaned1.Hide();
                EnvironmentPage.Child.Hide();
                LocalsPage.Child.Hide();
                HistoryPage.Child.Hide();
                property_notebook.Hide();
                // Update rest of GUI:
                ChatPrint(_("Chat commands:\n" +
                            "    MESSAGE\n" +
                            "    @USER MESSAGE\n" + "" +
                            "    /join CONF\n" +
                            "    /list\n" +
                            "    /create CONF\n" +
                            "    /help\n"
                           ), true);

                history = new History((List<string>)this.config.values ["shell"] ["history"]);
                ((Gtk.TextView)historyview).Buffer.InsertAtCursor(_("[Start of previous history]") + "\n");
                foreach (string text in (List<string>)this.config.values["shell"]["history"]) {
                    ((Gtk.TextView)historyview).Buffer.InsertAtCursor(text.Trim() + "\n");
                }
                ((Gtk.TextView)historyview).Buffer.InsertAtCursor(_("[Start of current history]") + "\n");
                UpdateUpDownArrows();
                UpdateZoom();
                addToRecentsMenu(null);
                MainNotebook.CurrentPage = 0;
                // Drop onto:
                Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
                this.DragDataReceived += new Gtk.DragDataReceivedHandler(HandleDragDataReceived);
                // Dragable:
                //Gtk.Drag.SourceSet (StartButton, Gdk.ModifierType.Button1Mask | Gdk.ModifierType.Button3Mask,
                //     target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
                //StartButton.DragDataGet += new Gtk.DragDataGetHandler (HandleSourceDragDataGet);
                // All done, show if minimized:
                // main & shell window
                //MainNotebook.GroupId = 0;
                //ToolNotebook.GroupId = 0;
                //EditorNotebook.GroupId = 0;
                for (int i = 0; i < MainNotebook.NPages; i++) {
                    MainNotebook.SetTabReorderable(MainNotebook.GetNthPage(i), true);
                    MainNotebook.SetTabDetachable(MainNotebook.GetNthPage(i), true);  
                }
                // output notebook
                for (int i = 0; i < ToolNotebook.NPages; i++) {
                    ToolNotebook.SetTabReorderable(ToolNotebook.GetNthPage(i), true);
                    ToolNotebook.SetTabDetachable(ToolNotebook.GetNthPage(i), true);  
                }
                // editor notebook
                for (int i = 0; i < EditorNotebook.NPages; i++) {
                    EditorNotebook.SetTabReorderable(ToolNotebook.GetNthPage(i), true);
                    EditorNotebook.SetTabDetachable(ToolNotebook.GetNthPage(i), true);  
                }
                this.Present();
                // End of GUI setup
                // Load images for animation:
                System.Reflection.Assembly thisExe;
                thisExe = System.Reflection.Assembly.GetExecutingAssembly();
                System.IO.Stream file;
                int c = 0;
                foreach (string filename in new string [] {"Calico.icon-1.png", "Calico.icon-2.png"}) {
                    file = thisExe.GetManifestResourceStream(filename);
                    animationImages [c++] = new Gtk.Image(file);
                }
                butterfly.Image = animationImages [0];
                this.KeepAbove = false;

                if ((bool)config.GetValue("config", "remember-window-position")) {
                    if (toolSwapped) {
                        SwapTool();
                    }	 
                    int sw = (int)config.GetValue("config", "shell-width");
                    int sh = (int)config.GetValue("config", "shell-height");
                    if (verticalSwapped) {
                        SwapVertical();
                        if (sh != -1) { // hpaned divider between Shell and Output
                            VPaned2.Position = sh;
                        }
                    } else {
                        if (sh != -1) { // hpaned divider between Shell and Output
                            VPaned2.Position = sh - 16;
                        }
                    }
                    if (horizontalSwapped) {
                        SwapHorizontal();
                    }
                    if (sw != -1) { // vpaned divider between Shell and Tools
                        hpaned2.Position = sw;
                    }
                }
                // Start up background updater
                GLib.Timeout.Add(500, UpdateGUI);
                if(runFile){
                    ExecuteFile(lastFile);
                }
            }

        void searchboxHide() {
            // hide all of the search components
            // row 1:
            searchMode = true;
            searchSeparator1.Hide();
            searchExpandButton.Hide();
            searchFindLabel.Hide();
            searchEntry.Hide();
            searchNextButton.Hide();
            searchPrevButton.Hide();
            searchStopButton.Hide();
            searchSeparator3.Hide();
            // row 2:
            searchSeparator2.Hide();
            searchReplaceLabel.Hide();
            searchReplaceEntry.Hide();
            searchReplaceButton.Hide();
            searchReplaceAllButton.Hide();
            searchSeparator4.Hide();
        }

        void searchboxShow() {
            // show all of the search components
            // show row 1:
            searchExpandButton.Image = new Gtk.Image("gtk-add", global::Gtk.IconSize.SmallToolbar);
            searchSeparator1.Show();
            searchExpandButton.Show();
            searchFindLabel.Show();
            searchEntry.Show();
            searchNextButton.Show();
            searchPrevButton.Show();
            searchStopButton.Show();
            searchSeparator3.Show();
            searchMode = true;
            // set the search/replace to search
            // hide row 2:
            searchSeparator2.Hide();
            searchReplaceLabel.Hide();
            searchReplaceEntry.Hide();
            searchReplaceButton.Hide();
            searchReplaceAllButton.Hide();
            searchSeparator4.Hide();
        }

        protected void OnSearchExpandButtonClicked (object sender, EventArgs e) {
            replaceboxToggle();
        }

        void replaceboxToggle() {
            if (searchMode) {
                // row 2: now in replace mode
                searchExpandButton.Image = new Gtk.Image("gtk-remove", global::Gtk.IconSize.SmallToolbar);
                searchSeparator2.Show();
                searchReplaceLabel.Show();
                searchReplaceEntry.Show();
                searchReplaceButton.Show();
                searchReplaceAllButton.Show();
                searchSeparator4.Show();
            } else {
                // row 2: now in search-only mode
                searchExpandButton.Image = new Gtk.Image("gtk-add", global::Gtk.IconSize.SmallToolbar);
                searchSeparator2.Hide();
                searchReplaceLabel.Hide();
                searchReplaceEntry.Hide();
                searchReplaceButton.Hide();
                searchReplaceAllButton.Hide();
                searchSeparator4.Hide();
            }
            searchMode = ! searchMode;
        }

        public string relativePath(string path_file) {
            return System.IO.Path.Combine(path, path_file);
        }

        public string relativePath() {
            return path;
        }

        public virtual CustomStream GetStdOut() {
            return new CustomStream(this, Tag.Normal);
        }

        public virtual CustomStream GetStdErr() {
            return new CustomStream(this, Tag.Error);
        }

        public void configureIO() {
            if (! Debug) {
                cstdout = GetStdOut();
                cstderr = GetStdErr();
                manager.SetRedirects(cstdout, cstderr);

                StreamWriter swout = new StreamWriter(cstdout);
                swout.AutoFlush = true;
                Console.SetOut(swout);

                StreamWriter swerr = new StreamWriter(cstderr);
                swerr.AutoFlush = true;
                Console.SetError(swerr);
            }
        }

        void AddGotoFileToMenu(String text, Gtk.PopulatePopupArgs args) {
            // Look for appropriate text:
            System.Text.RegularExpressions.Match match = System.Text.RegularExpressions.Regex.Match(
                    text, "^.*File \"(.*)\", line ([0-9]*).*$");
            string filename;
            int lineno;
            if (match.Success) {
                // Groups[0] is entire string
                try {
                    filename = match.Groups [1].Captures [0].Value;
                    if (filename == "<string>" || filename == "stdin" || filename == "") {
                        return;
                    } 
                    lineno = Convert.ToInt32(match.Groups [2].Captures [0].Value);
                } catch {
                    return;
                }
            } else {
                // Nothing to do
                return;
            }

            // If needed, add options to menu:
            string [] parts = filename.Split(System.IO.Path.DirectorySeparatorChar);
            Gtk.MenuItem menuitem = new Gtk.MenuItem(
                    String.Format(_("Go to \"{0}\" line {1}"), parts [parts.Length - 1], lineno));
            menuitem.Activated += delegate(object sender, EventArgs e) {
                Open(String.Format("{0}:{1}", filename, lineno));
            };
            menuitem.Show();
            // Add item to menu
            args.Menu.Insert(menuitem, 0);
        }

        void AddGotoHelpToMenu(String text, Gtk.PopulatePopupArgs args) {
            // Look for appropriate text:
            string [] parts = text.Split();
            if (parts.Length > 0) {
                parts[0] = parts[0].Replace(":", "");
                if (parts[0].Contains("Error") || 
                        parts[0].Contains("Exception") ||
                        parts[0].Contains("Warning") ||
                        parts[0].Contains("StopIteration") ||
                        parts[0].Contains("Interrupt")
                   ) {
                    // add to menu
                    Gtk.MenuItem menuitem = new Gtk.MenuItem(
                            String.Format(_("Get Help on \"{0}\""), parts [0]));
                    menuitem.Activated += delegate(object sender, EventArgs e) {
                        System.Diagnostics.Process.Start(String.Format("http://calicoproject.org/Calico:_{0}", parts[0]));
                    };
                    menuitem.Show();
                    // Add item to menu
                    args.Menu.Insert(menuitem, 0);
                }
            }
            return;
        }

        void HandlePopulatePopup(object o, Gtk.PopulatePopupArgs args) {
            // First, see what kind of line this is:
            int position = Output.Buffer.CursorPosition;
            Gtk.TextIter currentiter = Output.Buffer.GetIterAtOffset(position);
            int char_offset = currentiter.CharsInLine;
            int line = currentiter.Line;
            if (char_offset > 1) {
                Gtk.TextIter enditer = Output.Buffer.GetIterAtLineOffset(line, char_offset - 1);
                Gtk.TextIter textiter = Output.Buffer.GetIterAtLine(line);
                String text = textiter.GetVisibleText(enditer);
                AddGotoFileToMenu(text, args);
                AddGotoHelpToMenu(text, args);
            }
        }

        public void initialize_switch_menu(Gtk.MenuItem switch_menu) {
            int count = 0;
            language_group = null;
            Gtk.RadioMenuItem radioitem;
            // Menu item for "Switch Shell to ..."
            foreach (string language_name in manager.getLanguages()) {
                Language language = manager [language_name];
                if (! language.IsTextLanguage) {
                    // Skip non-text languages
                    continue;
                }
                // Skip non-visible languages:
                if (! ((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name)) {
                    continue;
                }
                if (language.name == "python") {
                    // FIXME: get from defaults, preferred lang
                    CurrentLanguage = language.name;
                    if (manager.languages.ContainsKey(language.name) && manager.languages [language.name].IsTextLanguage) {
                        ShellLanguage = language.name;
                    }
                } else if (CurrentLanguage == null) {
                    CurrentLanguage = language.name;
                    if (manager.languages.ContainsKey(language.name) && manager.languages [language.name].IsTextLanguage) {
                        ShellLanguage = language.name;
                    }
                }
                // FIXME: select default language initially
                // unique name, label, mnemonic, accel, tooltip, user data
                string name = String.Format(_("Switch to {0}"), language.proper_name);
                if (count == 0) {
                    radioitem = new Gtk.RadioMenuItem(name);
                    language_group = radioitem;
                } else {
                    radioitem = new Gtk.RadioMenuItem(language_group, name);
                }
                radioitem.Activated += OnSwitchLanguage;
                ((Gtk.Menu)switch_menu.Submenu).Add(radioitem);
                uint key;
                Gdk.ModifierType mod;
                Gtk.Accelerator.Parse(String.Format("{0}{1}", GetControlString(), count + 1), out key, out mod);
                radioitem.AddAccelerator("activate", UIManager.AccelGroup, new Gtk.AccelKey((Gdk.Key)key, mod, Gtk.AccelFlags.Visible));
                languages_by_count [count + 1] = language;
                //DragDataReceived += SelectionReceived;
                radioitem.Data ["id"] = count + 1;
                count++;
                if (language.name == "python") {
                    // FIXME: get lang preference from config
                    radioitem.Active = true;
                }
            }
            switch_menu.Submenu.ShowAll();
        }

        public string getExtension(string filename) {
            string ext = System.IO.Path.GetExtension(filename);
            if (ext.Length > 0) {
                ext = ext.Substring(1); // after the dot
            }
            return ext;
        }

        public void process_example_dir(Gtk.MenuItem root_menu, string menu_name, DirectoryInfo dir, Language language) {
            Gtk.MenuItem menu = new Gtk.MenuItem(menu_name);
            ((Gtk.Menu)root_menu.Submenu).Add(menu);
            menu.Submenu = new Gtk.Menu();
            foreach (FileInfo f in dir.GetFiles("*.*")) {
                if (!f.Name.EndsWith("~") && Contains(getExtension(f.Name), language.extensions) && !f.Name.StartsWith("_")) {
                    Gtk.MenuItem fmenu = new Gtk.MenuItem(f.Name.Replace("_", "__"));
                    ((Gtk.Menu)menu.Submenu).Add(fmenu);
                    var fullname = f.FullName;
                    // If on home page, it is button pressed:
                    fmenu.ButtonPressEvent += delegate {
                        Open(fullname);
                    };
                    // if in the menu, it is activated:
                    fmenu.Activated += delegate {
                        Open(fullname);
                    };
                }
            }
            menu.Submenu.ShowAll();
            foreach (DirectoryInfo d in dir.GetDirectories("*.*")) {
                if (!d.Name.StartsWith(".")) {
                    process_example_dir(menu, d.Name, d, language);
                }
            }
        }

        private bool UpdateGUI() {
            Invoke(delegate {
                if (isRunning) { // Program is running
                    animationSequence = ++animationSequence % animationImages.Length;
                    butterfly.Image = animationImages [animationSequence];
                } else if (animationSequence != 0) {
                    animationSequence = 0;
                    butterfly.Image = animationImages [0];
                } // else don't change image
            }
            );
            // update any pending requests
            while (Gtk.Application.EventsPending ()) {
                Gtk.Application.RunIteration();
            }
            // keep updating:
            return true;
        }

        private void HandleDragDataReceived(object sender, Gtk.DragDataReceivedArgs args) {
            if (args.SelectionData.Length >= 0 && args.SelectionData.Format == 8) {
                string filenames = Encoding.UTF8.GetString(args.SelectionData.Data, 0, args.SelectionData.Length);
                foreach (string filename in filenames.Split('\n')) {
                    string sfilename = filename.Replace("\r", "").Trim();
                    //Console.WriteLine ("Received '{0}'", sfilename);
                    Uri uri;
                    try {
                        uri = new Uri(sfilename);
                    } catch {
                        continue;
                    }
                    //Console.WriteLine ("Uri! '{0}'", uri);
                    sfilename = System.IO.Path.GetFullPath(uri.AbsolutePath);
                    sfilename = sfilename.Replace("%20", " ");
                    // FIXME: URI to path
                    // string path = Uri.UnescapeDataString(uri.Path);
                    // Path.GetDirectoryName(path);
                    //Console.WriteLine ("Filename! '{0}'", sfilename);
                    if (System.IO.File.Exists(sfilename)) {
                        Open(sfilename);
                    }
                }
                Gtk.Drag.Finish(args.Context, true, false, args.Time);
            }
            Gtk.Drag.Finish(args.Context, false, false, args.Time);
        }

        /*
           private static void HandleSourceDragDataGet (object sender, Gtk.DragDataGetArgs args)
           {
           if (args.Info == (uint) TargetType.RootWindow)
           Console.WriteLine ("I was dropped on the rootwin");
           else
           args.SelectionData.Text = "I'm data!";
           }
           */

        // ------------------------------------------------------------
        public Gtk.HPaned NotebookPane {
            get { return hpaned3;}
        }

        public Gtk.VPaned VPaned2 {
            get { return vpaned2; }
        }

        public Document CurrentDocument {
            get {
                return getFocusDocument();
            }
        }

        public Gtk.ToggleAction ChatTab {
            get { return ChatTabAction; }
        }

        public Gtk.UIManager GUIManager {
            get { return UIManager; }
        }

        public Gtk.HScale ProgramSpeed {
            get { return debugSpeed;}
        }

        public Gtk.Notebook.NotebookChild EnvironmentPage {
            get { return (Gtk.Notebook.NotebookChild)(this.notebook_tools [this.GtkScrolledWindow1]);}
        }

        public Gtk.Notebook.NotebookChild LocalsPage {
            get { return (Gtk.Notebook.NotebookChild)(this.notebook_tools [this.scrolledwindow2]);}
        }

        public Gtk.Notebook.NotebookChild HistoryPage {
            get { return (Gtk.Notebook.NotebookChild)(this.notebook_tools [this.scrolledwindow4]);}
        }

        public Gtk.Notebook MainNotebook {
            get { return notebook_docs; }
        }

        public Gtk.Notebook EditorNotebook {
            get { return editor_docs; }
        }

        public Gtk.Notebook ToolNotebook {
            get { return notebook_tools; }
        }

        public string OS {
            get {
                string retval = System.Environment.OSVersion.Platform.ToString();
                if (retval.Contains("Win")) {
                    retval = "Windows";
                } else if (System.IO.Directory.Exists("/Applications")) {
                    return "Mac";
                } 
                return retval; // Probably Unix
            }
        }

        public Gtk.TreeView EnvironmentTreeView {
            get { return treeview1; }
        }

        public Gtk.TreeView LocalTreeView {
            get { return treeview2; }
        }

        public bool RegisterLanguage(string language_name) {
            // RegisterLanguage("python")
            Language language = GetLanguageFromName(language_name);
            if (language != null) {
                IList<string > visible_languages = (IList<string>)config.GetValue("config", "visible-languages");
                if (!visible_languages.Contains(language.name)) {
                    // Add language on the fly!
                    manager.Register(language, true); // This may fail, which won't add language
                    if (language.engine != null) {
                        language.engine.Setup(path);
                        language.engine.Start(path);
                        language.engine.PostSetup(this);
                        visible_languages.Add(language.name); 
                        return true;
                    }	
                } else {
                    return true; // already visible
                }
            }
            return false;
        }

        public string GetCurrentProperLanguage() {
            return manager[CurrentLanguage].proper_name;
        }

        void OnChangeActiveLanguages(object sender, EventArgs e) {
            Gtk.MenuItem languages_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/CalicoAction/LanguagesAction");
            if (languages_menu == null) {
                throw new Exception("/menubar2/CalicoAction/LanguagesAction");
            }
            IList<string > visible_languages = (IList<string>)config.GetValue("config", "visible-languages");
            // if it is newly active language
            foreach (Gtk.CheckMenuItem menu_item in ((Gtk.Menu)languages_menu.Submenu).AllChildren) {
                Language language = GetLanguageFromProperName(((Gtk.Label)menu_item.Child).LabelProp);
                // active now, but wasn't
                if (menu_item.Active && !visible_languages.Contains(language.name)) {
                    // Add language on the fly!
                    manager.Register(language, true); // This may fail, which won't add language
                    if (language.engine != null) {
                        language.engine.Setup(path);
                        language.engine.Start(path);
                        language.engine.PostSetup(this);
                    }
                    // Add to examples menu
                    Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
                    string examples_path = language.getExamplesPath(path);
                    if (examples_path != null) {
                        DirectoryInfo dir = new DirectoryInfo(examples_path);
                        if (dir.Exists) {
                            process_example_dir(examples_menu, language.proper_name, dir, language);
                        }
                    }
                    examples_menu.ShowAll();
                    // Add to Shell languages:
                    Gtk.MenuItem switch_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ShellAction1/LanguageAction");
                    Gtk.RadioMenuItem language_group = (Gtk.RadioMenuItem)(((Gtk.Menu)switch_menu.Submenu).Children [0]);
                    if (language.IsTextLanguage) { // Skip non-text languages
                        string name = String.Format("Switch to {0}", language.proper_name);
                        var radioitem = new Gtk.RadioMenuItem(language_group, name);
                        radioitem.Activated += OnSwitchLanguage;
                        ((Gtk.Menu)switch_menu.Submenu).Add(radioitem);
                        int count = languages_by_count.Count;
                        uint key;
                        Gdk.ModifierType mod;
                        Gtk.Accelerator.Parse(String.Format("{0}{1}", GetControlString(), count + 1), out key, out mod);
                        radioitem.AddAccelerator("activate", UIManager.AccelGroup, new Gtk.AccelKey((Gdk.Key)key, mod, Gtk.AccelFlags.Visible));
                        languages_by_count [count + 1] = language;
                        radioitem.Data ["id"] = count + 1;
                        switch_menu.Submenu.ShowAll();
                    }
                    // Add it to New menu:
                    Gtk.MenuItem file_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/NewAction");
                    Gtk.MenuItem menu = new Gtk.MenuItem(language.proper_name);
                    ((Gtk.Menu)file_menu.Submenu).Add(menu);
                    menu.Activated += delegate {
                        Open(null, language.name);
                    };
                    file_menu.Submenu.ShowAll();
                } else if (!menu_item.Active && visible_languages.Contains(language.name)) {
                    // was active, but now is not
                    // Remove from examples menu
                    Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
                    foreach (Gtk.MenuItem menu in ((Gtk.Menu)examples_menu.Submenu).AllChildren) {
                        // if correct one, remove it:
                        if (((Gtk.AccelLabel)menu.Child).Text == language.proper_name) {
                            ((Gtk.Menu)examples_menu.Submenu).Remove(menu);
                        }
                    }
                    // Remove from New menu:
                    Gtk.MenuItem file_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/NewAction");
                    foreach (Gtk.MenuItem menu in ((Gtk.Menu)file_menu.Submenu).AllChildren) {
                        // if correct one, remove it:
                        if (((Gtk.AccelLabel)menu.Child).Text == language.proper_name) {
                            ((Gtk.Menu)file_menu.Submenu).Remove(menu);
                        }
                    }
                }
            }
            visible_languages.Clear();
            foreach (Gtk.CheckMenuItem menu_item in ((Gtk.Menu)languages_menu.Submenu).AllChildren) {
                Language language = GetLanguageFromProperName(((Gtk.Label)menu_item.Child).LabelProp);
                if (menu_item.Active || language.name.Equals("python")) {
                    visible_languages.Add(language.name); 
                }
            }
            // Remove from shell languages: (delay till after we set active)
            Gtk.MenuItem switch_menu2 = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ShellAction1/LanguageAction");
            // FIXME: some old accels still active
            switch_menu2.Submenu = new Gtk.Menu(); // wipe out old one
            initialize_switch_menu(switch_menu2);
        }

        protected override bool OnScrollEvent(Gdk.EventScroll evnt) {
            // After the other items have handled their scrolls
            if (Focus == ShellEditor.TextArea) {
                if (evnt.Direction == Gdk.ScrollDirection.Up) {
                    KeyUp(true);
                } else if (evnt.Direction == Gdk.ScrollDirection.Down) {
                    KeyDown(true);
                }
                return true;
            } else {
                return base.OnScrollEvent(evnt);
            }
        }

        public Gtk.ScrolledWindow ScrolledWindow {
            get { return scrolledwindow1; }
            set { scrolledwindow1 = value; }
        }

        public Mono.TextEditor.TextEditor ShellEditor {
            get { return _shell; }
        }

        public Gtk.Widget Home {
            get { return hbox2; }
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

        public Gtk.Button PlayButton {
            get { return _playButton; }
        }

        public Gtk.Button PauseButton {
            get { return _pauseButton1; }
        }

        public Gtk.Notebook PropertyNotebook {
            get { return property_notebook; }
        }

        public string CurrentProperLanguage {
            get {
                if (CurrentLanguage != null && manager.languages.ContainsKey(CurrentLanguage)) {
                    return manager.languages [CurrentLanguage].proper_name;
                }
                return null;
            }
            set {
                foreach (string lang in manager.getLanguages()) {
                    string lang_proper = manager.languages [lang].proper_name;
                    if (lang_proper == value) {
                        CurrentLanguage = lang;
                        break;
                    }
                }
            }
        }

        public string FindLanguage(string name) {
            name = name.Trim();
            foreach (string lang in manager.getLanguages()) {
                if ((name.ToLower() == manager.languages [lang].name.ToLower()) ||
                        (name.ToLower() == manager.languages [lang].proper_name.ToLower())) {
                    return lang;
                }
            }
            return null;
        }

        public Language GetLanguageFromName(string name) {
            foreach (string lang in manager.getLanguages()) {
                if (name == manager.languages [lang].name) {
                    return manager.languages [lang];
                }
            }
            return null;
        }

        public Language GetLanguageFromProperName(string proper_name) {
            foreach (string lang in manager.getLanguages()) {
                if (proper_name == manager.languages [lang].proper_name) {
                    return manager.languages [lang];
                }
            }
            return null;
        }

        public Document GetDocument(string name) {
            Document retval = null;
            ManualResetEvent ev = new ManualResetEvent(false);
            Invoke(delegate {
                for (int i = 0; i < MainNotebook.NPages; i++) {
                    Gtk.Widget widget = MainNotebook.GetNthPage(i);
                    if (documents.ContainsKey(widget) && 
                        documents [widget].widget.IsRealized &&
                        documents [widget].filename != null && 
                        documents [widget].filename.EndsWith(name)) {
                        retval = documents [widget];
                        break;
                    }
                }
                for (int i = 0; i < ToolNotebook.NPages; i++) {
                    Gtk.Widget widget = ToolNotebook.GetNthPage(i);
                    if (documents.ContainsKey(widget) && 
                        documents [widget].widget.IsRealized &&
                        documents [widget].filename != null && 
                        documents [widget].filename.EndsWith(name)) {
                        retval = documents [widget];
                        break;
                    }
                }
                for (int i = 0; i < EditorNotebook.NPages; i++) {
                    Gtk.Widget widget = EditorNotebook.GetNthPage(i);
                    if (documents.ContainsKey(widget) && 
                            documents [widget].widget.IsRealized &&
                            documents [widget].filename != null && 
                            documents [widget].filename.EndsWith(name)) {
                        retval = documents [widget];
                        break;
                    }
                }
                ev.Set();
            });
            ev.WaitOne();
            return retval;
        }

        protected void OnButtonExamplesClicked(object sender, System.EventArgs e) {
            // Examples
            Gtk.MenuItem recents_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
            ((Gtk.Menu)recents_menu.Submenu).Popup();
        }

        protected void OnButtonRecentlyUsedClicked(object sender, System.EventArgs e) {
            // Recently opened
            Gtk.MenuItem recents_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/RecentlyOpenedAction");
            ((Gtk.Menu)recents_menu.Submenu).Popup();
        }

        protected virtual void OnButton4Clicked(object sender, System.EventArgs e) {
            // System.dll:
            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Getting_Started");
        }

        public static bool Contains(string ext, string[] extensions) {
            foreach (string lext in extensions) {
                if (lext == ext) {
                    return true;
                }
            }
            return false;
        }

        public void UseLibrary(string fullname) {
            if (Focus == ShellEditor.TextArea) {
                string text = manager [CurrentLanguage].GetUseLibraryString(fullname);
                if (text != "") {
                    ShellEditor.Insert(0, text);
                }
            } else if (CurrentDocument != null) {
                CurrentDocument.UseLibrary(fullname);
            }
        }

        public void HandleException(GLib.UnhandledExceptionArgs args) {
            PrintLine(Tag.Error, String.Format(_("Exception: {0}"), args.ExceptionObject.ToString()));
            Invoke(OnStopRunning);
        }

        public void PostBuild() {
            // Had to add this here, as Stetic didn't like it
            _shell = new MyTextEditor();
            _shell.KeyReleaseEvent += ShellKeyReleaseEvent;
            _shell.DragDataReceived += (o, args) => {
                Invoke(delegate {
                    StartButton.Sensitive = true;
                    StartAction.Sensitive = true;
                });
            };
            ScrolledWindow.Add(_shell);
            ScrolledWindow.FocusChildSet += delegate(object o, Gtk.FocusChildSetArgs args) {
                updateShellControls(ShellEditor.Document, null);
            };
            ScrolledWindow.ShowAll();
            // Environment table:
            EnvironmentTreeView.AppendColumn(_("Variable"), new Gtk.CellRendererText(), "text", 0);
            EnvironmentTreeView.AppendColumn(_("Value"), new Gtk.CellRendererText(), "text", 1);
            // Create a ListStore as the Model
            EnvironmentList = new Gtk.ListStore(typeof(string), typeof(string));
            EnvironmentTreeView.Model = EnvironmentList;
            EnvironmentTreeView.ShowAll();
            // Local environment:
            LocalTreeView.AppendColumn(("Variable"), new Gtk.CellRendererText(), "text", 0);
            LocalTreeView.AppendColumn(_("Value"), new Gtk.CellRendererText(), "text", 1);
            LocalList = new Gtk.ListStore(typeof(string), typeof(string));
            LocalTreeView.Model = LocalList;
            LocalTreeView.ShowAll();
        }

        public bool ProgramRunning {
            get { return ((executeThread != null) || isRunning); }
        }

        [GLib.ConnectBeforeAttribute]
        public void ShellKeyReleaseEvent(object obj, System.EventArgs args) {
            // This code was interacting with OnKeyPress
            if (! ProgramRunning) {
                Invoke(delegate {
                    //if (_shell.Document.Text == "") {
                    //StartButton.Sensitive = false;
                    //StartAction.Sensitive = false;
                    //} else {
                    StartButton.Sensitive = true;
                    StartAction.Sensitive = true;
                    //}
                }
                );
            }
        }

        public static bool needInvoke() {
            return ((Thread.CurrentThread.ManagedThreadId != gui_thread_id) &&
                    (gui_thread_id != -1));
        }

        public delegate void InvokeDelegate();

        public static void Invoke(InvokeDelegate invoke) {
            if (needInvoke()) {
                Gtk.Application.Invoke(delegate {
                    invoke();
                }
                );
            } else {
                invoke();
            }
        }

        public static void InvokeBlocking (InvokeDelegate invoke)
        {
            System.Exception exception = null;
            ManualResetEvent ev = new ManualResetEvent(false);
            if (needInvoke ()) {
                Gtk.Application.Invoke (delegate {
                    try {
                        invoke ();
                    } catch (Exception e) {
                        exception = e;
                    }
                    ev.Set();
                });
                ev.WaitOne();
            } else {
                try {
                    invoke ();
                } catch (Exception e) {
                    exception = e;
                }
            }
            if (exception != null)
                throw exception;
        }

        public static string _(string message) {
            return global::Mono.Unix.Catalog.GetString(message);
        }

        public void SetCurrentLanguage(string possible) {
            // called after changing shell through history
            if (possible != "") {
                SetLanguage(possible);
            }
        }

        public void StartLanguage(string language) {
            if (manager.languages.ContainsKey(language) && manager[language].engine == null) {
                manager.Register(manager[language], true);
                manager[language].engine.Setup(path);
                manager[language].engine.Start(path);
                manager[language].engine.PostSetup(this);
                manager[language].engine.SetRedirects(cstdout, cstderr);
            }
        }

    public void TrySetCurrentLanguage(string language) {
        string lang = FindLanguage(language);
        if (lang != null) {
            ActivateLanguage(lang, CurrentLanguage);
	    } else {
            throw new Exception("unknown language: " + language);
	    }
	}

        public void ActivateLanguage(string language, string backup) {
            StartLanguage(language);
            if (manager[CurrentLanguage].engine == null) {
                CurrentLanguage = backup;
            } else {
                CurrentLanguage = language;
            }
        }

        public void SetLanguage(string language) {
            if (language == null)
                return;
            CurrentLanguage = language;
            if (manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                ShellLanguage = language;
            }
            if (CurrentLanguage != null) {
                Gtk.MenuItem options_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ScriptAction/ScriptOptionsAction");
                manager [language].SetOptionsMenu(options_menu);
                if (ShellEditor.Document.MimeType != String.Format("text/x-{0}", CurrentLanguage)) {
                    try {
                        ShellEditor.Document.MimeType = String.Format("text/x-{0}", CurrentLanguage);
                        // Force an update; works nicely as it keeps cursor in same place:
                        // FIXME: Interaction with ShellOnKeyPress?
                        ShellEditor.Document.Text = ShellEditor.Document.Text;
                        // FAIL:
                        //Shell.Document.CommitUpdateAll();
                    } catch {
                        // pass
                    }
                }
            }
            Invoke(delegate {
                prompt.Text = String.Format("{0}>", CurrentLanguage);
                status_langauge.Text = String.Format("<i>{0}</i> ", CurrentProperLanguage);
                status_langauge.UseMarkup = true;
            }
            );
        }

        public void SetStatus(string status) {
            if (GetEnvironment() == "tty") {
                Console.WriteLine("    Calico status: {0}", status);
            } else {
                Invoke(delegate {
                    this.label11.LabelProp = "<i>" + status + "</i>";
                    this.label11.UseMarkup = true;
                });
            }
        }

        public void OnSwitchLanguage(object obj, EventArgs e) {
            Gtk.RadioMenuItem radioitem = (Gtk.RadioMenuItem)obj;
            OnSwitchLanguage((int)radioitem.Data ["id"]);
        }

        public void OnSwitchLanguage(int lang_count) {
            if (languages_by_count.ContainsKey(lang_count)) {
                Language language = languages_by_count [lang_count];
                SetLanguage(language.name);
                if (manager.languages.ContainsKey(language.name) && manager.languages [language.name].IsTextLanguage) {
                    ShellLanguage = language.name;
                }
                switchToShell();
                Title = String.Format("{0} - Calico Shell - {1}", CurrentProperLanguage, System.Environment.UserName);
            }
        }

        public bool Create(string filename) {
            // Open a file, or Create it if it doesn't exist
            string language = manager.GetLanguageFromExtension(filename);
            return Create(filename, language);
        }

        public bool Create(string filename, string language) {
            // Open a file, or Create it if it doesn't exist
            filename = System.IO.Path.GetFullPath(filename);
            if (! System.IO.File.Exists(filename)) {
                // Create it, if it doesn't exist:
                System.IO.StreamWriter sw = new System.IO.StreamWriter(filename);
                sw.Close();
            }
            return Open(filename, language);
        }

        public bool Open() {
            return Open(null, null);
        }

        public bool Open(string filename) {
            Invoke(delegate {
                Open(filename, null);
            }
            );
            return true;
        }

	public void Dock(Gtk.ScrolledWindow window, string notebookName = "editor"){
	  Invoke(delegate {
	      //Gtk.Widget window = new Gtk.ScrolledWindow();

	      Gtk.HBox tab_widget = new Gtk.HBox();
	      Gtk.Label tab_label = new Gtk.Label();
	      ((Gtk.HBox)tab_widget).Add(tab_label);
	      
	      Gtk.Button close_button = new Gtk.Button();
	      Gtk.Image img = new Gtk.Image();
	      close_button.Relief = Gtk.ReliefStyle.None;
	      img = new Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu);
	      close_button.Add(img);
	      ((Gtk.HBox)tab_widget).Add(close_button);
	      tab_label.TooltipText = "It would be a miracle if I saw this text!";
	      tab_widget.ShowAll();
	      tab_label.Text = "hi!";
	      window.ShowAll();

	      Gtk.Notebook notebook = selectNotebook(notebookName);
	      int page_num = notebook.AppendPage(window, tab_widget);
    
	      notebook.SetTabReorderable(window, true);
	      notebook.SetTabDetachable(window, true);                
	      notebook.CurrentPage = page_num;
	    }
	    );
	  //close_button.Clicked += delegate {
	  //  TryToClose(page);
	  //};
	}

        public bool Open(string filename, string language) {
            // First, check for filename:N format:
            int lineno = 0;
            if (filename != null) {
                // Get full path to file if not given:
                filename = System.IO.Path.GetFullPath(filename);
                // Needs Systm.dll:
                System.Text.RegularExpressions.Match match = System.Text.RegularExpressions.Regex.Match(filename, "(.*)\\:(\\d+)$");
                if (match.Success) {
                    // Groups[0] is entire string
                    filename = (string)match.Groups [1].Captures [0].Value;
                    lineno = Convert.ToInt32(match.Groups [2].Captures [0].Value);
                }
                // -----
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
                foreach (KeyValuePair<Gtk.Widget, Document> d in documents) {
                    Document npage_document = d.Value;
                    if (npage_document.filename == filename) {                                       
                        Gtk.Notebook nb = searchForNotebook(d.Key);
                        int i = findTabByWidget(d.Key);
                        nb.CurrentPage = i;
                        add_it = false;
                        page = d.Value;
                    }
                }
                if (page == null) {
                    if (language == null) {
                        language = manager.GetLanguageFromExtension(filename);
                    }
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
                Gtk.Notebook notebook = selectNotebook(page.preferredNotebook);
                if (!notebook.Visible) // if it isn't visible, put in "main"
                    // EditorNotebook is the only one that can be hidden
                    notebook = selectNotebook("main");
                if (page.preferredNotebook == "main")
                    moveEditorNotebookToMain();
                int page_num = notebook.AppendPage(page.widget, page.tab_widget);
                documents [page.widget] = page;
                page.widget.FocusChildSet += delegate(object o, Gtk.FocusChildSetArgs args) {
                    Gtk.Widget nbp = searchForPage(args.Widget);
                    if (nbp != null && documents.ContainsKey(nbp))
                        updateControls(documents[nbp]);
                };
                notebook.SetTabReorderable(page.widget, true);
                notebook.SetTabDetachable(page.widget, true);                
                notebook.CurrentPage = page_num;
                page.close_button.Clicked += delegate {
                    TryToClose(page);
                };
                if (page.focus_widget != null)
                    GLib.Timeout.Add(0, delegate {
                        page.focus_widget.GrabFocus();
                        updateControls(page);
                        return false; 
                    });
                else
                    System.Console.Error.WriteLine("Document needs to set focus_widget.");
            }
            if (language != null && manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                ShellLanguage = language;
            }
            if (page != null && lineno != 0) {
                return page.GotoLine(lineno);
            }
            return false;
        }

        Gtk.Notebook selectNotebook(string preferredNotebook) {
            if (preferredNotebook == "tools") {
                return ToolNotebook;
            } else if (preferredNotebook == "editor") {
                return EditorNotebook;
            } else if (preferredNotebook == "main") {
                return MainNotebook;
            } else {
                throw new Exception(String.Format("No such notebook type: '{0}", preferredNotebook));
            }
        }

        public void TryToClose(Document document) {
            ManualResetEvent ev = new ManualResetEvent(false);
            Invoke (delegate {
                if ((!document.IsDirty) || (Close(document) != _("Cancel"))) {
                    bool result = document.Close(); // any cleanup?
                    if (result) {
                        Gtk.Notebook notebook = searchForNotebook(document.widget);
                        int page_num = notebook.PageNum(document.widget);
                        documents.Remove(document.widget);
                        notebook.RemovePage(page_num);
                    }
                }
                ev.Set();
            });
            ev.WaitOne();
        }


        public Document MakeDocument(string filename, string language) {
            Document document;
            if (language == null) {
                // Could not get language from filename, and not given
                string mime_type = GetMimeType(filename);
                document = manager.languages ["python"].MakeDocument(this, filename, mime_type);
            } else {
                document = manager.languages [language].MakeDocument(this, filename);
            }
            document.Configure(this.config);
            addToRecentsMenu(filename);
            return document;
        }

        public void addToRecentsMenu(string filename) {
            List<string > filenames = (List<string>)config.GetValue("config", "recent-files");
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
            Gtk.MenuItem recents_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/RecentlyOpenedAction");
            if (recents_menu == null) {
                throw new Exception("/menubar2/FileAction/RecentlyOpenedAction");
            }
            recents_menu.Submenu = new Gtk.Menu();
            foreach (string file in filenames) {
                Gtk.MenuItem fmenu = new Gtk.MenuItem(file.Replace("_", "__"));
                ((Gtk.Menu)recents_menu.Submenu).Add(fmenu);
                string fullname = file; // because C# doesn't make closures on values
                fmenu.Activated += delegate {
                    Open(fullname);
                };
            }
            recents_menu.Submenu.ShowAll();
        }

        public void Cleanup() {
            // Any additions before saving:
            int max = (int)config.GetValue("shell", "history-size");
            if (max != -1) { // no limit
                List<string > history = (List<string>)config.GetValue("shell", "history");
                if (history.Count > max) {
                    history.RemoveRange(0, history.Count - max);
                }
            }
            try {
                int x, y, w, h;
                GetPosition(out x, out y);
                GetSize(out w, out h);
                config.SetValue("config", "window-x", x);
                config.SetValue("config", "window-y", y);
                config.SetValue("config", "window-width", w);
                config.SetValue("config", "window-height", h);
                config.SetValue("config", "shell-width", hpaned2.Position);
                config.SetValue("config", "shell-height", VPaned2.Position);
                config.Save();
            } catch {
                // Something is no longer valid. Let's just make a new one then
                config.Initialize();
                try {
                    config.Save();
                } catch {
                    // Just ignore
                }
            }
        }

        public string Close(Document document) {
            while (true) {
                string answer = SaveAbandonCancel(document.basename);
                if (answer == _("Cancel")) {
                    return _("Cancel");
                } else if (answer == _("Abandon changes")) {
                    return _("Abandon changes");
                } else { // Save
                    bool result = false;
                    ManualResetEvent ev = new ManualResetEvent(false);
                    Invoke(delegate {
                        result = document.Save(); // save doc
                        ev.Set();
                    });
                    ev.WaitOne();
                    if (result) {
                        addToRecentsMenu(document.filename);
                        return _("Save");
                    }
                }
            }
        }

        public bool FileSavedAs(Document doc, string filename) {
            bool retval = false;
            ManualResetEvent ev = new ManualResetEvent(false);
            Invoke(delegate {
                // Some other doucment was saved as this document's name
                for (int page_num = 0; page_num < MainNotebook.NPages; page_num++) {
                    Gtk.Widget widget = MainNotebook.GetNthPage(page_num);
                    if (documents.ContainsKey(widget)) {
                        if (documents [widget].filename == filename && documents [widget] != doc) {
                            // Contents were just overwritten!
                            documents [widget].filename = null;
                            documents [widget].basename = "Old " + documents [widget].basename;
                            documents [widget].IsDirty = true;
                            documents [widget].UpdateDocument();
                            retval = true;
                            break;
                        }
                    }
                }
                ev.Set();
            });
            ev.WaitOne();
            return retval;
        }

        public bool Close() {
            // Delete Window
            foreach (Document document in documents.Values) {
                if (document.IsDirty) {
                    string answer = Close(document);
                    if (answer == _("Cancel")) {
                        return false;
                    } else if (answer == _("Abandon changes")) {
                        // ok, continue
                    } else { // saved
                        // ok, continue
                    }
                }
                // clean or dirty, shut it down:
                bool result = document.Close(); // any cleanup?
                if (! result) {
                    return false;
                }
            }
            return true;
        }

        public static string SaveAbandonCancel(string title) {
            ManualResetEvent ev = new ManualResetEvent(false);
            dialogResponse = null;
            Invoke(delegate {
                Gtk.Dialog fc = new Gtk.Dialog(title, _mainWindow, 0);
                fc.VBox.PackStart(new Gtk.Label(_("Save this file?")));
                fc.VBox.PackStart(new Gtk.Label(title));

                Gtk.Button button = new Gtk.Button(_("Save"));
                fc.AddActionWidget(button, Gtk.ResponseType.Ok);
                button.Clicked += (o, a) => DialogHandler(o, a, fc);

                button = new Gtk.Button(_("Abandon changes"));
                button.Clicked += (o, a) => DialogHandler(o, a, fc);
                fc.AddActionWidget(button, Gtk.ResponseType.Ok);

                button = new Gtk.Button(_("Cancel"));
                button.Clicked += (o, a) => DialogHandler(o, a, fc);
                fc.AddActionWidget(button, Gtk.ResponseType.Ok);

                fc.ShowAll();
                fc.Run();
                fc.Destroy();
                ev.Set();
            }
            );
            ev.WaitOne();
            return dialogResponse;
        }

        public static void DialogHandler(object obj, System.EventArgs args, Gtk.Dialog dialog) {
            dialogResponse = ((Gtk.Button)obj).Label;
            dialog.Respond(Gtk.ResponseType.Ok);
        }

        public void RequestInterrupt() {
            // kill execution of runn server script
            if (executeThread != null) {
                executeThread.Abort(new Microsoft.Scripting.KeyboardInterruptException(""));
                executeThread = null;
                manager["python"].engine.Execute(@"
def _invoke():
  if _.robot:
    _.robot.flush()
    _.robot.stop()
import Myro as _
_.InvokeBlocking(_invoke)
del _invoke, _
                        ");
            }
        }

        public virtual void Start() {
            // override in Server
        }

        public bool RequestQuit() {
            bool retval = Close();
            if (retval) {
                Cleanup();
                if (signal_thread != null) {
                    signal_thread.Abort(new Microsoft.Scripting.KeyboardInterruptException(""));
                }
                // Close up anything
                foreach (System.Reflection.Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()) {
                    if (assembly != null) {
                        // close_module if possible
                        try {
                            foreach (Type type in assembly.GetTypes()) {
                                System.Reflection.MethodInfo method;
                                try {
                                    method = type.GetMethod("close_module");
                                    if (method != null) {
                                        method.Invoke(type, new object [] {});
                                    }
                                } catch {
                                }
                            }
                        } catch {
                            continue;
                        }
                    }
                }
                if (connection != null) {
                    connection.Close();
                }
                foreach (String lang in manager.getLanguages()) {
                    //manager[lang].engine.Close();
                    if (manager[lang].engine != null)
                        manager[lang].engine.Close();
                }
                if (OS == "Windows" || OS == "Mac") {
                    // FIXME: it would be better not to do this, but some threads are still running:
                    foreach (System.Diagnostics.Process process in System.Diagnostics.Process.GetProcesses()) {
                        try {
                            if (process.ProcessName.EndsWith("mono")) {
                                process.Kill();
                            }
                        } catch {
                            // pass
                        }
                    }
                }
                System.Environment.Exit(0);
                //Gtk.Application.Quit();
            }
            return retval;
        }

        protected void OnDeleteEvent(object sender, Gtk.DeleteEventArgs a) {
            // set RetVal to true to keep window
            a.RetVal = ! RequestQuit();
        }

        public void PickNew() {
            if (manager.languages.Count > 1) {
                ManualResetEvent ev = new ManualResetEvent(false);
                dialogResponse = null;
                Invoke(delegate {
                    Gtk.Dialog fc = new Gtk.Dialog(_("Select item type"), this, 0);
                    fc.SetSizeRequest(200, -1);
                    fc.VBox.PackStart(new Gtk.Label(_("Create a new item")));
                    foreach (string lang in manager.getLanguages()) {
                        Language language = manager [lang];
                        if (! ((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name)) {
                            continue;
                        }
                        Gtk.Button button = new Gtk.Button(language.proper_name);
                        button.Clicked += (o, a) => DialogHandler(o, a, fc);
                        if (manager.languages.Count > 5) {
                            fc.VBox.PackStart(button, true, true, 5);
                        } else {
                            fc.AddActionWidget(button, Gtk.ResponseType.Ok);
                        }
                    }
                    fc.ShowAll();
                    fc.Run();
                    fc.Destroy();
                    ev.Set();
                }
                );
                ev.WaitOne();
                if (dialogResponse != null) {
                    CurrentProperLanguage = dialogResponse;
                    Open(null, CurrentLanguage);
                }
            } else {
                Open();
            }
        }

        protected virtual void OnNewActionActivated(object sender, System.EventArgs e) {
            PickNew();
        }

        protected void OnNewButtonClicked(object sender, System.EventArgs e) {
            PickNew();
        }

        protected virtual void OnOpenAction1Activated(object sender, System.EventArgs e) {
            Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog(_("Select the file to open"), this, Gtk.FileChooserAction.Open, _("Cancel"), Gtk.ResponseType.Cancel, _("Open"), Gtk.ResponseType.Accept);
            fc.KeepAbove = true;
            if (fc.Run() == (int)(Gtk.ResponseType.Accept)) {
                Open(fc.Filename);
            }
            fc.Destroy();
        }

        protected virtual void OnOpenActionActivated(object sender, System.EventArgs e) {
            OnOpenAction1Activated(sender, e);
        }

        protected virtual void OnSaveActionActivated(object sender, System.EventArgs e) {
            if (CurrentDocument != null) {
                Invoke(delegate {
                    CurrentDocument.Save();
                });
                SetLanguage(CurrentLanguage);
                addToRecentsMenu(CurrentDocument.filename); // needed, if didn't have filename before
            }
        }

        protected virtual void OnSaveAsActionActivated(object sender, System.EventArgs e) {
            if (CurrentDocument != null) {
                Invoke(delegate {
                    CurrentDocument.SaveAs();
                }
                );
                SetLanguage(CurrentLanguage);
                addToRecentsMenu(CurrentDocument.filename);
            }
        }

        protected virtual void OnQuitActionActivated(object sender, System.EventArgs e) {
            RequestQuit();
        }

        protected virtual void OnCopyActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;
                string text = editor.SelectedText;
                if (text != null) {
                    clipboard.Text = text;
                }
            } else if (Focus is Gtk.TextView) {
                ((Gtk.TextView)Focus).Buffer.CopyClipboard(clipboard);
            } else if (Focus is Gtk.Entry) {
                ((Gtk.Entry)Focus).CopyClipboard();
            } else if (CurrentDocument != null && CurrentDocument.HasSelection) {
                clipboard.Text = CurrentDocument.Selection.ToString();
            }
        }

        string [] RSplit(string s) {
            string [] data = s.Split(' ');
            string left = "";
            for (int i = 0; i < data.Length - 1; i++) {
                if (left != "") {
                    left += " ";
                }
                left += data [i];
            }
            return new string [] {left, data [data.Length - 1]};
        }

        protected void About(object sender, System.EventArgs e) {
            Gtk.AboutDialog aboutDialog = new Gtk.AboutDialog();
            aboutDialog.DefaultResponse = Gtk.ResponseType.Close;
            //aboutDialog.SetEmailHook(lambda dialog, email: self.message(email))
            //aboutDialog.SetUrlHook(lambda dialog, link: Gnome.Url.Show(link))
            //aboutDialog.Artists =""
            aboutDialog.Authors = new string[] {
                "Douglas Blank <dblank@cs.brynmawr.edu>",
                    "Keith O'Hara <kohara@bard.edu>",
                    "Jim Marshall <jmarshall@sarahlawrence.edu>",
                    "Mark Russo <russomf@gmail.com>",
                    "Jennifer Kay <kay@rowan.edu>",
            };
            aboutDialog.Comments = (_("Scripting Environment") + "\n\n" +
                    String.Format(_("OS: {0}") + "\n", System.Environment.OSVersion.VersionString) +
                    String.Format(_("Mono: {0}") + "\n", System.Environment.Version) +
                    String.Format(_("Gtk: {0}"), typeof(Gtk.Window).Assembly.GetName().Version)
                    );
            aboutDialog.Copyright = _("(c) 2011-2014, Institute for Personal Robots in Education");
            //aboutDialog.Documenters
            //aboutDialog.License
            //aboutDialog.Logo
            //aboutDialog.LogoIconName
            aboutDialog.ProgramName = _("Calico Project");
            //aboutDialog.TranslatorCredits
            aboutDialog.Version = MainClass.Version;
            aboutDialog.Website = "http://CalicoProject.org/";
            //aboutDialog.WebsiteLabel
            aboutDialog.WrapLicense = true;
            aboutDialog.Run();
            aboutDialog.Destroy();
        }

        protected virtual string CleanUpText(string text) {
            // Prevent weird characters like Word smart quotes.
            // First, replace all of those that we know about:
            if (text == null) {
                return "";
            }
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
	    ASCIIEncoding ascii = new ASCIIEncoding();
	    Encoding encoder = ASCIIEncoding.GetEncoding("us-ascii", 
						 new EncoderReplacementFallback(string.Empty), 
						 new DecoderExceptionFallback());
	    //encoder.Fallback = new EncoderReplacementFallback(string.Empty);
	    byte[] bytes = encoder.GetBytes(text);
	    return ascii.GetString(bytes); 
        }

        protected virtual void OnPasteActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                string text = ((Mono.TextEditor.TextArea)Focus).SelectedText;
                if (text != null) {
                    ((Mono.TextEditor.TextArea)Focus).DeleteSelectedText();
                }
                ((Mono.TextEditor.TextArea)Focus).InsertAtCaret(CleanUpText(clipboard.WaitForText()));
            } else if (Focus is Gtk.Entry) {
                ((Gtk.Entry)Focus).PasteClipboard();
            } else if (Focus is Gtk.TextView) {
                ((Gtk.TextView)Focus).Buffer.PasteClipboard(clipboard);
            } else if (CurrentDocument != null) {
                CurrentDocument.Paste(clipboard.WaitForText());
            }
        }

        protected virtual void OnCutActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                string text = ((Mono.TextEditor.TextArea)Focus).SelectedText;
                if (text != null) {
                    clipboard.Text = text;
                }
                ((Mono.TextEditor.TextArea)Focus).DeleteSelectedText();
            } else if (Focus is Gtk.Entry) {
                ((Gtk.Entry)Focus).CutClipboard();
            } else if (Focus is Gtk.TextView) {
                ((Gtk.TextView)Focus).Buffer.CutClipboard(clipboard, true);
            }
        }

        protected virtual void OnUndoActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;
                Mono.TextEditor.MiscActions.Undo(editor.GetTextEditorData());
            }
        }

        protected virtual void OnRedoActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;
                Mono.TextEditor.MiscActions.Redo(editor.GetTextEditorData());
            }
        }

        protected virtual void OnSelectAllActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;

                Mono.TextEditor.SelectionActions.SelectAll(editor.GetTextEditorData());
            } else if (Focus is Gtk.Entry) {
                ((Gtk.Entry)Focus).SelectRegion(0, ((Gtk.Entry)Focus).Text.Length);
            } else if (Focus is Gtk.TextView) {
                Gtk.TextView textview = (Gtk.TextView)Focus;
                textview.Buffer.SelectRange(textview.Buffer.StartIter, textview.Buffer.EndIter);
            }
        }

        protected virtual void OnIndentActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;

                Mono.TextEditor.MiscActions.IndentSelection(editor.GetTextEditorData());
            }
        }

        protected virtual void OnUnindentActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                Mono.TextEditor.TextArea editor = (Mono.TextEditor.TextArea)Focus;
                Mono.TextEditor.MiscActions.RemoveIndentSelection(editor.GetTextEditorData());
            }
        }

        public static int CommentLine(Mono.TextEditor.TextEditorData data,
                Mono.TextEditor.DocumentLine line,
                string line_comment) {
            data.Insert(line.Offset, String.Format("{0} ", line_comment));
            return 1;
        }

        public static int UnCommentLine(Mono.TextEditor.TextEditorData data,
                Mono.TextEditor.DocumentLine line,
                string line_comment) {
            // FIXME: assumes a two-char comment string OR allow string
            char c1 = line_comment [0];
            char c2 = line_comment [1];
            if (data.Document.GetCharAt(line.Offset) == c1 &&
                    data.Document.GetCharAt(line.Offset + 1) == c2 &&
                    data.Document.GetCharAt(line.Offset + 2) == ' ') {
                data.Remove(line.Offset, 3);
                return 1;
            } else {
                return 0;
            }
        }

        static void SelectLineBlock(Mono.TextEditor.TextEditorData data, int endLineNr, int startLineNr) {
            Mono.TextEditor.DocumentLine endLine = data.Document.GetLine(endLineNr);
            try {
                data.MainSelection = new Mono.TextEditor.Selection(startLineNr, 1, endLineNr, endLine.Length);
            } catch {
                // Not a valid selection area, just skip it.
            }
        }

        protected virtual void OnCommentRegionActionActivated(object sender, System.EventArgs e) {
            if (Focus is Mono.TextEditor.TextArea) {
                // Shell or Text Editor
                string language = "python";
                string line_comment = "##";
                if (Focus == ShellEditor.TextArea) {
                    //if (DocumentNotebook.Page == findTab(DocumentNotebook, "Shell")) {
                    language = ShellLanguage;
                } else {
                    language = CurrentLanguage;
                }
                if (manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                    line_comment = manager.languages [language].LineComment;
                }
                Mono.TextEditor.TextArea texteditor = (Mono.TextEditor.TextArea)Focus;
                Mono.TextEditor.TextEditorData data = texteditor.GetTextEditorData();
                int startLineNr = data.IsSomethingSelected ? data.MainSelection.MinLine : data.Caret.Line;
                int endLineNr = data.IsSomethingSelected ? data.MainSelection.MaxLine : data.Caret.Line;
                //data.Document.BeginAtomicUndo();
                int first = -1;
                int last = 0;
                foreach (Mono.TextEditor.DocumentLine line in data.SelectedLines) {
                    last = CommentLine(data, line, line_comment);
                    if (first < 0) {
                        first = last;
                    }
                }
                if (data.IsSomethingSelected) {
                    SelectLineBlock(data, endLineNr, startLineNr);
                }
                if (data.Caret.Column != 1) {
                    data.Caret.PreserveSelection = true;
                    data.Caret.Column = System.Math.Max(1, data.Caret.Column - last + 1);
                    data.Caret.PreserveSelection = false;
                }
                //data.Document.EndAtomicUndo();
                data.Document.RequestUpdate(new Mono.TextEditor.MultipleLineUpdate(startLineNr, endLineNr));
                data.Document.CommitDocumentUpdate();
                }
            }

            protected virtual void OnUncommentRegionActionActivated(object sender, System.EventArgs e) {
                if (Focus is Mono.TextEditor.TextArea) {
                    string language = "python";
                    string line_comment = "##";
                    //if (DocumentNotebook.Page == findTab(DocumentNotebook, "Shell")) {
                    if (Focus == ShellEditor.TextArea) {
                        language = ShellLanguage;
                    } else {
                        language = CurrentLanguage;
                    }
                    if (manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                        line_comment = manager.languages [language].LineComment;
                    }
                    Mono.TextEditor.TextArea texteditor = (Mono.TextEditor.TextArea)Focus;
                    Mono.TextEditor.TextEditorData data = texteditor.GetTextEditorData();
                    int startLineNr = data.IsSomethingSelected ? data.MainSelection.MinLine : data.Caret.Line;
                    int endLineNr = data.IsSomethingSelected ? data.MainSelection.MaxLine : data.Caret.Line;
                    //data.Document.BeginAtomicUndo();
                    int first = -1;
                    int last = 0;
                    foreach (Mono.TextEditor.DocumentLine line in data.SelectedLines) {
                        last = UnCommentLine(data, line, line_comment);
                        if (first < 0) {
                            first = last;
                        }
                    }
                    if (data.IsSomethingSelected) {
                        SelectLineBlock(data, endLineNr, startLineNr);
                    }
                    if (data.Caret.Column != 1) {
                        data.Caret.PreserveSelection = true;
                        data.Caret.Column = System.Math.Max(1, data.Caret.Column - last);
                        data.Caret.PreserveSelection = false;
                    }
                    //data.Document.EndAtomicUndo();
                    data.Document.RequestUpdate(new Mono.TextEditor.MultipleLineUpdate(startLineNr, endLineNr));
                    data.Document.CommitDocumentUpdate();
                }
                }

                public string GetControlString() {
                    if (OS == "Mac")
                        return "<Primary>";
                    else
                        return "<Control>";
                }

                public Gdk.ModifierType GetControlMask () {
                    if (OS == "Mac")
                        return Gdk.ModifierType.MetaMask;
                    else
                        return Gdk.ModifierType.ControlMask;
                }

                public bool handleOnKeyPressEvents(object o, Gtk.KeyPressEventArgs args, Gtk.Widget focus) {
                    bool retval = false;
                    //System.Console.WriteLine("'" + args.Event.Key.ToString() + "'");
                    if (onKey.ContainsKey(args.Event.Key.ToString())) {
                        object tretval = onKey[args.Event.Key.ToString()](o,args,focus);
                        if (tretval is bool)
                            retval = (bool)tretval;
                    }
                    return retval;
                }

                [GLib.ConnectBeforeAttribute]
                public virtual void OnKeyPressEvent(object o, Gtk.KeyPressEventArgs args) {
                    if (handleOnKeyPressEvents(o, args, Focus)) // handled
                        return;
                    // else let's have the normal system handle it:
                    if (Focus == searchEntry.Entry) {
                        if (args.Event.Key == Gdk.Key.Escape) {
                            HandleSearchboxHidden(null, null);
                            args.RetVal = true;
                        } else if (args.Event.Key == Gdk.Key.BackSpace) {
                            if (CurrentDocument != null) {
                                searchEntry.Entry.DeleteText(searchEntry.Entry.Text.Length - 1,
                                        searchEntry.Entry.Text.Length);
                                CurrentDocument.SearchMore(searchEntry.Entry.Text);
                                args.RetVal = true;
                            } else if (Focus == ShellEditor.TextArea) {
                                //} else if (MainNotebook.Page == findTabByLabel(MainNotebook, "Shell")) {
                                if (history.SearchMore(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                        }
                        } else if (args.Event.Key == Gdk.Key.Return) {
                            if (CurrentDocument != null) {
                                CurrentDocument.SearchNext(searchEntry.ActiveText);
                                args.RetVal = true;
                            } else if (Focus == ShellEditor.TextArea) {
                                //} else if (MainNotebook.Page == findTabByLabel(MainNotebook, "Shell")) {
                                if (history.SearchPrevious(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                        } 
                        } // else, don't handle
                    } else if (Focus == ShellEditor.TextArea) {
                        handleShellKey(o, args);
                        //Console.WriteLine("[press]");
                        updateShellControls(ShellEditor.Document, null);
                    } else if (Focus == ChatCommand) { // This is a TextView
                        if (args.Event.Key == Gdk.Key.Return) {
                            // if control key is down, too, just insert a return
                            if ((args.Event.State & GetControlMask()) != 0) {
                                // Don't handle!
                                return;
                            }
                            string text = ChatCommand.Buffer.Text;
                            ChatCommand.Buffer.Text = "";
                            //ChatOutput.Buffer.InsertAtCursor(text.TrimEnd() + "\n");
                            ProcessChatText(text);
                            args.RetVal = true;
                        }
                    } else if (Focus is Mono.TextEditor.TextArea) {
                        // Handle not dirty
                        // not shell
                        // Editor, handle : on end of line
                        if (args.Event.Key == Gdk.Key.e && (args.Event.State & GetControlMask()) != 0) {
                            // emacs mode control+e end of line
                            var texteditor = (Mono.TextEditor.TextArea)Focus;
                            var data = texteditor.GetTextEditorData();
                            var curLine = texteditor.GetLine(data.Caret.Line);
                            data.Caret.Column = System.Math.Min(curLine.LengthIncludingDelimiter, System.Math.Max(0, curLine.Length)) + 1;  
                            args.RetVal = true;
                        }
                    } else if (Focus is Gtk.TreeView) {
                        if (args.Event.Key == Gdk.Key.Tab) {
                            Gtk.TreeView treeView = ((Gtk.TreeView)Focus);
                            Gtk.TreeSelection selection = treeView.Selection;
                            if (selection.CountSelectedRows() > 0) {
                                Gtk.TreePath treePath = selection.GetSelectedRows()[0];
                                if (treePath != null) {
                                    bool startEditing = true;
                                    treePath.Next();
                                    treeView.SetCursor(treePath, treeView.Columns[1], startEditing);
                                    //treeView.ActivateRow(treePath, treeView.Columns[1]);
                                    args.RetVal = true;
                                }
                            }
                        }
                    } 
                }

                public void ProcessChatText(string text) {
                    /*
# Valid command structures:
# -------------------------
# "[password reset]\npassword: ENCRYPTED"
# "[register]\npassword: ENCRYPTED\nkeyword: KEY"
# "[package]\ndata: DATA\nid: ID\nfrom: FROM\nsegments: COUNT"
# "[segment]\ndata: DATA\nid: ID"
# "[join]\nconference: ROOM" (General, Developers)
# "[broadcast]\nconference: ROOM\n"
# "[blast]\nto: ROOM|ID\nfrom: ID\ntype: TYPE\nfile: FILENAME\nDATA"
# "[file]\nfilename: FILENAME\nRAWDATA..."
# "[photo]\nfilename: FILENAME\nRAWDATA..."
# "[help]"
# "[list]"

# Valid return structures:
# ------------------------
# "[result]\nDATA..."
# "[update]\nroom: ROOM"
*/
                    if (connection != null) {
                        if (text.StartsWith("@")) {
                            string [] parts = text.Split(new char[] {' '}, 2); // 2 parts
                            connection.Send(parts [0].Substring(1), parts [1]);
                            ChatPrint(Tag.Info, 
                                    String.Format("{0} to {1}: {2}\n", 
                                        connection.user, 
                                        parts [0].Substring(1), 
                                        parts [1])
                                    ); // @userid: MESSAGE
                        } else if (text.StartsWith("/list")) {
                            connection.Send("admin", "[list]");
                        } else if (text.StartsWith("/help")) {
                            connection.Send("admin", "[help]");
                        } else {
                            connection.Send("admin", "[broadcast]\nconference: General\n" + text);
                        }
                    } else {
                        ErrorLine(_("You need to login before using chat."));
                    }
                }

                public void handleShellKey(object o, Gtk.KeyPressEventArgs args) {
                    // Shell handler
                    // FIXME: control+c, if nothing is selected, else it is a copy
                    if (args.Event.Key == Gdk.Key.c && (args.Event.State & GetControlMask()) != 0) {
                        string text = ShellEditor.SelectedText;
                        if (text == null) {
                            Mono.TextEditor.SelectionActions.SelectAll(ShellEditor.GetTextEditorData());
                            ShellEditor.DeleteSelectedText();
                            args.RetVal = true;
                        }
                    } else if (args.Event.Key == Gdk.Key.e && (args.Event.State & GetControlMask()) != 0) {
                        // Emacs, go to end of line
                        var data = ShellEditor.GetTextEditorData();
                        var curLine = ShellEditor.GetLine(data.Caret.Line);
                        data.Caret.Column = System.Math.Min(curLine.LengthIncludingDelimiter, System.Math.Max(0, curLine.Length)) + 1;
                    } else if (args.Event.Key == Gdk.Key.Escape) {
                        // FIXME: escape with selected, delete; else delete all
                        string text = ShellEditor.SelectedText;
                        if (isRunning) { // Program is running
                            OnStopButtonClicked(null, null);
                        } else if (text == null) {
                            Mono.TextEditor.SelectionActions.SelectAll(ShellEditor.GetTextEditorData());
                        }
                        ShellEditor.DeleteSelectedText();
                        args.RetVal = true;
                    } else if (args.Event.Key == Gdk.Key.Return) {
                        // if control key is down, too, just insert a return
                        if ((args.Event.State & GetControlMask()) != 0) {
                            ShellEditor.InsertAtCaret("\n");
                            args.RetVal = true;
                            return;
                        }
                        // if cursor in middle, insert a Return
                        //Mono.TextEditor.Caret caret = Shell.Caret;
                        //int line = caret.Line;
                        //int line_count = Shell.Document.LineCount;
                        // This needs to be better written, and maybe dealt with 
                        // in each of the languages. Remove from here.
                        //if (line != line_count) { // caret.line not at bottom
                        //    completion = null;
                        //    args.RetVal = false;
                        //    return;
                        //}
                        // else, execute text
                        // extra line at end signals ready_to_execute:
                        string text = ShellEditor.Document.Text;
                        if (text == "") {
                            completion = null;
                            args.RetVal = true;
                            // nothing to do, but handled
                        } else if (manager [CurrentLanguage].engine != null && manager [CurrentLanguage].engine.ReadyToExecute(text)) {
                            history.last(text.TrimEnd(), CurrentLanguage);
                            ((Gtk.TextView)historyview).Buffer.InsertAtCursor(text.TrimEnd() + "\n");
                            history.add("", CurrentLanguage);
                            ExecuteShell();
                            completion = null;
                            args.RetVal = true;
                        }
                        UpdateUpDownArrows();
                    } else if (args.Event.Key == Gdk.Key.Up && (args.Event.State & GetControlMask()) != 0) {
                        // copy current (if anything), go up, paste
                        string current_text = ShellEditor.Document.Text;
                        if (current_text != null) {
                            // Part of KeyUp():
                            history.update(current_text.TrimEnd());
                            string text = history.up();
                            ShellEditor.Document.Text = text.TrimEnd() + "\n" + current_text;
                            ShellEditor.GrabFocus();
                            ShellEditor.Caret.Line = 1;
                            int col = ShellEditor.Document.GetLine(1).Length;
                            ShellEditor.Caret.Column = col + 1;
                            args.RetVal = true;
                            UpdateUpDownArrows();
                            SetCurrentLanguage(history.CurrentLanguage);
                        } else {
                            KeyUp(false); 
                        }
                    } else if (args.Event.Key == Gdk.Key.Down && (args.Event.State & GetControlMask()) != 0) {
                        // copy current (if anything), go down, paste
                        string current_text = ShellEditor.Document.Text;
                        if (current_text != null) {
                            // Part of KeyDown():
                            history.update(current_text.TrimEnd());
                            string text = history.down();
                            ShellEditor.Document.Text = current_text.TrimEnd() + "\n" + text;
                            ShellEditor.GrabFocus();
                            ShellEditor.Caret.Line = 1;
                            int col = ShellEditor.Document.GetLine(1).Length;
                            ShellEditor.Caret.Column = col + 1;
                            args.RetVal = true;
                            UpdateUpDownArrows();
                            SetCurrentLanguage(history.CurrentLanguage);
                        } else {
                            KeyDown(false); 
                        }
                    } else if (args.Event.Key == Gdk.Key.Up) {
                        KeyUp(false); // look at cursor (don't force)
                    } else if (args.Event.Key == Gdk.Key.Down) {
                        KeyDown(false); // look at cursor (don't force)
                    } else if (args.Event.Key == Gdk.Key.Tab) {
                        // where are we?
                        int lineNo = ShellEditor.Caret.Line;
                        string [] lines = ShellEditor.Document.Text.Split('\n');
                        string line = lines [lineNo - 1];
                        string text = line.Substring(0, ShellEditor.Caret.Column - 1);
                        if (text.Trim() != "") { // something there!
                            if (completion == null) {
                                completion = GetTabCompletion(text, ShellEditor);
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
                }

                public void KeyDown(bool force) {
                    string text = ShellEditor.Document.Text;
                    var caret = ShellEditor.Caret;
                    int line = caret.Line;
                    int line_count = ShellEditor.Document.LineCount;
                    if (force || line == (line_count)) {
                        history.update(text.TrimEnd());
                        text = history.down();
                        ShellEditor.Document.Text = text;
                        ShellEditor.GrabFocus();
                        ShellEditor.Caret.Line = ShellEditor.Document.LineCount;
                        ShellEditor.Caret.Column = 1; //Shell.Document.GetLine(1).Length;
                        SetCurrentLanguage(history.CurrentLanguage);
                    }
                    UpdateUpDownArrows();
                }

                public void KeyUp(bool force) {
                    string text = ShellEditor.Document.Text;
                    var caret = ShellEditor.Caret;
                    int line = caret.Line;
                    if (force || line == 1) {
                        history.update(text.TrimEnd());
                        text = history.up();
                        ShellEditor.Document.Text = text;
                        ShellEditor.GrabFocus();
                        ShellEditor.Caret.Line = 1;
                        int col = ShellEditor.Document.GetLine(1).Length;
                        ShellEditor.Caret.Column = col + 1;
                        SetCurrentLanguage(history.CurrentLanguage);
                    }
                    UpdateUpDownArrows();
                }

                public void UpdateUpDownArrows() {
                    if (! ProgramRunning) {
                        Invoke(delegate {
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
                            if (ShellEditor.Document.Text != "") {
                                StartAction.Sensitive = true;
                                StartButton.Sensitive = true;
                            } else {
                                StartAction.Sensitive = false;
                                StartButton.Sensitive = false;
                            }
                        }
                        );
                    }
                }

                public bool ExecuteShell() {
                    string text = ShellEditor.Document.Text;
                    bool results = ExecuteInBackground(text.TrimEnd(), CurrentLanguage);
                    if (results) {
                        Mono.TextEditor.SelectionActions.SelectAll(ShellEditor.GetTextEditorData());
                        Mono.TextEditor.DeleteActions.Delete(ShellEditor.GetTextEditorData());
                        ShellEditor.GrabFocus();
                        ShellEditor.Caret.Line = 1;
                        ShellEditor.Caret.Column = 1;
                    }
                    return true;
                }

                public bool Execute(string text, string language) {
                    ///
                    if (manager[language].engine != null) {
                        return manager [language].engine.Execute(text, false);
                    } else {
                        return false;
                    }
                }

                public void Execute(string text) {
                    if (CurrentLanguage == null || manager[CurrentLanguage].engine == null)
                        return;
                    manager [CurrentLanguage].engine.Execute(text);
                }

                public bool ExecuteInBackground(string text, string language) {
                    if (ProgramRunning) {
                        return false;
                    }
                    string prompt = String.Format("{0}> ", CurrentLanguage.PadRight(8, '>'));
                    int count = 2;
                    foreach (string line in text.Split('\n')) {
                        Print(Tag.Info, prompt);
                        // black
                        Print(line + "\n");
                        // blue
                        prompt = String.Format("{0}> ", count.ToString().PadLeft(8, '.'), count);
                        count += 1;
                    }
                    if (text == "") {
                        return false;
                    }
                    this.CurrentLanguage = language;
                    ExecuteInBackground(text);
                    return true;
                }

                public object Evaluate(string text, string language) {
                    if (manager[language].engine == null)
                        return null;
                    return manager [language].engine.Evaluate(text);
                }

                public object Evaluate(string text) {
                    if (CurrentLanguage == null || manager[CurrentLanguage].engine == null)
                        return null;
                    return manager [CurrentLanguage].engine.Evaluate(text);
                }

                public void ExecuteInBackground(string text) {
                    if (CurrentLanguage == null || manager[CurrentLanguage].engine == null)
                        return;
                    // This is the only approved method of running code
                    executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                        Invoke(OnStartRunning);
                        manager [CurrentLanguage].engine.Execute(text);
                        Invoke(OnStopRunning);
                    }
                    )
                            );
                    executeThread.IsBackground = true;
                    executeThread.Start();
                }

                public void OnStartRunning() {
                    isRunning = true;
                    Invoke(delegate {
                        StartButton.Sensitive = false;
                        StartAction.Sensitive = false;
                        StopButton.Sensitive = true;
                        noAction.Sensitive = true;
                        if (ProgramSpeed.Value == 0) {
                            PlayButton.Sensitive = true;
                            PauseButton.Sensitive = false;
                        } else if (ProgramSpeed.Value < 100) {
                            PlayButton.Sensitive = false;
                            PauseButton.Sensitive = true;
                        } else if (CurrentDocument != null && CurrentDocument.HasBreakpointSet) {
                            PlayButton.Sensitive = false;
                            PauseButton.Sensitive = true;
                        }
                    }
                    );
                }

                public void OnStopRunning() {
                    isRunning = false;
                    Invoke(delegate {
                        ProgramSpeed.Sensitive = true;
                        PlayButton.Sensitive = false;
                        noAction.Sensitive = false;
                        PauseButton.Sensitive = false;
                        StopButton.Sensitive = false;
                        if (CurrentDocument != null) { // Editor
                            if (CurrentDocument.HasContent) {
                                StartButton.Sensitive = true; // need something to execute
                                StartAction.Sensitive = true;
                            } else {
                                StartButton.Sensitive = false; // need something to execute
                                StartAction.Sensitive = false;
                            }
                        } else { // Shell
                            StartButton.Sensitive = false; // need something to execute
                            StartAction.Sensitive = false;
                        }
                    });
                    executeThread = null;
                }

                public void AbortThread() {
                    if (executeThread != null) {
                        Print(Tag.Warning, _("Stopping...\n"));
                        executeThread.Abort(new Microsoft.Scripting.KeyboardInterruptException(""));
                        //executeThread.Join(); don't really care, do we, if it completes?
                        executeThread = null;
                        manager ["python"].engine.Execute(@"
def _invoke():
  if _.robot:
    _.robot.flush()
    _.robot.stop()
import Myro as _
_.InvokeBlocking(_invoke)
del _invoke, _
                                ");
                    }
                    Invoke(OnStopRunning);
                }

                public void ExecuteFileInBackground(string filename) {
                    string language = manager.GetLanguageFromExtension(filename);
                    if (language != null && filename != null) {
                        ExecuteFileInBackground(filename, language);
                    }
                }

                public void ExecuteFile(string filename) {
                    string language = manager.GetLanguageFromExtension(filename);
                    if (manager[language].engine == null)
                        return;
                    manager [language].engine.ExecuteFile(filename); 
                }

                public void ExecuteFileInBackground(string filename, string language) {
                    if (language == null || filename == null) {
                        return;
                    }
                    // This is run from text documents that don't run themselves:
                    PrintLine(Tag.Info, String.Format(_("Running '{0}'..."), filename));
                    executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                        CurrentLanguage = language;
                        if (manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                            ShellLanguage = language;
                        }
                        if (config.HasValue(String.Format("{0}-language", language), "reset-shell-on-run") && 
                            ((bool)config.GetValue(String.Format("{0}-language", language), "reset-shell-on-run"))) {
                            ResetShell();
                        }
                        string dir = System.IO.Path.GetDirectoryName(filename);
                        if (dir != "" && dir != null) {
                            DirectoryInfo dirInfo = new DirectoryInfo(dir);
                            if (dirInfo.Exists)
                        System.IO.Directory.SetCurrentDirectory(dir);
                        }
                        if (CurrentLanguage != null && filename != null) {
			    Invoke(OnStartRunning);
                            manager [CurrentLanguage].engine.ExecuteFile(filename); // not in GUI thread
                            Invoke(OnStopRunning);
                        }
                    }
                    )
                        );
                    executeThread.IsBackground = true;
                    executeThread.Start();
                }

                public static Gtk.TextTag TagColor(Tag tag) {
                    return tags [tag];
                }

                public void Warn(string format) {
                    Print(Tag.Warning, format);
                }

                public void Print(string format) {
                    Print(Tag.Normal, format);
                }

                public void PrintLine(string format) {
                    Print(Tag.Normal, format + "\n");
                }

                public void Error(string format) {
                    Print(Tag.Error, format);
                }

                public void ErrorLine(string format) {
                    Print(Tag.Error, format + "\n");
                }

                public void Info(string format) {
                    Print(Tag.Info, format);
                }

                public void InfoLine(string format) {
                    Print(Tag.Info, format + "\n");
                }

                public void PrintLine(Tag tag, string format) {
                    Print(tag, format + "\n");
                }

                public static string now() {
                    return DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.ffffff");
                }

                public virtual void Print(Tag tag, string format) {
                    if (Debug) {
                        Console.Write(format);
                    } else {
                        Invoke(delegate {
                            //lock (this) {
                            if (tag == Tag.Error) {
                                ToolNotebook.Page = 0;
                            } // show output page
                            Gtk.TextIter end = Output.Buffer.EndIter;
                            Output.Buffer.PlaceCursor(end);
                            string colorname = MainWindow.tagnames [tag];
                            Output.Buffer.InsertWithTagsByName(ref end, format, colorname);
                            end = Output.Buffer.EndIter;
                            Gtk.TextMark mark = Output.Buffer.GetMark("insert");
                            Output.Buffer.MoveMark(mark, end);
                            Output.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
                            //ev.Set();
                            //}
                        }
                        );
                        //ev.WaitOne();
                        //while (Gtk.Application.EventsPending ()) {
                        //    Gtk.Application.RunIteration();
                        //}
                        Thread.Sleep(1); 
                    }
                }

                public void ChatPrint(string format) {
                    ChatPrint(Tag.Normal, format, false);
                }

                public void ChatPrint(string format, bool force) {
                    ChatPrint(Tag.Normal, format, force);
                }

                public void ChatPrint(Tag tag, string format) {
                    ChatPrint(tag, format, false);
                }

                public void ChatPrint(Tag tag, string format, bool force) {
                    // These Write functions are the only approved methods of output
                    Thread.Sleep(1); // Force a context switch, even for an instant
                    if (Debug || GetEnvironment() == "tty") {
                        Console.Write(format);
                    } else {
                        Invoke(delegate {
                            if (vpaned1.Visible || force) {
                                lock (ChatOutput) {
                                    Gtk.TextIter end = ChatOutput.Buffer.EndIter;
                                    ChatOutput.Buffer.PlaceCursor(end);
                                    string colorname = MainWindow.tagnames [tag];
                                    ChatOutput.Buffer.InsertWithTagsByName(ref end, format, colorname);
                                    end = ChatOutput.Buffer.EndIter;
                                    Gtk.TextMark mark = ChatOutput.Buffer.GetMark("insert");
                                    ChatOutput.Buffer.MoveMark(mark, end);
                                    ChatOutput.ScrollToMark(mark, 0.0, true, 0.0, 1.0);
                                }
                            } else {
                                Print(tag, format);
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

                public virtual void OnNotebookDocsSwitchPage(object o, Gtk.SwitchPageArgs args) {
                    Invoke(delegate {
                        // Always start by wiping out and hiding properties
                        Gtk.Widget widget = searchForPage((Gtk.Widget)o);
                        if (widget == null) // || widget == lastSelectedPage)
                        return;
                    lastSelectedPage = widget;
                    Gtk.Notebook nb = this.PropertyNotebook;
                    while (nb.NPages > 0) {
                        nb.RemovePage(0);
                    }
                    nb.Visible = false;
                    Gtk.MenuItem saveaspython_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExportAsPythonAction");
                    Gtk.MenuItem removemodule_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/EditAction/RemoveModuleAction");
                    if (lastSelectedPage == searchForPage(Home)) {
                        ProgramSpeed.Sensitive = false;
                        saveaspython_menu.Sensitive = false;
                        removemodule_menu.Sensitive = false;
                        if (! ProgramRunning) {
                            StartButton.Sensitive = false;
                            StartAction.Sensitive = false;
                        }
                        Title = String.Format("Calico - {0}", System.Environment.UserName);
                    } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                        //} else if (MainNotebook.Page == findTabByLabel(MainNotebook, "Shell")) {
                        //else if (Focus == Shell) {
                        ProgramSpeed.Sensitive = false;
                    saveaspython_menu.Sensitive = false;
                    removemodule_menu.Sensitive = false;
                    if (! ProgramRunning) {
                        if (ShellEditor.Document.Text != "") {
                            StartAction.Sensitive = true;
                            StartButton.Sensitive = true;
                        } else {
                            StartAction.Sensitive = false;
                            StartButton.Sensitive = false;
                        }
                    }
                    SetLanguage(ShellLanguage);
                    // Workaround: had to add this for notebook page selection:
                    Title = String.Format("{0} - Calico - {1}", CurrentProperLanguage, System.Environment.UserName);
                    GLib.Timeout.Add(0, delegate {
                        ShellEditor.GrabFocus();
                        updateShellControls(ShellEditor.Document, null);
                        return false;
                    });
                    } else if (CurrentDocument != null) {
                        // Set save as python menu:
                        saveaspython_menu.Sensitive = CurrentDocument.CanSaveAsPython();
                        removemodule_menu.Sensitive = CurrentDocument.CanRemoveModule();
                        ProgramSpeed.Value = CurrentDocument.SpeedValue;
                        // Set options menu:
                        if (! ProgramRunning) {
                            if (CurrentDocument.HasContent) {
                                ProgramSpeed.Sensitive = true;
                                StartButton.Sensitive = true;
                                StartAction.Sensitive = true;
                            } else {
                                ProgramSpeed.Sensitive = false;
                                StartButton.Sensitive = false;
                                StartAction.Sensitive = false;
                            }
                        }
                        SetLanguage(CurrentDocument.language);
                        Gtk.MenuItem options_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ScriptAction/ScriptOptionsAction");
                        CurrentDocument.SetOptionsMenu(options_menu);
                        CurrentDocument.focus_widget.GrabFocus();
                        //CurrentDocument.tab_label.Text =
                        Title = String.Format(_("{0} - Calico - {1}"), CurrentDocument.basename, System.Environment.UserName);

                        // Looks for property notebook widget from current document.
                        // Adds as new page in property notebook if one is provided.
                        Gtk.Widget propWidget = CurrentDocument.GetPropertyNotebookWidget();
                        if (propWidget != null) {
                            nb.AppendPage(propWidget, new Gtk.Label(_("Properties")));
                            nb.Visible = true;
                            nb.ShowAll();
                        }
                    } else {
                        ProgramSpeed.Sensitive = false;
                        saveaspython_menu.Sensitive = false;
                        removemodule_menu.Sensitive = false;
                        if (! ProgramRunning) {
                            StartButton.Sensitive = false;
                            StartAction.Sensitive = false;
                        }
                        // Some other page
                        Title = String.Format("Calico - {0}", System.Environment.UserName);
                    }
                    });
                    }

                    public void updateShellControls(object obj, System.EventArgs args) {
                        // When Shell is selected by clicking in it, or changes
                        Invoke( delegate {
                            //System.Console.WriteLine("updateShellControls...");
                            Gtk.Widget retval = searchForPage(ShellEditor);
                            if (retval == null) // || retval == lastSelectedPage)
                            return;
                        //System.Console.WriteLine("...set!");
                        lastSelectedPage = retval;
                        Gtk.MenuItem saveaspython_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExportAsPythonAction");
                        Gtk.MenuItem removemodule_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/EditAction/RemoveModuleAction");
                        ProgramSpeed.Sensitive = false;
                        saveaspython_menu.Sensitive = false;
                        removemodule_menu.Sensitive = false;
                        if (! ProgramRunning) {
                            //if (ShellEditor.Document.Text != "") {
                            StartAction.Sensitive = true;
                            StartButton.Sensitive = true;
                            //} else {
                            //StartAction.Sensitive = false;
                            //StartButton.Sensitive = false;
                            //}
                        }
                        SetLanguage(ShellLanguage);
                        // Workaround: had to add this for notebook page selection:
                        Title = String.Format("{0} - Calico - {1}", CurrentProperLanguage, System.Environment.UserName);
                        });
                    }

                    public void updateControls(Document document, bool force=false) { 
                        // When a document is selected by clicking in it, or changes
                        Invoke( delegate {
                            //System.Console.WriteLine("updateControls...");
                            Gtk.Widget nbp = searchForPage(document.widget);
                            if (lastSelectedPage == nbp && !force)
                            return;
                        //System.Console.WriteLine("...set!");
                        lastSelectedPage = nbp;
                        Gtk.Notebook nb = this.PropertyNotebook;
                        while (nb.NPages > 0) {
                            nb.RemovePage(0);
                        }
                        nb.Visible = false;
                        Gtk.MenuItem saveaspython_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExportAsPythonAction");
                        Gtk.MenuItem removemodule_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/EditAction/RemoveModuleAction");
                        // Set save as python menu:
                        saveaspython_menu.Sensitive = document.CanSaveAsPython();
                        removemodule_menu.Sensitive = document.CanRemoveModule();
                        ProgramSpeed.Value = document.SpeedValue;
                        // Set options menu:
                        if (! ProgramRunning) {
                            if (document.HasContent) {
                                ProgramSpeed.Sensitive = true;
                                StartButton.Sensitive = true;
                                StartAction.Sensitive = true;
                            } else {
                                ProgramSpeed.Sensitive = false;
                                StartButton.Sensitive = false;
                                StartAction.Sensitive = false;
                            }
                        }
                        SetLanguage(document.language);
                        Gtk.MenuItem options_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/ScriptAction/ScriptOptionsAction");
                        document.SetOptionsMenu(options_menu);
                        //document.focus_widget.GrabFocus();
                        //CurrentDocument.tab_label.Text =
                        Title = String.Format(_("{0} - Calico - {1}"), document.basename, System.Environment.UserName);

                        // Looks for property notebook widget from current document.
                        // Adds as new page in property notebook if one is provided.
                        Gtk.Widget propWidget = document.GetPropertyNotebookWidget();
                        if (propWidget != null) {
                            nb.AppendPage(propWidget, new Gtk.Label(_("Properties")));
                            nb.Visible = true;
                            nb.ShowAll();
                        }
                        });
                    }

                    public Document getFocusDocument() {
                        Document retval = null;
                        Gtk.Notebook notebook = searchForNotebook(lastSelectedPage);
                        ManualResetEvent ev = new ManualResetEvent(false);
                        Invoke(delegate {
                            if (notebook != null) {
                                Gtk.Widget widget = notebook.GetNthPage(notebook.Page);
                                if (documents.ContainsKey(widget)) {
                                    retval = documents [widget];
                                }
                            }
                            ev.Set();
                        });
                        ev.WaitOne();
                        return retval;
                    }

                    public Gtk.Widget searchForPage(Gtk.Widget widget) {
                        Gtk.Widget child = null;
                        ManualResetEvent ev = new ManualResetEvent(false);
                        Invoke(delegate {
                            while (widget != null) {
                                if (widget.GetType() == typeof(Gtk.Notebook)) {
                                    if (child == null) {
                                        child = ((Gtk.Notebook)widget).GetNthPage(((Gtk.Notebook)widget).Page);
                                    }
                                    break;
                                } else {
                                    child = widget;
                                    widget = widget.Parent;
                                }
                            }
                            ev.Set();
                        });
                        ev.WaitOne();
                        return child;
                    }

                    public Gtk.Notebook searchForNotebook(Gtk.Widget widget) {
                        Gtk.Notebook retval = null;
                        ManualResetEvent ev = new ManualResetEvent(false);
                        Invoke(delegate {
                            while (widget != null) {
                                if (widget.GetType() == typeof(Gtk.Notebook)) {
                                    retval = (Gtk.Notebook)widget;
                                    break;
                                } else {
                                    widget = widget.Parent;
                                }
                            }
                            ev.Set();
                        });
                        ev.WaitOne();
                        return retval;
                    }

                    public int findTabByWidget(Gtk.Widget w) {
                        Gtk.Notebook notebook = searchForNotebook(w);
                        for (int i = 0; i < notebook.NPages; i++) {
                            if (notebook.GetNthPage(i) == w) {
                                return i;
                            }
                        }
                        return -1;
                    }

                    public void switchToShell() {
                        Gtk.Notebook nb = searchForNotebook(ShellEditor);
                        Gtk.Widget widget = searchForPage(ShellEditor);
                        int idx = findTabByWidget(widget);
                        nb.Page = idx;
                        ShellEditor.GrabFocus();
                    }

                    protected virtual void OnShellActionActivated(object sender, System.EventArgs e) {
                        switchToShell();
                    }

                    public virtual void trace_off() {
                        if (CurrentLanguage == null || manager[CurrentLanguage].engine == null)
                            return;
                        manager [CurrentLanguage].engine.SetTraceOff();
                    }

                    public virtual void OnYesAction1Activated(object sender, System.EventArgs e) {
                        // doesn't work in document; but save does
                        if (lastSelectedPage == searchForPage(ShellEditor)) {
                            //} else if (tabShowing("Shell")) {
                            //else if (DocumentNotebook.Page == findTab(DocumentNotebook, "Shell")) {
                            string text = ShellEditor.Document.Text;
                        history.last(text.TrimEnd(), CurrentLanguage);
                        ((Gtk.TextView)historyview).Buffer.InsertAtCursor(text.TrimEnd() + "\n");
                        history.add("", CurrentLanguage);
                        ExecuteShell();
                        ShellEditor.GrabFocus();
                    } else if (CurrentDocument != null) {
                        if (! ((IList<string>)config.GetValue("config", "visible-languages")).Contains(CurrentLanguage)) {
                            ErrorLine(String.Format(_("Error: '{0}' is not an active language"), CurrentLanguage));
                            return;
                        }
                        bool retval = false;
                        ManualResetEvent ev = new ManualResetEvent(false);
                        Invoke(delegate {
                            retval = CurrentDocument.Save();
                            ev.Set();
                        });
                        ev.WaitOne();
                        if (retval) {
                            // if select, just send that
                            //if (manager[CurrentLanguage].IsTextLanguage && CurrentDocument.HasSelection) {
                            //    string text = (string)CurrentDocument.Selection;
                            //    history.last(text.TrimEnd());
                            //    ((Gtk.TextView)historyview).Buffer.InsertAtCursor(text.TrimEnd() + "\n");
                            //    history.add("");
                            //    Execute(text.TrimEnd(), CurrentLanguage);
                            // run as a file, if something selected
                            SetLanguage(CurrentLanguage);
                            if (ProgramSpeed.Value < 100 || CurrentDocument.HasBreakpointSet) {
                                manager [CurrentLanguage].engine.SetTraceOn(this);
                            } else {
                                Invoke(delegate {
                                    if (CurrentDocument.AlwaysAllowSpeedAdjustment) {
                                        ProgramSpeed.Sensitive = true;
                                    } else {
                                        ProgramSpeed.Sensitive = false;
                                    }
                                }
                                );
                                manager [CurrentLanguage].engine.SetTraceOff();
                            }
                            // If a language can handle it, it will run it
                            // Otherwise, it passes it back to calico.ExecuteInBackground():
                            OnStartRunning();
                            // If document handles running, it manages UI itself
                            CurrentDocument.ExecuteFileInBackground();
                            ((Gtk.TextView)historyview).Buffer.InsertAtCursor(_("[Run file]") + "\n");
                            //}
                        }
                    }
                    }

                    public bool EnvModelForeachFunc(Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter) {
                        string vname = (string)model.GetValue(iter, 0);
                        try {
                            model.SetValue(iter, 1, Repr(manager.scope.GetVariable(vname)));
                        } catch {
                            model.SetValue(iter, 1, "<undefined>");
                        }
                        return false;
                    }

                    public bool LocalModelForeachFunc(Gtk.TreeModel model, Gtk.TreePath path, Gtk.TreeIter iter,
                            string variable, string value) {
                        string vname = (string)model.GetValue(iter, 0);
                        if (variable == vname) {
                            model.SetValue(iter, 1, value);
                        } else {
                            model.SetValue(iter, 1, "<undefined>");
                        }
                        return false;
                    }

                    public static string ArrayTypeToString(Array args) {
                        return ArrayTypeToString(args, -1);
                    }

                    public static string ArrayTypeToHTML(Array args) {
                        return ArrayTypeToHTML(args, -1);
                    }

                    public static string ArrayTypeToString(Array args, int limit) {
                        string retval = "";
                        if (args != null) {
                            int count = ((Array)args).Length;
                            for (int i = 0; i < count; i++) {
                                if (i >= limit && limit != -1) {
                                    retval += ", ...";
                                    break;
                                }
                                if (args.GetValue(i) is object[]) {
                                    retval += ArrayToString((object[])args.GetValue(i));
                                } else if (args.GetValue(i) is Array) {
                                    retval += ArrayTypeToString((Array)args.GetValue(i));
                                } else {
                                    if (retval != "") {
                                        retval += ", ";
                                    }
                                    retval += args.GetValue(i);
                                }
                            }
                        }
                        return "[" + retval + "]";
                    }

                    public static string ArrayTypeToHTML(Array args, int limit) {
                        string retval = "";
                        string row;
                        if (args != null) {
                            int count = ((Array)args).Length;
                            retval += "<tr><th colspan=\"2\" align=\"center\">Array Type</th></tr>";
                            retval += "<tr><th>Position</th><th>Value</th></tr>";
                            for (int i = 0; i < count; i++) {
                                row = "<tr>";
                                row += String.Format("<td>{0}</td>", i);
                                if (args.GetValue(i) is object[]) {
                                    row += String.Format("<td>{0}</td>", ArrayToHTML((object[])args.GetValue(i)));
                                } else if (args.GetValue(i) is Array) {
                                    row += String.Format("<td>{0}</td>", ArrayTypeToHTML((Array)args.GetValue(i)));
                                } else {
                                    row += String.Format("<td>{0}</td>", args.GetValue(i));
                                }
                                row += "</tr>";
                                retval += row;
                            }
                        }
                        return "<table>" + retval + "<table>";
                    }

                    public static string ArrayToString(object[] args) {
                        return ArrayToString(args, -1);
                    }

                    public static string ArrayToHTML(object[] args) {
                        return ArrayToHTML(args, -1);
                    }

                    public static string ArrayToHTML(object[] args, int limit) {
                        string retval = "";
                        string row;
                        if (args != null) {
                            int count = ((Array)args).Length;
                            for (int i = 0; i < count; i++) {
                                row = "<tr>";
                                row += "<th colspan=\"2\" align=\"center\">Array</th>";
                                row += "<th>Position</th><th>Value</th>";
                                row += String.Format("<td>{0}</td>", i);
                                if (i >= limit && limit != -1) {
                                    row += "<td>...</td>";
                                    break;
                                }
                                if (args.GetValue(i) is object[]) {
                                    row += ArrayToHTML((object[])args.GetValue(i));
                                } else if (args.GetValue(i) is Array) {
                                    row += ArrayTypeToHTML((Array)args.GetValue(i));
                                } else {
                                    row += String.Format("<td>{0}</td>", args.GetValue(i));
                                }
                                row += "</tr>";
                                retval += row;
                            }
                        }
                        return "<table>" + retval + "<table>";
                    }

                    public static string ArrayToString(object[] args, int limit) {
                        string retval = "";
                        if (args != null) {
                            int count = ((Array)args).Length;
                            for (int i = 0; i < count; i++) {
                                if (i >= limit && limit != -1) {
                                    retval += ", ...";
                                    break;
                                }
                                if (args [i] is object[]) {
                                    retval += ArrayToString((object[])args [i]);
                                } else {
                                    if (retval != "") {
                                        retval += ", ";
                                    }
                                    retval += args [i];
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

                    public static bool HasField(object obj, string fieldName) {
                        if (obj != null) {
                            Type type = obj.GetType();
                            return type.GetField(fieldName) != null;
                        } else {
                            return false;
                        }
                    }

                    public static string InvokeMethod(object obj, string methodName, params object[] args) {
                        var type = obj.GetType();
                        try {
                            System.Reflection.MethodInfo mi = type.GetMethod(methodName);
                            return (string)mi.Invoke(obj, args);
                        } catch {
                            return null;
                        }
                    }


                    public string Repr(object obj) {
                        return Repr(obj, new Dictionary<int,bool>());
                    }

                    public IDictionary<int,bool> Prev(IDictionary<int,bool> hashes, object obj) {
                        if (obj is int || 
                                obj is Int32 ||
                                obj is Int64 ||
                                obj is Double ||
                                obj is Single ||
                                obj is float ||
                                obj is string ||
                                obj is bool 
                           ) {
                            return hashes;
                        }
                        hashes[obj.GetHashCode()] = true;
                        return hashes;
                    }

                    public string Repr(object obj, IDictionary<int,bool> prev) {
                        if (obj != null && prev.ContainsKey(obj.GetHashCode())) {
                            return "...";
                        }
                        string repr = null;
                        if (HasField(obj, "Value")) {
                            //if (obj is IronPython.Runtime.ClosureCell) {
                            //obj = ((IronPython.Runtime.ClosureCell)obj).Value;
                            System.Reflection.FieldInfo fi = obj.GetType().GetField("Value");
                            var obj2 = fi.GetValue(obj);
                            return Repr(obj2, Prev(prev, obj));
                        } else if (obj is Microsoft.Scripting.Runtime.Uninitialized) {
                            repr = "<Uninitialized value>";
                        } else if (HasMethod(obj, "__repr__")) {
                            // FIXME: make sure there is a python
                            repr = InvokeMethod(obj, "__repr__", manager ["python"].engine.GetDefaultContext());
                        } else if (HasMethod(obj, "to_s")) {
                            repr = InvokeMethod(obj, "to_s");
                        } else if (obj is object[]) {
                            repr = (string)ArrayToString((object[])obj);
                        } else if (obj is Array) {
                            try {
                                double [,] arr = (double[,])obj;
                                repr = "";
                                int rowLength = arr.GetLength(0);
                                int colLength = arr.GetLength(1);
                                for (int i = 0; i < rowLength; i++) {
                                    if (repr != "")
                                        repr += ", " + Environment.NewLine + " ";
                                    string row = "";
                                    for (int j = 0; j < colLength; j++) {
                                        if (row != "")
                                            row += ", ";
                                        row += Repr(arr[i, j], Prev(prev, obj));
                                    }
                                    repr += string.Format("[{0}]", row);
                                }
                                repr = string.Format("[{0}]", repr);
                            } catch { // not doubles
                                Array array = (Array)obj;
                                if (array.Rank == 1) {
                                    repr = (string)ArrayTypeToString((Array)obj);
                                } else {
                                    repr = String.Format("<Multidimensional array, rank={0}>", array.Rank);
                                }
                            }
                        } else if (obj is string) {
                            repr = String.Format("\"{0}\"", ((string)obj).Replace("\"", "\\\""));
                        } else if (obj is IList) {
                            repr = "";
                            foreach(var element in (IList)obj) {
                                if (repr != "")
                                    repr += ", ";
                                repr += Repr(element, Prev(prev, obj));
                            }
                            repr = string.Format("[{0}]", repr);
                        } else if (obj is IDictionary) {
                            repr = "";
                            foreach(System.Collections.DictionaryEntry kvp in (IDictionary)obj) { ///
                                if (repr != "")
                                    repr += ", ";
                                repr += string.Format("{0}: {1}", kvp.Key.ToString(), Repr(kvp.Value, Prev(prev, obj)));
                            }
                            repr = string.Format("{{{0}}}", repr);
                        }
                        if (repr == null) {
                            if (obj != null) {
                                repr = obj.ToString();
                            } else {
                                repr = "None";
                            }
                        }
                        return repr;
                        }

                        public string RichRepr(object obj) {
                            return RichRepr(obj, new Dictionary<int,bool>());
                        }

                        public string RichRepr(object obj, IDictionary<int,bool> prev) {
                            string repr = "";
                            if (obj != null && prev.ContainsKey(obj.GetHashCode())) {
                                return "...";
                            }
                            if (obj is object[]) {
                                repr = (string)ArrayToHTML((object[])obj);
                            } else if (obj is Array) {
                                try {
                                    double [,] arr = (double[,])obj;
                                    int rowLength = arr.GetLength(0);
                                    int colLength = arr.GetLength(1);
                                    repr = String.Format("<table><tr><th colspan=\"{0}\" align=\"center\">2D Array</th></tr>", colLength + 1);
                                    repr += "<tr><th>Position</th>";
                                    for (int i = 0; i < colLength; i++) {
                                        repr += String.Format("<th>{0}</th>", i);
                                    }
                                    repr += "</tr>";
                                    for (int i = 0; i < rowLength; i++) {
                                        string row = String.Format("<tr><td>{0}</td>", i);
                                        for (int j = 0; j < colLength; j++) {
                                            row += String.Format("<td>{0}</td>", RichRepr(arr[i, j], Prev(prev, obj)));
                                        }
                                        row += "</tr>";
                                        repr += row;
                                    }
                                    repr += "</table>";
                                } catch { // not doubles
                                    Array array = (Array)obj;
                                    if (array.Rank == 1) {
                                        repr = (string)ArrayTypeToHTML((Array)obj);
                                    } else {
                                        repr = String.Format("<Multidimensional array, rank={0}>", array.Rank);
                                    }
                                }
                            } else if (obj is IList) {
                                repr = "<table>";
                                repr += "<tr><th colspan=\"2\" align=\"center\">List</th></tr>";
                                repr += "<tr><th>Position</th><th>Value</th></tr>";
                                int count = 0;
                                foreach(var element in (IList)obj) {
                                    repr += String.Format("<tr><td>{0}</td><td>{1}</td></tr>", count++, RichRepr(element, Prev(prev, obj)));
                                }
                                repr += "</table>";
                            } else if (obj is IDictionary) {
                                repr = "<table>";
                                repr += "<tr><th colspan=\"2\" align=\"center\">Dictionary</th></tr>";
                                repr += "<tr><th>Key</th><th>Value</th></tr>";
                                foreach(System.Collections.DictionaryEntry kvp in (IDictionary)obj) { 
                                    repr += string.Format("<tr><td>{0}</td><td>{1}</td></tr>", kvp.Key.ToString(), RichRepr(kvp.Value, Prev(prev, obj)));
                                }
                                repr += "</table>";
                            } else {
                                repr = Repr(obj, Prev(prev, obj));
                            }
                            return repr;
                        }

                        public void ClearLocal() {
                            LocalList.Clear();
                            LocalVariables.Clear();
                        }

                        public void UpdateLocal(IDictionary<object,object> locals) {
                            //LocalList = new Gtk.ListStore(typeof(string), typeof(string));
                            LocalList.Clear();
                            foreach (object key in locals.Keys) {
                                string vname = key.ToString();
                                if (! vname.StartsWith("_") && vname != "calico") {
                                    string repr = Repr(locals [key]);
                                    //Console.WriteLine(locals[key]);
                                    LocalList.AppendValues(vname, repr);
                                }
                            }
                            //LocalTreeView.Model = LocalList;
                        }

                        public void UpdateLocal(IList<IList<object>> locals) {
                            LocalList.Clear();
                            foreach (IList<object> list in locals) {
                                string vname = list[0].ToString();
                                if (! vname.StartsWith("_") && vname != "calico") {
                                    string repr = Repr(list[1]);
                                    LocalList.AppendValues(vname, repr);
                                }
                            }
                        }

                        public bool UpdateEnvironment() {
                            // update tree in Environment tab
                            // update the ones that already exist:
                            EnvironmentList.Foreach(EnvModelForeachFunc);
                            // now, add those that aren't in it:
                            foreach (string vname in manager.scope.GetVariableNames()) {
                                if (vname.StartsWith("_") || vname == "calico") {
                                    continue;
                                }
                                if (! EnvironmentVariables.ContainsKey(vname)) {
                                    string repr = Repr(manager.scope.GetVariable(vname));
                                    Gtk.TreeIter iter = EnvironmentList.AppendValues(vname, repr);
                                    Gtk.TreePath treepath = EnvironmentList.GetPath(iter);
                                    EnvironmentTreeView.SetCursor(treepath, null, false);
                                    EnvironmentVariables [vname] = true;
                                }
                            }
                            return IsUpdateEnvironment; // keep going
                        }

                        protected virtual void OnNoActionActivated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.Stop();
                            }
                        }

                        protected virtual void OnEnvironmentTabActionActivated(object sender, System.EventArgs e) {
                            // show environment tab if active
                            if (! EnvironmentTabAction.Active) {
                                IsUpdateEnvironment = false;
                                EnvironmentPage.Child.Hide();
                            } else {
                                IsUpdateEnvironment = true;
                                GLib.Timeout.Add(250, new GLib.TimeoutHandler(UpdateEnvironment));
                                EnvironmentPage.Child.Show();
                                ToolNotebook.Page = 1;
                            }
                        }

                        protected virtual void OnHistoryUpClicked(object sender, System.EventArgs e) {
                            KeyUp(true); // force it regardless of cursor location
                        }

                        protected virtual void OnHistoryDownClicked(object sender, System.EventArgs e) {
                            KeyDown(true);
                        }

                        public virtual Pango.FontDescription GetFont() {
                            Pango.FontDescription pangofont = new Pango.FontDescription();
                            pangofont.Family = (string)config.GetValue("config", "font");
                            pangofont.Size = (int)config.GetValue("config", "font-size");
                            pangofont.Weight = ((bool)config.GetValue("config", "font-bold")) ? Pango.Weight.Bold : Pango.Weight.Normal;
                            pangofont.Style = ((bool)config.GetValue("config", "font-italic")) ? Pango.Style.Italic : Pango.Style.Normal;
                            return pangofont;
                        }

                        protected virtual void SetFont(string font) {
                            Pango.FontDescription desc = Pango.FontDescription.FromString(font);
                            config.SetValue("config", "font", desc.Family);
                            config.SetValue("config", "font-bold", desc.Weight == Pango.Weight.Bold);
                            config.SetValue("config", "font-italic", desc.Style == Pango.Style.Italic);
                            config.SetValue("config", "font-size", desc.Size);
                        }

                        protected virtual void SelectFont(object sender, System.EventArgs e) {
                            Gtk.FontSelectionDialog d = new Gtk.FontSelectionDialog(_("Select Calico Font"));
                            Pango.FontDescription pangofont = GetFont();
                            d.SetFontName(pangofont.ToString());
                            int response = d.Run();
                            if (response == (int)Gtk.ResponseType.Ok) {
                                SetFont(d.FontName);
                                UpdateZoom();
                            }
                            d.Destroy();
                        }

                        protected void UpdateZoom() {
                            ShellEditor.Options.FontName = GetFont().ToString();
                            Output.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatOutput.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatCommand.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            foreach (KeyValuePair<Gtk.Widget, Document> d in documents) {
                                d.Value.UpdateZoom();
                            }
                        }

                        protected void DefaultZoom() {
                            ShellEditor.Options.FontName = GetFont().ToString();
                            Output.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatOutput.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatCommand.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            foreach (KeyValuePair<Gtk.Widget, Document> d in documents) {                
                                d.Value.DefaultZoom();
                                d.Value.UpdateZoom();
                            }
                        }

                        protected void OnZoomInActionActivated(object sender, System.EventArgs e) {
                            config.SetValue("config", "font-size",
                                    ((int)config.GetValue("config", "font-size")) + 1024);
                            ShellEditor.Options.FontName = GetFont().ToString();
                            Output.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatOutput.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            ChatCommand.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                            foreach (KeyValuePair<Gtk.Widget, Document> d in documents) {                
                                d.Value.ZoomIn();
                                d.Value.UpdateZoom();
                            }            
                        }

                        protected void OnZoomOutActionActivated(object sender, System.EventArgs e) {
                            if (((int)config.GetValue("config", "font-size")) > 5 * 1024) {
                                config.SetValue("config", "font-size",
                                        ((int)config.GetValue("config", "font-size")) - 1024);
                                ShellEditor.Options.FontName = GetFont().ToString();
                                Output.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                                ChatOutput.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                                ChatCommand.ModifyFont(Pango.FontDescription.FromString(ShellEditor.Options.FontName));
                                foreach (KeyValuePair<Gtk.Widget, Document> d in documents) {                
                                    d.Value.ZoomOut();
                                    d.Value.UpdateZoom();
                                }
                            }
                        }

                        protected void OnOpenButtonClicked(object sender, System.EventArgs e) {
                            OnOpenAction1Activated(sender, e);
                        }

                        protected void OnStopButtonClicked(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.Stop();
                            } else {
                                AbortThread();
                            }
                        }

                        protected void OnStartButtonClicked(object sender, System.EventArgs e) {
                            OnYesAction1Activated(sender, e);
                        }

                        protected void OnPlayButtonClicked(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.OnPlayButton();
                                if (ProgramSpeed.Value != 0) {
                                    Invoke(delegate {
                                        PlayButton.Sensitive = false;
                                        PauseButton.Sensitive = true;
                                    }
                                    );
                                }
                            }
                        }

                        protected void OnPrintActionActivated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.Print(this);
                            }
                        }

                        protected void OnSpinbutton1ChangeValue(object o, Gtk.ChangeValueArgs args) {
                            //Console.WriteLine("ChangeValue: {0}", args);
                        }

                        protected void OnSpinbutton1Input(object o, Gtk.InputArgs args) {
                            //Console.WriteLine("Input: {0}", args);
                        }

                        public void ResetShell() {
                            manager.Setup(path);
                            manager.Start(path);
                            foreach(string path in persistentPaths){
                                manager.AddPath(path);
                            }
                            manager.SetCalico(this);
                            configureIO();

                            manager.PostSetup(this);
                            Print(Tag.Info, "Shell reset!\n");
                        }

                        protected void OnResetShellActionActivated(object sender, System.EventArgs e) {
                            // If running:
                            AbortThread();
                            // Now, reset the shell:
                            ResetShell();
                        }

                        protected void OnExportAction1Activated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.Export(this);
                            }
                        }

                        protected void OnClearOutputActionActivated(object sender, System.EventArgs e) {
                            Output.Buffer.Text = "";
                            Print(Tag.Info, String.Format("The Calico Project, Version {0}\n", MainClass.Version));
                        }

                        protected void OnPrintButtonClicked(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.Print(this);
                            }
                        }

                        protected void OnSaveButtonClicked(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                Invoke(delegate {
                                    CurrentDocument.Save();
                                }
                                );
                            }
                        }

                        public string GetMimeType(string filename) {
                            string strExtension = System.IO.Path.GetExtension(filename);
                            // First, let's look in languages:
                            foreach (string lang in manager.getLanguages()) {
                                Language language = manager [lang];
                                foreach (string ext in language.extensions) {
                                    if (("." + ext) == strExtension) {
                                        return language.mimetype;
                                    }
                                }
                            }
                            // else, let's look here:   

                            switch (strExtension) {
                                case ".htm":
                                    return "text/html";
                                case ".html":
                                    return "text/html";
                                case ".txt":
                                    return "text/plain";
                                case ".ss":
                                    return "text/x-scheme";
                                case ".scm":
                                    return "text/x-scheme";
                                case ".s":
                                    return "text/x-scheme";
                                case ".py":
                                    return "text/x-python";
                                case ".pyw":
                                    return "text/x-python";
                                case ".rb":
                                    return "text/x-ruby";
                                case ".cs":
                                    return "text/x-csharp";
                                case ".fif":
                                    return "application/fractals";
                                case ".hta":
                                    return "application/hta";
                                case ".hqx":
                                    return "application/mac-binhex40";
                                case ".vsi":
                                    return "application/ms-vsi";
                                case ".p10":
                                    return "application/pkcs10";
                                case ".p7m":
                                    return "application/pkcs7-mime";
                                case ".p7s":
                                    return "application/pkcs7-signature";
                                case ".cer":
                                    return "application/pkix-cert";
                                case ".crl":
                                    return "application/pkix-crl";
                                case ".ps":
                                    return "application/postscript";
                                case ".setpay":
                                    return "application/set-payment-initiation";
                                case ".setreg":
                                    return "application/set-registration-initiation";
                                case ".sst":
                                    return "application/vnd.ms-pki.certstore";
                                case ".pko":
                                    return "application/vnd.ms-pki.pko";
                                case ".cat":
                                    return "application/vnd.ms-pki.seccat";
                                case ".stl":
                                    return "application/vnd.ms-pki.stl";
                                case ".wpl":
                                    return "application/vnd.ms-wpl";
                                case ".xps":
                                    return "application/vnd.ms-xpsdocument";
                                case ".z":
                                    return "application/x-compress";
                                case ".tgz":
                                    return "application/x-compressed";
                                case ".gz":
                                    return "application/x-gzip";
                                case ".ins":
                                    return "application/x-internet-signup";
                                case ".iii":
                                    return "application/x-iphone";
                                case ".jtx":
                                    return "application/x-jtx+xps";
                                case ".latex":
                                    return "application/x-latex";
                                case ".nix":
                                    return "application/x-mix-transfer";
                                case ".asx":
                                    return "application/x-mplayer2";
                                case ".application":
                                    return "application/x-ms-application";
                                case ".wmd":
                                    return "application/x-ms-wmd";
                                case ".wmz":
                                    return "application/x-ms-wmz";
                                case ".xbap":
                                    return "application/x-ms-xbap";
                                case ".p12":
                                    return "application/x-pkcs12";
                                case ".p7b":
                                    return "application/x-pkcs7-certificates";
                                case ".p7r":
                                    return "application/x-pkcs7-certreqresp";
                                case ".sit":
                                    return "application/x-stuffit";
                                case ".tar":
                                    return "application/x-tar";
                                case ".man":
                                    return "application/x-troff-man";
                                case ".zip":
                                    return "application/x-zip-compressed";
                                case ".xaml":
                                    return "application/xaml+xml";
                                case ".xml":
                                    return "application/xml";
                                case ".aiff":
                                    return "audio/aiff";
                                case ".au":
                                    return "audio/basic";
                                case ".mid":
                                    return "audio/mid";
                                case ".mp3":
                                    return "audio/mpeg";
                                case ".m3u":
                                    return "audio/mpegurl";
                                case ".wav":
                                    return "audio/wav";
                                case ".wax":
                                    return "audio/x-ms-wax";
                                case ".wma":
                                    return "audio/x-ms-wma";
                                case ".bmp":
                                    return "image/bmp";
                                case ".gif":
                                    return "image/gif";
                                case ".jpg":
                                    return "image/jpeg";
                                case ".png":
                                    return "image/png";
                                case ".tiff":
                                    return "image/tiff";
                                case ".ico":
                                    return "image/x-icon";
                                case ".dwfx":
                                    return "model/vnd.dwfx+xps";
                                case ".css":
                                    return "text/css";
                                case ".323":
                                    return "text/h323";
                                case ".uls":
                                    return "text/iuls";
                                case ".wsc":
                                    return "text/scriptlet";
                                case ".htt":
                                    return "text/webviewhtml";
                                case ".htc":
                                    return "text/x-component";
                                case ".vcf":
                                    return "text/x-vcard";
                                case ".wm":
                                    return "video/x-ms-wm";
                                case ".wmv":
                                    return "video/x-ms-wmv";
                                case ".wmx":
                                    return "video/x-ms-wmx";
                                case ".wvx":
                                    return "video/x-ms-wvx";
                                case ".avi":
                                    return "video/x-msvideo";
                                default:
                                    return "text/plain";
                            }
                        }

                        protected void OnFindActionActivated(object sender, System.EventArgs e) {
                            HandleSearchboxShown(sender, e);
                        }
                        /*
                           bool selectedTab(Gtk.Notebook notebook, Gtk.Widget widget) {
                           return (notebook.Page == findTabByWidget(widget));
                           }

                           bool selectedTab(Gtk.Widget widget) {
                           if (lastSelectedNotebook != null) {
                           return (lastSelectedNotebook.Page == findTabByWidget(widget));
                           }
                           }
                           */

                        void HandleSearchboxHidden(object sender, EventArgs e) {
                            searchboxHide();
                            resetFocus();
                        }

                        void resetFocus() {
                            if (lastSelectedPage == searchForPage(ShellEditor)) {
                                ShellEditor.GrabFocus();
                            } else if (CurrentDocument != null) {
                                CurrentDocument.focus_widget.GrabFocus();
                            }
                        }

                        void HandleSearchboxShown(object sender, EventArgs e) {
                            searchboxShow();
                            searchEntry.GrabFocus();
                        }

                        protected void OnButton125Clicked(object sender, System.EventArgs e) {
                            // searchbox Close
                            HandleSearchboxHidden(sender, e);
                        }

                        public static bool yesno(string question) {
                            ManualResetEvent ev = new ManualResetEvent(false);
                            bool retval = false;
                            Invoke(delegate {
                                Gtk.MessageDialog fc = new Gtk.MessageDialog(_mainWindow,
                                    0, Gtk.MessageType.Question,
                                    Gtk.ButtonsType.YesNo,
                                    question);
                                fc.ShowAll();
                                if (fc.Run() == (int)(Gtk.ResponseType.Yes)) {
                                    retval = true;
                                }
                                fc.Destroy();
                                ev.Set();
                            }
                            );
                            ev.WaitOne();
                            return retval;
                        }

                        public static void inform(string information) {
                            ManualResetEvent ev = new ManualResetEvent(false);
                            Invoke(delegate {
                                Gtk.MessageDialog fc = new Gtk.MessageDialog(_mainWindow,
                                    0, Gtk.MessageType.Info,
                                    Gtk.ButtonsType.Ok,
                                    information);
                                fc.ShowAll();
                                fc.Run();
                                fc.Destroy();
                                ev.Set();
                            }
                            );
                            ev.WaitOne();
                        }

                        protected void OnButton26Clicked(object sender, System.EventArgs e) {
                            // Help
                            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Help");
                        }

                        protected void OnButton3Clicked(object sender, System.EventArgs e) {
                            // New
                            PickNew();
                        }

                        protected void OnButton7Clicked(object sender, System.EventArgs e) {
                            // Open
                            OnOpenAction1Activated(sender, e);
                        }

                        protected void OnGettingStartedActionActivated(object sender, System.EventArgs e) {
                            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Getting_Started");
                        }

                        protected void OnHelpAction1Activated(object sender, System.EventArgs e) {
                            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Help");
                        }

                        protected void OnWhatSNewActionActivated(object sender, System.EventArgs e) {
                            System.Diagnostics.Process.Start("http://calicoproject.org/Calico:_Whats_New");
                        }

                        protected void OnPauseButton1Clicked(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.OnPauseButton();
                                Invoke(delegate {
                                    PlayButton.Sensitive = true;
                                    PauseButton.Sensitive = false;
                                }
                                );
                            }
                        }

                        protected void OnDebugSpeedChangeValue(object o, Gtk.ChangeValueArgs args) {
                            if (CurrentDocument != null) {
                                CurrentDocument.SpeedValue = ((Gtk.Scale)o).Value;
                                if (ProgramRunning) {
                                    if (((Gtk.Scale)o).Value == 0) {
                                        Invoke(delegate {
                                            PlayButton.Sensitive = true;
                                        }
                                        );
                                        playResetEvent.Reset();
                                    }
                                }
                            }
                        }

                        protected void OnLocalsTabActionActivated(object sender, System.EventArgs e) {
                            // show tab if active
                            if (! LocalsTabAction.Active) {
                                LocalsPage.Child.Hide();
                            } else {
                                LocalsPage.Child.Show();
                                ToolNotebook.Page = 2;
                            }
                        }

                        protected void OnSearchEntryChanged(object sender, System.EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                documents[lastSelectedPage].SearchMore(searchEntry.ActiveText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                if (history.SearchMore(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                            }
                        }

                        protected void OnSearchNextButtonClicked(object sender, System.EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                documents[lastSelectedPage].SearchNext(searchEntry.ActiveText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                if (history.SearchNext(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                            }
                            searchEntry.Entry.GrabFocus();
                        }

                        protected void OnSearchPrevButtonClicked(object sender, System.EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                documents[lastSelectedPage].SearchPrevious(searchEntry.ActiveText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                if (history.SearchPrevious(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                            }
                            searchEntry.Entry.GrabFocus();
                        }

                        protected void OnSearchReplaceButtonClicked (object sender, EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                Document doc = documents[lastSelectedPage];
                                String searchText = searchEntry.ActiveText;
                                String replaceText = searchReplaceEntry.ActiveText;
                                doc.Replace(searchText, replaceText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                // shell replace
                            }
                            searchReplaceEntry.Entry.GrabFocus();
                        }

                        protected void OnSearchReplaceAllButtonClicked (object sender, EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                Document doc = documents[lastSelectedPage];
                                String searchText = searchEntry.ActiveText;
                                String replaceText = searchReplaceEntry.ActiveText;
                                doc.ReplaceAll(searchText, replaceText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                // shell replace
                            }
                            searchReplaceEntry.Entry.GrabFocus();
                        }

                        protected void OnSetBreakpointActionActivated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.ToggleBreakpoint();
                            }
                        }

                        protected void OnChangeDirectoryActionActivated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                if (System.IO.File.Exists(CurrentDocument.filename)) {
                                    string dir = System.IO.Path.GetDirectoryName(CurrentDocument.filename);
                                    System.IO.Directory.SetCurrentDirectory(dir);
                                }
                            }
                        }

                        protected void OnAboutActionActivated(object sender, System.EventArgs e) {
                            if (CurrentDocument != null) {
                                CurrentDocument.OnAbout();
                            }
                        }

                        protected void OnHistoryTabActionActivated(object sender, System.EventArgs e) {
                            // show history tab if active
                            if (! HistoryTabAction.Active) {
                                HistoryPage.Child.Hide();
                            } else {
                                HistoryPage.Child.Show();
                                ToolNotebook.Page = 3;
                            }
                        }

                        public static double currentTime() {
                            System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime(1970, 1, 1);
                            return t.TotalSeconds;
                        }

                        public static void wait(double seconds) {
                            if (seconds < .1) {
                                Thread.Sleep((int)(seconds * 1000));
                            } else {
                                double start = currentTime();
                                while (seconds > currentTime () - start) {
                                    while (Gtk.Application.EventsPending ()) {
                                        Gtk.Application.RunIteration();
                                    }
                                    Thread.Sleep(100);
                                }
                            }
                        }

                        protected void OnRegisterActionActivated(object sender, System.EventArgs e) {
                            Dictionary<string,string > response = ask(new List<string>() {_("Email"), _("Username"), _("Password"), _("Keyword")}, _("Register new user"));
                            if (response == null) {
                                return;
                            }
                            string email = response [_("Email")], user = response [_("Username")], password = response [_("Password")], keyword = response [_("Keyword")];
                            Chat chat = new Chat(this, "testname", "password");
                            wait(5);
                            chat.Send("admin", String.Format(
                                        "[register]\n" +
                                        "email: {0}\n" +
                                        "username: {1}\n" +
                                        "password: {2}\n" +
                                        "keyword: {3}\n", email, user, password, keyword)
                                    );
                            // send a special message to create account
                            // wait for response:
                            List<List<string >> messages = chat.ReceiveData();
                            int count = 0;
                            while (messages.Count == 0 && count < 10) {
                                messages = chat.ReceiveData();
                                wait(1);
                                Print(_("   waiting for confirmation...\n"));
                                count++;
                            }
                            Print(_("Received messages:\n"));
                            foreach (List<string> message in messages) {
                                Print(message [0]);
                                Print(" ");
                                Print(message [1]);
                                Print("\n");
                            }
                            chat.Close();
                        }

                        public void Login() {
                            Invoke( delegate {
                                OnLoginActionActivated(null, null);
                            });
                        }

                        public void Login(string username, string password) {
                            connection = new Chat(this, username, password);
                        }

                        public void OpenFromCloud (string filename) {
                            if (connection != null) {
                                connection.GetFileFromCloud(filename);
                            } else {
                                ErrorLine(_("You need to login before using chat."));
                            }
                        }

                        public void SaveToCloud(string filename) {
                            if (connection != null) {
                                string basename = System.IO.Path.GetFileName(filename);
                                if (!connection.SaveFileToCloud(filename, basename)) {
                                    ErrorLine(String.Format(_("Failed to save the file named '{0}' to the Calico Cloud."), basename));
                                }
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnLoginActionActivated(object sender, System.EventArgs e) {
                            Dictionary<string,string > response = (Dictionary<string,string>)ask(new List<string>() {_("Username"), _("Password")}, _("Login"));
                            if (response == null) {
                                return;
                            }
                            connection = new Chat(this,
                                    response [_("Username")].ToString(),
                                    response [_("Password")].ToString());
                            ChatTab.Active = true;
                            vpaned1.Show();
                            ChatCommand.GrabFocus();
                        }

                        protected void OnChatTabActionActivated(object sender, System.EventArgs e) {
                            if (vpaned1.Visible) {
                                vpaned1.Hide();
                            } else {
                                vpaned1.Show();
                                ChatCommand.GrabFocus();
                            }
                        }

                        public static Dictionary<string,string> ask(IList<string> question, string title) {
                            ManualResetEvent ev = new ManualResetEvent(false);
                            Dictionary<string,Gtk.Entry> entries = new Dictionary<string,Gtk.Entry>();
                            Dictionary<string,string > responses = null;
                            Invoke(delegate {
                                Gtk.MessageDialog fc = new Gtk.MessageDialog(_mainWindow,
                                    0, Gtk.MessageType.Question,
                                    Gtk.ButtonsType.OkCancel,
                                    title);
                                foreach (string choice in (IList<string>)question) {
                                    Gtk.HBox hbox = new Gtk.HBox();
                                    Gtk.Label label = new Gtk.Label(choice);
                                    Gtk.Entry entry = new Gtk.Entry();
                                    if (choice.ToLower() == "password" || choice == _("Password")) {
                                        entry.Visibility = false;
                                    }
                                    entries [choice] = entry;
                                    hbox.PackStart(label);
                                    hbox.PackStart(entry);
                                    fc.VBox.PackStart(hbox);
                                }
                                fc.ShowAll();
                                if (fc.Run() == (int)Gtk.ResponseType.Ok) {
                                    responses = new Dictionary<string,string>();
                                    foreach (string choice in entries.Keys) {
                                        responses [choice] = ((Gtk.Entry)entries [choice]).Text;
                                    }
                                }
                                fc.Destroy();
                                ev.Set();
                            }
                            );
                            ev.WaitOne();
                            return responses;
                        }

                        public static string pickOne(String question, string title, string [] options) {
                            ManualResetEvent ev = new ManualResetEvent(false);
                            string response = null;
                            Invoke(delegate {
                                Gtk.MessageDialog fc = new Gtk.MessageDialog(_mainWindow,
                                    0, Gtk.MessageType.Question,
                                    Gtk.ButtonsType.OkCancel,
                                    title);
                                Gtk.HBox hbox = new Gtk.HBox();
                                Gtk.Label label = new Gtk.Label(question);
                                Gtk.ComboBoxEntry entry = new Gtk.ComboBoxEntry(options);
                                hbox.PackStart(label);
                                hbox.PackStart(entry);
                                fc.VBox.PackStart(hbox);
                                fc.ShowAll();
                                if (fc.Run() == (int)Gtk.ResponseType.Ok) {
                                    response = entry.ActiveText;
                                }
                                fc.Destroy();
                                ev.Set();
                            });
                            ev.WaitOne();
                            return response;
                        }

                        public static string AcceptBlast(string title) {
                            ManualResetEvent ev = new ManualResetEvent(false);
                            dialogResponse = null;
                            Invoke(delegate {
                                Gtk.Dialog fc = new Gtk.Dialog(title, _mainWindow, 0);
                                fc.VBox.PackStart(new Gtk.Label(_("Accept this script?")));
                                fc.VBox.PackStart(new Gtk.Label(title));

                                Gtk.Button button = new Gtk.Button(_("Accept"));
                                fc.AddActionWidget(button, Gtk.ResponseType.Ok);
                                button.Clicked += (o, a) => DialogHandler(o, a, fc);

                                button = new Gtk.Button(_("Decline"));
                                button.Clicked += (o, a) => DialogHandler(o, a, fc);
                                fc.AddActionWidget(button, Gtk.ResponseType.Ok);

                                fc.ShowAll();
                                fc.Run();
                                fc.Destroy();
                                ev.Set();
                            }
                            );
                            ev.WaitOne();
                            return dialogResponse;
                        }

                        public void ReceiveBlast(string address, string mode, string filename, string code) {
                            //PrintLine(String.Format("{0}: {1} {2}", address, mode, filename));
                            // type: cloud-save, cloud-open, open, run, save
                            if (address.StartsWith(connection.user + "@")) {
                                // Ignore blasts from yourself
                                PrintLine("Blast was received from self.");
                                return;
                            }
                            if (address.StartsWith("admin@") || // always except from admin
                                    AcceptBlast(String.Format(_("Blast: Accept '{0}' from '{1}'?"), filename, address)) == _("Accept")) {
                                string tempPath = null;
                                if (mode == "cloud-save" || mode == "cloud-open") {
                                    tempPath = (string)config.GetValue("config", "cloud-path");
                                } else {
                                    tempPath = System.IO.Path.GetTempPath();
                                }
                                filename = System.IO.Path.Combine(tempPath, filename);
                                string language = manager.GetLanguageFromExtension(filename);
                                System.IO.StreamWriter sw = new System.IO.StreamWriter(filename, false, Encoding.ASCII);
                                sw.Write(code);
                                sw.Close();
                                Invoke(delegate {
                                    if (mode == "open" || mode == "cloud-open")
                                    Open(filename, language);
                                    else if (mode == "run")
                                    ExecuteInBackground(filename, language);
                                    else if (mode == "save")
                                    PrintLine(String.Format(_("File '{0}' saved."), filename));
                                    else if (mode == "cloud-save")
                                    PrintLine(String.Format(_("File '{0}' synched."), filename));
                                });
                            } else {
                                PrintLine(String.Format(_("Blast '{0}' from '{1}' declined."), filename, address));
                            }
                        }

                        protected void OnBlastScriptActionActivated(object sender, System.EventArgs e) {
                            // Blast a script:
                            if (connection != null && CurrentDocument != null) {
                                connection.Send("admin",
                                        String.Format("[blast]\n" +
                                            "from: {0}\n" +
                                            "type: {1}\n" +
                                            "file: {2}\n" +
                                            "{3}",
                                            connection.user,
                                            CurrentDocument.DocumentType,
                                            CurrentDocument.basename,
                                            CurrentDocument.GetText())
                                        );
                            }
                        }

                        protected void OnZoom100ActionActivated(object sender, System.EventArgs e) {
                            config.SetValue("config", "font-size", "int", 10240); // default
                            DefaultZoom();
                        }

                        protected void OnPropertyTabActionActivated(object sender, System.EventArgs e) {
                            if (property_notebook.Visible) {
                                property_notebook.Hide();
                            } else {
                                property_notebook.Show();
                            }
                        }

                        protected void OnButton8Clicked(object sender, System.EventArgs e) {
                            OnZoomInActionActivated(sender, e);
                        }

                        protected void OnButton9Clicked(object sender, System.EventArgs e) {
                            OnZoomOutActionActivated(sender, e);
                        }

                        protected void OnButton10Clicked(object sender, System.EventArgs e) {
                            OnZoom100ActionActivated(sender, e);
                        }

                        protected void OnInstallNewAddonActionActivated(object sender, System.EventArgs e) {
                            inform(_("No addons are currently available"));
                        }

                        void moveEditorNotebookToMain() {
                            // FIXME: get in same order
                            for(int idx=EditorNotebook.NPages - 1; idx >= 0; idx--) {
                                Gtk.Widget page = EditorNotebook.GetNthPage(idx);
                                Gtk.Widget l = EditorNotebook.GetTabLabel(page);
                                EditorNotebook.RemovePage(idx);
                                MainNotebook.AppendPage(page, l);
                                MainNotebook.SetTabReorderable(page, true);
                                MainNotebook.SetTabDetachable(page, true);            
                            }
                            EditorNotebook.Hide();
                        }


                        public void SwapTool() {
                            Invoke( delegate {
                                if (EditorNotebook.Visible) {
                                    // Empty it to MainNoteBook
                                    moveEditorNotebookToMain();
                                } else { 
                                    // Show it, and load it with Documents from MainNotebook
                                    // FIXME: get in same order
                                    for(int idx=MainNotebook.NPages - 1; idx >= 0; idx--) {
                                        Gtk.Widget page = MainNotebook.GetNthPage(idx);
                                        if (documents.ContainsKey(page)) { // it is a document
                                            Document doc = documents[page];
                                            if (doc.preferredNotebook == "editor") {
                                                Gtk.Widget l = MainNotebook.GetTabLabel(page);
                                                MainNotebook.RemovePage(idx);
                                                EditorNotebook.AppendPage(page, l);
                                                EditorNotebook.SetTabReorderable(page, true);
                                                EditorNotebook.SetTabDetachable(page, true);            
                                            }
                                        }
                                    }
                                    EditorNotebook.Show();
                                }
                            });
                        }

                        protected void OnButton11Clicked(object sender, System.EventArgs e) {
                            // Move all editor tabs back and forth between EditorNotebook and MainNotebook
                            toolSwapped = ! toolSwapped;
                            SwapTool();
                        }

                        public void SwapVertical() {
                            Invoke(delegate {
                                if (vpaned2.Child1 == notebook_docs) { // normal
                                    vpaned2.Remove(NotebookPane);
                                    vpaned2.Remove(notebook_docs);
                                    vpaned2.Add1(NotebookPane);
                                    vpaned2.Add2(notebook_docs);
                                } else {
                                    vpaned2.Remove(NotebookPane);
                                    vpaned2.Remove(notebook_docs);
                                    vpaned2.Add1(notebook_docs);
                                    vpaned2.Add2(NotebookPane);
                                }
                            });
                        }

                        protected void OnSwapVerticalClicked(object sender, System.EventArgs e) {
                            // Swap the vertical parts of MainNotebook
                            verticalSwapped = ! verticalSwapped;
                            SwapVertical();
                        }

                        public void SwapHorizontal() {
                            if (!EditorNotebook.Visible) {
                                OnButton11Clicked(null, null); // show EditorNotebook
                            }
                            Invoke(delegate {
                                if (hpaned2.Child1 == vpaned2) {
                                    hpaned2.Remove(vpaned2);
                                    hpaned2.Remove(editor_docs);
                                    hpaned2.Add1(editor_docs);
                                    hpaned2.Add2(vpaned2);
                                } else {
                                    hpaned2.Remove(vpaned2);
                                    hpaned2.Remove(editor_docs);
                                    hpaned2.Add1(vpaned2);
                                    hpaned2.Add2(editor_docs);
                                }
                            });
                        }

                        protected void OnSwapHorizontalClicked(object sender, System.EventArgs e) {
                            // Swap MainNotebook and EditorNotebook
                            horizontalSwapped = ! horizontalSwapped;
                            SwapHorizontal();
                        }

                        protected void OnFindNextActionActivated(object sender, EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                documents[lastSelectedPage].SearchNext(searchEntry.ActiveText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                if (history.SearchPrevious(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                            }
                        }

                        protected void OnFindPreviousActionActivated(object sender, EventArgs e) {
                            if (documents.ContainsKey(lastSelectedPage)) {
                                documents[lastSelectedPage].SearchPrevious(searchEntry.ActiveText);
                            } else if (lastSelectedPage == searchForPage(ShellEditor)) {
                                if (history.SearchNext(searchEntry.ActiveText)) {
                                    ShellEditor.Text = history.update();
                                    SetCurrentLanguage(history.CurrentLanguage);
                                    UpdateUpDownArrows();
                                }
                            }
                        }

                        public void SendData(string to, string text) {
                            if (connection != null) {
                                connection.SendData(to, text);
                            } else {
                                throw new Exception(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        public void SendEvent(string to, string text) {
                            if (connection != null) {
                                connection.SendEvent(to, text);
                            } else {
                                throw new Exception(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        public void SendMessage(string to, string text) {
                            if (connection != null) {
                                connection.Send(to, text);
                            } else {
                                throw new Exception(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        public List<List<string>> ReceiveData() {
                            if (connection != null) {
                                return connection.ReceiveData();
                            } else {
                                throw new Exception(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        public List<List<string>> ReceiveEvents() {
                            if (connection != null) {
                                return connection.ReceiveEvents();
                            } else {
                                throw new Exception(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnSyncToCloudActionActivated (object sender, System.EventArgs e)
                        {
                            // copy all local cloud files to cloud
                            if (connection != null) {
                                // for all local files
                                string cloud_path = (string)config.GetValue("config", "cloud-path");
                                if (!System.IO.Directory.Exists(cloud_path)) {
                                    System.IO.Directory.CreateDirectory(cloud_path);
                                }
                                DirectoryInfo dirInfo = new DirectoryInfo(cloud_path);
                                foreach (FileInfo f in dirInfo.GetFiles("*.*")) {
                                    SaveToCloud(f.FullName);
                                }
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnSyncFromCloudActionActivated (object sender, System.EventArgs e)
                        {
                            // copy all cloud files to local cloud
                            if (connection != null) {
                                // make sure that there is a cloud directory
                                string cloud_path = (string)config.GetValue("config", "cloud-path");
                                if (!System.IO.Directory.Exists(cloud_path)) {
                                    System.IO.Directory.CreateDirectory(cloud_path);
                                }
                                connection.Send("admin", "[sync-from-cloud]");
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        public void OnOpenFromCloudCallback (string [] list) {
                            string response = pickOne(_("Open"), _("Open Cloud Filename"), list);
                            if (response == null) {
                                ErrorLine(_("Open a file from the Calico Cloud aborted."));
                                return;
                            }
                            // get from cloud, put in local cloud dir:
                            if (connection.GetFileFromCloud(response.Trim())) {
                                // it will open when received...
                            } else {
                                ErrorLine(String.Format(_("Failed to get the file named '{0}' from the Calico Cloud."), response));
                            }
                        }

                        public void OnDeleteFromCloudCallback (string [] list) {
                            string response = pickOne(_("Delete"), _("Delete Cloud Filename"), list);
                            if (response == null) {
                                ErrorLine(_("Delete a file from the Calico Cloud aborted."));
                                return;
                            }
                            // get from cloud, put in local cloud dir:
                            connection.DeleteFileFromCloud(response.Trim());
                            string filename = System.IO.Path.Combine((string)config.GetValue("config", "cloud-path"), 
                                    response.Trim());
                            if (System.IO.File.Exists(filename)) {
                                System.IO.File.Delete(filename);
                            }
                        }

                        protected void OnOpenFromCloudActionActivated (object sender, System.EventArgs e)
                        {
                            if (connection != null) {
                                connection.Send("admin", "[list-cloud]"); // when received, process from there
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnDeleteFromCloudActionActivated (object sender, System.EventArgs e)
                        {
                            if (connection != null) {
                                connection.Send("admin", "[delete-list-cloud]"); // when received, process from there
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnRemoveModuleActionActivated (object sender, System.EventArgs e) {
                            if (CurrentDocument != null)
                                CurrentDocument.RemoveModule();
                        }

                        protected void OnSaveToCloudActionActivated (object sender, System.EventArgs e)
                        {
                            // save to local cloud, and in the cloud
                            if (connection != null) {
                                if (CurrentDocument != null) { // current document
                                    string filename = CurrentDocument.basename;
                                    string cloud_path = (string)config.GetValue("config", "cloud-path");
                                    // save in local cloud dir:
                                    if (!System.IO.Directory.Exists(cloud_path)) {
                                        System.IO.Directory.CreateDirectory(cloud_path);
                                    }
                                    if (CurrentDocument.filename == null) {
                                        Dictionary<string,string > response = ask(new List<string>() {_("Filename")}, _("Cloud Filename")); 
                                        if (response == null) {
                                            ErrorLine(_("Save to the Calico Cloud aborted."));
                                            return;
                                        }
                                        filename = response[_("Filename")];
                                    }
                                    if (filename == "") {
                                        ErrorLine(_("File must have a proper name to save to the Calico Cloud."));
                                        return;
                                    }
                                    filename = System.IO.Path.Combine(cloud_path, filename);
                                    string basename = System.IO.Path.GetFileName(filename);
                                    CurrentDocument.filename = filename;
                                    CurrentDocument.basename = basename;
                                    CurrentDocument.Save(true); // force save, even if not dirty
                                    // save in cloud:
                                    if (!connection.SaveFileToCloud(filename, basename)) {
                                        ErrorLine(String.Format(_("Failed to save the file named '{0}' to the Calico Cloud."), basename));
                                    } else {
                                        CurrentDocument.inCloud = true; // will mirror a save to cloud when saved locally
                                    }
                                } else {
                                    ErrorLine(_("You need to select an open document before attempting to save in the Calico Cloud."));
                                }
                            } else {
                                ErrorLine(_("You need to login before using the Calico Cloud."));
                            }
                        }

                        protected void OnResetOptionsActionActivated (object sender, System.EventArgs e) {
                            // reset config
                            // put all things back
                            // restart
                            if (yesno(_("Are you sure you want to reset the configuration and exit?"))) {
                                string config_path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData);
                                config_path = System.IO.Path.Combine(config_path, "calico", "config.xml");
                                config = new Config(config_path, true);
                                RequestQuit();
                            }
                        }

                        public string GetEnvironment() {
                            return environment;
                        }

                        public void SetVariable(string name, object value) {
                            manager.SetVariable(name, value);
                        }

                        public object TryGetVariable(string name) {
                            return manager.TryGetVariable(name);
                        }

                        public object GetVariable(string name) {
                            return manager.GetVariable(name);
                        }

                        public IList<string> GetVariableNames() {
                            return manager.GetVariableNames();
                        }

                        // -----------------------------------------------------------------------------
                        // IPython stuff:

                        // FIXME: add:
                        // PNG, JPEG

                        public AudioRepresentation Audio(string filename) {
                            return Representation.Audio(filename);
                        }

                        public MimeRepresentation HTML(string text) {
                            return Representation.HTML(text);
                        }

                        public MimeRepresentation SVG(string text) {
                            return Representation.SVG(text);
                        }

                        public MimeRepresentation JSON(string text) {
                            return Representation.JSON(text);
                        }

                        public MimeRepresentation Latex(string text) {
                            return Representation.Latex(text);
                        }

                        public MimeRepresentation Math(string text) {
                            return Representation.Math(text);
                        }

                        public MimeRepresentation Javascript(string text) {
                            return Representation.Javascript(text);
                        }

                        public MimeRepresentation FileLink(string filename) {
                            return FileLink(filename);
                        }

                        public MimeRepresentation FileLinks(string directory) {
                            return Representation.FileLinks(directory);
                        }

                        public MimeRepresentation YouTubeVideo(string id) {
                            return Representation.YouTubeVideo(id);
                        }

                        public MimeRepresentation VimeoVideo(string id) {
                            return Representation.VimeoVideo(id);
                        }

                        public MimeRepresentation IFrame(string url, int width, int height) {
                            return Representation.IFrame(url, width, height);
                        }

                        public MimeRepresentation Javascript(string text, string lib) {
                            return Representation.Javascript(text, lib);
                        }

                        public ImageRepresentation Image(string filename) {
                            return Representation.Image(filename);
                        }

                        public MimeRepresentation Table(IList<IList> list) {
                            return Representation.Table(list);
                        }

                        public Widgets.Widget makeBoundedFloatTextWidget() {
                            return BoundedFloatTextWidget();
                        }
                        public Widgets.Widget makeBoundedIntTextWidget() {
                            return BoundedIntTextWidget();
                        }
                        public Widgets.ButtonWidget makeButtonWidget() {
                            return ButtonWidget();
                        }
                        public Widgets.CheckboxWidget makeCheckboxWidget() {
                            return CheckboxWidget();
                        }
                        public Widgets.DropdownWidget makeDropdownWidget() {
                            return DropdownWidget();
                        }
                        public Widgets.FloatProgressWidget makeFloatProgressWidget() {
                            return FloatProgressWidget();
                        }
                        public Widgets.FloatSliderWidget makeFloatSliderWidget() {
                            return FloatSliderWidget();
                        }
                        public Widgets.FloatTextWidget makeFloatTextWidget() {
                            return FloatTextWidget();
                        }
                        public Widgets.HTMLWidget makeHTMLWidget() {
                            return HTMLWidget();
                        }
                        public Widgets.ImageWidget makeImageWidget() {
                            return ImageWidget();
                        }
                        public Widgets.IntProgressWidget makeIntProgressWidget() {
                            return IntProgressWidget();
                        }
                        public Widgets.IntSliderWidget makeIntSliderWidget() {
                            return IntSliderWidget();
                        }
                        public Widgets.IntTextWidget makeIntTextWidget() {
                            return IntTextWidget();
                        }
                        public Widgets.LatexWidget makeLatexWidget() {
                            return LatexWidget();
                        }
                        public Widgets.RadioButtonsWidget makeRadioButtonsWidget() {
                            return RadioButtonsWidget();
                        }
                        public Widgets.SelectWidget makeSelectWidget() {
                            return SelectWidget();
                        }
                        public Widgets.TextWidget makeTextWidget() {
                            return TextWidget();
                        }
                        public Widgets.PasswordWidget makePasswordWidget() {
                            return PasswordWidget();
                        }
                        public Widgets.TextareaWidget makeTextareaWidget() {
                            return TextareaWidget();
                        }
                        public Widgets.ToggleButtonWidget makeToggleButtonWidget() {
                            return ToggleButtonWidget();
                        }
                        public Widgets.ToggleButtonsWidget makeToggleButtonsWidget() {
                            return ToggleButtonsWidget();
                        }
                        public Widgets.ContainerWidget makeContainerWidget() {
                            return ContainerWidget();
                        }
                        public Widgets.PopupWidget makePopupWidget() {
                            return PopupWidget();
                        }
                        public Widgets.AccordionWidget makeAccordionWidget() {
                            return AccordionWidget();
                        }
                        public Widgets.TabWidget makeTabWidget() {
                            return TabWidget();
                        }
                        public Widgets.CameraWidget makeCameraWidget() {
                            return CameraWidget();
                        }

                        public Widgets.BoundedFloatTextWidget BoundedFloatTextWidget(double min=0.0, 
                                double max=100.0, 
                                double step=0.1, 
                                double value=0.0, 
                                string description="", 
                                bool disabled=false, 
                                bool visible=true)  {
                            return new Widgets.BoundedFloatTextWidget(ZMQServer.session,
                                    min, max, step, 
                                    value, description, 
                                    disabled, visible);
                        }

                        public Widgets.BoundedIntTextWidget BoundedIntTextWidget(Int64 min=0, 
                                Int64 max=100, 
                                Int64 step=1, 
                                Int64 value=0, 
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.BoundedIntTextWidget(ZMQServer.session,
                                    min, max, step, 
                                    value, description, 
                                    disabled, visible);
                        }

                        public Widgets.ButtonWidget ButtonWidget(string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.ButtonWidget(ZMQServer.session, description, 
                                    disabled, visible);
                        }

                        public Widgets.CheckboxWidget CheckboxWidget(string description="", 
                                bool value=false,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.CheckboxWidget(ZMQServer.session,
                                    description, 
                                    value,
                                    disabled, 
                                    visible);
                        }

                        public Widgets.DropdownWidget DropdownWidget(string description="", 
                                IList value_names=null,
                                IDictionary values=null,
                                string value_name=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.DropdownWidget(ZMQServer.session,
                                    description, 
                                    value_names,
                                    values,
                                    value_name,
                                    disabled, 
                                    visible);
                        }

                        public Widgets.FloatProgressWidget FloatProgressWidget(double min=0.0, 
                                double max=100.0, 
                                double step=0.1, 
                                double value=0.0, 
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.FloatProgressWidget(ZMQServer.session,
                                    min, 
                                    max, 
                                    step, 
                                    value, 
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.FloatSliderWidget FloatSliderWidget(double min=0.0, 
                                double max=100.0, 
                                double step=0.1, 
                                double value=0.0, 
                                string description="",
                                string orientation="horizontal", 
                                bool readout=true, 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.FloatSliderWidget(ZMQServer.session, min, max, 
                                    step, value, description,
                                    orientation, readout, 
                                    disabled, visible);
                        }

                        public Widgets.FloatTextWidget FloatTextWidget(double value=0.0,
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.FloatTextWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.HTMLWidget HTMLWidget(string value="",
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.HTMLWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.ImageWidget ImageWidget(byte [] value=null, 
                                string format="png", 
                                string width="", 
                                string height="", 
                                string _b64value="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.ImageWidget(ZMQServer.session,
                                    value, 
                                    format, 
                                    width, 
                                    height, 
                                    _b64value, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.IntProgressWidget IntProgressWidget(Int64 min=0, 
                                Int64 max=100, 
                                Int64 step=1, 
                                Int64 value=0, 
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.IntProgressWidget(ZMQServer.session,
                                    min, 
                                    max, 
                                    step, 
                                    value, 
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.IntSliderWidget IntSliderWidget(Int64 min=0, 
                                Int64 max=100, 
                                Int64 step=1, 
                                Int64 value=0, 
                                string description="", 
                                bool disabled=false, 
                                string orientation="horizontal", 
                                bool readout=true, 
                                bool visible=true) {
                            return new Widgets.IntSliderWidget(ZMQServer.session,
                                    min, 
                                    max, 
                                    step, 
                                    value, 
                                    description, 
                                    disabled, 
                                    orientation, 
                                    readout, 
                                    visible);
                        }

                        public Widgets.IntTextWidget IntTextWidget(Int64 value=0,
                                string description="",
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.IntTextWidget(ZMQServer.session,
                                    value,
                                    description,
                                    disabled, 
                                    visible=true);
                        }

                        public Widgets.LatexWidget LatexWidget(string value="",
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.LatexWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.RadioButtonsWidget RadioButtonsWidget(string description="", 
                                IList value_names=null,
                                IDictionary values=null,
                                string value_name=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.RadioButtonsWidget(ZMQServer.session,
                                    description, 
                                    value_names,
                                    values,
                                    value_name,
                                    disabled, 
                                    visible);
                        }

                        public Widgets.SelectWidget SelectWidget(string description="", 
                                IList value_names=null,
                                IDictionary values=null,
                                string value_name=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.SelectWidget(ZMQServer.session,
                                    description, 
                                    value_names,
                                    values,
                                    value_name,
                                    disabled, 
                                    visible);
                        }

                        public Widgets.TextWidget TextWidget(string value="",
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.TextWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.PasswordWidget PasswordWidget(string value="",
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.PasswordWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.TextareaWidget TextareaWidget(string value="",
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.TextareaWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.ToggleButtonWidget ToggleButtonWidget(bool value=false,
                                string description="", 
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.ToggleButtonWidget(ZMQServer.session,
                                    value,
                                    description, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.ToggleButtonsWidget ToggleButtonsWidget(string description="", 
                                IList value_names=null,
                                IDictionary values=null,
                                string value_name=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.ToggleButtonsWidget(ZMQServer.session,
                                    description, 
                                    value_names,
                                    values,
                                    value_name,
                                    disabled, 
                                    visible); 
                        }

                        public Widgets.ContainerWidget ContainerWidget(IList<Widgets.Widget> children=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.ContainerWidget(ZMQServer.session,
                                    children, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.AccordionWidget AccordionWidget(IList<Widgets.Widget> children=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.AccordionWidget(ZMQServer.session,
                                    children, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.TabWidget TabWidget(IList<Widgets.Widget> children=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.TabWidget(ZMQServer.session,
                                    children, 
                                    disabled, 
                                    visible);
                        }

                        public Widgets.PopupWidget PopupWidget(string button_text="", 
                                string description="", 
                                IList<Widgets.Widget> children=null,
                                bool disabled=false, 
                                bool visible=true) {
                            return new Widgets.PopupWidget(ZMQServer.session,
                                    button_text, 
                                    description, 
                                    children,
                                    disabled, 
                                    visible);
                        }

                        public Widgets.CameraWidget CameraWidget() {
                            return new Widgets.CameraWidget(ZMQServer.session);
                        }

                        public Widgets.GeoChart GeoChart(IList keys, IDictionary data) {
                            return new Widgets.GeoChart(ZMQServer.session, keys, data);
                        }

                        public Widgets.GeoChart GeoChart(IList keys, IDictionary data, 
                                IDictionary options) {
                            return new Widgets.GeoChart(ZMQServer.session, keys, data, options);
                        }

                        public Widgets.GoogleChart GoogleChart(string type, IList data) {
                            return new Widgets.GoogleChart(ZMQServer.session, type, data);
                        }

                        public Widgets.GoogleChart GoogleChart(string type, IList data, IDictionary options) {
                            return new Widgets.GoogleChart(ZMQServer.session, type, data, options);
                        }

                        public Widgets.GoogleChart GoogleChart(string type, IList keys, IList data) {
                            return new Widgets.GoogleChart(ZMQServer.session, type, keys, data);
                        }

                        public Widgets.GoogleChart GoogleChart(string type, IList keys, IList data,
                                IDictionary options) {
                            return new Widgets.GoogleChart(ZMQServer.session, type, keys, data, options);
                        }

                        public Widgets.ScatterChart ScatterChart(IList keys, IList data,
                                IDictionary options) {
                            return new Widgets.ScatterChart(ZMQServer.session, keys, data, options);
                        }

                        public Widgets.ScatterChart ScatterChart(IList keys, IList data) {
                            return new Widgets.ScatterChart(ZMQServer.session, keys, data);
                        }

                        public Widgets.LineChart LineChart(IList data,
                                IDictionary options) {
                            return new Widgets.LineChart(ZMQServer.session, data, options);
                        }

                        public Widgets.LineChart LineChart(IList data) {
                            return new Widgets.LineChart(ZMQServer.session, data);
                        }

                        // ---------- end of Widgets

                        public ZMQServer.Session GetSession() {
                            // return zmq server session
                            return ZMQServer.session;
                        }

                        public ZMQServer.Client Client(string path=null, string sshserver=null) {
                            return new ZMQServer.Client(path, sshserver);
                        }

                        // FIXME: add:
                        // display_pretty, display_audio, display_video

                        public void display(object obj1) {
                            display(obj1, new object [0]);
                        }

                        public void display_png(object obj) {
                            ZMQServer.session.display_mimetype(obj, "image/png");
                        }
                        public void display_jpeg(object obj) {
                            ZMQServer.session.display_mimetype(obj, "image/jpeg");
                        }
                        public void display_html(object obj) {
                            ZMQServer.session.display_mimetype(obj, "text/html");
                        }
                        public void display_javascript(object obj) {
                            ZMQServer.session.display_mimetype(obj, "application/javascript");
                        }
                        public void display_json(object obj) {
                            ZMQServer.session.display_mimetype(obj, "application/json");
                        }
                        public void display_latex(object obj) {
                            ZMQServer.session.display_mimetype(obj, "text/latex");
                        }
                        public void display_pdf(object obj) {
                            ZMQServer.session.display_mimetype(obj, "application/pdf");
                        }
                        public void display_svg(object obj) {
                            ZMQServer.session.display_mimetype(obj, "image/svg+xml");
                        }

                        public void animate(object obj1) {
                            clear_output(true);
                            display(obj1, new object [0]);
                        }

                        public void animate(object obj1, params object [] objs) {
                            clear_output(true);
                            display(obj1, objs);
                        }

                        public void animate_png(object obj) {
                            clear_output(true);
                            display_png(obj);	    
                        }
                        public void animate_jpeg(object obj) {
                            clear_output(true);
                            display_jpeg(obj);	    
                        }
                        public void animate_html(object obj) {
                            clear_output(true);
                            display_html(obj);	    
                        }
                        public void animate_javascript(object obj) {
                            clear_output(true);
                            display_javascript(obj);	    
                        }
                        public void animate_json(object obj) {
                            clear_output(true);
                            display_json(obj);	    
                        }
                        public void animate_latex(object obj) {
                            clear_output(true);
                            display_latex(obj);	    
                        }
                        public void animate_pdf(object obj) {
                            clear_output(true);
                            display_pdf(obj);	    
                        }
                        public void animate_svg(object obj) {
                            clear_output(true);
                            display_svg(obj);	    
                        }

                        public string input(string prompt="") {
                            return ZMQServer.session.input(prompt);
                        }

                        // These two display functions are written to work with Scheme
                        // which is having issues with calling params
                        public void display(object obj1, params object [] objs) {
                            if (ZMQServer.session != null) {
                                ZMQServer.session.display(obj1);
                                foreach (object obj in objs) {
                                    ZMQServer.session.display(obj);
                                }
                            } else {
                                PrintLine(Repr(obj1));
                                foreach (object obj in objs) {
                                    PrintLine(Repr(obj));
                                }
                            }
                        }

                        public void clear_output() {
                            if (ZMQServer.session != null)
                                ZMQServer.session.clear_output();
                            else {
                                // pass
                            }
                        }

                        public void clear_output(bool wait) {
                            if (ZMQServer.session != null)
                                ZMQServer.session.clear_output(wait);
                            else {
                                // pass
                            }
                        }

                        public IDictionary<string,object> GetHelpOnFunctionCall(string oname) {
                            // Given an object name, get help on it from the current
                            // language, environment:
                            if (CurrentLanguage != null && manager[CurrentLanguage].engine != null) {
                                return manager[CurrentLanguage].engine.GetHelpOnFunctionCall(oname);
                            } else {
                                return manager["python"].engine.GetHelpOnFunctionCall(oname);
                            }
                        }

                        public string GetHelpOn(string expression) {
                            if (CurrentLanguage != null && manager[CurrentLanguage].engine != null) {
                                return manager[CurrentLanguage].engine.GetHelpOn(expression);
                            } else {
                                return manager["python"].engine.GetHelpOn(expression);
                            }
                        }

                        public TabCompletion GetTabCompletion(string to_match) {
                            if (CurrentLanguage != null && manager[CurrentLanguage].engine != null) {
                                return manager[CurrentLanguage].engine.GetTabCompletion(to_match);
                            } else {
                                return manager["python"].engine.GetTabCompletion(to_match);
                            }
                        }

                        public TabCompletion GetTabCompletion(string to_match, 
                                Mono.TextEditor.TextEditor shell) {
                            if (CurrentLanguage != null && manager[CurrentLanguage].engine != null) {
                                return manager[CurrentLanguage].engine.GetTabCompletion(to_match, shell);
                            } else {
                                return manager["python"].engine.GetTabCompletion(to_match, shell);
                            }
                        }

                        public int GetGuiThreadID() {
                            return gui_thread_id;
                        }

                        public override string ToString() {
                            return String.Format("<Calico, version {0}, language {1}>", MainClass.Version, CurrentProperLanguage);
                        }
                    }
                    }

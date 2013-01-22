//
//  Runtime.cs
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
using System.IO;

// Path
using System.Threading;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using Calico;
using Mono.Terminal;

namespace Calico {
    public partial class CalicoConsole: MainWindow {
       
        public CalicoConsole(): base(){}

        public CalicoConsole(string[] args, LanguageManager manager, bool Debug, Config config, bool startREPL):
        base() {
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            manager.SetCalico(this);
            CurrentLanguage = "python";

            Gtk.Application.Invoke(delegate {
                gui_thread_id = Thread.CurrentThread.ManagedThreadId;
            });

            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }

            GLib.ExceptionManager.UnhandledException += HandleException;
            
            //manager.SetCalico(this);
            // FIXME: move to Python language
            manager ["python"].engine.Execute("from __future__ import division, with_statement, print_function;" +
                "import sys as _sys; _sys.setrecursionlimit(1000);" +
                "del division, with_statement, print_function, _sys", false);
            bool debug_handler = true;

            //Build();
            // Run this in in the GUI thread, after we start:
            //manager.PostSetup(this); 

            // Run this in in the GUI thread, after we start:
            Gtk.Application.Invoke(delegate {
                manager.PostSetup(this); });

            manager ["python"].engine.Execute("from __builtin__ import raw_input, input", false);

            // All done, show if minimized:
            //this.Present();

            foreach (string arg in args) {
                if (arg.StartsWith("--")) {
                    if (arg == "--debug-handler") {
                        debug_handler = false;
                    }
                } else {
                    CurrentLanguage = manager.GetLanguageFromExtension(arg);
                    //Console.WriteLine("NO GUI MODE" + CurrentLanguage + " "  + arg);
                    ExecuteFileInBackground(arg, CurrentLanguage);
                    //manager [CurrentLanguage].engine.ExecuteFile(arg);
                }
            }

  
            // Start up background updater
            GLib.Timeout.Add(500, UpdateGUI);

            if (startREPL) {
                executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                    REPL();
                    Environment.Exit(0);
                }));
                executeThread.IsBackground = true;
                executeThread.Start();
            } 
        }

        public new void HandleException(GLib.UnhandledExceptionArgs args) {
            Console.WriteLine(String.Format("Exception: {0}\n", args.ExceptionObject.ToString()));
        }

        public void REPL() {
            LineEditor le = new LineEditor("Calico", 1000);
            le.TabAtStartCompletes = false;
            string line, expr = "";
            string prompt = CurrentLanguage + "> ";
            string indent = "";
            bool is_unix, isatty, dumb;

            int p = (int)Environment.OSVersion.Platform;
            is_unix = (p == 4) || (p == 128);
#if NET_4_5
          isatty = !Console.IsInputRedirected && !Console.IsOutputRedirected;
#else
            isatty = true;
#endif
            if (is_unix) {
                string term = Environment.GetEnvironmentVariable("TERM");
                dumb = term == "dumb" || term == null || isatty == false;
            } else {
                dumb = false;
            }

            while ((line = getline(le, prompt, indent, dumb, isatty)) != null) {
                if (line.StartsWith(":")) {
                    string[] t = line.Split();
                    if (t [0] == ":lang") {
                        if (Array.Find(manager.getLanguages(), delegate(string lang) {
                            return lang == t [1];
                        }) != null) {
                            CurrentLanguage = t [1];
                        }
                        expr = "";
                        prompt = CurrentLanguage + "> ";
                        indent = "";
                    } else if (t [0].Contains("exit") || t [0].Contains("quit")) {
                        Console.WriteLine("Bye Bye");
                        // Gtk.Application.Quit();
                        Environment.Exit(0);
                    }
                } else {
                    if (expr != "") {
                        expr = expr + "\n" + line;
                    } else {
                        expr = line;
                    }
                    if (manager [CurrentLanguage].engine.ReadyToExecute(expr)) {
                        try {   
                            manager [CurrentLanguage].engine.Execute(expr);     
                        } catch (Exception e) {
                            Console.WriteLine(e);
                        }
                        expr = "";
                        prompt = CurrentLanguage + "> ";
                        indent = "";
                    } else {
                        prompt = repeat(".", CurrentLanguage.Length) + "> ";
                        Match match = Regex.Match(line, "^\t*");
                        if (match.Success) {
                            indent = match.Value;
                        }
                    }
                }
            }
        }

        public static string getline(LineEditor le, string prompt, string indent, bool dumb, bool isatty) {
            if (dumb) {
                if (isatty) {
                    Console.Write(prompt);
                }
                return Console.ReadLine();
            } else {
                return le.Edit(prompt, indent);
            }
        }

        public static string repeat(string s, int times) {
            string retval = "";
            for (int i=0; i < times; i++) {
                retval += s;
            }
            return retval;
        }

        public new void ExecuteFileInBackground(string filename, string language) {
            // This is run from text documents that don't run themselves:
            executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                manager [CurrentLanguage].engine.ExecuteFile(filename); // not in GUI thread
            }));
            executeThread.IsBackground = true;
            executeThread.Start();
        }

        public new void Print(Tag tag, string format) {
            System.Console.Write(format);
        }

        public new void Print(string format) {
            Print(Tag.Normal, format);
        }

        private bool UpdateGUI() {
            // update any pending requests
            while (Gtk.Application.EventsPending ()) {
                Gtk.Application.RunIteration();
            }
            // keep updating:
            return true;
        }

        /*
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
                        Gtk.Accelerator.Parse(String.Format("<control>{0}", count + 1), out key, out mod);
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
                        Open(null, language.name); };
                    file_menu.Submenu.ShowAll();
                } else if (!menu_item.Active && visible_languages.Contains(language.name)) {
                    // was active, but now is not
                    // Remove from examples menu
                    Gtk.MenuItem examples_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/ExamplesAction");
                    foreach (Gtk.MenuItem menu in ((Gtk.Menu)examples_menu.Submenu).AllChildren) {
                        // if correct one, remove it:
                        if (((Gtk.AccelLabel)menu.Child).Text == language.proper_name)
                            ((Gtk.Menu)examples_menu.Submenu).Remove(menu);
                    }
                    // Remove from New menu:
                    Gtk.MenuItem file_menu = (Gtk.MenuItem)UIManager.GetWidget("/menubar2/FileAction/NewAction");
                    foreach (Gtk.MenuItem menu in ((Gtk.Menu)file_menu.Submenu).AllChildren) {
                        // if correct one, remove it:
                        if (((Gtk.AccelLabel)menu.Child).Text == language.proper_name)
                            ((Gtk.Menu)file_menu.Submenu).Remove(menu);
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
            if (needInvoke()) {
                Gtk.Application.Invoke(delegate {
                    invoke(); });
            } else {
                invoke();
            }
        }

        public void ExecuteFileInBackground(string filename) {
            string language = manager.GetLanguageFromExtension(filename);
            ExecuteFileInBackground(filename, language);
        }
        
        public void ExecuteFileInBackground(string filename, string language) {
            // This is run from text documents that don't run themselves:
            executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
                Console.WriteLine(filename);
                CurrentLanguage = language;
                if (manager.languages.ContainsKey(language) && manager.languages [language].IsTextLanguage) {
                    ShellLanguage = language;
                }
                string dir = System.IO.Path.GetDirectoryName(filename);
                System.IO.Directory.SetCurrentDirectory(dir);
                manager [CurrentLanguage].engine.ExecuteFile(filename); // not in GUI thread
            }));
            executeThread.IsBackground = true;
            executeThread.Start();
        }


        public static string _(string message) {
            return global::Mono.Unix.Catalog.GetString(message);
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
        */
    }



    public partial class CalicoConsoleNoGUI: CalicoConsole{
        public CalicoConsoleNoGUI(string[] args, LanguageManager manager, bool Debug, Config config, bool startREPL){
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            manager.SetCalico(this);
            CurrentLanguage = "python";

            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }

            // FIXME: move to Python language
            manager ["python"].engine.Execute("from __future__ import division, with_statement, print_function;" +
                "import sys as _sys; _sys.setrecursionlimit(1000);" +
                "del division, with_statement, print_function, _sys", false);
            manager.PostSetup(this); 

            foreach (string arg in args) {
                if (!arg.StartsWith("--")) {
                    CurrentLanguage = manager.GetLanguageFromExtension(arg);
                    manager [CurrentLanguage].engine.ExecuteFile(arg);
                }
            }

            if (startREPL) {
                REPL();
            } else {
                Environment.Exit(0);
            }
        }
    }
}

// enter not on bottom row inserts, not executes

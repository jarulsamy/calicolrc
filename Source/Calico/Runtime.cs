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
	// ran without --nographics
       
        public CalicoConsole(): base(){}

        public CalicoConsole(string[] args, LanguageManager manager, bool Debug, Config config, bool startREPL,
			     int gui_thread_id):
        base() {
	    environment = "tty";
	    MainWindow.gui_thread_id = gui_thread_id;
            _mainWindow = this;
	    this.args = args;
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            manager.SetCalico(this);
            //configureIO();
            CurrentLanguage = "python";

            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }

            GLib.ExceptionManager.UnhandledException += HandleException;
            
            // Run this in in the GUI thread, after we start:
	    if (!((IList<string>)args).Contains("--nomodules")) {
		manager.PostSetup(this); 
	    }

	    manager ["python"].engine.Execute("from __builtin__ import raw_input, input", false);

            foreach (string arg in args) {
                if (arg.StartsWith("--")) {
		    if (arg.StartsWith("--lang=")) {
			string [] parts = arg.Split('=');
			TrySetCurrentLanguage(parts[1]);
		    }
                } else {
                    CurrentLanguage = manager.GetLanguageFromExtension(arg);
		    string filename = System.IO.Path.GetFullPath(arg);
		    string dirname = System.IO.Path.GetDirectoryName(filename);
		    if (dirname != "" && dirname != null) {
			if (System.IO.File.Exists(arg)) {
			    System.IO.Directory.SetCurrentDirectory(dirname);
			    ExecuteFileInBackground(filename, CurrentLanguage);
			} else {
			    Console.Error.WriteLine("Error: no such file '{0}'; skipping...", arg);
			}
		    }
                }
            }

            // Start up background updater
            GLib.Timeout.Add(500, UpdateGUI);

            if (startREPL) {
                executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
			    REPL(); // this will take care of shutdown
                }));
                executeThread.IsBackground = true;
                executeThread.Start();
            } else { //if (!((IList<string>)this.args).Contains("--noquit")) {
		//Environment.Exit(0);
		// do not exit, graphics must take care of exit
	    }
        }

        public new void HandleException(GLib.UnhandledExceptionArgs args) {
            Console.WriteLine(String.Format("Exception: {0}\n", args.ExceptionObject.ToString()));
        }

        public new Document GetDocument(string name) {
	    return null;
	}

        public new void Info(string message) {
	    Console.Write(message);
	}

        public new void Warn(string message) {
	    Console.Error.Write(message);
	}

	public new double ProgramSpeedValue {
	    get { return 100;}
	    set {}
	}

	public void usage() {
	    Console.WriteLine("Calico command-line usage:");
	    Console.WriteLine("   %lang LANGUAGE  - where LANGUAGE python, ruby, scheme, etc.");
	    Console.WriteLine("   %run FILENAME   - where FILENAME is a Calico Language program");
	    Console.WriteLine("   %help           - this message");
	    Console.WriteLine("   COMMAND         - a command in the current LANGAUGE");
	    Console.WriteLine("   ^d              - <control>d will exit");
	}

        public void REPL() {
	    ActivateLanguage(CurrentLanguage, "python");

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

            int counter = 0;
            while ((line = getline(le, prompt, indent, dumb, isatty)) != null) {
		/*
		if (line == "") {
		    if (counter > 1) {
			usage();
			counter = 0;
		    } else {
			counter++;
		    }
		    continue;
		}
		*/
                if (line.StartsWith("%")) {
                    string[] t = line.Split();
                    if (t [0] == "%lang" || t [0] == "%%lang" || t [0] == "%%%lang") {
                        if (Array.Find(manager.getLanguages(), delegate(string lang) {
                            return lang == t [1];
                        }) != null) {
			    ActivateLanguage(t[1], CurrentLanguage);
                        } 
                        expr = "";
                        prompt = CurrentLanguage + "> ";
                        indent = "";
                    } else if (t [0] == "%help") {
			usage();
                    } else if (t [0] == "%run") {
			string arg = t[1];
			CurrentLanguage = manager.GetLanguageFromExtension(arg);
			string filename = System.IO.Path.GetFullPath(arg);
			string dirname = System.IO.Path.GetDirectoryName(filename);
			if (dirname != "" && dirname != null) {
			    if (System.IO.File.Exists(arg)) {
				System.IO.Directory.SetCurrentDirectory(dirname);
				ExecuteFileInBackground(filename, CurrentLanguage); 
				prompt = CurrentLanguage + "> ";
				indent = "";
				expr = "";
			    } else {
				Console.Error.WriteLine("Error: no such file '{0}'; skipping...", arg);
			    }
			}
		    } else {
			usage();
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
	    // If running with graphics, we need to tell everything to stop:
	    if (!((IList<string>)this.args).Contains("--nographics")) {
		Environment.Exit(0);
	    }
        }

        public static string getline(LineEditor le, string prompt, string indent, bool dumb, bool isatty) {
            if (dumb) {
                if (isatty) {
                    Console.Write(prompt);
                }
                return Console.ReadLine();
            } else {
		string retval = "";
		try {
		    retval = le.Edit(prompt, indent);
		} catch {
		    // ignore
		}
		return retval;
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

	public new static bool yesno(string question) {
	    System.Console.WriteLine(question);
	    string answer = System.Console.ReadLine();
	    return (answer.Substring(0,1).ToLower() == "y");
	}

        public override void Print(Tag tag, string format) {
	    if (tag == Tag.Error) {
		System.Console.Error.Write(format);
	    } else {
		System.Console.Write(format);
	    }
        }

        private bool UpdateGUI() {
            // update any pending requests
            while (Gtk.Application.EventsPending ()) {
                Gtk.Application.RunIteration();
            }
            // keep updating:
            return true;
        }

    }

    public partial class CalicoConsoleNoGUI: CalicoConsole{
	// ran with --nographics
        public CalicoConsoleNoGUI(string[] args, LanguageManager manager, bool Debug, Config config, bool startREPL,
				  int gui_thread_id){
	    environment = "tty";
	    MainWindow.gui_thread_id = gui_thread_id;
            _mainWindow = this;
	    this.args = args;
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            manager.SetCalico(this);
            //configureIO();
            CurrentLanguage = "python";

            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }

	    if (!((IList<string>)args).Contains("--nomodules")) {
		manager.PostSetup(this); 
	    }

            foreach (string arg in args) {
                if (arg.StartsWith("--")) {
		    if (arg.StartsWith("--lang=")) {
			string [] parts = arg.Split('=');
			TrySetCurrentLanguage(parts[1]);
		    }
		} else {
                    CurrentLanguage = manager.GetLanguageFromExtension(arg);
		    string filename = System.IO.Path.GetFullPath(arg);
		    string dirname = System.IO.Path.GetDirectoryName(filename);
		    if (dirname != "" && dirname != null) {
			if (System.IO.File.Exists(arg)) {
			    System.IO.Directory.SetCurrentDirectory(dirname);
			    //System.Console.WriteLine("cd: " + dirname);
			    manager [CurrentLanguage].engine.ExecuteFile(filename);
			} else {
			    Console.Error.WriteLine("Error: no such file '{0}'; skipping...", arg);
			}
		    }
                }
            }

            if (startREPL) {
                REPL();
            } else if (!((IList<string>)this.args).Contains("--noquit")) {
		Environment.Exit(0);
	    }
        }
    }

    public partial class CalicoServer: CalicoConsole {
	string config_file = "";

        public CalicoServer (string[] args, LanguageManager manager, bool Debug, Config config,
			     int gui_thread_id) {
	    environment = "tty";
	    MainWindow.gui_thread_id = gui_thread_id;
            _mainWindow = this;
	    this.serverMode = true;
	    this.args = args;
            this.config = config;
            this.Debug = Debug;
            this.manager = manager;
            manager.SetCalico(this);
            configureIO();
            CurrentLanguage = "python";

            path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }

	    if (!((IList<string>)args).Contains("--nomodules")) {
		manager.PostSetup(this); 
	    }

	    config_file = "";
            for(int i = 0; i < args.Length; i++) {
		string arg = args[i];
                if (arg.StartsWith("--")) {
		    if (arg.StartsWith("--lang=")) {
			string [] parts = arg.Split('=');
			TrySetCurrentLanguage(parts[1]);
		    } else if (arg == "--profile-dir") {
			if ((i + 1) < args.Length && !args[i + 1].StartsWith("--")) {
			    i++;
			}
		    } else if (arg == "--server") {
			if ((i + 1) < args.Length && !args[i + 1].StartsWith("--")) {
			    config_file = args[i + 1];
			    i++;
			}
		    }
		} else {
                    CurrentLanguage = manager.GetLanguageFromExtension(arg);
		    string filename = System.IO.Path.GetFullPath(arg);
		    string dirname = System.IO.Path.GetDirectoryName(filename);
		    if (dirname != "" && dirname != null) {
			if (System.IO.File.Exists(arg)) {
			    System.IO.Directory.SetCurrentDirectory(dirname);
			    //System.Console.WriteLine("cd: " + dirname);
			    manager [CurrentLanguage].engine.ExecuteFile(filename);
			} else {
			    Console.Error.WriteLine("Error: no such file '{0}'; skipping...", arg);
			}
		    }
                }
            }
        }

	public override void Start() {
	    ZMQServer.Start(this, config_file);
	}

        public override void Print(Tag tag, string format) {
	    if (tag == Tag.Error) {
		ZMQServer.StdErrWrite(format);
	    } else {
		ZMQServer.StdOutWrite(format);
	    }
        }
    }
}


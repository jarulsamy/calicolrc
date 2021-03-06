//
//  Main.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011-2015 The Calico Project
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
using Gtk;
using System.Collections.Generic; // IList
using Mono.Unix;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using System.Threading;

namespace Calico {
    class MainClass {
        public static string Version = "4.0.1";
        public static bool verbose = false;

        /*
           private static bool IsApplicationRunningOnMono(string processName)
           {
           var processFound = 0;

           Process[] monoProcesses;
           ProcessModuleCollection processModuleCollection;

        //find all processes called 'mono', that's necessary because our app runs under the mono process!
        monoProcesses = Process.GetProcessesByName("mono");

        for (var i = 0; i <= monoProcesses.GetUpperBound(0); ++i)
        {
        processModuleCollection = monoProcesses[i].Modules;

        for (var j = 0; j < processModuleCollection.Count; ++j)
        {
        if (processModuleCollection[j].FileName.EndsWith(processName))
        {
        processFound++;
        }
        }
        }

        //we don't find the current process, but if there is already another one running, return true!
        return (processFound == 1);
        }
        */

        private static void DirectoryCopy(string sourceDirName, string destDirName, bool copySubDirs) {
            DirectoryInfo dir = new DirectoryInfo(sourceDirName);
            DirectoryInfo[] dirs = dir.GetDirectories();
            if (!dir.Exists) {
                throw new DirectoryNotFoundException("Source directory does not exist or could not be found: "
                        + sourceDirName);
            }
            if (!Directory.Exists(destDirName)) {
                System.Console.WriteLine("Making directory \"{0}\"...", destDirName);
                Directory.CreateDirectory(destDirName);
            } else {
                System.Console.WriteLine("Using directory \"{0}\"...", destDirName);
            }
            FileInfo[] files = dir.GetFiles();
            foreach (FileInfo file in files) {
                string temppath = Path.Combine(destDirName, file.Name);
                //System.Console.WriteLine("    Copying \"{0}\" to \"{1}\"...", file.FullName, temppath);
                file.CopyTo(temppath, true);
            }
            if (copySubDirs) {
                foreach (DirectoryInfo subdir in dirs) {
                    string temppath = Path.Combine(destDirName, subdir.Name);
                    DirectoryCopy(subdir.FullName, temppath, copySubDirs);
                }
            }
        }

        public static string GetIPythonPath() {
            Process proc = new Process {
                StartInfo = new ProcessStartInfo {
                    FileName = "ipython",
                             Arguments = "locate",
                             UseShellExecute = false,
                             RedirectStandardOutput = true,
                             CreateNoWindow = true
                }
            };
            proc.Start();
            string ipython_path = "";
            while (!proc.StandardOutput.EndOfStream) {
                ipython_path = proc.StandardOutput.ReadLine();
            }
            return ipython_path;
        }

        public static void LoadLanguage(FileInfo f, Dictionary<string, Language> languages) {
            if (verbose) {
                Print("        Loading {0}...", f.FullName);
            }
            Assembly assembly = Assembly.LoadFrom(f.FullName);
            if (assembly != null) {
                foreach (Type type in assembly.GetTypes()) {
                    MethodInfo method;
                    try {
                        method = type.GetMethod("MakeLanguage");
                    } catch (Exception e) {
                        Print("Failure; skipping language file '{0}': {1}", f.Name, e.Message);
                        continue;
                    }
                    if (method != null) {
                        Language language = null;
                        try {
                            language = (Language)method.Invoke(type, new object[]{});
                        } catch (Exception e) {
                            Print("Failure; skipping old language file '{0}': {1}", f.Name, e.Message);
                            continue;
                        }
                        languages [language.name] = language;
                        //Print("Registering language...'{0}'", language.name);
                        break;
                    }
                }
            } else {
                Console.WriteLine("Unable to load: '{0}'", f.FullName);
            }
        }

        public static void LoadLanguageSecondary(FileInfo f, Dictionary<string, Language> languages, string language_dependency,
                Config config, string path, Calico.LanguageManager manager) {
            System.Text.RegularExpressions.Match match = System.Text.RegularExpressions.Regex.Match(f.Name, @"Calico(.*)\.(.*)");
            if (verbose) {
                Print("        Loading {0}...", f.FullName);
            }
            //Print("Loading {0}...", f.FullName);
            string def_language = match.Groups [2].ToString().ToLower();
            if (def_language == "dll" || 
                    def_language == "cs" || 
                    def_language == "exe" || 
                    def_language.EndsWith("~") || 
                    def_language == "mdb") {
                return;
            }
            //Console.WriteLine(def_language); FIXME: need general interface for the following:
            // GetVariable, extend classes, call a method from C#
            try {
                Calico.DLREngine engine = ((Calico.DLREngine)languages [language_dependency].engine);
                var scope = engine.engine.ExecuteFile(f.FullName);
                Func<object> method = scope.GetVariable<Func<object>>("MakeLanguage");
                Language language = (Language)method();
                languages [language.name] = language;
                bool visible = ((IList<string>)config.GetValue("config", "visible-languages")).Contains(language.name);
                manager.Register(language, visible); // This may fail, which won't add language
                if (language.engine != null) {
                    language.engine.Setup(path);
                    language.engine.Start(path);
                }
            } catch (Exception e) {
                Print("Failure; skipping language file '{0}': {1}", f.Name, e.Message);
            }
        }

	public static string propercase(string text) {
	    return text.Substring(0,1).ToUpper() + text.Substring(1).ToLower();
	}

        [STAThread]
        public static void Main(string[] args) {
            System.Console.WriteLine(_("Loading Calico version {0}..."), Version);
            if (((IList<string>)args).Contains("--verbose")) {
                verbose = true;
            }
	    string lang_string = "";
	    bool lang_only = false;
	    string display_name = "";
	    foreach (string arg in args) { 
		if (arg == "--only") {
		    lang_only = true;
		} else if (arg.StartsWith("--lang=")) {
		    lang_string = arg.Split('=')[1];
		} else if (arg.StartsWith("--display-name=")) {
		    display_name = arg.Split('=')[1];
		}
	    }
            // Setup config
            string config_path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData);
            if (verbose) {
                System.Console.WriteLine(_("    looking for config in \"{0}\"..."), config_path);
            }
            config_path = System.IO.Path.Combine(config_path, "calico", "config.xml");
            Config config;
            if (((IList<string>)args).Contains("--reset")) {
                config = new Config(config_path, true);
            } else {
                config = new Config(config_path);
            }
            // Setup translations:
            string path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
            if (path.StartsWith("\\")) {
                path = path.Substring(1);
            }
            Catalog.Init("calico", System.IO.Path.Combine(path, "..", "locale"));

            // Process non executing flags which stop quickly:
            if (((IList<string>)args).Contains("--help")) {
                Usage();
                System.Environment.Exit(0);
            } else if (((IList<string>)args).Contains("--check-profile")) {
                // check if profile exists; create if not
                string ipython_base = GetIPythonPath();
                if (! System.IO.Directory.Exists(System.IO.Path.Combine(ipython_base, "profile_calico"))) {
                    create_profile(args, path, ipython_base);
                }
                System.Environment.Exit(0);
            } else if (((IList<string>)args).Contains("--create-profile")) {
                create_profile(args, path, GetIPythonPath());
                System.Environment.Exit(0);
            } else if (((IList<string>)args).Contains("--create-kernelspec")) {
		create_kernelspec(lang_string, display_name, args, path, GetIPythonPath());
                System.Environment.Exit(0);
            } else if (((IList<string>)args).Contains("--version")) {
                Print("Calico Project, version {0} on {1}", Version, System.Environment.OSVersion.VersionString);
                Print("  " + _("Using Mono runtime version {0}"), MonoRuntimeVersion);
                System.Environment.Exit(0);
            }
	    Dictionary<string, Language> languages = new Dictionary<string, Language>();
	    DirectoryInfo dir = new DirectoryInfo(System.IO.Path.Combine(path, "..", "languages"));
	    if (verbose) {
		System.Console.WriteLine(_("    Looking for languages in \"{0}\"..."), 
					 System.IO.Path.Combine(path, "..", "languages"));
	    }
	    foreach (DirectoryInfo d in dir.GetDirectories("*")) {
		foreach (FileInfo f in d.GetFiles("Calico*.dll")) {
		    if (lang_only) {
			if ((f.Name == string.Format("Calico{0}.dll", propercase(lang_string))) ||
			    (f.Name == "CalicoPython.dll")) {
			    LoadLanguage(f, languages);
			}
			// else skip
		    } else {
			LoadLanguage(f, languages);
		    }
		}
	    }
            // Now, let's load engines
            Calico.LanguageManager manager = new Calico.LanguageManager((IList<string>)config.GetValue("config", "visible-languages"), path, languages);
	    // First, let's dynamically load any need module DLLs
	    foreach (string filename in new string [] {"../modules/Graphics.dll"}) {
		Assembly.LoadFrom(System.IO.Path.Combine(path, filename));
	    }
	    // Load Calico languages that depend on other Calico languages:
	    foreach (DirectoryInfo d in dir.GetDirectories("*")) {
		foreach (FileInfo f in d.GetFiles("Calico*.py")) { // FIXME: allow other languages
		    if (lang_only) {
			if (f.Name == string.Format("Calico{0}.py", propercase(lang_string))) {
			    LoadLanguageSecondary(f, languages, "python", config, path, manager);
			}
		    } else {
			LoadLanguageSecondary(f, languages, "python", config, path, manager);
		    }
		}
	    }
	    string local_lang_path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData);
	    local_lang_path = System.IO.Path.Combine(local_lang_path, "calico", "languages");
	    if (verbose) {
		System.Console.WriteLine(_("    Looking for local languages in \"{0}\"..."), local_lang_path);
	    }
	    dir = new DirectoryInfo(local_lang_path);
	    if (dir.Exists) {
		foreach (DirectoryInfo d in dir.GetDirectories("*")) {
		    foreach (FileInfo f in d.GetFiles("Calico*.py")) { // FIXME: allow other languages
			if (lang_only) {
			    if (f.Name == string.Format("Calico{0}.py", propercase(lang_string))) {
				LoadLanguageSecondary(f, languages, "python", config, path, manager);
			    }
			} else {
			    LoadLanguageSecondary(f, languages, "python", config, path, manager);
			}
		    }
		}
	    }
	    // If a language isn't in the manager, it doesn't exist; remove it
	    List<string> vlangs = new List<string>();
	    foreach (string lang in ((IList<string>)config.GetValue("config", "visible-languages"))) {
		if (manager.languages.ContainsKey(lang)) {
		    vlangs.Add(lang);
		}
	    }
            foreach (string lang in manager.getLanguages()) {
                Language language = manager [lang];
                language.InitializeConfig();
                language.LoadConfig(config);
            }
            config.SetValue("config", "visible-languages", vlangs);
            // End of loading languages
            // -------------------------------------------

            // Global settings:
            MainWindow win = null;
            bool Debug = false;
            if (((IList<string>)args).Contains("--debug")) {
                Debug = true;
            }
            bool withGraphics = true;
            if (((IList<string>)args).Contains("--nographics")) {
                withGraphics = false;
            }
            // Process executable commands here:
            if (((IList<string>)args).Contains("--exec") || ((IList<string>)args).Contains("--server")) {
                // implies running with noconsole, with or without repl
                if (withGraphics) {
                    Application.Init();
                    if (((IList<string>)args).Contains("--server")) {
                        ///---------------------
                        System.Threading.Thread  signal_thread = null;
                        if (!System.Environment.OSVersion.Platform.ToString().Contains("Win")) {
                            UnixSignal[] signals = new UnixSignal [] {
                                new UnixSignal (Mono.Unix.Native.Signum.SIGINT),
                            };
                            signal_thread = new System.Threading.Thread (delegate () {
                                // Wait for a signal to be delivered
                                while (true) {
                                    UnixSignal.WaitAny (signals, -1);
                                    win.RequestInterrupt(); 
                                }
                            });
                            signal_thread.Start();
                        } else {
                            StartWindowsInterruptPoller(win);
                        }
                        ///-----------------------
                        string ipython_base = GetIPythonPath();
                        config.SetValue("ipython", "security", "string", System.IO.Path.Combine(ipython_base, "profile_calico", "security"));
                        GLib.Timeout.Add( 500, delegate { 
                            int t =  Thread.CurrentThread.ManagedThreadId;
                            System.Threading.Thread thread = new System.Threading.Thread ( delegate() {                            
                                win = new CalicoServer(args, manager, Debug, config, t); //Thread.CurrentThread.ManagedThreadId); 
                                win.Start();
                            });
                        thread.Start();
                        return false; 
                        }); 
                        Application.Run();
                    } else {
                        // THIS MAY NOT WOK ON MAC
                        System.Console.WriteLine(_("Running your code..."));
                        GLib.Timeout.Add( 500, delegate { 
                            win = new CalicoConsole(args, manager, Debug, config, ((IList<string>)args).Contains("--repl"),
                                Thread.CurrentThread.ManagedThreadId);
                            return false; 
                        }); 
                        Application.Run();
                    }
                } else {
                    if (((IList<string>)args).Contains("--server")) {
                        ///---------------------
                        System.Threading.Thread  signal_thread = null;
                        if (!System.Environment.OSVersion.Platform.ToString().Contains("Win")) {
                            UnixSignal[] signals = new UnixSignal [] {
                                new UnixSignal (Mono.Unix.Native.Signum.SIGINT),
                            };
                            signal_thread = new System.Threading.Thread (delegate () {
                                // Wait for a signal to be delivered
                                while (true) {
                                    UnixSignal.WaitAny (signals, -1);
                                    win.RequestInterrupt(); 
                                }
                            });
                            signal_thread.Start();
                        } else {
                            StartWindowsInterruptPoller(win);
                        }
                        ///-----------------------
                        string ipython_base = GetIPythonPath();
                        config.SetValue("ipython", "security", "string", System.IO.Path.Combine(ipython_base, "profile_calico", "security"));
                        win = new CalicoServer(args, manager, Debug, config, -1);
                        win.Start();
                    } else {
                        win = new CalicoConsoleNoGUI(args, manager, Debug, config, ((IList<string>)args).Contains("--repl"), -1);
                    }
                }
            } else if (((IList<string>)args).Contains("--repl")) {
                if (withGraphics) {
                    Application.Init();
                    GLib.Timeout.Add( 500, delegate { 
                        win = new CalicoConsole(args, manager, Debug, config, true, Thread.CurrentThread.ManagedThreadId);  
                        return false;
                    });
                    Application.Run();
                } else {
                    win = new CalicoConsoleNoGUI(args, manager, Debug, config, true, -1);  
                }
            } else {
                // Catch SIGINT
                System.Threading.Thread  signal_thread = null;
                if (!System.Environment.OSVersion.Platform.ToString().Contains("Win")) {
                    UnixSignal[] signals = new UnixSignal [] {
                        new UnixSignal (Mono.Unix.Native.Signum.SIGINT),
                    };
                    signal_thread = new System.Threading.Thread (delegate () {
                        // Wait for a signal to be delivered
                        UnixSignal.WaitAny (signals, -1);
                        Gtk.Application.Invoke( delegate { 
                            if (win != null)
                            win.RequestQuit(); 
                        });
                    });
                    //signal_thread.IsBackground = true;
                    signal_thread.Start();
                } else {
                    StartWindowsInterruptPoller(win);
                }
                // Ok, we are going to run this thing!
                // If Gui, let's go:
                System.Console.WriteLine(_("Launching app..."));
                Application.Init();
                win = new MainWindow(args, manager, Debug, config, signal_thread, Thread.CurrentThread.ManagedThreadId);
                win.Show();
                Application.Run();
            }
        }

        public static void StartWindowsInterruptPoller(MainWindow win) {
            string ipy_interrupt_event = Environment.GetEnvironmentVariable("IPY_INTERRUPT_EVENT");
            if (ipy_interrupt_event != null) {
                // start loop thread:
                var interrupt_thread = new System.Threading.Thread (delegate () {
                    while (true) {
                        var waits = new WaitHandle[1];
                        var wait_handle = new System.Threading.EventWaitHandle(false, EventResetMode.AutoReset);
                        var ipy_interrupt_event_intptr = new IntPtr(Int32.Parse(ipy_interrupt_event));
                        wait_handle.Handle = ipy_interrupt_event_intptr;
                        waits[0] = wait_handle; 
                        WaitHandle.WaitAny(waits);
                        if (win != null)
                    win.RequestInterrupt(); 
                    }
                });
                interrupt_thread.Start();
            }
        }

        public static void Print(string message, params object[] args) {
            Console.WriteLine(String.Format(message, args));
        }

        public static string _(string message) {
            return global::Mono.Unix.Catalog.GetString(message);
        }

        public static string MonoRuntimeVersion {
            get {
                Type mtype = System.Type.GetType("Mono.Runtime");
                System.Reflection.MethodInfo method = mtype.GetMethod("GetDisplayName", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);
                return (string)method.Invoke(null, null);
            }
        }

	public static void create_kernelspec(string kernelname, string display_name, string[] args, string path, string ipython_base) {
	    string kernelspec_name;
	    if (kernelname != "") {
		kernelspec_name = "calico_" + kernelname;
		if (display_name == "") 
		    kernelname = "Calico " + propercase(kernelname);
		else
		    kernelname = display_name;
	    } else {
		kernelspec_name = "calico";
		if (display_name == "")
		    kernelname = "Calico";
		else
		    kernelname = display_name;
	    }
	    // Copy the /notebooks/kernelspec/* to $(ipython locate)/kernels/calico.../
	    // Next, get destination:
	    string ipython_path = System.IO.Path.Combine(ipython_base, "kernels", kernelspec_name);
	    // Now, copy logos:
	    DirectoryCopy(System.IO.Path.Combine(path, "..", "notebooks", "kernelspecs"), ipython_path, false);
	    // Now, create kernel.json
	    string ipython_config = "";
	    string lang_string = "";
	    foreach (string arg in args) { 
		if (arg.StartsWith("--lang=")) {
		    lang_string = String.Format("      \"{0}\", \n", arg);
		    break;
		}
	    }
	    if (System.Environment.OSVersion.Platform.ToString().Contains("Win")) {
		string executable_path = System.IO.Path.Combine(path, "Calico.exe");
		ipython_config = String.Format(
					       "{{\n" + 
					       " \"display_name\": \"{2}\",\n" +
					       " \"env\" = {{\"MONO_PATH\": \"{0}\\..\\mono\\lib\\4.0;{0}\\..\\mono\\lib\\gtk-sharp-2.0;{0};{0}\\..\\mono\\lib\\2.0;{0}\\..\\mono\\lib\\3.5;{0}\\windows\", \n" +
					       "            \"PATH\": \"c:\\Python27\\Scripts\" \n}}\n" +
					       "\"argv\" = [\n" +
					       "      \"{0}\\..\\mono\\bin\\mono.exe\", \"{1}\", \n", path, executable_path, kernelname) +
		    (((IList<string>)args).Contains("--nographics") ? "     \"--nographics\",\n" : "") +
		    (((IList<string>)args).Contains("--only") ? "     \"--only\",\n" : "") +
		    lang_string +												     
		    ("     \"--server\", \"{connection_file}\"]\n}}\n");
	    } else { // Linux, Mac OSX, etc
		string executable_path = System.IO.Path.Combine(path, "Calico.exe");
		string oslib_path;
		string ld_lib_path;
		string oslib;
		if (System.IO.Directory.Exists("/Applications")) {
		    oslib = "mac";
		    ld_lib_path = "/Library/Frameworks/Mono.framework/Libraries/";
		} else {
		    oslib = "linux";
		    ld_lib_path = "";
		}
		oslib_path = System.IO.Path.Combine(path, oslib);
		ipython_config = String.Format(
					       "{{\n" +
					       " \"display_name\": \"{4}\",\n" +
					       " \"env\": {{\n" + 
					       "      \"LD_LIBRARY_PATH\": \"{1}{2}\", \n" +
					       "      \"MONO_PATH\": \"{2}\"\n" + 
					       "  }}, \n" +
					       " \"argv\": [" +
					       "\"mono\", \"{3}\", ", 
					       oslib, // {0}
					       ld_lib_path, // {1}
					       oslib_path, // {2}
					       executable_path, // {3}
					       kernelname // {4}
					       ) + 
		    // rest:
		    (((IList<string>)args).Contains("--nographics") ? " \"--nographics\", " : "") +
		    (((IList<string>)args).Contains("--only") ? " \"--only\", " : "") +
		    lang_string +												     
		    ("      \"--server\", \"{connection_file}\"]\n}\n");
	    }
	    string filename = System.IO.Path.Combine(ipython_path, "kernel.json");
	    System.Console.WriteLine("    Creating kernelspec: \"{0}\"...", filename);
	    System.IO.StreamWriter sw = new System.IO.StreamWriter(filename);
	    sw.Write(ipython_config);
	    sw.Close();
	}

	public static void create_profile(string[] args, string path, string ipython_base) {
		// Copy the /notebooks/profile to $(ipython locate)
		// First, make a default profile:
		var proc = new Process {
			StartInfo = new ProcessStartInfo {
				FileName = "ipython",
				    Arguments = "profile create calico",
				    UseShellExecute = false,
				    RedirectStandardOutput = false,
				    CreateNoWindow = true
				    }
		    };
		proc.Start();
		// Next, get destination:
		string ipython_path = System.IO.Path.Combine(ipython_base, "profile_calico");
		string nbextensions_path = System.IO.Path.Combine(ipython_base, "nbextensions");
		// Now, copy recursively:
		DirectoryCopy(System.IO.Path.Combine(path, "..", "notebooks", "profile"), ipython_path, true);
		DirectoryCopy(System.IO.Path.Combine(path, "..", "notebooks", "nbextensions"), nbextensions_path, true);
		// Now, put the top level ipython_config.py
		string ipython_config = "";
		string lang_string = "";
		foreach (string arg in args) { 
		    if (arg.StartsWith("--lang=")) {
			lang_string = String.Format("      '{0}', \n", arg);
			break;
		    }
		}
		if (System.Environment.OSVersion.Platform.ToString().Contains("Win")) {
		    string executable_path = System.IO.Path.Combine(path, "Calico.exe");
		    ipython_config = String.Format(
			"# Configuration file for ipython.\n" +
			"\n" +
			"# set environment vars for Windows:\n" +
			"import os \n" +
                        "os.environ[\"MONO_PATH\"] = r\"{0}\\..\\mono\\lib\\4.0;{0}\\..\\mono\\lib\\gtk-sharp-2.0;{0};{0}\\..\\mono\\lib\\2.0;{0}\\..\\mono\\lib\\3.5;{0}\\windows\" \n" +
                        "os.environ[\"PATH\"] = os.environ[\"PATH\"] + r\";c:\\Python27\\Scripts\" \n" +
                        "\n" +
                        "c = get_config()\n" +
                        "c.KernelManager.kernel_cmd = [\n" +
                        "      r'{0}\\..\\mono\\bin\\mono.exe', r'{1}', \n", path, executable_path) +
                    (((IList<string>)args).Contains("--nographics") ? "     '--nographics',\n" : "") +
                    lang_string +												     
                    ("     '--server', '{connection_file}']\n");
            } else { // Linux, Mac OSX, etc
                string executable_path = System.IO.Path.Combine(path, "Calico.exe");
                string oslib_path;
                string ld_lib_path;
                string oslib;
                if (System.IO.Directory.Exists("/Applications")) {
                    oslib = "mac";
                    ld_lib_path = "/Library/Frameworks/Mono.framework/Libraries/";
                } else {
                    oslib = "linux";
                    ld_lib_path = "";
                }
                oslib_path = System.IO.Path.Combine(path, oslib);
                ipython_config = String.Format(
                        "# Configuration file for ipython.\n" +
                        "\n" +
                        "# set environment vars for {0}:\n" +
                        "import os \n" +
                        "if \"LD_LIBRARY_PATH\" in os.environ:\n" +
                        "    os.environ[\"LD_LIBRARY_PATH\"] = (\"{1}\" + \n" +
                        "        os.pathsep + \"{2}\" + os.pathsep + os.environ[\"LD_LIBRARY_PATH\"]) \n" +
                        "else:\n" +
                        "    os.environ[\"LD_LIBRARY_PATH\"] = (\"{1}\" + \n" +
                        "        os.pathsep + \"{2}\") \n" +
                        "\n" +
                        "if \"MONO_PATH\" in os.environ:\n" +
                        "    os.environ[\"MONO_PATH\"] = \"{2}\" + os.pathsep + os.environ[\"MONO_PATH\"] \n" +
                        "else:\n" +
                        "    os.environ[\"MONO_PATH\"] = \"{2}\" \n" +
                        "\n" +
                        "c = get_config()\n" +
                        "c.KernelManager.kernel_cmd = [\n" +
                        "      'mono', '{3}', \n", 
                        oslib, // {0}
                ld_lib_path, // {1}
                oslib_path, // {2}
                executable_path) + // {3}
            // rest:
            (((IList<string>)args).Contains("--nographics") ? "     '--nographics',\n" : "") +
                lang_string +												     
                ("      '--server', '{connection_file}']\n");
            }
            string filename = System.IO.Path.Combine(ipython_path, "ipython_config.py");
            System.Console.WriteLine("    Creating ipython config: \"{0}\"...", filename);
            System.IO.StreamWriter sw = new System.IO.StreamWriter(filename);
            sw.Write(ipython_config);
            sw.Close();
        }

        public static void Usage() {
            Print("");
            Print(_("Calico Project, version {0} on {1}"), Version, System.Environment.OSVersion.VersionString);
            Print("  " + _("Using Mono runtime version {0}"), MonoRuntimeVersion);
            Print("----------------------------------------------------------------------------");
            Print(_("Start calico with the following options:"));
            Print(_("  calico                     Standard GUI"));
            Print(_("  calico FILENAME:LINE ...   Edit FILENAMEs, positioned on LINEs"));
            Print(_("  calico --lang=LANGUAGE     Set default language (python, etc.)"));
            Print(_("  calico   --only            Load only this language, skipping others"));	    
            Print(_("  calico --exec FILENAMEs    Run FILENAMEs"));
        Print(_("  StartCalico --run FILENAME     Run FILENAME and open it in the IDE"));
            Print(_("  calico --repl FILENAMEs    Run FILENAMEs and starts read-eval-print loop"));
            Print(_("  calico   --nographics      Don't run with graphics (with --exec, --repl)"));
	    Print(_("  calico   --noquit          Don't quit after executing file with --exec"));
	    Print(_("  calico --nomodules         Does not load the modules from modules/*.dll"));
            Print(_("  calico --version           Display the version number ({0})"), Version);
            Print(_("  calico --verbose           Display detailed information (for debugging)"));
            Print(_("  calico --debug             Calico output goes to console rather than GUI"));
            Print(_("  calico --debug-handler     Calico will not catch system errors"));
            Print(_("  calico --reset             Reset config settings to factory defaults"));
            Print(_("  calico --create-kernelspec Create a kernelspec for Jupyter (overwrites)"));
            Print(_("  calico   --display-name=\"\" Display name to use with kernelspec"));
            Print(_("  calico --create-profile    Create a profile for IPython 2 (overwrites)"));
            Print(_("  calico --check-profile     Check to see if profile for IPython 2 exists"));
            Print(_("  calico --server [FILE]     Use as language kernel for IPython/Jupyter"));
            Print(_("  calico --help              Display this message"));
            Print("");
        }
    }
}

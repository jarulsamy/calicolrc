using System;
using Gtk;
using System.Collections.Generic; // IList
using Mono.Unix; 
using System.IO;
using System.Reflection;

namespace Calico
{
	class MainClass
	{
		public static string Version = "2.0.0";
		
		[STAThread]
		public static void Main (string [] args)
		{
			// Set up translations:
			string path = System.IO.Path.GetDirectoryName(
                       System.Reflection.Assembly.GetExecutingAssembly()
                       .GetName().CodeBase).Substring(5);
    		if (path.StartsWith("\\")) {
      			path = path.Substring(1);
    		}
			Catalog.Init("calico", System.IO.Path.Combine(path, "../locale"));
			
			Dictionary<string,Language> languages = new Dictionary<string,Language>();
			// for language in directory, load languages:
			/*
		    DirectoryInfo dir = new DirectoryInfo(System.IO.Path.Combine(path, "../languages"));
        	foreach (DirectoryInfo d in dir.GetDirectories("*"))
        	{
	        	foreach (FileInfo f in d.GetFiles("Calico*.dll"))
    	    	{
					Print("Loading {0}...", f.FullName);
					Assembly assembly = Assembly.LoadFrom(f.FullName);
					if (assembly != null) {
						foreach (Type type in assembly.GetTypes()) {
							MethodInfo method = type.GetMethod("RegisterLanguage");
							if (method != null) {
								Language language = (Language)method.Invoke(type, new object[]{});
								languages[language.name] = language;
								Print("Registering language...'{0}'", language.name);
								break;
							}
						}
					}
				}
        	}
        	*/
			languages["python"] = CalicoPythonLanguage.RegisterLanguage();
			

			// Global settings:
			bool Debug = false;
			if (! ((IList<string>)args).Contains("--debug-handler")) {
				GLib.ExceptionManager.UnhandledException += HandleException;
			} 
			if (((IList<string>)args).Contains("--debug")) {
				Debug = true;
			}
			// Process some commands here:
			if (((IList<string>)args).Contains("--help")) {
				Usage();
			} else if (((IList<string>)args).Contains("--version")) {
			    Print("Calico Project, version {0} on {1}", Version, System.Environment.OSVersion.VersionString);
			    Print("  " + _("Using Mono runtime version {0}"), MonoRuntimeVersion);
			} else {
				// Ok, we are going to run this thing!
				// If Gui, let's go:
				Application.Init();
				MainWindow win = new MainWindow(args, languages, Debug);
				win.Show();
				Application.Run();
			}
		}
	
		public static void HandleException(GLib.UnhandledExceptionArgs args) {
			Console.WriteLine("Unhandled exception: {0}", args);
		}
		
		public static void Print(string message, params object [] args) {
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
		
		public static void Usage() {
		    Print("");
		    Print(_("Calico Project, version {0} on {1}"), 
				  Version,
		          System.Environment.OSVersion.VersionString);
		    Print("  " + _("Using Mono runtime version {0}"), MonoRuntimeVersion);
		    Print("----------------------------------------------------------------------------");
		    Print(_("Start calico with the following options:"));
		    Print(_("  StartCalico                            Defaults to shell"));
		    Print(_("  StartCalico FILENAME:LINE ...          Edits FILENAMEs, positioned on LINEs"));
		    Print(_("  StartCalico --lang=LANGUAGE            Sets default language (python, etc.)"));
		    Print(_("  StartCalico --chat                     Brings up chat window"));
		    Print(_("  StartCalico --exec FILENAMEs           Run FILENAMEs standalone with graphics"));
		    Print(_("  StartCalico --exec --nogui FILENAMEs   Run FILENAMEs standalone no graphics"));
		    Print(_("  StartCalico --version                  Displays the version number ({0})"), Version);
		    Print(_("  StartCalico --help                     Displays this message"));
		    Print("");
		}
	}
}

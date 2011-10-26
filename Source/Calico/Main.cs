//using Mono.Addins;
using System;
using Gtk;
using System.Collections.Generic; // IList
using Mono.Unix; 

//[assembly:AddinRoot ("Calico", "1.0")]
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
			
			/*
			// Addins:
			AddinManager.Initialize();
			// Detect changes in add-ins
			AddinManager.Registry.Update(); 			
			foreach (ICommand cmd in AddinManager.GetExtensionObjects(typeof(ICommand)))
				cmd.Run();
			*/
			if (((IList<string>)args).Contains("--help")) {
				Usage();
			} else if (((IList<string>)args).Contains("--version")) {
			    Print("{0}", Version);
			} else {
				if (! ((IList<string>)args).Contains("--debug-handler")) {
					GLib.ExceptionManager.UnhandledException += HandleException;
				}
				// If Gui, let's go:
				Application.Init();
				MainWindow win = new MainWindow(args);
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
		    Print(_("Calico Project, Version {0}, on {1}"), 
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

	/*
	[TypeExtensionPoint]
	public interface ICommand
	{
		void Run ();
	}
	*/
}

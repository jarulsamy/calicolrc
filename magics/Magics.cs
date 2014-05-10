using System;
using System.Collections; // IDictionary
using System.Collections.Generic; // List
using System.Net; // http stuff
using System.IO; // stream

namespace Calico {

    public class Time : Calico.MagicBase {
	System.Diagnostics.Stopwatch sw;

	public Time(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    // time a single line?
	    command = "time";
	    evaluate = true;
	    Console.Error.WriteLine("Can't time a line; use %%time to time cell");
	}
	
	public override void cell(string args) {
	    // start the clock!
	    command = "time";
	    evaluate = true;
	    sw = new System.Diagnostics.Stopwatch();
	    sw.Start();
	}

	public override void notebook(string args) {
	    bool on = session.ToggleStickyMagic(this);
	    if (on) {
		magic_line = "%%time";
		cell("");
		Console.WriteLine("%%time is now on");
	    } else {
		magic_line = "";
		Console.WriteLine("%%time is now off");
	    }
	}

	public override object post_process(object result) {
	    if (sw != null) {
		sw.Stop();
		TimeSpan ts = sw.Elapsed;
		Console.WriteLine(String.Format("Time: {0:00} s, {1:00} ms",
						ts.Seconds,
						ts.Milliseconds));
		return result;
	    } else {
		return null;
	    }
	}
    }
    
    public class File : Calico.MagicBase {
	
	public File(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "file";
	    Console.Error.WriteLine("No such magic; use %%file instead");
	    evaluate = false;
	}
	
	public override void cell(string args) {
	    command = "file";
	    evaluate = false;
	    System.Console.Write("Writing \"{0}\"...", args);
	    System.IO.StreamWriter sw = new System.IO.StreamWriter(args);
	    sw.Write(code);
	    sw.Close();
	}

	public override object post_process(object result) {
	    System.Console.WriteLine(" done!");
	    return result;
	}
    }    

    public class Rich_display : Calico.MagicBase {
	bool previous;
	string state;
	
	public Rich_display(ZMQServer.Session session, string code, 
			    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "rich_display";
	    Console.Error.WriteLine("No such magic; use %%rich_display or %%%rich_display instead");
	    evaluate = false;
	}
	
	public override void cell(string args) {
	    // %%rich_display
	    command = "rich_display";
	    previous = session.GetRichDisplay();
	    session.SetRichDisplay(true);
	    state = "reset";
	}

	public override void notebook(string args) {
	    // toggle rich_text
	    // %%%rich_display
	    command = "rich_display";
	    bool current = session.GetRichDisplay();
	    session.SetRichDisplay(! current);
	}

	public override object post_process(object result) {
	    if (state == "reset") {
		session.SetRichDisplay(previous);
	    }
	    return result;
	}
    }    

    public class Lang : Calico.MagicBase {
	string tempLanguage = null;
	
	public Lang(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "lang";
	    if (args == "") {
		Console.WriteLine("Calico Language is currently \"{0}\"", session.calico.GetCurrentProperLanguage());
		Console.WriteLine("");
		Console.WriteLine("Use: %%lang LANGUAGE to change language for a cell");
		Console.WriteLine("     %%%lang LANGUAGE to change language (sticky)");
		Console.WriteLine("");
		Console.WriteLine("Possible languages are: Python, Ruby, Java, F#, Basic,");
		Console.WriteLine("  Logo, Boo, Console, LC3, Scheme, BrainScrew, etc.");
		evaluate = true;
	    } else {
		args = args.Trim();
		string expr = "";
		string language = "";
		for (int i=0; i < args.Length; i++) {
		    if (args.Substring(i, 1) == " ") {
			expr = args.Substring(i + 1).Trim();
			break;
		    }
		    language += args.Substring(i, 1);
		}
		if (session.calico.manager.ContainsKey(language)) {
		    session.calico.StartLanguage(language);
		    session.calico.display(session.calico.Evaluate(expr, language));
		    evaluate = true;
		} else {
		    Console.Error.WriteLine("Unknown language: \"{0}\"", language);
		    evaluate = false;
		}
	    }
	}
	
	public override void cell(string args) {
	    command = "lang";
	    // eval this cell in lang in args, then put the lang back
	    tempLanguage = session.calico.CurrentLanguage;
	    string language = session.calico.FindLanguage(args);
	    if (session.calico.manager.ContainsKey(language)) {
		session.calico.ActivateLanguage(language, session.calico.CurrentLanguage);
		evaluate = true;
	    } else {
		Console.Error.WriteLine("Unknown language: \"{0}\"", args);
		evaluate = false;
	    }
	    // then we will do the processing...
	}

	public override object post_process(object result) {
	    // if cell mode, put language back
	    if (mtype == "cell" && tempLanguage != null) {
		session.calico.ActivateLanguage(tempLanguage, session.calico.CurrentLanguage);
	    }
	    return result;
	}

	public override void notebook(string args) {
	    command = "lang";
	    string language = session.calico.FindLanguage(args);
	    if (session.calico.manager.ContainsKey(language)) {
		session.calico.ActivateLanguage(language, session.calico.CurrentLanguage);
		Console.WriteLine("Calico Language is now \"{0}\"", session.calico.GetCurrentProperLanguage());
		evaluate = true;
	    } else {
		Console.Error.WriteLine("Unknown language: \"{0}\"", args);
		evaluate = false;
	    }
	}
    }    

    public class Run : Calico.MagicBase {
	
	public Run(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "run";
	    evaluate = true;
	    // run
	    try {
		session.calico.ExecuteFile(args);
	    } catch (Exception e) {
		System.Console.Error.WriteLine("Error in %run: {0}", e.Message);
	    }
	}
    }    
    
    public class Qtconsole : Calico.MagicBase {
	
	public Qtconsole(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "qtconsole";
	    evaluate = true;
	    // run
	    string session_name = System.IO.Path.GetFileName(session.filename);
	    System.Diagnostics.Process.Start("/usr/bin/env", "ipython qtconsole --profile calico --existing " + session_name + " " + args);
	}
	
    }    

    public class Edit : Calico.MagicBase {

	public Edit(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "edit";
	    evaluate = true;
	    System.Diagnostics.Process.Start("/usr/bin/env", "emacs " + args);
	}
	
    }    

    public class Magic : Calico.MagicBase {
	
	public Magic(ZMQServer.Session session, string code, 
		      string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    line(args);
	}

	public override void notebook(string args) {
	    line(args);
	}

	public override void line(string args) {
	    command = "magics";
	    evaluate = true;
	    Console.WriteLine("%connect_info        - show ICalico JSON connection information");
	    Console.WriteLine("%%dot [options]      - create a GraphViz dot diagram with contents of cell");
	    Console.WriteLine("%download url [file] - download a URL to a file");
	    Console.WriteLine("%edit FILENAME       - edit a file in an external editor");
	    Console.WriteLine("%%file FILENAME      - create a filename with contents of cell");
	    Console.WriteLine("%%html               - treat the cell as HTML");
	    Console.WriteLine("%%javascript         - treat the cell as Javascript data");
	    Console.WriteLine("%lang                - get information on current language");
	    Console.WriteLine("%lang LANG EXPR      - evaluate the EXPR in the given LANG");
	    Console.WriteLine("%%lang LANG          - change language for just this cell");
	    Console.WriteLine("%%%lang LANG         - change language for rest of cells");
	    Console.WriteLine("%%latex              - treat the cell as Latex");
	    Console.WriteLine("%magic               - get information on magic meta-commands");
	    Console.WriteLine("%nuget ...           - perform a NuGet command, such as install");
	    Console.WriteLine("%qtconsole           - start a qtconsole");
	    Console.WriteLine("%run FILENAME        - run a file (language determined by extension)");
	    Console.WriteLine("%%rich_display       - output the cell data/return value as HTML");
	    Console.WriteLine("%%%rich_display      - use %%rich_display for each cell");
	    Console.WriteLine("%%svg                - treat the cell as SVG data");
	    Console.WriteLine("%%time               - time how long it takes to run this cell");
	    Console.WriteLine("%%%time              - use %%time for each cell");
	}	
    }    

    public class Connect_info : Calico.MagicBase {
	
	public Connect_info(ZMQServer.Session session, string code, 
			    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "connect_info";
	    evaluate = true;
	    Console.WriteLine("{");
	    Console.WriteLine("  \"stdin_port\": {0},", session.config["stdin_port"]);
	    Console.WriteLine("  \"shell_port\": {0},", session.config["shell_port"]);
	    Console.WriteLine("  \"iopub_port\": {0},", session.config["iopub_port"]);
	    Console.WriteLine("  \"hb_port\": {0},", session.config["hb_port"]);
	    Console.WriteLine("  \"ip\": \"{0}\",", session.config["ip"]);
	    Console.WriteLine("  \"key\": \"{0}\",", session.config["key"]);
	    Console.WriteLine("  \"signature_scheme\": \"{0}\",", session.config["signature_scheme"]);
	    Console.WriteLine("  \"transport\": \"{0}\"", session.config["transport"]);
	    Console.WriteLine("}");
	    Console.WriteLine("");
	    Console.WriteLine("Paste the above JSON into a file, and connect with:");
	    Console.WriteLine("    $> ipython <app> --profile calico --existing <filename>");
	    Console.WriteLine("or, if you are local, you can connect with just:");
	    Console.WriteLine("    $> ipython <app> --profile calico --existing {0} ", 
			      System.IO.Path.GetFileName(session.filename));
	    Console.WriteLine("or even just:");
	    Console.WriteLine("    $> ipython <app> --profile calico --existing ");
	    Console.WriteLine("if this is the most recent ICalico session you have started.");
	}	
    }    

    public class Html : Calico.MagicBase {
	
	public Html(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    command = "html";
	    evaluate = false;
	    session.display_html(session.calico.HTML(code));
	}
    }

    public class Latex : Calico.MagicBase {
	
	public Latex(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    command = "latex";
	    evaluate = false;
	    session.display_latex(session.calico.Latex(code));
	}
    }

    public class Javascript : Calico.MagicBase {
	
	public Javascript(ZMQServer.Session session, string code, 
			  string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    command = "javascript";
	    evaluate = false;
	    session.display_javascript(session.calico.Javascript(code));
	}
    }

    public class Svg : Calico.MagicBase {
	
	public Svg(ZMQServer.Session session, string code, 
		   string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    command = "svg";
	    evaluate = false;
	    session.display_svg(session.calico.SVG(code));
	}
    }

    public class Dot : Calico.MagicBase {
	
	public Dot(ZMQServer.Session session, string code, 
		   string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void cell(string args) {
	    command = "dot";
	    evaluate = false;
	    Graphics.Graph g = new Graphics.Graph();
	    IDictionary options = null;
	    bool preprocess = true;
	    if (args != "") {
		options = (IDictionary)session.calico.Evaluate(args, "python");
	    } else {
		options = new Dictionary<string,object>();
	    }
	    if (options.Contains("preprocess")) {
		preprocess = (bool)options["preprocess"];
	    }
	    g.layout(code, preprocess); // pre-process Dot file?
	    if (args != "") {
		session.display(g.render(options));
	    } else {
		session.display(g.render());
	    }
	}
    }

    public class Download : Calico.MagicBase {
	
	public Download(ZMQServer.Session session, string code, 
			string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "download";
	    evaluate = true;
	    List<string> arg = Split(args);
	    string url;
	    string filename;
	    if (arg.Count == 1) {
		url = arg[0];
		var uri = new System.Uri(url);
		filename = System.IO.Path.GetFileName(uri.LocalPath);
	    } else if (arg.Count == 2) {
		url = arg[0];
		filename = arg[1];
	    } else {
		System.Console.Error.WriteLine("Incorrect number of arguments to %download: expected 1 or 2, received {0}", arg.Count);
		evaluate = false;
		return;
	    }
	    if (url.StartsWith ("http://") || url.StartsWith ("https://")) {
		HttpWebRequest req = (HttpWebRequest)WebRequest.Create (url);
		req.KeepAlive = false;
		req.Timeout = 10000;        
		WebResponse resp = req.GetResponse ();
		Stream s = resp.GetResponseStream ();
		//System.IO.StreamWriter sw = new System.IO.StreamWriter(filename);
		System.IO.FileStream sw = new System.IO.FileStream(filename, System.IO.FileMode.Create,
								   System.IO.FileAccess.Write);
		byte[] buffer = new byte[8 * 1024];
		int len;
		while ((len = s.Read(buffer, 0, buffer.Length)) > 0) {
		    sw.Write(buffer, 0, len);
		}    
		sw.Close();
	    }
	}
    }

    public class Nuget : Calico.MagicBase {
	
	public Nuget(ZMQServer.Session session, string code, 
		     string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "nuget";
	    evaluate = true;
	    string nuget = System.IO.Path.Combine(session.calico.path, "NuGet.exe");
	    string output_directory = System.IO.Path.Combine(session.calico.path, "..", "modules");
	    if (args.StartsWith("install ") && ! args.Contains("-OutputDirectory")) {
		args += " -OutputDirectory " + output_directory;
	    }
	    System.Diagnostics.Process proc = new System.Diagnostics.Process {
		    StartInfo = new System.Diagnostics.ProcessStartInfo {
			    FileName = nuget,
				Arguments = args,
				UseShellExecute = false,
				RedirectStandardOutput = true,
				RedirectStandardError = true,
				CreateNoWindow = true
				}
		};
	    proc.Start();
	    string output = "";
	    string error = "";
	    while (!proc.StandardError.EndOfStream) {
		error = proc.StandardError.ReadLine();
	    }
	    while (!proc.StandardOutput.EndOfStream) {
		output = proc.StandardOutput.ReadLine();
	    }
	    if (output != "")
		session.display(output);
	    if (error != "")
		session.calico.Error(error);
	}
    }
}

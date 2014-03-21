using System;

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

	public override object post_process(object result) {
	    sw.Stop();
	    TimeSpan ts = sw.Elapsed;
	    Console.WriteLine(String.Format("Time: {0:00} s, {1:00} ms",
					    ts.Seconds,
					    ts.Milliseconds));
	    return result;
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

    public class Lang : Calico.MagicBase {
	string tempLanguage = null;
	
	public Lang(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    Console.WriteLine("Calico Language is currently \"{0}\"", session.calico.GetCurrentProperLanguage());
	    Console.WriteLine("");
	    Console.WriteLine("Use: %%lang LANGUAGE to change language for a cell");
	    Console.WriteLine("     %%%lang LANGUAGE to change language (sticky)");
	    Console.WriteLine("");
	    Console.WriteLine("Possible languages are: Python, Ruby, Java, F#, Basic,");
	    Console.WriteLine("  Logo, Boo, Console, LC3, Scheme, BrainScrew, etc.");
	    command = "lang";
	    evaluate = true;
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
	    Console.WriteLine("%connect_info       - show ICalico JSON connection information");
	    Console.WriteLine("%edit FILENAME      - edit a file in an external editor");
	    Console.WriteLine("%%file FILENAME     - create a filename with contents of cell");
	    Console.WriteLine("%%html              - treat the cell as HTML");
	    Console.WriteLine("%%javascript        - treat the cell as Javascript data");
	    Console.WriteLine("%lang               - get information on current language");
	    Console.WriteLine("%%lang LANGUAGE     - change language for just this cell");
	    Console.WriteLine("%%%lang LANGUAGE    - change language for rest of cells");
	    Console.WriteLine("%magic              - get information on magic meta-commands");
	    Console.WriteLine("%qtconsole          - start a qtconsole");
	    Console.WriteLine("%run FILENAME       - run a file (language determined by extension)");
	    Console.WriteLine("%%svg               - treat the cell as SVG data");
	    Console.WriteLine("%%time              - time how long it takes to run this cell");
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
}

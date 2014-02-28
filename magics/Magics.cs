using System;

namespace Calico {

    public class Time : Calico.Magic {
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
					    ts.Milliseconds / 10));
	    return result;
	}
    }
    
    public class File : Calico.Magic {
	
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

    public class Lang : Calico.Magic {
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

    public class Run : Calico.Magic {
	
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
    
    public class Qtconsole : Calico.Magic {
	
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

    public class Edit : Calico.Magic {
	
	public Edit(ZMQServer.Session session, string code, 
		    string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "edit";
	    evaluate = true;
	    System.Diagnostics.Process.Start("/usr/bin/env", "emacsclient " + args);
	}
	
    }    

    public class Magics : Calico.Magic {
	
	public Magics(ZMQServer.Session session, string code, 
		      string mtype, string args) : base(session, code, mtype, args) {
	}

	public override void line(string args) {
	    command = "magics";
	    evaluate = true;
	    Console.WriteLine("%edit FILENAME      - edit a file in an external editor");
	    Console.WriteLine("%%file FILENAME     - create a filename with contents of cell");
	    Console.WriteLine("%lang               - get information on current language");
	    Console.WriteLine("%%lang LANGUAGE     - change language for just this cell");
	    Console.WriteLine("%%%lang LANGUAGE    - change language for rest of cells");
	    Console.WriteLine("%magics             - get information on magics");
	    Console.WriteLine("%qtconsole          - start a qtconsole");
	    Console.WriteLine("%run FILENAME       - run a file (language determined by extension)");
	    Console.WriteLine("%%time              - time how long it takes to run this cell");
	}	
    }    
}




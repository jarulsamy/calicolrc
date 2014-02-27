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
	    Console.Error.WriteLine("No line magic for lang; use %%lang or %%%lang");
	    command = "lang";
	    evaluate = false;
	    Console.WriteLine(2);
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
}




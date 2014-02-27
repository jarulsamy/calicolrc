using System;

namespace Calico {

    public class Time : Calico.Magic {
	System.Diagnostics.Stopwatch sw;

	public Time(ZMQServer.Session session, string code) : base(session, code) {
	    command = "time";
	    sw = new System.Diagnostics.Stopwatch();
	    evaluate = true; // run the code
	}

	public override void line(string text) {
	    base.line(text);
	    // time a single line?
	}
	
	public override void cell(string args) {
	    base.cell(args);
	    // start the clock!
	    sw.Start();
	}

	public override object post_process(object result) {
	    // stop the clock! print results
	    sw.Stop();
	    TimeSpan ts = sw.Elapsed;
	    Console.WriteLine(String.Format("Time: {0:00} s, {1:00} ms",
					    ts.Seconds,
					    ts.Milliseconds / 10));
	    return result;
	}
    }
    
    public class File : Calico.Magic {
	
	public File(ZMQServer.Session session, string code) : base(session, code){
	    command = "file";
	    evaluate = false;
	}

	public override void line(string text) {
	    base.line(text);
	    throw new Exception("use %%file instead");
	}
	
	public override void cell(string args) {
	    base.cell(args);
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

    public class Language : Calico.Magic {
	
	public Language(ZMQServer.Session session, string code) : base(session, code){
	    command = "lang";
	    evaluate = false;
	}

	public override void line(string text) {
	    base.line(text);
	    // just eval the rest of this line in the language given in args
	}
	
	public override void cell(string args) {
	    base.cell(args);
	    // eval this cell in lang in args, then put the lang back
	}

	public override object post_process(object result) {
	    // if cell mode, put language back
	    return result;
	}

	public override void notebook(string args) {
	    base.notebook(args);
	}
    }    
}

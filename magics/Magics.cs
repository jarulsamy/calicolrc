using System;

namespace Calico {

    public class Time : Calico.Magic {
	System.Diagnostics.Stopwatch sw;

	public Time(string code) : base(code) {
	    command = "time";
	    sw = new System.Diagnostics.Stopwatch();
	    evaluate = true; // run the code
	}

	public override void line(string text) {
	    // time a single line?
	}
	
	public override void cell_start(string args) {
	    // start the clock!
	    sw.Start();
	}

	public override object cell_stop(object result) {
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
	
	public File(string code) : base(code){
	    command = "file";
	    evaluate = false;
	}

	public override void line(string text) {
	    throw new Exception("use %%file instead");
	}
	
	public override void cell_start(string args) {
	    System.Console.Write("Writing \"{0}\"...", args);
	    System.IO.StreamWriter sw = new System.IO.StreamWriter(args);
	    sw.Write(code);
	    sw.Close();
	}

	public override object cell_stop(object result) {
	    System.Console.WriteLine(" done!");
	    return result;
	}
    }    
}

//
//  CalicoCSharp.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2012 The Calico Project
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
using Mono.CSharp;
using Calico;

public class CalicoCSharpEngine : Engine
{
    Mono.CSharp.CompilerSettings settings;
    Mono.CSharp.ConsoleReportPrinter reportPrinter;
    Mono.CSharp.CompilerContext ctx;
    Mono.CSharp.Evaluator evaluator;
    
    public CalicoCSharpEngine (LanguageManager manager) : base(manager) {
	settings = new CompilerSettings() { Unsafe = true };
	reportPrinter = new ConsoleReportPrinter();
	ctx = new CompilerContext(settings, reportPrinter);
	evaluator = new Evaluator(ctx);

	string path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase).Substring(5);
	if (path.StartsWith("\\")) {
	    path = path.Substring(1);
	}
	System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(System.IO.Path.Combine(path, "..", "..", "modules"));
	foreach (System.IO.FileInfo f in dir.GetFiles("*.dll")) {
	    // FIXME: this causes interaction with ROS loading, so we don't load it:
	    if (!f.FullName.EndsWith("ros.dll")) {
		//System.Console.WriteLine(f.FullName);
		evaluator.LoadAssembly(f.FullName);
	    }
	}
    }
    
    /*
      public override void PostSetup(MainWindow calico) {
      base.PostSetup(calico);
      CSharp.set_dlr(manager.scope, manager.scriptRuntime);
      }
    */
    
    public override bool Execute(string text) {
	string program = "";
        foreach(string line in text.Split('\n')) {
            if (line.StartsWith("using ")) {
                evaluator.Run(line);
            } else {
                program += line + "\n";
	    }
	}
	// Next, everything else:
        if (program != "") {
	    try {
		object retval = evaluator.Evaluate(program);
		if (retval != null) {
		    System.Console.WriteLine(retval);
		}
	    } catch {
		// no result;
	    }
	}
	return true;
    }

    public override bool ExecuteFile(string filename) {
        string text = null;
        if (System.IO.File.Exists(filename)) {
            System.IO.TextReader reader = new System.IO.StreamReader(filename);
            text = reader.ReadToEnd();
            reader.Close();
        } else {
            return false;
        }
        if (text != null) {
            Execute(text);
	    return true;
        }
        return false;
    }
    
    public override bool ReadyToExecute(string text) {
	//Return True if expression parses ok.
	string [] lines = text.Split('\n');
	if (lines[lines.Length - 1] == "") {
	    return true; // force it
	}
	// else, only if valid parse
	return true;
    }
}

public class CalicoCSharpDocument : TextDocument {
    
    public override string [] GetAuthors() {
        return new string[] {
	    "Doug Blank <dblank@cs.brynmawr.edu>"
	};
    }
    
    public CalicoCSharpDocument(MainWindow calico, string filename, string language, string mimetype) :
	base(calico, filename, language, mimetype) {
    }
}

public class CalicoCSharpLanguage : Language {
    public CalicoCSharpLanguage () : 
	base("csharp",  "C#", new string[] { "cs" }, "text/x-csharp") {
	LineComment = "//";
    }
    
    public override void MakeEngine (LanguageManager manager) {
	engine = new CalicoCSharpEngine (manager);
    }
    
    public override Document MakeDocument(MainWindow calico, string filename) {
	return new CalicoCSharpDocument(calico, filename, name, mimetype);
    }
    
    public static new Language MakeLanguage () {
	return new CalicoCSharpLanguage ();
    }
	
    public override string GetUseLibraryString(string fullname) {
	string bname = System.IO.Path.GetFileNameWithoutExtension(fullname);
	return String.Format("using {0};\n", bname);
    }
}

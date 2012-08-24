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
    ConsoleReportPrinter reportPrinter;
    CompilerContext ctx;
    Evaluator evaluator;
    
    public CalicoCSharpEngine (LanguageManager manager) : base(manager) {
	settings = new CompilerSettings();
	reportPrinter = new ConsoleReportPrinter();
	ctx = new CompilerContext(settings, reportPrinter);
	evaluator = new Evaluator(ctx);
    }
    
    /*
      public override void PostSetup(MainWindow calico) {
      base.PostSetup(calico);
      CSharp.set_dlr(manager.scope, manager.scriptRuntime);
      }
    */
    
    public override bool Execute(string text) {
	evaluator.Run(text);
	return true;
    }

    public override bool ExecuteFile(string filename) {
	System.Console.WriteLine("Run filename '{0}'!", filename);
	// FIXME: howto?
	return true;
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

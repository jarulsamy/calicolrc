//
//  CalicoScheme.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011 The Calico Project
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
using System.Collections.Generic;
using System.IO;
using Calico;
using System.Text.RegularExpressions;
using Mono.Terminal;

public class CalicoSchemeEngine : Engine
{
  public CalicoSchemeEngine (LanguageManager manager) : base(manager)
  {
  }

  public override void PostSetup(MainWindow calico) {
    base.PostSetup(calico);
    this.calico = calico;
    Scheme.set_dlr(manager.scope, manager.scriptRuntime);
    Scheme.reset_toplevel_env();
  }

  public override bool Execute(string text, bool ok) {
    PJScheme.initialize_execute();
    object result = PJScheme.execute_string_rm(text);
    return HandleOutput(result, ok);
  }

  public void format_trace_back(object list) {
      object current = list;
      while (current != Scheme.EmptyList) {
	  Scheme.Cons info = (Scheme.Cons)Scheme.car(current);
	  object message = Scheme.car(info);
	  object line = Scheme.cadr(info);
	  object column = Scheme.caddr(info);
	  object proc_name = Scheme.cadddr(info);
	  System.Console.Error.WriteLine(
	       String.Format("  File: \"{0}\", line {1}, col {2}, calling '{3}'", 
			     message, line, column, proc_name));
	  current = Scheme.cdr(current);
      }
  }

  public bool HandleOutput(object result, bool ok) {
    string resultString = Scheme.repr(result);
    if (resultString.StartsWith("(exception ")) {
	System.Console.Error.WriteLine("Traceback (most recent call last):");
	if (Scheme.list_q(result) && ((int)Scheme.length(result)) == 2) {
	    object list = Scheme.cadr(result);
	    if (Scheme.list_q(list) && ((int)Scheme.length(list)) == 6) {
		object error = ((Scheme.Cons)list)[0];
		object message = ((Scheme.Cons)list)[1];
		object src_file = ((Scheme.Cons)list)[2];
		object src_line = ((Scheme.Cons)list)[3];
		object src_col = ((Scheme.Cons)list)[4];
		object stack = ((Scheme.Cons)list)[5];
		format_trace_back(stack);
		if (src_file.ToString() != "none") {
		    System.Console.Error.WriteLine(String.Format("  File: \"{0}\", line {1}, col {2}", 
								 src_file, src_line, src_col));
		}
		System.Console.Error.WriteLine(String.Format("{0}: {1}", error, message));
	    } else {
		System.Console.Error.WriteLine(list);
	    }
	} else {
	    System.Console.Error.WriteLine(resultString);
	}
	return false;
    }
    if (result != null) {
	System.Console.WriteLine(resultString);
    }
    if (ok) {
	calico.manager.stderr.PrintLine(Calico.Tag.Info, "Done");
    }
    return true;
  }

  public override bool Execute(string text) {
    PJScheme.initialize_execute();
    object result = PJScheme.execute_string_rm(text);
    return HandleOutput(result, false);
  }

  public override bool ExecuteFile(string filename) {
    PJScheme.initialize_execute();
    object obj = PJScheme.execute_file_rm(filename);
    return HandleOutput(obj, true);
  }

  public override bool ReadyToExecute(string text) {
    //Return True if expression parses ok.
    string [] lines = text.Split('\n');
    if (lines[lines.Length - 1].Trim() == "") {
      return true; // force it
    }
    // else, only if valid parse
    return PJScheme.try_parse(text);
  }

  public override void SetTraceOn(MainWindow calico) {
      PJScheme.tracing_on(PJScheme.list(true));
  }

  public override void SetTraceOff() {
      PJScheme.tracing_on(PJScheme.list(false));
  }

  public static void Main(string[] args) {
      LanguageManager manager = new LanguageManager(new List<string>(){"scheme"}, 
						    "..", 
						    new Dictionary<string, Language>());
	CalicoSchemeLanguage scheme = new CalicoSchemeLanguage();
	scheme.MakeEngine(manager);
	bool interactive = false;

	if (args.Length > 0) {
	  foreach (string file in args) {
		if (file.StartsWith("-")) {
		  if (file == "-i") {
			interactive = true;
		  }
		} else {
		  scheme.engine.ExecuteFile(file);
		}
	  } 
	} else {
	  interactive = true;
	}
	
	if (interactive) {
	  LineEditor le = new LineEditor ("Calico Scheme", 1000);
	  le.TabAtStartCompletes = false;
	  string line, expr = "";
	  string prompt = "scheme>>> ";
	  string indent = "";
	  while ((line = le.Edit(prompt, indent)) != null) {
        if (expr != "")
          expr = expr + "\n" + line;
        else
          expr = line;
	    if (scheme.engine.ReadyToExecute(expr)) {
  	          PJScheme.initialize_execute();
		  scheme.engine.Execute(expr);
		  expr = "";
		  prompt = "scheme>>> ";
		  indent = "";
	    } else {
		  prompt = "......>>> ";
		  Match match = Regex.Match(line, "^\t*");
		  if (match.Success)
		    indent = match.Value;
	    }
	  }
	}
  }
}

public class CalicoSchemeDocument : TextDocument {

	public override string [] GetAuthors() 
	{
        return new string[] {
			"Jim Marshall <jmarshall@sarahlawrence.edu>",
			"Doug Blank <dblank@cs.brynmawr.edu>"
		};
    }

	public CalicoSchemeDocument(MainWindow calico, string filename, string language, string mimetype) :
            	   base(calico, filename, language, mimetype) {
    }

}
	
public class CalicoSchemeLanguage : Language
{
	public CalicoSchemeLanguage () : 
        base("scheme",  "Scheme", new string[] { "ss", "scm", "s" }, "text/x-scheme")
	{
	  LineComment = ";;";
	}
    
	public override void MakeEngine (LanguageManager manager)
	{
		engine = new CalicoSchemeEngine (manager);
	}

        public override Document MakeDocument(MainWindow calico, string filename) {
	        return new CalicoSchemeDocument(calico, filename, name, mimetype);
        }

	public static new Language MakeLanguage ()
	{
		return new CalicoSchemeLanguage ();
	}

        public override string GetUseLibraryString(string fullname) {
		string bname = System.IO.Path.GetFileNameWithoutExtension(fullname);
                return String.Format("(using \"{0}\")\n", bname);
     }
}

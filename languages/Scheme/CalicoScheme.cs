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
    Scheme.set_dlr(manager.scope, manager.scriptRuntime);
  }

	public override bool Execute(string text, bool ok) {
    object result = PJScheme.execute_string_rm(text);
    if (result == null) {
       return true;
    }
    string resultString = Scheme.repr(result);
    // FIXME: when exceptions have a better format in Scheme:
    if (resultString.StartsWith("(exception ")) {
      return false;
    }
    return true;
	}


  public override bool Execute(string text) {
    object result = PJScheme.execute_string_rm(text);
    if (result == null) {
       return true;
    }
    string resultString = Scheme.repr(result);
    // FIXME: when exceptions have a better format in Scheme:
    if (resultString.StartsWith("(exception ")) {
      System.Console.Error.WriteLine(resultString);
      return false;
    }
    System.Console.WriteLine(resultString);
    return true;
  }

  public override bool ExecuteFile(string filename) {
    System.Console.WriteLine("Run filename '{0}'!", filename);
    object obj = PJScheme.execute_file_rm(filename);
    if (obj != null) {
      string str = Scheme.repr(obj);
      if (str != "") {
	Console.Error.WriteLine(str);
	return false;
      }
    }
    return true;
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

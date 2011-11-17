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

public class CalicoSchemeEngine : Engine
{
  public CalicoSchemeEngine (LanguageManager manager) : base(manager)
  {
  }

  public override void Setup() {
    base.Setup();
    Scheme.set_dlr(manager.scope, manager.scriptRuntime);
  }

  public override bool Execute(string text) {
    string result = PJScheme.execute(text).ToString();
    // FIXME: when exceptions have a better format in Scheme:
    if (result.StartsWith("(exception ")) {
      System.Console.Error.WriteLine(result);
      return false;
    } else {
      System.Console.WriteLine(result);
      return true;
    }
  }

  public override bool ExecuteFile(string filename) {
    System.Console.WriteLine("Run filename '{0}'!", filename);
    PJScheme.execute_file(filename);
    return true;
  }

  public override bool ReadyToExecute(string text) {
    //Return True if expression parses ok.
    string [] lines = text.Split('\n');
    if (lines[lines.Length - 1] == "") {
      return true; // force it
    }
    // else, only if valid parse
    string retval = PJScheme.try_parse_string(text).ToString();
    // FIXME: when exceptions have a better format in Scheme:
    return (!retval.StartsWith("(exception "));
  }
}

public class CalicoSchemeLanguage : Language
{
	public CalicoSchemeLanguage () : 
        base("scheme",  "Scheme", new string[] { "ss", "scm", "s" }, null)
	{
	}
    
	public override void MakeEngine (LanguageManager manager)
	{
		engine = new CalicoSchemeEngine (manager);
	}

	public static new Language MakeLanguage ()
	{
		return new CalicoSchemeLanguage ();
	}
}

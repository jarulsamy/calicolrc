//
//  CalicoRuby.cs
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

public class CalicoRubyEngine : DLREngine {

    public CalicoRubyEngine(LanguageManager manager) : base(manager) {
        dlr_name = "rb";
        scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
        languageSetup = IronRuby.Ruby.CreateRubySetup();
        // Set LanguageSetup options here:
        // languageSetup.Options["FullFrames"] = true; // for debugging Python only?
        scriptRuntimeSetup.LanguageSetups.Add(languageSetup);
        // Ruby-only scope:
        scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
        scope = scriptRuntime.CreateScope();
    }

    public override void Start(string path) {
	  // Get engine from manager:
	  try {
		engine = manager.scriptRuntime.GetEngine(dlr_name);
	  } catch {
		System.Console.Error.WriteLine("ERROR: Ruby failed to start");
	  }
	  // Set the compiler options here:
	  if (engine != null)
		compiler_options = engine.GetCompilerOptions();
	  //IronRuby.Compiler.RubyCompilerOptions options = (IronRuby.Compiler.RubyCompilerOptions)compiler_options;
	  // set some ruby options
    }
}

public class CalicoRubyDocument : TextDocument {

    public CalicoRubyDocument(MainWindow calico, string filename, string language, string mimetype) :
         	   base(calico, filename, language, mimetype) {
    }
	
	public override string [] GetAuthors() 
	{
	    return new string[] {
			"Microsoft Corporation",
			"The IronRuby Team"
		};
	}
}
	
public class CalicoRubyLanguage : Language {

    public CalicoRubyLanguage() :
	    base("ruby", "Ruby", new string[] { "rb"}, "text/x-ruby") {
	}

    public override void MakeEngine(LanguageManager manager) {
        engine = new CalicoRubyEngine(manager);
    }

    public override Document MakeDocument(MainWindow calico, string filename) {
      	return new CalicoRubyDocument(calico, filename, name, mimetype);
    }

    public static new Language MakeLanguage() {
        return new CalicoRubyLanguage();
    }

    public override string GetUseLibraryString(string fullname) {
		string bname = System.IO.Path.GetFileNameWithoutExtension(fullname);
                return String.Format("require \"{0}\"\n", bname);
    }
}

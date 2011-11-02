//
//  EngineManager.cs
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
using System.Collections.Generic; // IList, Dictionary

namespace Calico {
    public class EngineManager {
        // FIXME: make MainWindow a general interface (without Gtk)
        public MainWindow calico;
        private Dictionary<string, Engine> engines;
        public CustomStream stderr;
        public CustomStream stdout;
        public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        public Microsoft.Scripting.Hosting.ScriptScope scope;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;

        public EngineManager(MainWindow calico) {
            this.calico = calico;
            engines = new Dictionary<string, Engine>();
        }

        public Engine this[string name] {
            get {return engines[name];}
            set {engines[name] = value;}
        }

        public string[] get_languages() {
            // FIXME: sort
            string[] keys = new string[engines.Count];
            engines.Keys.CopyTo(keys, 0);
            return keys;
        }

        public void register(Language language) {
            try {
                engines[language.name] = language.make_engine(this);
            } catch {
                Console.WriteLine("Skipping language {0}", language.name);
            }
        }

        public void setup() {
            scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
            foreach (string engine in engines.Keys) {
                try {
                    engines[engine].setup();
                } catch {
                    Console.Error.WriteLine("Engine failed to initialize: {0}", engine);
                    engines.Remove(engine);
                }
            }
            // Language neutral scope:
            //scriptRuntimeSetup.LanguageSetups.Add( FIXME: in progress
            scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
            scope = scriptRuntime.CreateScope();
      }

        public void set_redirects(CustomStream stdout, CustomStream stderr) {
            // textviews:
            this.stderr = stderr;
            this.stdout = stdout;
            foreach (string engine in engines.Keys) {
                engines[engine].set_redirects(this.stdout, this.stderr);
            }
        }

        public void start() {
            foreach (string engine in engines.Keys) {
                engines[engine].start();
            }
        }

        public void reset() {
            setup();
            start();
            set_redirects(stdout, stderr);
        }
    }
}



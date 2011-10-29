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
using System.Collections.Generic;
// IList, Dictionary
namespace Calico {
    public class EngineManager {
        public Project calico;
        //public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
        public Dictionary<string, Engine> engines;
        //public Microsoft.Scripting.Hosting.ScriptRuntime runtime;
        //public Microsoft.Scripting.Hosting.ScriptScope scope;
        public string stderr;
        public string stdout;
        public string stdin;


        public EngineManager(Project calico) {
            this.calico = calico;
            engines = new Dictionary<string, Engine>();
        }

        //def __getitem__(self, name):
        //return self.engine[name]

        public string[] get_languages() {
            // FIXME: sort
            string[] keys = new string[engines.Count];
            engines.Keys.CopyTo(keys, 0);
            return keys;
        }

        public void register(Language language) {
            try {
                engines[language.name] = language.make_engine();
            } catch {
                Console.WriteLine("Skipping language {0}", language.name);
            }
        }

        public void setup() {
            foreach (string engine in engines.Keys) {
                try {
                    engines[engine].setup();
                } catch {
                    Console.Error.WriteLine("Engine failed to initialize: {0}", engine);
                    engines.Remove(engine);
                }
            }
        }

        public void set_redirects(string stdout, string stderr, string stdin) {
            // textviews:
            this.stderr = stderr;
            this.stdout = stdout;
            this.stdin = stdin;
            foreach (string engine in engines.Keys) {
                engines[engine].set_redirects(this.stdout, this.stderr, this.stdin);
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
            set_redirects(stdout, stderr, stdin);
        }
    }
}



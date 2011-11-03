//
//  LanguageManager.cs
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
    public class LanguageManager {
        // FIXME: make MainWindow a general interface (without Gtk)
        public MainWindow calico;
        public Dictionary<string, Language> languages;
        public CustomStream stderr;
        public CustomStream stdout;
        public Microsoft.Scripting.Hosting.ScriptRuntime scriptRuntime;
        public Microsoft.Scripting.Hosting.ScriptScope scope;
        public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;

        public LanguageManager(Dictionary<string,Language> langs) {
            languages = new Dictionary<string, Language>();
            foreach (string name in langs.Keys) {
                register(langs[name]); // This may fail, which won't add language
            }
            setup();
            start();
        }

        public Language this[string name] {
            get {return languages[name];}
            set {languages[name] = value;}
        }

        public string[] getLanguages() {
            // FIXME: sort
            string[] keys = new string[languages.Count];
            languages.Keys.CopyTo(keys, 0);
            return keys;
        }

        public void register(Language language) {
            try {
                language.MakeEngine(this); // Makes a default engine
            } catch {
                Console.WriteLine("Register failed; skipping language {0}", language.name);
                return;
            }
            languages[language.name] = language; // ok, save it
        }

        public void setup() {
            // In case it needs it:
            scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
            foreach (string language in getLanguages()) {
                try {
                    languages[language].engine.setup();
                } catch {
                    Console.Error.WriteLine("Language failed to initialize: {0}", language);
                    languages.Remove(language);
                }
            }
            // Language neutral scope:
            try {
                scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
                scope = scriptRuntime.CreateScope();
            } catch {
                Console.Error.WriteLine("No DLR languages were loaded.");
            }
      }

        public void set_redirects(CustomStream stdout, CustomStream stderr) {
            // textviews:
            this.stderr = stderr;
            this.stdout = stdout;
            foreach (string language in languages.Keys) {
                languages[language].engine.set_redirects(this.stdout, this.stderr);
            }
        }

        public void start() {
            foreach (string language in languages.Keys) {
                languages[language].engine.start();
            }
        }

        public void reset() {
            setup();
            start();
            set_redirects(stdout, stderr);
        }
    }
}



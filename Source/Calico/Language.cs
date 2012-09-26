//
//  Language.cs
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

namespace Calico {

    public class Language {
        public string name;
        public Engine engine;
        public string proper_name;
        public string[] extensions;
        public string mimetype;
        public bool IsTextLanguage = true;
        public string LineComment = "";
        public Config config;

        public Language() {
        }

        public Language(string language, string proper, string[] extensions, string mimetype) {
            this.name = language;
            this.proper_name = proper;
            this.extensions = extensions;
            this.mimetype = mimetype;
            InitializeConfig();
        }
        
        public virtual void InitializeConfig() {
            // Just used for init and load
            config = new Config();
            config.SetValue(System.String.Format("{0}-language", name), "reset-shell-on-run", "bool", true);
        }

        public virtual void LoadConfig(Config global_config) {
            // Load the Config into the Global Config
            string section = System.String.Format("{0}-language", name);
            if (config != null && config.HasValue(section)) {
                foreach(string setting in config.GetValue(section)) {
                    if (! global_config.HasValue(section, setting)) {
                        global_config.SetValue(
                            section, 
                            setting, 
                            config.types[section][setting], 
                            config.values[section][setting]);
                    } // else already been defined
                }
            }
        }
        
        public virtual void MakeEngine(LanguageManager manager) {
            engine = new Engine(manager);
        }

        public virtual Document MakeDocument(MainWindow calico, string filename) {
          return new TextDocument(calico, filename, name, mimetype);
        }

        public virtual Document MakeDocument(MainWindow calico, string filename, string mimetype) {
          return new TextDocument(calico, filename, name, mimetype);
        }

        public static Language MakeLanguage() {
            return null;
        }

        public virtual string GetUseLibraryString (string fullname)
        {
            return "";
        }
        
        public virtual string getExamplesPath(string path) {
            return System.IO.Path.Combine(path, System.IO.Path.Combine("..", System.IO.Path.Combine("examples", name)));
        }
    }    
}

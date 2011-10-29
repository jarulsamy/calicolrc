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
        public string proper_name;
        public string[] extensions;

        public Language(string language, string proper, string[] extensions) {
            this.name = language;
            this.proper_name = proper;
            this.extensions = extensions;
        }

        public virtual Engine make_engine(EngineManager manager) {
            return new Engine(manager);
        }

        public static Language RegisterLanguage() {
            return null;
        }
    }
    
}

//
//  Engine.cs
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

    public class Engine {
        public LanguageManager manager;

        public Engine(LanguageManager manager) {
            this.manager = manager;
        }

        public virtual bool ReadyToExecute(string text) {
            return true;
        }

        public virtual bool Execute(string text) {
            return true;
        }

        public virtual bool ExecuteFile(string filename) {
            return true;
        }

        public virtual void Setup() {
        }

        public virtual void Start() {
        }

        public virtual void SetRedirects(CustomStream stdout, CustomStream stderr) {
        }

    }
    
}

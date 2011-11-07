//  
//  History.cs
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

namespace Calico {
    public class History {

        List<string> history;
        int position;

        public int Last {
            get {return history.Count - 1;}
        }
        public int Position {
            get {
                if (position < 0)
                    return history.Count - position;
                else
                    return position;
            }
        }

        public History() : this(new List<string>()) {
        }

        public History(List<string> history) {
             this.history = history;
            if (history.Count == 0 || history[Last] != "") {
                history.Add("");
            }
            position = history.Count - 1;
        }

        public string up() {
            if (position > 0) {
                position -= 1;
            }
            return history[Position];
        }

        public string down() {
            if (position < history.Count - 1) {
                position += 1;
            }
            return history[Position];
        }

        public void update(string text) {
            if (text != "")
                text = text.TrimEnd() + "\n";
            history[Position] = text;
        }

        public void add(string text) {
            if (text != "")
                text = text.TrimEnd() + "\n";
            if (history[Last] != text) { // different
                history.Add(text);
            }
            position = history.Count - 1;
        }

        public void last(string text) {
            if (text != "")
                text = text.TrimEnd() + "\n";
            // turns space into last command
            if (history.Count > 1 && history[Last - 1] == text) {
                // pass // same, skip it!
            } else {
                history[Last] = text;
            }
            position = history.Count - 1;
        }
    }
}

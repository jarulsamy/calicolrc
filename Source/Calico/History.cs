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

        public string CurrentLanguage {
            get {
		string line = null;
                if (position < 0)
                    line = history[history.Count - position];
                else
                    line = history[position];
		if (line.Contains("|;|")) {
		    string [] parts = line.Split(new string [] {"|;|"}, StringSplitOptions.None);
		    if (parts[0] == String.Empty)
			return "";
		    else
			return parts[0];
		} else {
		    return "";
		}
            }
        }

	public string GetCode(string line) {
	    if (line.Contains("|;|")) {
		string [] parts = line.Split(new string [] {"|;|"}, StringSplitOptions.None);
		return String.Join("|;|", parts, 1, parts.Length - 1);
	    } else {
		return line;
	    }
	}

        public History() : this(new List<string>()) {
        }

        public History(List<string> history) {
            // reference
            this.history = history;
            if (history.Count == 0 || GetCode(history[Last]) != "") {
                history.Add("");
            }
            position = history.Count - 1;
        }

        public string up() {
            if (position > 0) {
                position -= 1;
            }
            return GetCode(history[Position]);
        }

        public string down() {
            if (position < history.Count - 1) {
                position += 1;
            }
            return GetCode(history[Position]);
        }

        public void update(string text) {
            if (text != "")
                text = text.TrimEnd() + "\n";
	    string language = CurrentLanguage;
            history[Position] = language + "|;|" + text;
        }

        public void update(string text, string language) {
            if (text != "")
                text = text.TrimEnd() + "\n";
	    if (language == null)
		language = "";
            history[Position] = language + "|;|" + text;
        }

        public string update() {
            return GetCode(history[Position]);
        }

        public void add(string text, string language) {
            if (text != "")
                text = text.TrimEnd() + "\n";
	    if (language == null)
		language = "";
            if (history[Last] != language + "|;|" + text) { // different
                history.Add(language + "|;|" + text);
            }
            position = history.Count - 1;
        }

        public void last(string text, string language) {
            if (text != "")
                text = text.TrimEnd() + "\n";
	    if (language == null)
		language = "";
            // turns space into last command
            if (history.Count > 1 && history[Last - 1] == language + "|;|" + text) {
                // pass // same, skip it!
            } else {
                history[Last] = language + "|;|" + text;
            }
            position = history.Count - 1;
        }

        public bool SearchMore(string text) {
            if (text == "") return true;
            text = text.ToLower();
            for (int i = position; i > 0; i--) {
                if (GetCode(history[i]).ToLower().Contains(text)) {
                    position = i;
                    return true;
                }
            }
            return false;
        }

        public bool SearchPrevious(string text) {
            if (text == "") return true;
            text = text.ToLower();
            for (int i = position - 1; i > 0; i--) {
                if (i > 0 && GetCode(history[i]).ToLower().Contains(text)) {
                    position = i;
                    return true;
                }
            }
            return false;
        }

        public bool SearchNext(string text) {
            if (text == "") return true;
            text = text.ToLower();
            for (int i = position + 1; i < history.Count; i++) {
                if (i < history.Count && GetCode(history[i]).ToLower().Contains(text)) {
                    position = i;
                    return true;
                }
            }
            return false;
        }
    }
}

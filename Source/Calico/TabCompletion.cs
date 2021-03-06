//  
//  TabCompletition.cs
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

    public class Variable {
	public bool valid;
	public object value;

	public Variable(bool valid, object value) {
	    this.valid = valid;
	    this.value = value;
	}
    }

    public class TabCompletion {
	Engine engine = null;
        public IList<string> items = null;
        Mono.TextEditor.TextEditor shell;
        int tab_position;
        int original_offset;
        string variable;
        string partial;
	public string full_prefix = "";
	string prefix = "";

        public static string [] ArrayRange(string [] array, int start, int stop) {
            List<string> temp = new List<string>();
            if (stop == -1)
                stop = array.Length - 2;
            int pos = 0;
            foreach (string s in array) {
                if (pos >= start && pos <= stop)
                    temp.Add(s);
                pos++;
            }
            return temp.ToArray();
        }

        public bool hasattr(object value, string part) {
            return dir(value).Contains(part);
        }

        public object getattr(object value, string part) {
            return engine.GetMember(value, part);
        }

        public IList<string> dir(object obj) {
            return engine.GetMemberNames(obj);
        }

        public IList<string> getItems() {
	    List<string> full_items = new List<string>();
	    foreach (string item in items) {
		full_items.Add(prefix + item);
	    }
	    return full_items;
        }

        public TabCompletion(Engine engine, 
			     Mono.TextEditor.TextEditor shell, 
			     string text) {
	    this.engine = engine;
            this.shell = shell;
            tab_position = 0;
	    if (shell != null) 
		original_offset = shell.Caret.Offset;
            variable = find_variable(text);
            partial = "";
            items = new List<string>();
            if (variable != null) {
                string [] parts = engine.GetVariableParts(variable);
                if (parts.Length == 1) { // Easy, just get the vars that match:
                    string root = parts[0];
                    partial = root;
		    full_prefix = root;
                    items = engine.GetCompletions(root);
                    // and not hasattr(x, "DeclaringType")]
                } else {
                    string root = parts[0];
                    Calico.Variable cvariable = engine.TryGetVariable(root);
		    object value = cvariable.value;
		    prefix = root + ".";
                    if (cvariable.valid) {
                        foreach (string part in ArrayRange(parts, 1, -1)) {
                            if (hasattr(value, part)) {
                                value = getattr(value, part);
				prefix += part + ".";
                            } else {
                                value = null;
                                break;
                            }
                        }
                        if (value != null) {
                            partial = parts[parts.Length - 1];
			    full_prefix = prefix + partial;
                            items = new List<string>();
                            foreach(string x in dir(value)) {
                                if (x.StartsWith(partial) && ! x.StartsWith("_"))
                                    items.Add(x);
                            }
                        } else {
			    full_prefix = prefix;
			}
                    }
                }
            }
        }

        public void insertText() {
            // Remove any stuff:
            shell.Remove(original_offset, shell.Caret.Offset - original_offset);
            // Move cursor:
            shell.Caret.Offset = original_offset;
            if (tab_position >= items.Count) {
                tab_position = 0;
            }
            // insert text:
            string word = items[tab_position].Substring(partial.Length);
            shell.InsertAtCaret(word);
            // get ready for next possible completion:
            tab_position += 1;
        }

        public string format() {
            string retval = "";
            if (items.Count > 0) {
                retval = "----------------------\n" + String.Format("Possible completions for '{0}' ({1}):\n   ", variable, items.Count);
                int count = 0;
                foreach (string item in items) {
                    if (count % 3 == 0) {
                        retval += "\n";
                    }
                    retval += String.Format("{0}\t", item); //"%-25s" % item;
                    count += 1;
                }
                return retval + "\n----------------------\n";
            } else {
                return "";
            }
        }

        public static string reversed(string text) {
            string retval = "";
            foreach (char c in text) {
                retval = c + retval;
            }
            return retval;
        }

        public static bool IsNumber(string text) {
            foreach (char c in text) {
                if (! char.IsNumber(c))
                    return false;
            }
            return true;
        }

        public string find_variable(string text) {
            //
            // Finds variable-like characters in a text.
            //
            string candidate = "";
            foreach (char c in reversed(text)) {
                if (char.IsLetterOrDigit(c) || c == '_' || c == '.') {
                    candidate += c;
                } else {
                    break;
                }
            }
            candidate = reversed(candidate);
            if (IsNumber(candidate)) {
                return "";
            }
            return candidate;
        }
    }
}


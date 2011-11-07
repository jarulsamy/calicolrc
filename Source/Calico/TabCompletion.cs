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

    public class TabCompletion {
        public List<string> items = null;
        Mono.TextEditor.TextEditor shell;
        int tab_position;
        int original_offset;
        string variable;
        string partial;

        public static string [] ArrayRange(string [] array, int start, int stop) {
            List<string> temp = new List<string>();
            if (stop == -1)
                stop = array.Length - 1;
            int pos = 0;
            foreach (string s in array) {
                if (pos >= start && pos <= stop)
                    temp.Add(s);
                pos++;
            }
            return temp.ToArray();
        }

        public static bool hasattr(object value, string part) {
            // FIXME:
            Console.WriteLine("hasattr({0}, {1})", value, part);
            return false;
        }

        public static object getattr(object value, string part) {
            // FIXME:
            return null;
        }

        public List<string> dir(object obj) {
            // FIXME: is this all? Constructors, other members?
            return Reflection.Utils.getMethodNames(obj.GetType());
        }

        public TabCompletion(MainWindow calico, Mono.TextEditor.TextEditor shell, string text) {
            this.shell = shell;
            tab_position = 0;
            original_offset = shell.Caret.Offset;
            variable = find_variable(text);
            partial = "";
            items = new List<string>();
            if (variable != null) {
                string [] parts = calico.manager[calico.CurrentLanguage].engine.getVariableParts(variable);
                if (parts.Length == 1) { // Easy, just get the vars that match:
                    string root = parts[0];
                    partial = root;
                    items = calico.manager[calico.CurrentLanguage].engine.getCompletions(root);
                    // and not hasattr(x, "DeclaringType")]
                } else {
                    string root = parts[0];
                    bool found;
                    object value = null;
                    found = calico.manager[calico.CurrentLanguage].engine.tryGetVariable(root, out value);
                    if (found) {
                        foreach (string part in ArrayRange(parts, 1, -1)) {
                            if (hasattr(value, part)) {
                                value = getattr(value, part);
                            } else {
                                value = null;
                                break;
                            }
                        }
                        if (value != null) {
                            partial = parts[parts.Length - 1];
                            items = new List<string>();
                            foreach(string x in dir(value)) {
                                if (x.StartsWith(partial) && ! x.StartsWith("_"))
                                    items.Add(x);
                            }
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
                    retval += item; //"%-25s" % item;
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


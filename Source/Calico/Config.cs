//  
//  Config.cs
//  
//  Author:
//       dblank <${AuthorEmail}>
// 
//  Copyright (c) 2011 dblank
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
using System.Xml;
using System.Collections.Generic;

namespace Calico {
    public class Config {
        public string filename;
        public Dictionary<string, Dictionary<string,object>> values = new Dictionary<string, Dictionary<string,object>>();
        public Dictionary<string, Dictionary<string,string>> types = new Dictionary<string, Dictionary<string,string>>();
        public Config(string filename) {
            this.filename = filename;
            // Check path:
            string directory = System.IO.Path.GetDirectoryName(filename);
            if (!System.IO.Directory.Exists(directory)) {
                System.IO.Directory.CreateDirectory(directory);
            }
            Initialize();
            // Load file, if exists:
            if (System.IO.File.Exists(filename)) {
                Load();
            } else {
                Save();
            }
        }
        public void Initialize() {
            SetValue("config", "font", "string", "Courier");
            SetValue("config", "font-size", "int", 10);
            SetValue("config", "languages", "strings", new List<string>() {"all"});
            SetValue("config", "recent-files", "strings", new List<string>());
            SetValue("shell", "history", "strings", new List<string>());
        }
        public object GetValue(string section, string setting) {
            return values[section][setting];
        }
        public void SetValue(string section, string setting, string type, object value) {
            if (!values.ContainsKey(section)) {
                values[section] = new Dictionary<string,object>();
                types[section] = new Dictionary<string,string>();
            }
            values[section][setting] = value;
            types[section][setting] = type;
        }
        public void Load() {
            // Loads the file on top of those already loaded
            XmlReader xr = new XmlTextReader(filename);
            string node_type = null;
            string section = null;
            string setting = null;
            string type = null;
            string value = null;
            object list = null;
            while (xr.Read()) {
                switch (xr.NodeType) {
                case XmlNodeType.Text:
                    if (xr.HasValue)
                        if (list != null)
                            ((List<string>)list).Add((string)makeValue(type, xr.Value));
                        else
                            value = xr.Value;
                    break;
                case XmlNodeType.Element: // section or setting
                    if (xr.Name.ToLower() == "section") {
                        node_type = xr.Name.ToLower();
                        section = xr.GetAttribute("name");
                        if (!values.ContainsKey(section)) {
                            // allow new sections defined in file
                            values[section] = new Dictionary<string, object>();
                        }
                    } else if (xr.Name.ToLower() == "setting") {
                        list = null;
                        value = null;
                        node_type = xr.Name.ToLower();
                        setting = xr.GetAttribute("name");
                        type = xr.GetAttribute("type");
                        if (type == "strings")
                            list = new List<string>();
                    } else if (xr.Name.ToLower() == "item") {
                        // ok, will add to list
                    }
                    break;
                case XmlNodeType.EndElement: // section or setting
                    if (node_type == "setting") {
                        if (type == "strings")
                            values[section][setting] = list;
                        else
                            values[section][setting] = makeValue(type, value);
                    }
                    break;
                }
            }
            xr.Close();
        }
        public object makeValue(string type, string value) {
            if (type == "int") {
                return System.Int32.Parse(value);
            } else if (type == "string") {
                return value;
            } else if (type == "strings") {
                return value;
            } else {
                throw new Exception(String.Format("no such config type: {0}", type));
            }
        }
        public void Save() {
            XmlWriterSettings settings = new XmlWriterSettings();
            settings.Indent = true;
            settings.IndentChars = "    ";
            settings.Encoding = System.Text.Encoding.UTF8;
            using (XmlWriter xw = XmlWriter.Create(filename, settings)) {
                xw.WriteStartDocument();
                xw.WriteStartElement("sections");
                foreach(KeyValuePair<string,Dictionary<string,object>> pair in values) {
                    xw.WriteStartElement("section");
                    xw.WriteAttributeString("name", pair.Key);
                    foreach(KeyValuePair<string,object> vs in pair.Value) {
                        xw.WriteStartElement("setting");
                        xw.WriteAttributeString("name", vs.Key);
                        xw.WriteAttributeString("type", types[pair.Key][vs.Key]);
                        if (types[pair.Key][vs.Key] == "strings") {
                            foreach(string item in (List<string>)values[pair.Key][vs.Key]) {
                                xw.WriteStartElement("item");
                                xw.WriteValue(item);
                                xw.WriteEndElement();
                            }
                        } else {
                            xw.WriteValue(vs.Value);
                        }
                        xw.WriteEndElement();
                    }
                    xw.WriteEndElement();
                }
                xw.WriteEndElement();
                xw.WriteEndDocument();
            }
        }
    }
}


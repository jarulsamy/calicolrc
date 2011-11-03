//
//  Document.cs
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

namespace Calico {
    public class Document {
        public string language;
        public string filename;
        public string basename;
        public Document document;
        public Gtk.ScrolledWindow widget;
        public Gtk.Widget tab_widget;
        public Gtk.Label tab_label;
        // label for notebook page
        public Gtk.Button close_button;
        // tab close button
        public Document(string filename, string language) : base() {
            this.filename = filename;
            this.language = language;
            widget = new Gtk.ScrolledWindow();
            basename = System.IO.Path.GetFileName(filename);
            tab_widget = new Gtk.HBox();
            tab_label = new Gtk.Label(basename);
            ((Gtk.HBox)tab_widget).Add(tab_label);
            close_button = new Gtk.Button();
            Gtk.Image img = new Gtk.Image();
            close_button.Relief = Gtk.ReliefStyle.None;
            img = new Gtk.Image(Gtk.Stock.Close, Gtk.IconSize.Menu);
            close_button.Add(img);
            ((Gtk.HBox)tab_widget).Add(close_button);
            tab_label.TooltipText = filename;
            tab_widget.ShowAll();
        }
        public virtual bool GotoLine(int lineno) {
            return true;
        }

        public virtual bool IsDirty {
            get { return true; }
        }

        public virtual void Configure() {
            // For setting defaults
        }

        public virtual bool Close() {
            // Close document, and return successful close status
            return true;
        }

        public virtual void OnModified(object obj, System.EventArgs args) {
            if (IsDirty)
                tab_label.Text = String.Format("*{0}", basename);
            else
                tab_label.Text = basename;
        }
    }

    public class TextDocument : Document {
        public Mono.TextEditor.TextEditor texteditor;
        public Mono.TextEditor.TextEditorOptions options;

        public TextDocument(string filename, string language, string mimetype) : base(filename, language) {
            options = new Mono.TextEditor.TextEditorOptions();
            Mono.TextEditor.Document document = new Mono.TextEditor.Document();
            if (System.IO.File.Exists(filename)) {
                System.IO.TextReader reader = new System.IO.StreamReader(filename);
                document.Text = reader.ReadToEnd();
            } else {
                // FIXME: new file? invalid path? no longer exists?
            }
            texteditor = new Mono.TextEditor.TextEditor(document, options);
            texteditor.Document.MimeType = mimetype;
            widget.Add(texteditor);
            texteditor.Document.DocumentUpdated += OnModified;
            widget.ShowAll();
        }

        public override void Configure() {
            // FIXME: take into account user's defaults
            texteditor.Options.ShowInvalidLines = false;
            texteditor.Options.ShowLineNumberMargin = true;
            texteditor.Options.TabsToSpaces = true;
            texteditor.Options.HighlightCaretLine = true;
            texteditor.Options.HighlightMatchingBracket = true;
            texteditor.Options.OverrideDocumentEolMarker = true;
            texteditor.Options.DefaultEolMarker = "\n";
        }

        public override bool GotoLine(int lineno) {
            texteditor.GrabFocus();
            texteditor.ScrollTo(lineno);
            return true;
        }

        public override bool IsDirty {
            get { return texteditor.Document.IsDirty; }
        }

        public override bool Close() {
            return true;
        }
    }
}


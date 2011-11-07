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
        public MainWindow calico;
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

        public Document(MainWindow calico, string filename, string language) : base() {
            this.calico = calico;
            this.filename = filename;
            this.language = language;
            widget = new Gtk.ScrolledWindow();
            if (filename != null)
                basename = System.IO.Path.GetFileName(filename);
            else
               basename = String.Format("New {0} Script", calico.manager.languages[language].proper_name);
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

        public static string _(string message) {
            return global::Mono.Unix.Catalog.GetString(message);
        }

        public virtual bool GotoLine(int lineno) {
            return true;
        }

        public virtual bool HasContent {
            get {return false;}
        }

        public virtual bool IsDirty {
            get { return true; }
        }

        public virtual void Configure() {
            // For setting defaults
        }

        public virtual bool SaveAs() {
            return true;
        }

        public virtual bool Save() {
            return true;
        }

        public virtual bool Close() {
            // Close document, and return successful close status
            return true;
        }

        public virtual void OnDocumentUpdated(object obj, System.EventArgs args) {
            if (IsDirty)
                tab_label.Text = String.Format("*{0}", basename);
            else
                tab_label.Text = basename;
        }
    }

    public class TextDocument : Document {
        public Mono.TextEditor.TextEditor texteditor;
        public Mono.TextEditor.TextEditorOptions options;

        public TextDocument(MainWindow calico, string filename, string language, string mimetype) :
        base(calico, filename, language) {
            options = new Mono.TextEditor.TextEditorOptions();
            Mono.TextEditor.Document document = new Mono.TextEditor.Document();
            if (System.IO.File.Exists(filename)) {
                System.IO.TextReader reader = new System.IO.StreamReader(filename);
                document.Text = reader.ReadToEnd();
                reader.Close();
            } else {
                // FIXME: new file? invalid path? no longer exists?
            }
            texteditor = new Mono.TextEditor.TextEditor(document, options);
            texteditor.Document.MimeType = mimetype;
            widget.Add(texteditor);
            texteditor.Document.DocumentUpdated += OnDocumentUpdated;
            texteditor.Document.DocumentUpdated += OnDocumentUpdatedRunCheck;
            widget.ShowAll();
        }

        public void OnDocumentUpdatedRunCheck(object obj, System.EventArgs args) {
            if (texteditor.Document.Text == "") {
                calico.StartButton.Sensitive = false;
            } else {
                calico.StartButton.Sensitive = true;
            }
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
            var data = texteditor.GetTextEditorData();
            data.Caret.Line = lineno - 1;
            data.Caret.Column = 0;
            texteditor.ScrollToCaret();
            return true;
        }

        public override bool HasContent {
            get { return texteditor.Document.Text != ""; }
        }

        public override bool IsDirty {
            get { return texteditor.Document.IsDirty; }
        }

        public override bool Close() {
            return true;
        }

        public bool IsWritable(string filename) {
            string directory = System.IO.Path.GetDirectoryName(filename);
            string tempfile = System.IO.Path.Combine(directory, "tempfile.tmp");
            bool retval = true;
            try {
                System.IO.FileStream fp = System.IO.File.OpenWrite(tempfile);
                fp.Close();
                System.IO.File.Delete(tempfile);
            } catch {
                retval = false;
            }
            return retval;
        }

        public override bool SaveAs() {
            bool retval = false;
            string proposed_dir = "";
            if (filename != null) {
                proposed_dir = System.IO.Path.GetDirectoryName(filename);
                basename = System.IO.Path.GetFileName(filename);
            } else {
                proposed_dir = System.IO.Directory.GetCurrentDirectory();
                basename = "Untitled." + calico.manager[language].extensions[0]; // default
            }
            // first, let's make sure the directory is writable
            if (! IsWritable(proposed_dir)) {
                // if not, let's change dirs
                proposed_dir = System.Environment.GetFolderPath(
                    System.Environment.SpecialFolder.Personal);
            }
            if (System.IO.Directory.GetCurrentDirectory() != proposed_dir) {
                System.IO.Directory.SetCurrentDirectory(proposed_dir);
            }
            Gtk.FileChooserDialog fc = new Gtk.FileChooserDialog(_("Enter the file to save"),
                                       calico,
                                       Gtk.FileChooserAction.Save,
                                       _("Cancel"), Gtk.ResponseType.Cancel,
                                       _("Save"), Gtk.ResponseType.Accept);
            fc.CurrentName = basename;      // the file: entry text box
            fc.SelectFilename(basename);    // the file selection, if it exists
            fc.KeepAbove = true;
            if (fc.Run() == (int)Gtk.ResponseType.Accept) {
                // FIXME: check to see if already exists
                // ask to overwrite
                filename = fc.Filename;
                tab_label.TooltipText = filename;
                retval = Save();
                if (retval) {
                    language = calico.manager.GetLanguageFromExtension(filename);
                    basename = System.IO.Path.GetFileName(filename);
                    tab_label.Text = basename;
                    tab_label.TooltipText = filename;
                    //self.on_change_file();
                }
            }
            fc.Destroy();
            return retval;
        }

        public override bool Save() {
            if (filename != null) {
                try {
                    System.IO.StreamWriter sw = new System.IO.StreamWriter(filename);
                    sw.Write(texteditor.Document.Text);
                    sw.Close();
                    texteditor.Document.SetNotDirtyState();
                    return true;
                } catch {
                    // fail.. let's try SaveAs...
                }
            }
            bool retval = SaveAs();
            // if successful
            if (retval) {
                // change name of tab/filename/basefile
                basename = System.IO.Path.GetFileName(filename);
                tab_label.TooltipText = filename;
                tab_label.Text = basename;
            }
            return retval;
        }
    }
}


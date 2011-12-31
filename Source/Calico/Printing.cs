//
// Calico - Scripting Environment
//
// Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// $Id: $

using System;

public class Printing {
    Calico.MainWindow calico;
    string title;
    string contents;
    double headerHeight;
    double headerGap;
    int pangoScale;
    double fontSize;
    Gtk.PrintOperation printop;
    int linesPerPage;
    string [] lines;
    int numPages;
    int numLines;

    public Printing(Calico.MainWindow calico, string title, string text, string filename) {
      this.calico = calico;
      this.title = title;
      this.contents = text;
      this.headerHeight = (10*72/25.4);
      this.headerGap = (3*72/25.4);
      this.pangoScale = 1024;
      this.fontSize = 12.0; // FIXME: on Windows make bigger
      this.printop = new Gtk.PrintOperation();
      this.printop.BeginPrint += this.OnBeginPrint;
      this.printop.DrawPage += this.OnDrawPage;
      this.printop.EndPrint += this.OnEndPrint;
        // invoke
      this.printop.Run(Gtk.PrintOperationAction.PrintDialog, null);
      //Gtk.Application.Invoke(invoke);
    }

    public void OnBeginPrint(object obj, Gtk.BeginPrintArgs args) {
      Gtk.PrintContext context = args.Context;
      double height = context.Height;
      this.linesPerPage = (int)(Math.Floor(height / this.fontSize));
      this.lines = this.contents.Split('\n');
      numLines = this.lines.Length;
      numPages = (numLines - 1) / this.linesPerPage + 1;
      this.printop.NPages = numPages;
    }

    public void OnDrawPage(object obj, Gtk.DrawPageArgs args) {
      Gtk.PrintContext context = args.Context;
      var cr = context.CairoContext;
      double width = context.Width;
      cr.Rectangle (0, 0, width, this.headerHeight);
      cr.SetSourceRGB(0.8, 0.8, 0.8);
      cr.FillPreserve();
      cr.SetSourceRGB(0, 0, 0);
      cr.LineWidth = 1;
      cr.Stroke();
      
      var layout = context.CreatePangoLayout();
      var desc = calico.GetFont();
      layout.FontDescription = desc;
      layout.SetText(this.title);
      layout.Width = (int)(width);
      layout.Alignment = Pango.Alignment.Center;
        int layoutWidth = 0;
        int layoutHeight = 0;
      layout.GetSize(out layoutWidth, out layoutHeight);
      double textHeight = layoutHeight / this.pangoScale;
      cr.MoveTo(width/2, (this.headerHeight - textHeight) / 2);
      Pango.CairoHelper.ShowLayout(cr, layout);
      
      string pageStr = System.String.Format("{0}/{1}", args.PageNr + 1, this.numPages);
      layout.SetText(pageStr);
      layout.Alignment = Pango.Alignment.Right;
      cr.MoveTo(width - 2, (this.headerHeight - textHeight) / 2);
      Pango.CairoHelper.ShowLayout(cr, layout);
      
      string byline = System.Environment.UserName;
      layout.SetText(byline);
      layout.Alignment = Pango.Alignment.Left;
      cr.MoveTo(2, (this.headerHeight - textHeight) / 2);
      Pango.CairoHelper.ShowLayout(cr, layout);
      
      layout = context.CreatePangoLayout();
      desc = Pango.FontDescription.FromString("Monospace 12");
      //desc.Size = (int)(this.fontSize * this.pangoScale);
      layout.FontDescription = desc;
      cr.MoveTo(0, this.headerHeight + this.headerGap);
      int line = args.PageNr * this.linesPerPage;
      int i = 0;
      while (i < this.linesPerPage && line < numLines) {
	    layout.SetText(this.lines[line]);
	    Pango.CairoHelper.ShowLayout(cr, layout);
	    cr.RelMoveTo(0, this.fontSize);
	    line++;
	    i++;
      }
      //cr.Dispose();
      layout = null;
    }

    public void OnEndPrint(object obj, Gtk.EndPrintArgs args) {
      //pass
    }
}
    //if __name__ == "__main__":
    //    p = Printing("Test.py", "this is a test")

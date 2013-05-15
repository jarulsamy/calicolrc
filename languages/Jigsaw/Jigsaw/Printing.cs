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
using System.Collections.Generic;

namespace Jigsaw
{
	public class Printing 
	{
		Jigsaw.Canvas cvs = null;
		//Calico.MainWindow calico;
		string OS;
		string title;
		double headerHeight;
		double headerGap;
		int pangoScale;
		//double fontSize;
		Gtk.PrintOperation printop;
		int linesPerPage;
		string [] lines;
		int numPages;
		int numLines;

		//public Printing(Calico.MainWindow calico, string title, string filename) {
		public Printing(string OS, Jigsaw.Canvas cvs, string title) 
		{
			this.cvs = cvs;
			//this.calico = calico;
			this.OS = OS;
	      	this.title = title;
	        //if (calico.OS == "Windows") {
			if (OS == "Windows") {
	          this.headerHeight = (10*72/25.4 * 5);
	          this.headerGap = (3*72/25.4 * 5);
	        } else {
	          this.headerHeight = (10*72/25.4);
	          this.headerGap = (3*72/25.4);
	        }
			this.pangoScale = 1024;
			//this.fontSize = 10.0;

			this.printop = new Gtk.PrintOperation();
			this.printop.BeginPrint += this.OnBeginPrint;
			this.printop.DrawPage += this.OnDrawPage;
			this.printop.EndPrint += this.OnEndPrint;

			// invoke
			this.printop.Run(Gtk.PrintOperationAction.PrintDialog, null);
			//Gtk.Application.Invoke(invoke);
	    }

		public void OnBeginPrint(object obj, Gtk.BeginPrintArgs args) {
	      //Gtk.PrintContext context = args.Context;
	      //double height = context.Height;
	      //if (calico.OS == "Windows")
			if (OS == "Windows")
	            this.linesPerPage = 60;
	      	else
	            this.linesPerPage = 70;

			this.printop.NPages = 1;

			/*
	      this.lines = this.contents.Split('\n');
	      numLines = this.lines.Length;
	      numPages = (numLines - 1) / this.linesPerPage + 1;
	      this.printop.NPages = numPages;
	      */
	    }

	    public void OnDrawPage(object obj, Gtk.DrawPageArgs args) 
		{
			double width;
			Gtk.PrintContext context = args.Context;
			Cairo.Context g = context.CairoContext;

			//if (calico.OS == "Windows")
			if (this.OS == "Windows")
				width = context.Width - 200;
			else
				width = context.Width;

			// ---------------
			// Get the bounds of all the current blocks
			Bounds bounds = cvs.GetBlockBounds();

			// If bounds not initialized, set some default values
			if (!bounds.IsInitialized()) {
				bounds.Left = 0.0;
				bounds.Right = 1000.0;
				bounds.Top = 0.0;
				bounds.Bottom = 1000.0;
			}

			// TODO: Eliminate the block palette by translating
			// TODO: Check that there is at least one block (cvs.AllBlocks().Length)
			// TODO: Translation not accounted for when measuring bounds.

			// Calculate a suitable scale factor
			double s = 1.0;
			if ((double)bounds.Right > (double)bounds.Bottom ) s = context.Width/((double)bounds.Right - (double)bounds.Left);
			if ((double)bounds.Bottom > (double)bounds.Right ) s = context.Height/((double)bounds.Bottom - (double)bounds.Top);
			if (s > 10.0) s = 10.0;		// Clip at a scale of 10
			s = s * 0.95;				// Shrink 5% to leave a kind of margin 

			double ty = this.headerHeight + this.headerGap;		// Translate down to leave room for header
			double tx = -(double)bounds.Left * s;				// translate by left block position

			cvs.DrawPrint (g, tx, ty, s, s);

			// ---------------

			g.Rectangle (0, 0, width, this.headerHeight);
			g.SetSourceRGB(0.8, 0.8, 0.8);
			g.FillPreserve();
			g.SetSourceRGB(0, 0, 0);
			g.LineWidth = 1;
			g.Stroke();

			var layout = context.CreatePangoLayout();
			var desc = Pango.FontDescription.FromString("Monospace 10");
			layout.FontDescription = desc;
			layout.SetText(this.title);
			layout.Width = (int)(width);
			layout.Alignment = Pango.Alignment.Center;

			int layoutWidth = 0;
			int layoutHeight = 0;
			this.numPages = 1;

			layout.GetSize(out layoutWidth, out layoutHeight);
			double textHeight = layoutHeight / this.pangoScale;
			g.MoveTo(width/2, (this.headerHeight - textHeight) / 2);
			Pango.CairoHelper.ShowLayout(g, layout);
	      
			string pageStr = System.String.Format("{0}/{1}", args.PageNr + 1, this.numPages);
			layout.SetText(pageStr);
			layout.Alignment = Pango.Alignment.Right;
			g.MoveTo(width - 2, (this.headerHeight - textHeight) / 2);
			Pango.CairoHelper.ShowLayout(g, layout);
			
			string byline = System.Environment.UserName;
			layout.SetText(byline);
			layout.Alignment = Pango.Alignment.Left;
			g.MoveTo(2, (this.headerHeight - textHeight) / 2);
			Pango.CairoHelper.ShowLayout(g, layout);
			
			layout = context.CreatePangoLayout();
			desc = Pango.FontDescription.FromString("Monospace 10");
			//desc.Size = (int)(this.fontSize * this.pangoScale);
			layout.FontDescription = desc;


			/*
			  int line = args.PageNr * this.linesPerPage;
			  int i = 0;
			  while (i < this.linesPerPage && line < numLines) {
			    layout.SetText(this.lines[line]);
			    Pango.CairoHelper.ShowLayout(cr, layout);
			    if (calico.OS == "Windows")
			    	    cr.RelMoveTo(0, desc.Size/pangoScale * 10);
			    else
			    	    cr.RelMoveTo(0, desc.Size/pangoScale);
			    line++;
			    i++;
			  }
			  */

			// Must dispose the Cairo context and the Pango layout
			((IDisposable) g).Dispose(); 
			layout.Dispose();
	    }

	    public void OnEndPrint(object obj, Gtk.EndPrintArgs args) {
	      //pass
	    }
	}
}

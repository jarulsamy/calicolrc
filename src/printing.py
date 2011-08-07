#
# Calico - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

import Gtk
import System.Math as Math
import Pango
import System

class Printing:
    def __init__(self, title, text):
        self.title = title
        self.contents = text
        self.headerHeight = (10*72/25.4)
        self.headerGap = (3*72/25.4)
        self.pangoScale = 1024
        self.fontSize = 12.0
        self.printop = Gtk.PrintOperation()
        self.printop.BeginPrint += self.OnBeginPrint
        self.printop.DrawPage += self.OnDrawPage
        self.printop.EndPrint += self.OnEndPrint

        def invoke(sender, args):
            self.printop.Run(Gtk.PrintOperationAction.PrintDialog, None)

        Gtk.Application.Invoke(invoke)

    def OnBeginPrint(self, obj, args):
        context = args.Context
        height = context.Height
        self.linesPerPage = int(Math.Floor(height / self.fontSize))
        self.lines = self.contents.Split('\n')
        self.numLines = len(self.lines)
        self.numPages = (self.numLines - 1) / self.linesPerPage + 1
        self.printop.NPages = self.numPages

    def OnDrawPage(self, obj, args):
        context = args.Context
        cr = context.CairoContext
        width = context.Width
        cr.Rectangle (0, 0, width, self.headerHeight)
        cr.SetSourceRGB(0.8, 0.8, 0.8)
        cr.FillPreserve()
        cr.SetSourceRGB(0, 0, 0)
        cr.LineWidth = 1
        cr.Stroke()

        layout = context.CreatePangoLayout()
        desc = Pango.FontDescription.FromString("sans 14")
        layout.FontDescription = desc
        layout.SetText(self.title)
        layout.Width = int(width)
        layout.Alignment = Pango.Alignment.Center
        layoutWidth, layoutHeight = layout.GetSize()
        textHeight = layoutHeight / self.pangoScale
        cr.MoveTo(width/2, (self.headerHeight - textHeight) / 2)
        Pango.CairoHelper.ShowLayout(cr, layout)

        pageStr = System.String.Format("{0}/{1}", args.PageNr + 1, self.numPages)
        layout.SetText(pageStr)
        layout.Alignment = Pango.Alignment.Right
        cr.MoveTo(width - 2, (self.headerHeight - textHeight) / 2)
        Pango.CairoHelper.ShowLayout(cr, layout)

        byline = System.Environment.UserName
        layout.SetText(byline)
        layout.Alignment = Pango.Alignment.Left
        cr.MoveTo(2, (self.headerHeight - textHeight) / 2)
        Pango.CairoHelper.ShowLayout(cr, layout)

        layout = context.CreatePangoLayout()
        desc = Pango.FontDescription.FromString("mono")
        desc.Size = int(self.fontSize * self.pangoScale)
        layout.FontDescription = desc
        cr.MoveTo(0, self.headerHeight + self.headerGap)
        line = args.PageNr * self.linesPerPage
        i = 0
        while i < self.linesPerPage and line < self.numLines:
            layout.SetText(self.lines[line])
            Pango.CairoHelper.ShowLayout(cr, layout)
            cr.RelMoveTo(0, self.fontSize)
            line += 1
            i += 1
        cr.Dispose()
        layout = None

    def OnEndPrint(self, obj, args):
        pass

#if __name__ == "__main__":
#    p = Printing("Test.py", "this is a test")

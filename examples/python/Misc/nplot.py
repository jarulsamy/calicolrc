import System
import Gtk
import NPlot.Gtk
import Myro

def PlotTest(plotSurface):
     plotSurface.Clear()
     plotSurface.Title = "My Title"

     a = System.Array[System.Double]([0, 2, 1, 4, 2, 3, 5, 8, 7, 9])
     lp = NPlot.LinePlot()
     lp.DataSource = a
     lp.Label = "My Label"

     plotSurface.Add( lp )

     plotSurface.Legend = NPlot.Legend()
     plotSurface.Legend.NeverShiftAxes = True
     plotSurface.Legend.HorizontalEdgePlacement = NPlot.Legend.Placement.Inside
     plotSurface.Legend.VerticalEdgePlacement = NPlot.Legend.Placement.Inside
     plotSurface.Legend.XOffset = -10
     plotSurface.Legend.YOffset = 10
     ##plotSurface.AddAxesConstraint(NPlot.AxesConstraint.EqualSpacing() )

     plotSurface.XAxis1.Offset = 10.0
     plotSurface.XAxis1.Scale = 27.0
     plotSurface.XAxis1.TicksIndependentOfPhysicalExtent = True
     plotSurface.YAxis1.TicksIndependentOfPhysicalExtent = True

     plotSurface.Refresh()

def main(o=None, e=None):
    global w
    plot = NPlot.Gtk.PlotSurface2D ()
    PlotTest (plot)
    w = Myro.Window("Test", plot)

Myro.Invoke(main)

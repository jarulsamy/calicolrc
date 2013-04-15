import Florence
import System

plot = Florence.GtkSharp.ImperativeHost()
#plot.Start()
x = System.Array[System.Double]([1.0, 2.0, 3.0])
y = System.Array[System.Double]([1.0, 2.0, 3.0])
z = System.Array[System.Double]([0.0, 2.0, 4.0])
plot.points(x, y)
plot.lines(x, z, title="Test Plot 2", x_label="X2", y_label="Y2")

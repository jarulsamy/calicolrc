from Graphics import Plot

plot = Plot("Sample Plot", 600, 300)
plot.xLabel.text = "time"
plot.yLabel.text = "balls in the air"

for data in [10, 30, 40, 50, 0, 100, 110, 40, 50]:
    plot.append(data)
    
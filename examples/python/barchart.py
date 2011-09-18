from Graphics import *
import Myro

def draw(obj, event):
    bc.draw()

class BarChart:
    def __init__(self, title, width, height, data, x_label=None, y_label="Count"):
        self.colors = ["yellow", "red", "blue", "green", "orange", "purple"] + Myro.getColorNames()
        self.title = title
        self.x_label = x_label
        self.y_label = y_label
        self.width = width
        self.height = height
        self.data = data
        self.left, self.right, self.top, self.bottom = 100, 50, 100, 100
        self.rotate = 45
        self.use_height = .50
        self.use_width = .75
        self.win = Window(self.title, self.width, self.height)
        self.background = Rectangle((self.left, self.top),
                               (self.width - self.right, self.height - self.bottom))
        self.background.fill = Color("white")
        self.background.draw(self.win)
        self.button = Button("Rescale")
        self.button.draw(self.win, (self.width - 70, self.height - 50))
        self.button.connect("click", draw)

        title = Text((self.width/2,self.top/2), self.title)
        title.color = Color("black")
        title.draw(self.win)

        if self.y_label:
            y_label = Text((self.left/2, self.height/2), self.y_label)
            y_label.fontSize = 12
            y_label.rotate(90)
            y_label.color = Color("black")
            y_label.draw(self.win)

        if self.x_label:
            x_label = Text((self.left + (self.width - self.left - self.right)/2,
                           (self.height - self.bottom) + self.bottom/2), self.x_label)
            x_label.fontSize = 12
            x_label.color = Color("black")
            x_label.draw(self.win)

        self.bars = {}
        self.draw()

    def animate(self, new_data):
        # First, get old info:
        columns = len(self.bins)
        # Columns are 95% of span
        col_width = (self.width - self.right - self.left) /columns
        # Offset from left:
        col_off = (self.width - self.right - self.left) * (1.0 - self.use_width) / (columns)
        # now, new diffs:
        new_bins = {}
        for datum in new_data:
            new_bins[datum] = new_bins.get(datum, 0) + 1
        diffs = {}
        for bin in self.bins.keys():
            diffs[bin] = new_bins.get(bin, 0) - self.bins.get(bin, 0)
        # Now, redraw:
        fps = 10
        for scale in range(0, fps + 1, 1):
            count = 0
            for bin in sorted(self.bins.keys()):
                # Bottom left:
                x1 = self.left + (count * col_width) + col_off/2
                y1 = self.height - self.bottom
                # Top right:
                x2 = self.left + (count * col_width) + col_off/2 + col_width - col_off
                # Height is 90% of available
                y2 = self.height - self.bottom - ((self.bins.get(bin, 0) + scale/fps * diffs.get(bin, 0))/self.max_count * (self.height - self.top - self.bottom) * self.use_height)
                self.bars[bin].set_points(Point(x1, y1), Point(x1, y2), Point(x2, y2), Point(x2, y1))
                count += 1
            self.win.step(1/fps)
        self.bins = new_bins
        self.data = new_data

    def draw(self):
        for bar in self.bars:
            self.win.undraw(self.bars[bar])
        bins = {}
        for datum in self.data:
            bins[datum] = bins.get(datum, 0) + 1
        self.max_count = max(bins.values())
        columns = len(bins)
        # Columns are 95% of span
        col_width = (self.width - self.right - self.left) /columns
        # Offset from left:
        col_off = (self.width - self.right - self.left) * (1.0 - self.use_width) / (columns)

        # Draw bars:
        count = 0
        self.bins = bins
        self.bars = {}
        for bin in sorted(bins.keys()):
            # Bottom left:
            x1 = self.left + (count * col_width) + col_off/2
            y1 = self.height - self.bottom
            # Top right:
            x2 = self.left + (count * col_width) + col_off/2 + col_width - col_off
            # Height is 90% of available
            y2 = self.height - self.bottom - (bins[bin]/self.max_count * (self.height - self.top - self.bottom) * self.use_height)
            bar = Rectangle((x1, y1), (x2, y2))
            self.bars[bin] = bar
            bar.fill = Color(self.colors[count])
            bar.draw(self.win)
            text = Text((x1 + 10, y1 - 10), str(bins[bin]))
            text.fontSize = 8
            text.draw(self.win)
            count += 1

        # Draw legend:
        count = 0
        for bin in sorted(bins.keys()):
            # Bottom left:
            x1 = self.left + (count * col_width) + col_off/2 + col_width * .45
            y1 = self.height - self.bottom * .75
            text = Text((x1, y1), str(bin))
            text.fontSize = 12
            text.rotate(self.rotate)
            text.color = Color("black")
            text.draw(self.win)
            count += 1

        # Draw ticks:
        for percent in range(0, int(110 + (1.0 - self.use_height) * 200), 10):
            x = self.left
            y = self.height - self.bottom - (percent/100 * (self.height - self.top - self.bottom) * self.use_height)
            tick = Line((x - 10, y), (x, y))
            tick.draw(self.win)
            text = Text((x - 25, y), str(percent/100 * self.max_count))
            text.fontSize = 8
            text.color = Color("black")
            text.draw(self.win)


if __name__ == "<module>":
    #BarChart("test 2", 800, 600, ["1", "2"])
    #BarChart("test 3", 640, 480, range(10))
    bc = BarChart("Barchart Test #1", 800, 600, [Myro.pickOne(*range(4)) for x in range(100)], x_label="Choice")
    def animate():
        while True:
            bc.animate([Myro.pickOne(*range(4)) for x in range(100)])
    animate()
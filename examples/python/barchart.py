from Graphics import *
import Myro

def barchart(title, width, height, data):
    left, right, top, bottom = 100, 100, 100, 100
    win = Window(title, width, height)
    background = Rectangle((left, top), (width - right, height - bottom))
    background.fill = Color("white")
    background.draw(win)

    bins = {}
    for datum in data:
        bins[datum] = bins.get(datum, 0) + 1

    max_count = max(bins.values())

    use_height = .90
    use_width = .98

    columns = len(bins)
    # Columns are 95% of span
    col_width = (width - right - left) * use_width /columns
    # Offset from left:
    col_off = (width - right - left) * (1.0 - use_width) / (columns + 1)

    count = 0
    for bin in sorted(bins.keys()):
        # Bottom left:
        x1 = left + count * col_width + col_off * (columns + 1)
        y1 = height - bottom
        # Top right:
        x2 = left + count * col_width + col_off + col_width - col_off
        # Height is 90% of available
        y2 = height - bottom - (bins[bin]/max_count * (height - top - bottom) * use_height)
        bar = Rectangle((x1, y1), (x2, y2))
        bar.fill = Color(Myro.pickOne(Myro.getColorNames()))
        bar.draw(win)
        count += 1

    count = 0
    for bin in sorted(bins.keys()):
        # Bottom left:
        x1 = left + count * col_width + col_off * (columns + 1) + col_width * .45
        y1 = height - bottom * .75
        text = Text((x1, y1), str(bin))
        text.fontSize = 12
        text.rotate(45)
        text.color = Color("black")
        text.draw(win)
        count += 1


barchart("test 1", 800, 600, ["1", "2", "3", "2", "3", "4", "1", "1", "2", "3", "4",
                    "2", "2", "2", "1", "2", "2", "1"])

barchart("test 2", 800, 600, ["1", "2"])

barchart("test 3", 640, 480, range(10))
barchart("test 4", 640, 480, [y for y in [x for x in range(20)]])
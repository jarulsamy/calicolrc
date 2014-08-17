
__all__ = ['Canvas', 'Shape', 'Line', 'Circle', 'Text',
           'cm', 'em', 'ex', 'mm', 'pc', 'pt', 'px']

import svgwrite
from svgwrite import cm, em, ex, mm, pc, pt, px

class Canvas(object):
    def __init__(self, *args, **kwargs):
        if "debug" not in kwargs:
            kwargs["debug"] = False
        self.args = args
        self.kwargs = kwargs
        self.shapes = []

    def _repr_svg_(self):
        canvas = svgwrite.Drawing(*self.args, **self.kwargs)
        for shape in self.shapes:
            shape._add(canvas)
        return canvas.tostring()

    def draw(self, shape):
        shape.canvas = self
        self.shapes.append(shape)

    def undraw(self, shape):
        shape.canvas = None
        del self.shapes[shape]

    def __str__(self):
        return self._repr_svg_()

class Shape(object):
    def __init__(self):
        self.canvas = None

    def draw(self, canvas):
        canvas.draw(self)

    def undraw(self, canvas):
        canvas.undraw(self)

class Circle(Shape):
    def __init__(self, center=(0,0), radius=1, **extra):
        super(Circle, self).__init__()
        if "fill" not in extra:
            extra["fill"] = "purple"
        if "stroke" not in extra:
            extra["stroke"] = "black"
        if "stroke_width" not in extra:
            extra["stroke_width"] = 1
        self.center = center
        self.radius = radius
        self.extra = extra

    def moveTo(self, center):
        self.center = center

    def move(self, (delta_x, delta_y)):
        self.center = self.center[0] + delta_x, self.center[1] + delta_y

    def _add(self, drawing):
        drawing.add(drawing.circle(center=self.center, r=self.radius, **self.extra))

class Line(Shape):
    def __init__(self, start=(0,0), end=(0,0), **extra):
        super(Line, self).__init__()
        self.start = start
        self.end = end
        self.extra = extra

    def moveTo(self, start):
        diff_x = start[0] - self.start[0]
        diff_y = start[1] - self.start[1]
        self.start = start
        self.end = self.end[0] + diff_x, self.end[1] + diff_y

    def move(self, (delta_x, delta_y)):
        self.start = self.start[0] + delta_x, self.start[1] + delta_y
        self.end = self.end[0] + delta_x, self.end[1] + delta_y

    def _add(self, drawing):
        drawing.add(drawing.line(start=self.start, end=self.end, **self.extra))

class Text(Shape):
    def __init__(self, text="", start=(0,0), **extra):
        super(Text, self).__init__()
        self.text = text
        self.start = start
        self.extra = extra

    def moveTo(self, start):
        self.start = start

    def move(self, (delta_x, delta_y)):
        self.start = self.start[0] + delta_x, self.start[1] + delta_y

    def _add(self, drawing):
        drawing.add(drawing.text(text=self.text, insert=self.start, **self.extra))

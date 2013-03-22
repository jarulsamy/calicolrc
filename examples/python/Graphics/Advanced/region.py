from Graphics import *
from Myro import show

pic = Picture(calico.relativePath("../examples/images/blankenship.jpg"))
show(pic.getRegion(Point(100, 100), 100, 100, 45))

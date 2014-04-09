from Processing import *
from random import choice

#http://www.pygtk.org/docs/pygtk/pango-markup-language.html

window(800, 400)
smooth()
textSize(48)
txt = """<span foreground="blue">Calico Processing</span> <b>is</b> <i>sweet</i>!"""
print(txt)
markup(txt, 50, 150)

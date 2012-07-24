from Tao.OpenGl import Gl, Glu
from SdlDotNet.Graphics import Video

width, height = 640, 480
screen = Video.SetVideoMode(width, height, True, True)
distance = 100.0

Gl.glViewport(0, 0, width, height)
## Select The Projection Matrix
Gl.glMatrixMode(Gl.GL_PROJECTION)
## Reset The Projection Matrix
Gl.glLoadIdentity()
## Calculate The Aspect Ratio Of The Window
Glu.gluPerspective(45.0, (width / height), 0.1, distance)
## Select The Modelview Matrix
Gl.glMatrixMode(Gl.GL_MODELVIEW)
## Reset The Modelview Matrix
Gl.glLoadIdentity()

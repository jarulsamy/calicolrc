from examples.python.nehe.lesson01 import NeHe001, Events, Gl

class NeHe002(NeHe001):

    ### Renders the scene
    def DrawGLScene(self):
        ## Clear Screen And Depth Buffer
        Gl.glClear(Gl.GL_COLOR_BUFFER_BIT | Gl.GL_DEPTH_BUFFER_BIT)
        ## Reset The Current Modelview Matrix
        Gl.glLoadIdentity()
        ## Move Left 1.5 Units And Into The Screen 6.0
        Gl.glTranslatef(-1.5, 0, -6)
        ## Drawing Using Triangles
        Gl.glBegin(Gl.GL_TRIANGLES)
        ## Top
        Gl.glVertex3f(0, 1, 0)
        ## Bottom Left
        Gl.glVertex3f(-1, -1, 0)
        ## Bottom Right
        Gl.glVertex3f(1, -1, 0)
        ## Finished Drawing The Triangle
        Gl.glEnd()
        ## Move Right 3 Units
        Gl.glTranslatef(3, 0, 0)
        ## Draw A Quad
        Gl.glBegin(Gl.GL_QUADS)
        ## Top Left
        Gl.glVertex3f(-1, 1, 0)
        ## Top Right
        Gl.glVertex3f(1, 1, 0)
        ## Bottom Right
        Gl.glVertex3f(1, -1, 0)
        ## Bottom Left
        Gl.glVertex3f(-1, -1, 0)
        ## Done Drawing The Quad
        Gl.glEnd()

def main():
    t = NeHe002()
    t.Reshape()
    t.InitGL()
    Events.Run()

from System import EventHandler
#import System.Reflection
#import System.Diagnostics.CodeAnalysis

from SdlDotNet.Core import Events, TickEventArgs, QuitEventArgs
from SdlDotNet.Input import KeyboardEventArgs, Key
from SdlDotNet.Graphics import Video
from Tao.OpenGl import Gl, Glu

### Lesson 01: Setting Up An OpenGL Window
class NeHe001(object):
    def __init__(self):
        ##Width of screen
        self.width = 640
        ##Height of screen
        self.height = 480
        ## Bits per pixel of screen
        self.bpp = 16
        ## Surface to render on
        self.screen = None
        self.lesson_number = "01"
        self.title = "Lesson %s: Setting Up An OpenGL Window" % self.lesson_number
        self.Initialize()

    ### Initializes methods common to all NeHe lessons
    def Initialize(self):
        ## Sets keyboard events
        Events.KeyboardDown += EventHandler[KeyboardEventArgs](self.KeyDown)
        ## Sets the ticker to update OpenGL Context
        Events.Tick += EventHandler[TickEventArgs](self.Tick)
        Events.Quit += EventHandler[QuitEventArgs](self.Quit)
        ##      ## Sets the resize window event
        ##      Events.VideoResize += new EventHandler<VideoResizeEventArgs> (this.Resize)
        ## Set the Frames per second.
        Events.Fps = 60
        ## Sets Window icon and title
        self.WindowAttributes()
        ## Creates SDL.NET Surface to hold an OpenGL scene
        self.screen = Video.SetVideoMode(self.width, self.height, True, True)

    ### Sets Window icon and caption
    def WindowAttributes(self):
        Video.WindowIcon()
        Video.WindowCaption = "SDL.NET - NeHe Lesson " + self.lesson_number

    ### Resizes window
    def Reshape(self, distance=100.0):
        ## Reset The Current Viewport
        Gl.glViewport(0, 0, self.width, self.height)
        ## Select The Projection Matrix
        Gl.glMatrixMode(Gl.GL_PROJECTION)
        ## Reset The Projection Matrix
        Gl.glLoadIdentity()
        ## Calculate The Aspect Ratio Of The Window
        Glu.gluPerspective(45.0, (self.width / float(self.height)), 
                           0.1, distance)
        ## Select The Modelview Matrix
        Gl.glMatrixMode(Gl.GL_MODELVIEW)
        ## Reset The Modelview Matrix
        Gl.glLoadIdentity()
        print("reshape")

    ### Initializes the OpenGL system
    def InitGL(self):
        ## Enable Smooth Shading
        Gl.glShadeModel(Gl.GL_SMOOTH)
        ## Black Background
        Gl.glClearColor(0.0, 0.0, 0.0, 0.5)
        ## Depth Buffer Setup
        Gl.glClearDepth(1.0)
        ## Enables Depth Testing
        Gl.glEnable(Gl.GL_DEPTH_TEST)
        ## The Type Of Depth Testing To Do
        Gl.glDepthFunc(Gl.GL_LEQUAL)
        ## Really Nice Perspective Calculations
        Gl.glHint(Gl.GL_PERSPECTIVE_CORRECTION_HINT, Gl.GL_NICEST)
        print("initgl")

    ### Renders the scene
    def DrawGLScene(self):
        ## Clear Screen And Depth Buffer
        Gl.glClear((Gl.GL_COLOR_BUFFER_BIT | Gl.GL_DEPTH_BUFFER_BIT))
        ## Reset The Current Modelview Matrix
        Gl.glLoadIdentity()

    def KeyDown(self, sender, e):
        if e.Key == Key.Escape:
            ## Will stop the app loop
            Events.QuitApplication()
        elif e.Key == Key.F1:
            ## Toggle fullscreen
            if (self.screen.FullScreen):
                self.screen = Video.SetVideoMode(self.width, self.height, 
                                                   True, True, True)
                self.WindowAttributes()
                print("reg screen")
            else:
                self.screen = Video.SetVideoMode(self.width, self.height, 
                                                   True, True)
                print("full screen")
            self.Reshape()

    def Tick(self, sender, e):
        self.DrawGLScene()
        Video.GLSwapBuffers()

    def Quit(self, sender, e):
        Events.QuitApplication()

        ##    private void Resize (object sender, VideoResizeEventArgs e)
        ##    {
        ##      screen = Video.SetVideoMode(e.Width, e.Height, true)
        ##      if (screen.Width != e.Width || screen.Height != e.Height)
        ##      {
        ##        ##this.InitGL()
        ##        this.Reshape()
        ##      }
        ##    }

def main():
    t = NeHe001()
    t.Reshape()
    t.InitGL()
    Events.Run()

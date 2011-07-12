from Tao.Sdl import Sdl
import System

class Joystick:
    def __init__(self, index):
        Sdl.SDL_Init(Sdl.SDL_INIT_JOYSTICK)
        intptr = System.IntPtr(index)
        self.handle = Sdl.SDL_JoystickOpen(intptr)
        self.index = index
        print(Sdl.SDL_NumJoysticks())

    def getName(self):
        return Sdl.SDL_JoystickName(self.index)

    def getButton(self, button):
        Sdl.SDL_JoystickUpdate()
        return Sdl.SDL_JoystickGetButton(self.handle, button)

    def getAxes(self):
        return Sdl.SDL_JoystickNumAxes(self.handle)

    def getBalls(self):
        return Sdl.SDL_JoystickNumBalls(self.handle)

    def getButtons(self):
        return Sdl.SDL_JoystickNumButtons(self.handle)

    def getGamePadNow(self):
        return {"button": []}

js = Joystick(0)
print("Name:", js.getName())
print("Buttons:", js.getButtons())
print("Balls:", js.getBalls())
print("Axes:", js.getAxes())

while True:
    if js.getButton(0):
        print("hit")

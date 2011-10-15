import Myro
import Graphics
import math

def makeTone(freq):
    slice = (1/100000 * 360 * math.pi/180) # time in radians
    def wave(array, position):
        for i in range(len(array)):
            angle = position * slice * freq
            array[i] = (math.cos(angle)) * 127
            position += 1
    return wave

def plotSound(function, width=500):
    win = Myro.Window("Sound Plot", width, 255)
    array = [0] * width
    function(array, 0)
    for i in range(width):
        Graphics.Dot((i, array[i] + 127)).draw(win)

f440 = makeTone(880)
plotSound(f440)
Myro.play(1, f440)


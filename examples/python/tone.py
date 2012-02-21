import Myro
import Graphics
import math

def makeTone(freq):
    slice = (1/44100 * 360 * math.pi/180) # time in radians
    def wave(array, position):
        for i in range(len(array)):
            angle = position * slice * freq
            array[i] = 127 + math.cos(angle) * 127
            position += 1
    return wave

def plotSound(function, width=500):
    win = Myro.Window("Sound Plot", width, 255)
    array = [0] * width
    function(array, 0)
    for i in range(width):
        Graphics.Dot((i, array[i])).draw(win)

tone = makeTone(440)
plotSound(tone)
Myro.play(1, tone)


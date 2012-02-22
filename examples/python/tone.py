import Myro
import Graphics
import math

def makeTone(freq1, freq2, freq3):
    """
    Make a function that is based on the given frequencies.
    """
    # Each time slice is 1/playbackfreq of 2pi
    slice = (1/44100 * 360 * math.pi/180) # time in radians
    def wave(array, position):
        """
        The actual function that will compute the wave.
        """
        # Fill the array with bytes (unsigned 8-bit values)
        for i in range(len(array)):
            angle1 = position * slice * freq1
            angle2 = position * slice * freq2
            angle3 = position * slice * freq3
            array[i] = ((127 + math.cos(angle1) * 127) +
                        (127 + math.sin(angle2) * math.sin(angle1) * 127) +
                        (127 + math.cos(angle3) * 127))/3
            position += 1
    return wave

def plotSound(function, width=500):
    win = Myro.Window("Sound Plot", width, 255)
    array = [0] * width
    function(array, 0)
    prev = (0, 255)
    for i in range(width):
        Graphics.Line(prev, (i, array[i])).draw(win)
        prev = (i, array[i])

tone = makeTone(440, 440, 220)
plotSound(tone)

Myro.play(2, tone)

import Myro
import Graphics
import math

gfreq = 440
gslice = (1/44100 * 360 * math.pi/180)

def wave(array, position):
    """
    The actual function that will compute the wave.
    """
    # Fill the array with bytes (unsigned 8-bit values)
    for i in range(len(array)):
        angle1 = position * gslice * gfreq
        array[i] = (127 + math.cos(angle1) * 127)
        position += 1

def makeTone(freq1):
    """
    Make a function that is based on the given frequency.
    """
    # Each time slice is 1/playbackfreq of 2pi
    slice = (1/44100 * 360 * math.pi/180) # time in radians
    def wave(array, position):
        """
        The actual function that will compute the wave.
        """
        # Fill the array with bytes (unsigned 8-bit values)
        for i in range(len(array)):
            angle = position * slice * freq1
            array[i] = (127 + math.cos(angle) * 127)
            position += 1
    return wave

def make2Tone(freq1, freq2):
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
            array[i] = ((127 + math.cos(angle1) * 127) +
                        (127 + math.cos(angle2) * 127))/2
            position += 1
    return wave

def make3Tone(freq1, freq2, freq3):
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
                        (127 + math.cos(angle2) * 127) +
                        (127 + math.cos(angle3) * 127))/3
            position += 1
    return wave

def plotSound(function, width=500):
    win = Myro.Window("Sound Plot", width, 275) # height + 20 for scrollbars
    width = 44100
    win.addScrollbars(width/2, 255) # height actual height of canvas
    win.mode = "bitmap"
    array = [0] * width
    function(array, 0)
    prev = (0, width)
    for i in range(0, width, 2):
        Graphics.Line(prev, (i/2, array[i])).draw(win)
        prev = (i/2, array[i])

def plotMoreSound(function, color, width=500):
    win = Myro.getWindow()
    width = 44100
    array = [0] * width
    function(array, 0)
    prev = (0, width)
    for i in range(0, width, 2):
        l = Graphics.Line(prev, (i/2, array[i]))
        l.color = Myro.Color(color)
        l.draw(win)
        prev = (i/2, array[i])

#Myro.play(0, wave)

tone440 = makeTone(440)
tone441 = makeTone(441)
tone = make2Tone(440, 441)

plotSound(tone)
plotMoreSound(tone440, "red")
plotMoreSound(tone441, "blue")

Myro.play(2, tone)


from Myro import *

ard = makeRobot("Arduino", "/dev/ttyUSB0")

def blink():
    ard.pinMode(13, ard.OUTPUT)        #led
    for t in timer(10):
        ard.digitalWrite(13, 1)
        wait(.5)
        ard.digitalWrite(13, 0)
        wait(.5)

blink()
ard.Close()

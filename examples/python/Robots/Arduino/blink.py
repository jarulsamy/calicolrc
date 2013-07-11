from Myro import *

makeRobot("Arduino", "/dev/ttyUSB0")

def blink():
    makeDigitalOutput(13)            #led
    for t in timer(10):
        digitalWrite(13, 1)
        wait(.5)
        digitalWrite(13, 0)
        wait(.5)

blink()

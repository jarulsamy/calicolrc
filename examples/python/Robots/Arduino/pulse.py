from Myro import *
makeRobot("Arduino", "/dev/ttyUSB0")

def pulse():
    makePWMOutput(3)               #led + resistor
    for i in range(256):
        analogWrite(3, i)
        wait(.01)
    for i in range(255, -1, -1):
        analogWrite(3, i)
        wait(.01)

pulse()

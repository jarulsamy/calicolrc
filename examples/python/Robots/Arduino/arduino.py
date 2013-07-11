from Myro import *

makeRobot("Arduino", "/dev/ttyUSB0")

def pulse():
    for i in range(256):
        analogWrite(3, i)
        wait(.01)
    for i in range(255, 0, -1):
        analogWrite(3, i)
        wait(.01)

def blink():
    for t in timer(5):
        digitalWrite(13, 1)
        wait(.5)
        digitalWrite(13, 0)
        wait(.5)


makeDigitalOut(13)        #led
makePWMOut(3)             #led + resistor
blink()
pulse()

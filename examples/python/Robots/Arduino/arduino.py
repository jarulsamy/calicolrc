from Myro import *

ard = makeRobot("Arduino", "/dev/ttyACM0")

from Myro import timer, wait

def pulse():
    for i in range(256):
        ard.analogWrite(3, i)
        wait(.01)
    for i in range(255, 0, -1):
        ard.analogWrite(3, i)
        wait(.01)

def blink():
    for t in timer(5):
        ard.digitalWrite(13, 1)
        wait(.5)
        ard.digitalWrite(13, 0)
        wait(.5)


ard.pinMode(13, ard.OUTPUT)        #led
ard.pinMode(3, ard.PWM)            #led + resistor
blink()
pulse()
ard.Close()

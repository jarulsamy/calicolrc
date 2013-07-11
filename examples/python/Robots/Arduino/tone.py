from Myro import *
import Processing
import math

makeRobot("Arduino", "/dev/ttyUSB0")

def square(t, freq):
    makeDigitalOutput(9)          #buzzer on pin 9
    period = 1.0/freq/2
    for i in timer(t):
        digitalWrite(9, 1)
        wait(period)
        digitalWrite(9, 0)
        wait(period)

square(2, 440)

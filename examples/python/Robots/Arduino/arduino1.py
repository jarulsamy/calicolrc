from Processing import *
from Myro import *
import Processing

ard = makeRobot("Arduino", "/dev/ttyACM0")
while True:
    v = ard.analogRead(0)
    print(v)
  
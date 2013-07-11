from Processing import *
from Myro import *
import Processing

makeRobot("Arduino", "/dev/ttyUSB0")
while True:
    v = analogRead(0)
    print(v)
  
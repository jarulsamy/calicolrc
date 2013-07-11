from Processing import *
from Myro import *
import Processing

makeRobot("Arduino", "/dev/ttyUSB0")
makeServoOutput(6)                      #servo on 6
makeDigitalOutput.pinMode(3)            #led on 3
window(800, 200)
smooth()
background(0)
noStroke()

x = 0
def draw(o, e):
    global x
    v = analogRead(0)       #photoresistor on analog 0
    y3 = Processing.map(v, 320, 0, 0, height())
    servo = Processing.map(v, 0, 320, 255, 0)
    analogWrite(3, servo)
    analogWrite(6, servo)
    fill(255, 0, 196)
    ellipse(x, y3, 3, 3)

    x = x + 1
    if x > width():
        x = 0
        background(0)

frameRate(120)
onLoop += draw
loop()

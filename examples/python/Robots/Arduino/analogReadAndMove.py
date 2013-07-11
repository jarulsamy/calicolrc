from Processing import *
from Myro import *
import Processing

ard = makeRobot("Arduino", "/dev/ttyUSB0")
ard.pinMode(6, ard.SERVO)        #servo on 6
ard.pinMode(3, ard.PWM)          #led on 3
window(800, 200)
smooth()
background(0)
noStroke()

x = 0
def draw(o, e):
    global x
    v = ard.analogRead(0)       #photoresistor on analog 0
    y3 = Processing.map(v, 320, 0, 0, height())
    servo = Processing.map(v, 0, 320, 255, 0)
    ard.analogWrite(3, servo)
    ard.analogWrite(6, servo)
    fill(255, 0, 196)
    ellipse(x, y3, 3, 3)

    x = x + 1
    if x > width():
        x = 0
        background(0)

def stop(o, e):
    noLoop()
    ard.Close()
    print("Closed")

#onKeyPressed += stop

frameRate(120)
onLoop += draw
loop()

from Processing import *
from Myro import *
import Processing

makeRobot("Arduino", "/dev/ttyUSB0")
makeServoOutput(6)                      #servo on 6
makePWMOutput(3)                   #led on 3
window(800, 200)
smooth()
background(0)
noStroke()

x = 0
def draw():
    global x
    v = analogRead(0)       #photoresistor on analog 0
    y3 = Processing.map(v, 1024, 0, 0, height())
    led = Processing.map(v, 400, 800, 255, 0)
    led = Processing.constrain(led, 0, 255)
    servo = Processing.map(v, 0, 1024, 0, 255)
    servo = Processing.constrain(servo, 0, 255)
    analogWrite(3, led)
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

"""
Car alarm
If the Finch is tapped, then it sounds an alarm!
To exit out of the program, shake the Finch.

"""
from Myro import *

makeRobot("Finch")

# Set the color of the led light to green:
setLED("front","#00FF00")
    
# counter, used to prevent an immediate return:
x = 0

# if the Finch was shaken, end the loop
while not getAcceleration("shake") or x < 10:
    x = x + 1
    # if the Finch is tapped, sound the alarm!!
    if getAcceleration("tap"):
        setLED("front", "#FF0000") #hex for red
        beep(1.0, 500)

        # reset the led to green:
        setLED("front","#00FF00")



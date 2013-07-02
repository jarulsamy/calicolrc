
"""
Example program, lap swimmer
"""
from Myro import *

makeRobot("Finch")
setLED("front","#FFFF00")

## get the number of laps Finch will swim:
laps = int(input("Enter number of laps"))

#Some finches return true the first time obstacle is called
inital = getObstacle("left")
inital1 = getObstacle("right")

start = currentTime()
end = 0
motors(0.5,0.5)
## move forwards until an obstacle is present:
while laps > 0:
    if getObstacle("left") or getObstacle("right"):
        end = currentTime()
        motors(0,0)
        break

## move backwards for the same amount of time spent moving forwards:
if laps > 0:
    motors(-0.5,-0.5)
    laps = laps - 1
    wait(end - start)

## now lapswim!
while laps > 0:
    motors(0.5,0.5)
    wait(end - start)
    motors(0,0)
    wait(0.05)
    motors(-0.5,-0.5)
    wait(end - start)
    laps = laps - 1
motors(0,0)

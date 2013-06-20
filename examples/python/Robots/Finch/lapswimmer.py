"""
Example program, lap swimmer
"""
from Myro import *

finch = Myro.makeRobot("Finch")
finch.setLED("front","#FFFF00")

## get the number of laps Finch will swim:
laps = int(input("Enter number of laps"))

#Some finches return true the first time obstacle is called
inital = finch.getObstacle("left")
inital1 = finch.getObstacle("right")

start = currentTime()
end = 0
finch.motors(0.5,0.5)
## move forwards until an obstacle is present:
while laps > 0:
    if finch.getObstacle("left") or finch.getObstacle("right"):
        end = currentTime()
        finch.motors(0,0)
        break

## move backwards for the same amount of time spent moving forwards:
if laps > 0:
    finch.motors(-0.5,-0.5)
    laps = laps - 1
    wait(end - start)

## now lapswim!
while laps > 0:
    finch.motors(0.5,0.5)
    wait(end - start)
    finch.motors(0,0)
    wait(0.05)
    finch.motors(-0.5,-0.5)
    wait(end - start)
    laps = laps - 1

finch.stop()

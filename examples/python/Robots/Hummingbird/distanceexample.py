"""
Driver example program.
Connect a motor into motor port 1, a distance sensor into port 1, and a single LED into LED port 1.

Drives faster when the distance sensor is further away from objects,
and LED is brighter when driving faster.
Stops when an object <12cm away is detected.


"""

import Myro

humm = Myro.makeRobot("Hummingbird")

distance = humm.get("1","distance")

"""Drive until object <12cm away detected"""
while distance > 12:
    humm.motors(distance/80,0)
    humm.setLED("s1",(int)(distance/80*255))

    Myro.wait(0.1)

    distance = humm.get("1","distance")

humm.idle()
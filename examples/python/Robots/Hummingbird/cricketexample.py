"""
Cricket example program.
Have a temperature sensor plugged into sensor port 3,
and a vibration motor plugged into the first vibration port.


Vibrates as many times as a cricket would have chirpped given the temperature.
"""

from Myro import *


makeRobot("Hummingbird")
temp = get("3","temperature")

"""Convert temperature to number of times to chirp"""
timesToChirp = int((temp - 4)*3)


"""Chirp"""
while timesToChirp > 0:
    setVolume("v1200")
    wait(0.25)
    setVolume("v10")
    wait(0.25)
    timesToChirp -= 1

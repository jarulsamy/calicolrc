from Myro import *
from Rhyduino import *

arduino = Arduino("/dev/ttyACM0")
arduino.Connect()
pin = arduino.DigitalPins[13]
pin.SetPinMode(PinMode.Output)

count = 0
for t in timer(10):
    if count % 2 == 0:
       pin.SetPinValue(DigitalPinValue.Low)
    else:
        pin.SetPinValue(DigitalPinValue.High)
    count += 1
    wait(.25)

arduino.Disconnect()
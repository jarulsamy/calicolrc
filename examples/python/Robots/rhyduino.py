from Myro import *
from Rhyduino import *

arduino = Arduino("/dev/ttyACM0")
arduino.Connect()
pin = arduino.DigitalPins[13]
pin.SetPinMode(PinMode.Output)
wait(5)

count = 0
for t in range(10):
    if count % 2 == 1:
        pin.SetPinValue(DigitalPinValue.Low)
        print('off')
    else:
        pin.SetPinValue(DigitalPinValue.High)
        print('on')
    count += 1
    wait(1)

#arduino.Disconnect()
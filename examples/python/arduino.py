import Myro
import Rhyduino

with Rhyduino.Arduino("COM5") as arduino:
    arduino.Connect()
    pin = arduino.DigitalPins[13]
    pin.SetPinMode(Rhyduino.PinMode.Output)

    count = 0
    for t in Myro.timer(10):
        if count % 2 == 0:
            pin.SetPinValue(Rhyduino.DigitalPinValue.Low)
        else:
            pin.SetPinValue(Rhyduino.DigitalPinValue.High)
        count += 1
        Myro.wait(1)

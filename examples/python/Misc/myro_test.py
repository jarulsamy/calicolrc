from Myro import *
init()
pic = takePicture()

show(pic, init=True)
backward(1, 1)
forward(1, 1)
beep(1, 440)
for pixel in getPixels(pic):
    r, g, b = getRGB(pixel)
    gray = (r + g + b)/3
    setRGB(pixel, gray, gray, gray)
    

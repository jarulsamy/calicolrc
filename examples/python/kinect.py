import Kinect
client = Kinect.Client("127.0.0.1", 8001)
client.initRGB(640)
client.initDepth(320)
#client.initRGB(1280)
#client.initDepth(640)
client.startKinect(False)

from Graphics import *
win = Window(320, 240)
pic = Picture(320, 240)
pic.draw(win)

def getDepth():
    depthValues = client.readDepth()
    maximum = max([depthValues[x,0] for x in range(depthValues.GetUpperBound(0) + 1)])
    for v in range(depthValues.GetUpperBound(0) + 1):
        depth = depthValues[v,0]
        gray = depth/maximum * 255
        pic.setPixel(320 - v % 320, int(v/320), Color(gray, gray, gray))

def main():
    while True:
        getDepth()

win.run(main)
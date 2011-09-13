import Kinect
client = Kinect.Client("165.106.10.93", 8001)

depthWidth, depthHeight = 320, 240
rgbWidth, rgbHeight = 640, 480

client.initRGB(rgbWidth)
client.initDepth(depthWidth)
client.startKinect(False)

from Graphics import *
win = Window("Depth", depthWidth, depthHeight)
pic = Picture(depthWidth, depthHeight)
pic.draw(win)

win2 = Window("RGB", rgbWidth, rgbHeight)
pic2 = Picture(rgbWidth, rgbHeight)
pic2.draw(win2)

def getDepth():
    depthValues = client.readDepth()
    maximum = max([depthValues[x,0] for x in range(depthValues.GetUpperBound(0) + 1)])
    for v in range(depthValues.GetUpperBound(0) + 1):
        depth = depthValues[v,0]
        gray = depth/maximum * 255
        pic.setPixel(depthWidth - v % depthWidth, int(v/depthWidth), Color(gray, gray, gray))

def getRGB():
    data = client.readRGB()
    pic2.fromArray(data, "BGRX")

lines = []

def getJoints():
    global lines
    s = client.readSkeleton()
    skeletons = s[0]
    for line in lines:
        line.undraw()
    lines = []
    for index in range(1, skeletons + 1):
        joints = client.getJointPositions(s, index)
        segments = client.getJointSegments(s, index)

        z = joints[1,3]
        zscale = z/500.0

        for i in range(segments.GetUpperBound(0) + 1):
            if (segments[i,4] != 0):
                x1 = 640 - (int)(segments[i,0]/zscale+320.0);
                y1 = 50 + (int)(-segments[i,1]/zscale+200.0);
                x2 = 640 - (int)(segments[i,2]/zscale+320.0);
                y2 = 50 + (int)(-segments[i,3]/zscale+200.0);
                line = Line((x1, y1), (x2, y2))
                line.setWidth(10)
                line.draw(win2)
                lines.append(line)

        headsize = 40;
        head = Circle((640 - (segments[0,0]/zscale+320),
                      480 - 50 - (segments[0,1]/zscale+200)), headsize);
        head.fill = Color("yellow")
        head.draw(win2)
        lines.append(head)

def main():
    while win.IsRealized and win2.IsRealized:
        getRGB()
        getDepth()
        getJoints()

win.run(main)

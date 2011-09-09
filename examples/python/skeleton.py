# D.S. Blank
# Kinect example: reading skeletons

from Graphics import *
import Kinect

client = Kinect.Client("colossus.brynmawr.edu", 8001)
client.startKinect(False)
rgbWidth, rgbHeight = 640, 480
win = Window("Skeleton", rgbWidth, rgbHeight)

# Global places for graphical objects:
bodies = {}
heads = {}

def getJoints():
    try:
        s = client.readSkeleton()
    except:
        return
    skeletons = s[0]
    for index in range(1, skeletons + 1):
        joints = client.getJointPositions(s, index)
        segments = client.getJointSegments(s, index)

        z = joints[1,3]
        zscale = z/500.0

        bodies[index] = bodies.get(index, {})

        for i in range(segments.GetUpperBound(0) + 1):
            if (segments[i,4] != 0):
                x1 = 640 - (int)(segments[i,0]/zscale+320.0);
                y1 = 50 + (int)(-segments[i,1]/zscale+200.0);
                x2 = 640 - (int)(segments[i,2]/zscale+320.0);
                y2 = 50 + (int)(-segments[i,3]/zscale+200.0);
                if i in bodies[index]:
                    line = bodies[index][i]
                else:
                    line = Line((x1, y1), (x2, y2))
                    line.setWidth(10)
                    line.outline = Color("black")
                    line.draw(win)
                    bodies[index][i] = line
                line.set_points(Point(x1, y1), Point(x2, y2))
                line.update()

        if index in heads:
            head = heads[index]
        else:
            head = Circle((640 - (segments[0,0]/zscale+320),
                          480 - 50 - (segments[0,1]/zscale+200)), 35);
            head.fill = Color("yellow")
            head.draw(win)
            heads[index] = head
        head.moveTo(640 - (segments[0,0]/zscale+320),
                    480 - 50 - (segments[0,1]/zscale+200))

while win.IsRealized:
    getJoints()

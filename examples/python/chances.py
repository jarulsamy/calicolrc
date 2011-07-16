from Graphics import *
from Myro import randomNumber, Random, show, wait

Random.seed = 3456

pic = Picture("../images/Flathead_Lake.jpg")

width, height = 640, 480
win = Window("What are the Chances?", width, height)
win.mode = "physics"

background = Picture("missing.png")
win.canvas.shapes.Add(background)

thickness = 25
wall = Rectangle((0, -1000), (thickness, height - thickness))
wall.bodyType = "static"
wall.draw(win)
wall = Rectangle((width - thickness, -1000), (width, height - thickness))
wall.bodyType = "static"
wall.draw(win)

ground = Rectangle((0, height - thickness), (width, height))
ground.bodyType = "static"
ground.draw(win)

balls = []
for i in range(250):
    ball = Rectangle((10, 10), (42, 42))
    ball.moveTo(width/2 + i, 0 - i * 40)
    ball.bounce = .98
    ball.color = None
    ball.draw(win)
    balls.append(ball)
    #text = Text((0, 0), str(i))
    #text = Text((0, 0), str(i))
    #text.fontSize = 26
    #text.draw(ball)
    try:
        sect = Picture("sect-%s.png" % i)
        sect.draw(ball)
        sect.move(-16, -16)
        # BUG:
        #sect.outline = None
        sect.outline = Color("white")
    except:
        print(i)
        ball.outline = None
        ball.fill = Color("white")

def makePics():
    i = 0
    for ball in balls:
        if 0 < ball.center.x < 640 and 0 < ball.center.y < 480:
            sect = pic.getRegion(ball.center, ball.width, ball.height, ball.rotation)
            pic.setRegion(ball.center, ball.width, ball.height, ball.rotation, Color("black"))
            sect.savePicture("sect-%s.png" % i)
        i += 1
    pic.savePicture("missing.png")

getMouse()
win.run()



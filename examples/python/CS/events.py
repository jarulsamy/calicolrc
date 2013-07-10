
import Myro
import Graphics
import Events

Events.init()

win = Graphics.Window("Test", 600, 300)
circle1 = Graphics.Circle((100, 100), 50)
circle1.draw(win)
circle2 = Graphics.Circle((400, 100), 50)
circle2.draw(win)
square = Graphics.Rectangle((200, 100), (300, 200))
square.draw(win)
picture = Graphics.Picture(100, 100)
picture.draw(win)

def message0(o, e):
    Myro.wait(1)
    print("Message 1 received by 0:", o, e)

def message1(o, e):
    Myro.wait(1)
    print("Message 1 received by 1:", o, e)

def message2(o, e):
    Myro.wait(1)
    print("Message 2 received:", o, e)

def message3(o, e):
    print("You clicked me!")

def message4(o, e):
    print("You clicked me too!")

def message5(o, e):
    print(str(e) + " " + str(o))

Events.subscribe("message1", message0)
Events.subscribe("message1", message1)
Events.subscribe("message2", message2)
circle1.subscribe("mouse-press", message3)
circle1.subscribe("mouse-motion", message5)
circle2.subscribe("mouse-press", message4)
circle2.subscribe("mouse-motion", message5)
square.subscribe("mouse-motion", message5)
picture.subscribe("mouse-motion", message5)

print(1)
Events.publish("message1")
print(2)
Events.publishAndWait("message2")
print(3)
Events.publish("message1")
print(4)

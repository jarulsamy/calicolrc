from Graphics import *
win=Window(500,500) 
win.mode="physics"

rectangle = Rectangle((0,400),(500,500)) 
rectangle.draw(win) 
rectangle.bodyType="static"

monkey=Picture("../../../images/monkey.png") 
monkey.rotate(10)
monkey.draw(win)

circle=Circle((250,250),20) 
circle.draw(win)

oval=Oval((250,250),20, 10) 
oval.draw(win)

rectangle=Rectangle((400,100),(420, 110)) 
rectangle.draw(win)

def main():
    while True:
        if getKeyPressed():
            key = getLastKey()
            if key == "Up":
                circle.body.ApplyForce( Vector(0, -10))
                monkey.body.ApplyForce( Vector(0, -50))
                oval.body.ApplyForce( Vector(0, -10))
                rectangle.body.ApplyForce( Vector(0, -10))
            elif key == "Left":
                circle.body.ApplyForce( Vector(-10, 0))
                monkey.body.ApplyForce( Vector(-10, 0))
                oval.body.ApplyForce( Vector(-10, 0))
                rectangle.body.ApplyForce( Vector(-10, 0))
            elif key == "Right":
                circle.body.ApplyForce( Vector(10, 0))
                monkey.body.ApplyForce( Vector(10, 0))
                oval.body.ApplyForce( Vector(10, 0))
                rectangle.body.ApplyForce( Vector(10, 0))
        win.step(.01)

win.run(main)
from Processing import *

# Declare a new class called Face
class Face:
    # Faces have an data attribute named isHappy, which can be True or False
    isHappy = True

    # Faces also have a data attribute named 'diam' which stores the diameter of the Face instance
    diam = 50

    # The special __init__ method is called when an instance is created to initialize the instance.
    # In this case the diameter passed to the instance is saved.
    def __init__(self, diam):
        self.diam = diam

    # Faces have a method (function attribute) called draw,
    # that draws the face at a given x,y location.
    def draw(self, x, y):

        # Change the face color depending upon is happiness
        if self.isHappy == True:
            fill(255, 255, 0)
        else:
            fill(0, 0, 255)
        stroke(0)
        strokeWeight(2)
        ellipseMode(CENTER)
        ellipse(x, y, self.diam, self.diam )

        # Eyes
        offset = 0.2*self.diam
        eyeDiam = 0.1*self.diam
        fill(0)
        ellipse(x-offset, y-offset, eyeDiam, eyeDiam)
        ellipse(x+offset, y-offset, eyeDiam, eyeDiam)

        # Smile or Frown
        noFill()
        smileDiam = 0.6*self.diam
        if self.isHappy == True:
            startAng = 0.1*PI
            endAng = 0.9*PI
            arc(x, y, smileDiam, smileDiam, startAng, endAng)
        else:
            startAng = 1.1*PI
            endAng = 1.9*PI
            arc(x, y+0.35*self.diam, smileDiam, smileDiam, startAng, endAng)

# Create the window
window(500, 300)

# Instantiate and draw the first face, diameter 60, which defaults to happy
f1 = Face(60)
f1.draw( 150, 100 )

# Instantiate and draw the second face, diameter 50, which is not happy
f2 = Face(50)
f2.isHappy = False
f2.draw( 350, 100 )

from Processing import *

window(500, 500)

fill(255, 255, 0)
strokeWeight(5)
strokeJoin("BEVEL")

# Test shape creation
beginShape()
vertex(30, 30)
vertex(100, 60)
vertex(300, 45)
vertex(200, 190)
vertex(90, 150)
endShape(False)

from Processing import *

window(500, 500)

fill(255, 255, 0)
strokeWeight(5)
strokeJoin(BEVEL)

# Test shape creation
beginShape()
bezierVertex(150, 250, 150, 250, 90, 140)
vertex(30, 30)
vertex(100, 60)
vertex(300, 45)
vertex(200, 180)
endShape()
#endShape(False)

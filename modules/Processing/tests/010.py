from Processing import *

window(500, 500)
keepAbove(True)
fill(255, 255, 0)
strokeWeight(5)
strokeCap(ROUND)
curveTightness(0.2)

# Test shape creation
## pts = []
## pts = [(100, 250)]
## pts = [(100, 250), (50, 80)]
## pts = [(100, 250), (50, 80), (200, 100)]
pts = [(100, 250), (50, 80), (200, 100), (300, 80), (250, 240)]
beginShape()
for p in pts: curveVertex(p[0], p[1])
endShape() #False)

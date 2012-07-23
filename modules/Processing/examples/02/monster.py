# Monster1
# A sketch that can be drawn using simple shapes
# introduced in week 1...

from Processing import *

PI = 3.1415926

window(400, 400)
background(188, 183, 8)
smooth()

# Draw the face

# ears
fill(0)
arc(107, 200, 14, 160, PI, 2*PI)
arc(293, 200, 14, 165, PI, 2*PI)
# Face is made up of four quads, so we will use arcs to draw them
# Quad1
ellipseMode(CENTER)
fill(242, 111, 10)
arc(200, 200, 200, 120, 0, PI/2.0)

# Quad2
fill(10, 201, 196)
arc(200, 200, 200, 120, PI/2.0, PI)

# Quad3
fill(242, 111, 10)
arc(200, 200, 200, 120, PI, 3*PI/2.0)

# Quad3
fill(250, 15, 43)
arc(200, 200, 200, 120, 3*PI/2.0, 2*PI)

# Four eyes
fill(255)
ellipse(140, 190, 10, 10)
ellipse(180, 185, 10, 10)
ellipse(220, 185, 10, 10)
ellipse(260, 190, 10, 10)

fill(0)
ellipse(140, 190, 4, 4)
ellipse(180, 185, 4, 4)
ellipse(220, 185, 4, 4)
ellipse(260, 190, 4, 4)

# Mouth
ellipse(195, 230, 30, 15)
ellipse(205, 227, 30, 20)

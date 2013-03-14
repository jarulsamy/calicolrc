from Processing import *

# Ndebele
size(850, 600)
immediateMode(False)
smooth()
noLoop()
background(135, 205, 235)
strokeWeight(5)

# draw house
# front
fill(152, 251, 152)
rect(50, 200, 750, 400)
fill(124, 252, 0)
rect(50, 225, 750, 50)

# roof
fill(139, 69, 19)
quad(0, 200, 850, 200, 700, 50, 150, 50)
# Door
fill(0)
rect(400, 225, 100, 375)

# white line in middle
fill(255)
rect(50, 400, 350, 15)

rect(500, 400, 300, 15)

# bottom panel
#fill(152, 251, 152)
fill(124, 252, 0)
rect(50, 500, 350, 100)
rect(500, 500, 300, 100)

# bottom panel triangle
fill(0, 100, 0)
triangle(50, 600, 225, 500, 400, 600)
triangle(500, 600, 650, 500, 800, 600)

fill(255, 255, 0)
triangle(150, 600, 225, 540, 300, 600)
triangle(575, 600, 650, 540, 725, 600)

fill(255, 0, 0)
beginShape()
vertex(150, 600)
vertex(150, 550)
vertex(200, 550)
vertex(200, 560)
vertex(250, 560)
vertex(250, 550)
vertex(300, 550)
vertex(300, 600)
endShape(CLOSE)

beginShape()
vertex(575, 600)
vertex(575, 550)
vertex(625, 550)
vertex(625, 560)
vertex(675, 560)
vertex(675, 550)
vertex(725, 550)
vertex(725, 600)
endShape(CLOSE)
# end of bottom panel

# middle panel
fill(0, 100, 0)
triangle(50, 415, 125, 457, 50, 500)
triangle(400, 415, 325, 457, 400, 500)

triangle(500, 415, 575, 457, 500, 500)
triangle(800, 415, 725, 457, 800, 500)

fill(255, 0, 0)
beginShape()
vertex(140, 415)
vertex(140, 500)
vertex(225, 450)
vertex(310, 500)
vertex(310, 415)
endShape(CLOSE)

beginShape()
vertex(590, 415)
vertex(590, 500)
vertex(650, 450)
vertex(710, 500)
vertex(710, 415)
endShape(CLOSE)

fill(255, 255, 0)
triangle(155, 500, 225, 460, 295, 500)
triangle(600, 500, 650, 460, 700, 500)
#end of middle panel

# top panel
#  fill(173, 216, 230)
#  rect(50, 275, 350, 125)
#  rect(500, 275, 300, 125)

fill(255, 0, 0)
beginShape()
vertex(50, 400)
vertex(50, 375)
vertex(150, 375)
vertex(150, 325)
vertex(300, 325)
vertex(300, 375)
vertex(400, 375)
vertex(400, 400)
endShape(CLOSE)

beginShape()
vertex(500, 400)
vertex(500, 375)
vertex(600, 375)
vertex(600, 325)
vertex(700, 325)
vertex(700, 375)
vertex(800, 375)
vertex(800, 400)
endShape(CLOSE)

fill(255, 255, 0)
rect(90, 315, 20, 60)
rect(340, 315, 20, 60)

rect(540, 315, 20, 60)
rect(740, 315, 20, 60)

fill(255, 0, 0)

triangle(70, 315, 100, 290, 130, 315)
triangle(320, 315, 350, 290, 380, 315)

triangle(520, 315, 550, 290, 580, 315)
triangle(720, 315, 750, 290, 780, 315)

fill(255, 255, 0)
rect(200, 285, 50, 40)
rect(625, 285, 50, 40)

fill(255, 255, 0)
beginShape()
vertex(150, 350)
vertex(225, 400)
vertex(300, 350)
vertex(300, 325)
vertex(150, 325)
endShape(CLOSE)

beginShape()
vertex(600, 350)
vertex(650, 400)
vertex(700, 350)
vertex(700, 325)
vertex(600, 325)
endShape(CLOSE)

fill(0, 100, 0)
beginShape()
vertex(225, 325)
vertex(175, 305)
vertex(175, 335)
vertex(225, 365)
vertex(275, 335)
vertex(275, 305)
endShape(CLOSE)

beginShape()
vertex(650, 325)
vertex(612, 305)
vertex(612, 335)
vertex(650, 365)
vertex(687, 335)
vertex(687, 305)
endShape(CLOSE)
#end of top panel

# arcs

fill(255, 255, 0)
arc(100, 225, 100, 50, 0, PI)
arc(200, 225, 100, 50, 0, PI)
arc(300, 225, 100, 50, 0, PI)
arc(400, 225, 100, 50, 0, PI)
arc(500, 225, 100, 50, 0, PI)
arc(600, 225, 100, 50, 0, PI)
arc(700, 225, 100, 50, 0, PI)
arc(800, 225, 100, 50, PI/2, PI)

# just below roof
fill(0, 100, 0)
rect(50, 200, 750, 25)

redraw()


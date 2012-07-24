# A character in the style of South park TV Series... new friend!
from Processing import *

window(400, 500)
immediateMode(False)

smooth()
background(255)

# the entire character is anchored at 500, 200 in a 400, 500 pixel window...

# arms
noStroke()
fill(255, 206, 8)
beginShape()    # left arm
curveVertex(120, 240)
curveVertex(120, 240)
curveVertex(82, 300)
curveVertex(75, 365)
curveVertex(100, 373)
curveVertex(122, 375)
curveVertex(138, 318)
curveVertex(160, 260)
curveVertex(140, 240)
curveVertex(120, 240)
endShape(CLOSE)

beginShape()    #right arm
curveVertex(290, 240)
curveVertex(290, 240)
curveVertex(320, 300)
curveVertex(328, 358)
curveVertex(290, 370)
curveVertex(260, 320)
curveVertex(250, 260)
curveVertex(270, 235)
curveVertex(290, 240)
endShape(CLOSE)

# legs
noStroke()
fill(134, 4, 69)
rectMode(CORNERS)
rect(120, 420, 290, 450)
fill(0)
quad(120, 450, 110, 457, 300, 457, 290, 450) 
stroke(0)
line(200, 420, 202, 450)

# torso
fill(10, 138, 15)
noStroke()
beginShape()
curveVertex(158, 260)
curveVertex(158, 260)
curveVertex(138, 300)
curveVertex(120, 350)
curveVertex(110, 385)
curveVertex(110, 420)
curveVertex(200, 430)
curveVertex(300, 417)
curveVertex(297, 390)
curveVertex(285, 340)
curveVertex(270, 300)
curveVertex(250, 260)
curveVertex(158, 260)
endShape(CLOSE)

# lapel
stroke(0)
strokeWeight(2)
beginShape()
curveVertex(200, 260)
curveVertex(200, 260)
curveVertex(185, 350)
curveVertex(192, 420)
curveVertex(192, 420)
endShape()

# collars
beginShape()
curveVertex(155, 265)
curveVertex(155, 265)
curveVertex(175, 280)
curveVertex(200, 270)
curveVertex(200, 270)
endShape()

beginShape()
curveVertex(200, 270)
curveVertex(200, 270)
curveVertex(220, 280)
curveVertex(252, 262)
curveVertex(252, 262)
endShape()

# hands
stroke(203, 165, 103)
strokeWeight(1)
fill(250, 222, 199)

# left hand
ellipse(95, 380, 50, 50)
ellipse(95, 355, 20, 20)

# right hand
ellipse(310, 375, 50, 50)
ellipse(290, 370, 20, 20)

# head
fill(250, 222, 199)
ellipseMode(CENTER)
noStroke()
ellipse(200, 150, 280, 240)

# hair
# left bangs
stroke(0)  # hair color
fill(0)
beginShape()
curveVertex(65, 120)
curveVertex(65, 120)
curveVertex(75, 135)
curveVertex(92, 150)
curveVertex(84, 128)
curveVertex(90, 100)
curveVertex(88, 125)
curveVertex(100, 140)
curveVertex(95, 125)
curveVertex(100, 100)
curveVertex(95, 108)
curveVertex(103, 122)
curveVertex(105, 125)
curveVertex(110, 107)
curveVertex(125, 92)
curveVertex(95, 100)
curveVertex(70, 90)
curveVertex(65, 120)
endShape(CLOSE)

#much room above for improvement, right bangs...full head etc.

# hat
stroke(77, 64, 234)  # flap color
fill(77, 64, 234)    # hat color

# left and right flaps
strokeWeight(10)
line(65, 100, 62, 220)
line(335, 100, 340, 230)

# beanie
strokeWeight(1)
beginShape()
curveVertex(62, 120)
curveVertex(62, 120)
curveVertex(100, 100)
curveVertex(190, 80)
curveVertex(260, 90)
curveVertex(340, 130)
curveVertex(340, 90)
curveVertex(300, 40)
curveVertex(200, 20)
curveVertex(120, 40)
curveVertex(70, 80)
curveVertex(55, 100)
curveVertex(62, 120)
endShape(CLOSE)

# beanie top
noStroke()
fill(234, 64, 147)
ellipse(210, 19, 32, 25)
ellipse(190, 20, 40, 20)
ellipse(225, 22, 40, 22)

# eyes
fill(255)
ellipse(175, 140, 50, 60) # left eye
ellipse(222, 138, 52, 58)  # right eye

strokeWeight(1)
stroke(0)
fill(255, 230, 210)   # same skin color as face/head
arc(175, 140, 50, 60, 0.9*PI, 1.9*PI)  # left eyelid
arc(222, 138, 52, 58, 1.1*PI, 2.1*PI)  # right eyelid

fill(0)
arc(175, 140, 8, 8, 1.9*PI, 2.9*PI)
arc(222, 138, 8, 8, 0.1*PI, 1.1*PI)

# mouth
strokeWeight(4)
stroke(0)
line(165, 195, 235, 188)

# tongue
fill(255, 0, 0)
beginShape()
curveVertex(178, 193)
curveVertex(178, 193)
curveVertex(177, 207)
curveVertex(180, 220)
curveVertex(182, 230)
curveVertex(185, 236)
curveVertex(195, 240)
curveVertex(205, 240)
curveVertex(215, 237)
curveVertex(220, 230)
curveVertex(222, 220)
curveVertex(223, 207)
curveVertex(220, 190)
curveVertex(178, 193)
endShape(CLOSE)
line(200, 192, 205, 226)

redraw()

from Processing import *

window(500, 500)

noFill()
rect(50, 50, 400, 400)

textAlign(LEFT)
text("Upper left", 50, 50, 400, 400)

textAlign(CENTER)
text("In the middle", 50, 50, 400, 400)

textAlign(RIGHT)
text("Upper right", 50, 50, 400, 400)

textAlign(LEFT, CENTER)
text("Left of center", 50, 50, 400, 400)

textAlign(CENTER, CENTER)
text("Center Center", 50, 50, 400, 400)

textAlign(RIGHT, CENTER)
text("Right of center", 50, 50, 400, 400)

textAlign(LEFT, BOTTOM)
text("Down here", 50, 50, 400, 400)

textAlign(CENTER, BOTTOM)
text("Center Bottom", 50, 50, 400, 400)

textAlign(RIGHT, BOTTOM)
text("Over here", 50, 50, 400, 400)

## textAlign(LEFT, BASELINE)
## text("Base left", 50, 50, 400, 400)
## 
## textAlign(CENTER, BASELINE)
## text("Base Center", 50, 50, 400, 400)
## 
## textAlign(RIGHT, BASELINE)
## text("Base right", 50, 50, 400, 400)

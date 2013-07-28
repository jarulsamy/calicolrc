from Processing import *
from Myro import *
import Processing

makeRobot("Hummingbird")
window(500, 500)
smooth()
noStroke()

paddle_width = 100
paddle_height = 15
ball_x = Processing.random(0, width())
ball_y = Processing.random(0, height()/2)
ball_dx = Processing.random(-6, 6)
ball_dy = Processing.random(-6, 6)
size = 15

def keyPress(o, e):
    global ball_dx, ball_dy
    ball_dx = Processing.random(-6, 6)
    ball_dy = Processing.random(-6, 6)

onKeyPressed += keyPress

def draw():
    global ball_x, ball_y, ball_dx, ball_dy, paddle_x, paddle_y
    background(0)
    pos = get("1","knob")
    #pos = get("2","distance")
    paddle_x = Processing.map(pos, 0, 255, 0, width())
    paddle_y = height() - paddle_height
    paddle_x = constrain(paddle_x, 0, width() - paddle_width)

    ball_x += ball_dx
    ball_y += ball_dy
    if (ball_x < size/2 or ball_x > width() - size/2): ball_dx *= -1
    if (ball_y < size/2): ball_dy *= -1
    if (ball_y + size/2 > width() - paddle_height and
        paddle_x < ball_x < paddle_x + paddle_width):
        ball_dy *= -1

    fill(196, 0, 0)
    ellipse(ball_x, ball_y, size, size)
    fill(196, 255, 0)
    rect(paddle_x, paddle_y, paddle_width, paddle_height)

frameRate(60)
onLoop += draw
loop()

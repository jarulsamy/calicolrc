from Processing import *

# Draw the earth and saturn orbitaing around the sun
# Make extensive use of transformations

window(500, 500)
smooth()

# Keep track of rotation angle
angle = 0.0

# Draw a simple earth
def drawEarth(x, y, s):
    pushMatrix()
    translate( x, y )
    scale( s )
    fill(128, 128, 255)
    ellipse(0, 0, 100, 100)
    popMatrix()

# Draw the sun
def drawSun(x, y, s):
    pushMatrix()
    translate( x, y )
    scale( s )

    # Sun
    noStroke()
    ellipseMode(CENTER)

    fill(255, 255, 0)
    ellipse(0, 0, 100, 100)

    fill(220, 220, 0)

    # Sun rays
    da = radians(10.0)
    for i in range(36):
        triangle(50, 5, 50, -5, 90, 0)
        rotate(da)
    popMatrix()

# Draw saturn
def drawSaturn(x, y, s, r):
    pushMatrix()
    translate( x, y )
    scale( s )
    rotate( r )

    # Draw the planet
    fill(255, 0, 255)
    ellipse(0, 0, 100, 100)

    # Draw a ring
    strokeWeight(10)
    noFill()
    stroke(200)
    a1 = radians( -60.0 )
    a2 = radians( 240.0 )
    arc(0, 0, 200, 50, a1, a2)
    popMatrix()

# Draw the entire scene, after rotating by the global angle
def draw(o, e):
    global angle
    background(0)
    translate( 0.5*width(), 0.5*height() )

    drawSun( 0, 0, 1.0 )

    rotate(angle)
    drawEarth( 100, 150, 0.3 )

    # Apply the reverse of global rotation angle to keep saturn from rotating
    drawSaturn( -100, -150, 0.5, -angle)

    #drawSun( 150, 150, 0.2 )
    angle = (angle + 0.01) % TWO_PI

# Set up looping
onLoop += draw
frameRate(10)
loop()

from Processing import *
import random
size(640, 360)

yoff = 0.0        ## 2nd dimension of perlin noise

def draw():
  global yoff
  random.seed(frameCount())
  background(51)

  fill(255)
  ## We are going to draw a polygon out of the wave points
  beginShape()

  xoff = 0;       ## Option #1: 2D Noise
  ## float xoff = yoff; // Option #2: 1D Noise

  ## Iterate over horizontal pixels
  for x in range(0, int(width()) + 1, 10): ## (float x = 0; x <= width; x += 10) {
    ## Calculate a y value according to noise, map to
    y = map(noise(xoff, yoff), 0, 1, 200,300); ## Option #1: 2D Noise
    ## float y = map(noise(xoff), 0, 1, 200,300);    // Option #2: 1D Noise

    ## Set the vertex
    vertex(x, y)
    ## Increment x dimension for noise
    xoff += 0.05
  ## increment y dimension for noise
  yoff += 0.01
  vertex(width(), height())
  vertex(0, height())
  endShape(CLOSE)

# Set up looping
onLoop += draw
frameRate(40)
loop()

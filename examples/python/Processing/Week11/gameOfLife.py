from Processing import *

window(700, 700)
smooth()
fill(255)
noStroke()
frameRate(15)
randomSeed(1) 

sz = 10
sx = int(width() / sz)
sy = int(height() / sz)

world = [[[0 for z in range(2)] for y in range(sy)] for x in range(sx)]
paused = True

for i in range(50):
	#a bunch of random gliders
    x =int(random(5, width()/sz-5))
    y =int(random(5, height()/sz-5))
    #print (x,y)
    world[x + 0][y][0] = 1
    world[x + 2][y+1][0] = 1
    world[x + 0][y+2][0] = 1
    world[x + 1][y+2][0] = 1
    world[x + 2][y+2][0] = 1
	#world(random(sx).toInt)(random(sy).toInt)(1) = 1


def neighbors(x, y):
 	return world[(x + 1) % sx][y][0] +\
    world[x][(y + 1) % sy][0] +\
    world[(x + sx - 1) % sx][y][0] +\
    world[x][(y + sy - 1) % sy][0] +\
    world[(x + 1) % sx][(y + 1) % sy][0] +\
    world[(x + sx - 1) % sx][(y + 1) % sy][0] +\
    world[(x + sx - 1) % sx][(y + sy - 1) % sy][0] +\
    world[(x + 1) % sx][(y + sy - 1) % sy][0]


def addCells(o, e):
    world[int(mouseX() / sz)][int(mouseY() / sz)][0] = 1

onMouseDragged += addCells

def handleKeys(o, e):
    global paused, world
    if (key == 'c'):
        world = [[[0 for z in range(2)] for y in range(sy)] for x in range(sx)]
    else:
        paused = not paused

onKeyPressed += handleKeys

def step(o, e):
    global paused
    background(0)

    # draw the board
    for x in range(sx):
        for y in range(sy):
            if world[x][y][0] == 1:
                rect(x*sz, y*sz, sz, sz)

    doEvents()
    if (paused):
        return

	# birth and death
    for x in range(sx):
        for y in range(sy):
            count = neighbors(x, y)
		    #birth
            if count == 3:
                world[x][y][1] = 1
		    #death
            elif count < 2 or count > 3:
                world[x][y][1] = 0
		    #stay-ing alive
            elif world[x][y][0] == 1:
                world[x][y][1] = 1

    #update the state of the game
    for x in range(sx):
        for y in range(sy):
            world[x][y][0] = world[x][y][1]

onLoop += step

loop()

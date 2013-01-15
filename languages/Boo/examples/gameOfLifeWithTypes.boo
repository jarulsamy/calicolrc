import Processing

window(700, 700)
smooth()
fill(255) 
noStroke()
frameRate(12) 
randomSeed(1) 

sz as int = 10
sx as int = width() / sz
sy as int = height() / sz

world = matrix(int, sx, sy, 2)
paused as bool = true

for i as int in range(50):
	#a bunch of random gliders
    x as int = toInt(random(5, width()/sz-5))
    y as int = toInt(random(5, height()/sz-5))
    world[x + 0, y, 0] = 1
    world[x + 2, y+1, 0] = 1
    world[x + 0, y+2, 0] = 1
    world[x + 1, y+2, 0] = 1
    world[x + 2, y+2, 0] = 1
	#world(random(sx).toInt)(random(sy).toInt)(1) = 1


def neighbors(x as int, y as int):
 	return world[(x + 1) % sx, y, 0] +\
    world[x, (y + 1) % sy, 0] +\
    world[(x + sx - 1) % sx, y, 0] +\
    world[x, (y + sy - 1) % sy, 0] +\
    world[(x + 1) % sx, (y + 1) % sy, 0] +\
    world[(x + sx - 1) % sx, (y + 1) % sy, 0] +\
    world[(x + sx - 1) % sx, (y + sy - 1) % sy, 0] +\
    world[(x + 1) % sx, (y + sy - 1) % sy, 0]


def addCells() as void:
    world[mouseX() / sz, mouseY() / sz, 0] = 1

onMouseDragged += addCells

def handleKeys() as void:
    if (key == 'c'):
        world = matrix(int, sx, sy, 2)
    else:
        paused = not paused

onKeyPressed += handleKeys

def step(o as object, e as PElapsedEventArgs) as void:
    background(0)

    # draw the board
    for x as int in range(sx):
        for y as int in range(sy):
            if world[x,y,0] == 1:
                rect(x*sz, y*sz, sz, sz)

    if (paused):
        return

 # birth and death
    for x as int in range(sx):
        for y as int in range(sy):
            count = neighbors(x, y)
         #birth
            if count == 3:
                world[x,y,1] = 1
         #death
            elif count < 2 or count > 3:
                world[x,y,1] = 0
         #stay-ing alive
            elif world[x,y,0] == 1:
                world[x,y,1] = 1

    #update the state of the game
    for x as int in range(sx):
        for y as int in range(sy):
            world[x,y,0] = world[x,y,1]

onLoop += step

loop()

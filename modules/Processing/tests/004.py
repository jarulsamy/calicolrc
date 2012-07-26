from Processing import *

window(500, 500)

for i in range(10000):
    x = random(500)
    y = random(500)
    point(x, y)
    delay(1)        # Magic

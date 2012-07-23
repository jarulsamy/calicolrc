from Processing import *

window(700, 500)

for i in range(10000):
    stroke( random(255), random(255), random(255) )
    line( random(700), random(500), random(700), random(500))
    delay(1)

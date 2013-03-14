from Processing import *

window(500, 500)

# Test key detection using while-loop
## while True:
##     if isKeyPressed():
##         print( "key() is ", key() )
##         print( "keyCode() is ", keyCode() )
##         delay(100)

# Test keys detection using internal timer
def testKey(o, e):
    if isKeyPressed():
        print( o )
        print( "key() is ", key() )
        print( "keyCode() is ", keyCode() )
frameRate(100)
onLoop += testKey
loop()

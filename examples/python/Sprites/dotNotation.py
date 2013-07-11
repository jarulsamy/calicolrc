import Common
from Sprites import *

'''
Instead of using the selectSprite function to switch between
Sprites, you can use dot notation.
'''

init(False)
defaultSprite = getSprite("Sprite1")
defaultSprite.changeCostume("pinkHair")
defaultSprite.show()

goldy = makeSprite()
goldy.addCostume("goldHair.png")
goldy.changeCostume("goldHair")
goldy.forward(30)
goldy.show()
goldy.glideTo(170, 220, 1)

alex = makeSprite("Alex")
alex.changeCostume("greenHair")
alex.moveTo(400, 220)
alex.show()

print("\nBefore the remove: ")
printAllSpriteNames()

removeSprite(defaultSprite.getName())

print("\nAfter the remove: ")
printAllSpriteNames()

print()
print("Contains " + defaultSprite.getName() + ": " + str(containsSprite(defaultSprite.getName())))
print("Contains " + goldy.getName() + ": " + str(containsSprite(goldy.getName())))
print("Contains " + alex.getName() + ": " + str(containsSprite(alex.getName())))

alexAlias = selectSprite(alex.getName())
alexAlias.glideTo(300,300, 1)
print("Contains " + alexAlias.getName() + ": " + str(containsSprite(alexAlias.getName())))

print("\nAfter alias: ")
printAllSpriteNames()

goldy.hide()


anotherSprite = makeSprite()
anotherSprite.moveTo(400, 220)
anotherSprite.addCostume("redHair.png")
anotherSprite.changeCostume("redHair")
anotherSprite.show()

makeSpritesVisibleOnCreation(True)
aBasicSprite = makeSprite()


print("\nAfter additions: ")
printAllSpriteNames()


listOfSprites = []
listOfSprites.append(alex)
listOfSprites.append(goldy)
listOfSprites.append(aBasicSprite)
listOfSprites.append(anotherSprite)



for sprite in listOfSprites:
 #   print("\t" + sprite.getName() + " is visible: " + str(sprite.getIsVisible()))
    print()
    sprite.printStatus()
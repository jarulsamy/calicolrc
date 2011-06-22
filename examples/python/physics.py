from Graphics import *
from Myro import pickOne

import clr
clr.AddReference("FarseerPhysics")
from FarseerPhysics.Dynamics import World, BodyType
from FarseerPhysics.Collision.Shapes import CircleShape
from FarseerPhysics.Factories.BodyFactory import CreateBody, CreateCircle, CreateRectangle
from Microsoft.Xna.Framework import Vector2

def MyOnCollision(fixture1, fixture2, contact):
    return True

def pixelsToMeters(p):
    return

# World
world = World(Vector2(0, 9.8))

win = Window("Physics Experiments", 700, 700)
win.mode = "manual"
MeterInPixels = 64.0
width = 300
height = 300
screenCenter = Vector2(width / 2.0, height / 2.0)

circles = []
for i in range(10):
    # Convert screen center from pixels to meters
    circlePosition = Vector2.Add(Vector2.Divide(screenCenter, MeterInPixels),
                                 Vector2(2 - .25 * i, -2.0 + i * .05))
    # Create the circle fixture
    circle = CreateCircle(world,                           # world
                          96.0 / (2.0 * MeterInPixels),    # ?
                          1.0,                             # mass
                          circlePosition)                  # center
    circle.BodyType = BodyType.Dynamic                     # or

    # Give it some bounce and friction
    circle.Restitution = 0.8
    circle.Friction = 0.5

    sprite = Circle((150, 10), 47)
    sprite.color = Color(pickOne(["red", "blue", "green"]))
    sprite.draw(win)
    sprite.outline = Color("black")

    circles.append( (circle, sprite) )

# Ground #
groundPosition = Vector2.Add(Vector2.Divide(screenCenter, MeterInPixels),
                             Vector2(0, 7.25))

# Create the ground fixture
groundBody = CreateRectangle(world,
                            512.0 / MeterInPixels,
                            64.0 / MeterInPixels,
                            1.0,                          # mass
                            groundPosition)
groundBody.IsStatic = True;
groundBody.Restitution = 0.3;
groundBody.Friction = 0.5;

groundSprite = Rectangle((0, 583), (400, 600))
groundSprite.color = Color("green")
groundSprite.draw(win)

seconds = 0.01

def draw():
    global seconds
    world.Step(seconds)

    # Circle position and rotation #
    # Convert physics position (meters) to screen coordinates (pixels)
    for circle, sprite in circles:
        circlePos = Vector2.Multiply(circle.Position, MeterInPixels)
        circleRotation = circle.Rotation
        # Align sprite center to body position
        circleOrigin = Vector2(sprite.radius, sprite.radius)
        # Draw circle
        sprite.moveTo(circlePos.X, circlePos.Y)
        sprite.rotateTo(circleRotation)

    # Ground position and origin
    groundPos = Vector2.Multiply(groundBody.Position, MeterInPixels)
    groundOrigin = Vector2(groundSprite.width / 2.0,
                           groundSprite.height / 2.0)
    win.step(0.005)


def loop(n):
    for i in range(n):
        draw()

loop(900)

## We fix the body and shape together using a Fixture object
#fixture = myBody.CreateFixture(circleShape)
#fixture.OnCollision = MyOnCollision


from Graphics import *

import clr
clr.AddReference("FarseerPhysics")
from FarseerPhysics.Dynamics import World, BodyType
from FarseerPhysics.Collision.Shapes import CircleShape
from FarseerPhysics.Factories.BodyFactory import CreateBody, CreateCircle, CreateRectangle
from Microsoft.Xna.Framework import Vector2

def MyOnCollision(fixture1, fixture2, contact):
    return True

MeterInPixels = 64.0
width = 300
height = 300
screenCenter = Vector2(width / 2.0, height / 2.0)

# Convert screen center from pixels to meters
circlePosition = Vector2.Add(Vector2.Divide(screenCenter, MeterInPixels), Vector2(-2, -2.0))

# World
world = World(Vector2(.07, 9.8))

# Create the circle fixture
circleBody = CreateCircle(world,
                          96.0 / (2.0 * MeterInPixels),
                          1.0,
                          circlePosition)
circleBody.BodyType = BodyType.Dynamic

# Give it some bounce and friction
circleBody.Restitution = 0.8
circleBody.Friction = 0.0

# Ground *
groundPosition = Vector2.Add(Vector2.Divide(screenCenter, MeterInPixels), Vector2(0, 3.25))

# Create the ground fixture
groundBody = CreateRectangle(world,
                            512.0 / MeterInPixels,
                            64.0 / MeterInPixels,
                            1.0,
                            groundPosition)
groundBody.IsStatic = True;
groundBody.Restitution = 0.3;
groundBody.Friction = 0.5;

groundSprite = Rectangle((0, 285), (400, 300))
groundSprite.color = Color("green")
circleSprite = Pie((150, 10), 10, 0, 180)
circleSprite.color = Color("red")

win = Window("Physics Experiments", 500, 300)
win.mode = "manual"
groundSprite.draw(win)
circleSprite.draw(win)

def update(TotalMilliseconds):
    world.Step(TotalMilliseconds * 0.001)

def draw():
    #GraphicsDevice.Clear(Color.CornflowerBlue);

    # Circle position and rotation */
    # Convert physics position (meters) to screen coordinates (pixels)
    circlePos = Vector2.Multiply(circleBody.Position, MeterInPixels)
    circleRotation = circleBody.Rotation

    # Ground position and origin
    groundPos = Vector2.Multiply(groundBody.Position, MeterInPixels)
    groundOrigin = Vector2(groundSprite.width / 2.0,
                           groundSprite.height / 2.0)

    # Align sprite center to body position
    circleOrigin = Vector2(circleSprite.radius, circleSprite.radius)

    # Draw circle
    circleSprite.moveTo(circlePos.X, circlePos.Y)
    circleSprite.rotateTo(circleRotation)
    win.step(.04)


for i in range(0, 170, 1):
    update(i)
    draw()

## Create a World object with zero gravity
#world = World(Vector2(0, 20))
#
## We create a body object and make it dynamic (movable)
#myBody = CreateBody(world)
#myBody.BodyType = BodyType.Dynamic
#
## We create a circle shape with a radius of 0.5 meters
#circleShape = CircleShape(0.5, 1.0) # radius, density
#
## We fix the body and shape together using a Fixture object
#fixture = myBody.CreateFixture(circleShape)
#fixture.OnCollision = MyOnCollision


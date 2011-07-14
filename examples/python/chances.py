from Graphics import *
from Myro import randomNumber, Random

Random.seed = 3456

width, height = 640, 480
win = Window("What are the Chances?", width, height)
win.mode = "physics"

thickness = 25
wall = Rectangle((0, 0), (thickness, height - thickness))
wall.bodyType = "static"
wall.draw(win)
wall = Rectangle((width - thickness, 0), (width, height - thickness))
wall.bodyType = "static"
wall.draw(win)

ceil = Rectangle((0, 0), (width/2 - 20, thickness))
ceil.bodyType = "static"
ceil.draw(win)

ceil = Rectangle((width/2 + 95, 0), (width, thickness))
ceil.bodyType = "static"
ceil.draw(win)

flipper = Rectangle((0, 0), (110, thickness))
flipper.rotateTo(45)
flipper.moveTo(width/2 + 10, 75)
flipper.bodyType = "static"
flipper.draw(win)

ground = Rectangle((0, height - thickness), (width, height))
ground.bodyType = "static"
ground.draw(win)

balls = []
for i in range(60):
    ball = Pie((10, 10), 16, 0, 360)
    ball.moveTo(width/2 + i, 0 - i * 32)
    ball.bounce = .98
    ball.fill = None
    ball.draw(win)
    balls.append(ball)

final_degrees = [
    17.8843602756,
    -693.021264588,
    469.914228180,
    -859.084855992,
    -1698.16534466,
    96.8005691441,
    -1229.78299131,
    -1097.58954302,
    1040.98585291,
    -1536.58973396,
    -865.234539754,
    4448.39532763,
    1293.60526246,
    23058.3584411,
    -6336.31250721,
    -17709.3389594,
    -10324.8278102,
    -1369.51368995,
    -3695.12622113,
    1992.39962150,
    -1191.15286204,
    678.860369985,
    -1344.75780591,
    3218.42360454,
    1004.00119818,
    25435.7591058,
    -1794.27299111,
    -1311.92634332,
    -1654.66479874,
    -12293.3875580,
    -2332.42873212,
    150.946907450,
    -17897.1239184,
    -1008.46475412,
    -306.782864307,
    -972.677513512,
    -1551.56249216,
    -1704.71861064,
    19619.3754607,
    37377.6697190,
    13141.1571948,
    -29159.1804511,
    2566.85612406,
    -7504.13759580,
    -298.986640502,
    982.110387247,
    15764.9735995,
    722.827663711,
    1045.05052581,
    -1013.16479853,
    4715.89788330,
    333.882432141,
    2399.18120907,
    -1818.07013516,
    17.4513433577,
    105.601363512,
    1548.42694356,
    22164.2938914,
    -3599.85043707,
    722.134481472]

i = 0
for ball in balls:
    ball.rotateTo(-final_degrees[i])
    i += 1

getMouse()
win.run()

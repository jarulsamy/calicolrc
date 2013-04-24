from ai.conx import *
import time

network = Network(seed=2354)
network.addLayers(2, 2, 1)

network.setInputs([[0, 0], [0, 1], [1, 0], [1, 1]])
network.setTargets([[0], [1], [1], [0]])

network.setTolerance(.2)
network.setEpsilon(.5)
network.setMomentum(.975)
network.setBatch(1)
start = time.time()
network.train()
stop = time.time()
print("Learning took %s secconds" % str(stop - start))
network.setLearning(0)

#network.setInteractive(1)
#network.sweep()

from Graphics import *
resolution = 50
size = 200
## See map of all possible outputs, given sampling of all possible inputs:
window = Window("XOR Generalization", size, size)
window.mode = "bitmap"
for i in range(resolution):
    for j in range(resolution):
        x = i * size/resolution
        y = j * size/resolution
        rectangle = Rectangle((x, y), (x + size/resolution, y + size/resolution))
        gray = 255 - network.propagate(input=[i/resolution, j/resolution])[0] * 255
        rectangle.color = Color( gray, gray, gray)
        rectangle.draw(window)

## See activations at hidden layer, unit h for sampling of inputs:
for h in range(len(network["hidden"].activation)):
    window = Window("XOR Hidden%d" % h, size, size)
    window.mode = "bitmap"
    for i in range(resolution):
        for j in range(resolution):
            x = i * size/resolution
            y = j * size/resolution
            rectangle = Rectangle((x, y), (x + size/resolution, y + size/resolution))
            network.propagate(input=[i/resolution, j/resolution])
            gray = 255 - network["hidden"].activation[h] * 255
            rectangle.color = Color( gray, gray, gray)
            rectangle.draw(window)


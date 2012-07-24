from Graphics import *

import gis

data = {}
fp = open("state-smoke.dat")
for line in fp:
    line = line.strip()
    if line:
        state, abbrev, code, percent, male, female = line.split(",")
        data[abbrev[1:-1]] = state[1:-1], code[1:-1], float(percent), float(male), float(female)

values = []
for key in data:
    state, code, percent, male, female = data[key]
    if key in gis.states:
        values.append(percent)

minp = min(values)
maxp = max(values)

win = Window("Smokers by State", gis.width, gis.height)

for key in data:
    state, code, percent, male, female = data[key]
    if key in gis.states:
        color = 255 - ((percent - minp) / (maxp - minp)) * 255
        gis.drawState(win, key, Color(0, color, color))


import Graphics

win = Graphics.Window("Test", 200, 200)

entry = Graphics.Entry((10, 20), 15) # center at (10, 20), max chars 15
entry.text = "Hello" # default text
entry.draw(win)
#...
t = entry.text # later, get the text from the box

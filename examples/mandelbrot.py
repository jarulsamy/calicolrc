from Graphics import *
win = Window("Mandelbrot", 150, 150)
win.mode = 'manual'
pic = Picture(win.width, win.height) 
pic.draw(win)
pic.move(pic.width/2, pic.height/2)

xa = 0.0 #-2.0
xb = 1.0
ya = 0.0 #-1.5
yb = 1.0 # 1.5
maxIt = 255 # max iterations allowed

for y in range(pic.height):
    zy = y * (yb - ya) / (pic.height - 1)  + ya
    for x in range(pic.width):
        zx = x * (xb - xa) / (pic.width - 1)  + xa
        z = zx + zy * 1j
        c = z
        for i in range(maxIt):
            if abs(z) > 2.0: break 
            z = z * z + c
        pic.setRGB(x, y, i % 4 * 64, i % 16 * 16, i % 8 * 32)
    win.update()

# Spatial Filtering
from Processing import *

# Difference convolution matrices applied to the bottom half of an image

# Sharpen
matrix = [[ -1., -1., -1.],[ -1.,  9., -1.],[ -1., -1., -1. ]]

# Laplacian Edge Detection
#matrix = [[  0.,  1.,  0. ],[  1., -4.,  1. ],[  0.,  1.,  0. ]]

# Average
#matrix = [ [ 1./9., 1./9., 1./9.],[ 1./9., 1./9., 1./9. ],[ 1./9., 1./9., 1./9.]]

# Gaussian Blur
#matrix = [[ 1./16.,  2./16.,  1./16.],[2./16.,  4./16.,  2./16.],[1./16.,  2./16.,  1./16. ]]

# Perform spatial filtering on one pixel location
def spatialFilter(c, r, matrix, img):
    rtotal = 0.0
    gtotal = 0.0
    btotal = 0.0

    # Loop through filter matrix
    for i in range(3):
        for j in range(3):
            # Calculate the pixel in the filter
            cc = c + j - 1
            rr = r + i - 1

            # Apply the filter
            pix = img.getPixel(cc, rr)
            mul = matrix[i][j]
            rtotal += red(pix) * mul
            gtotal += green(pix) * mul
            btotal += blue(pix) * mul

    # Make sure RGB is within range
    rtotal = constrain(rtotal,0,255)
    gtotal = constrain(gtotal,0,255)
    btotal = constrain(btotal,0,255)

    # return resulting color
    return color(rtotal, gtotal, btotal)


def main():
    # Load image and open a window
    #img = loadImage("bmc3.jpg")
    img = loadImage("moon.jpg")

    w = toInt( img.width() )
    h = toInt( img.height() )
    window( w, h )
    keepAbove(True)

    # Draw the image on the window
    img.loadPixels()
    image(img,0,0)

    # Filter rectangle
    loadPixels()

    for r in range( 1, h-1):
        #for c in range( int(0.5*w), w-1):
        for c in range( 1, w-1):
            clr = spatialFilter(c, r, matrix, img)
            setPixel(c, r, clr)

    updatePixels()

# Invoke the main procedure
main()

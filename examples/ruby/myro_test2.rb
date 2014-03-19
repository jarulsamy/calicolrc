class Main < Myro
    init
    pic = Myro::takePicture
    show pic
    backward 1, 1
    forward 1, 1
    beep 1, 440
    getPixels(pic).each do |pixel|
        r, g, b = getRGB pixel
        gray = (r + g + b)/3
        setRGB(pixel, gray, gray, gray)
    end
end
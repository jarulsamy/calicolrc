require 'Myro'
Myro::init
pic = Myro::takePicture
Myro::show pic
Myro::backward 1, 1
Myro::forward 1, 1
Myro::beep 1, 440
Myro::getPixels(pic).each do |pixel|
    r, g, b = Myro::getRGB pixel
    gray = (r + g + b)/3
    Myro::setRGB(pixel, gray, gray, gray)
end

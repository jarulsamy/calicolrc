from Graphics import *

win = Window()

car = Frame(150, 150)
wheel1 = Rectangle((-10, -10), (10, -5))
wheel2 = Rectangle((-10, 10), (10, 5))
wheel1.draw(car)
wheel2.draw(car)
car.draw(win)

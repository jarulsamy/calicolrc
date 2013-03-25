import Myro
Myro.init()
pic = Myro.takePicture()

#from Graphics import *
#win = Window()
#pic.draw(win)
Myro.show(pic, init=True)

print("all", Myro.get("all"))
print("light", Myro.getLight())

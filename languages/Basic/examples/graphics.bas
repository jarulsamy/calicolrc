10 IMPORT "Myro"
11 x = 300
12 y = 100
20 win = Myro.Window("My Window", x, y)
30 IMPORT "Graphics"
40 d = Graphics.Dot(RND(0) * x, RND(0) * y)
50 d.draw(win)
60 GOTO 40
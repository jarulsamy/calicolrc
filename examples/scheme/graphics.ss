(define test
   (lambda ()
     (using "Graphics")
     (define win (Graphics.Window "Hello"))
     (define line (Graphics.Line (Graphics.Point 0 0)
                                 (Graphics.Point 100 100)))
     (line.draw win)
     (line.rotate 10)))

(import "Myro")
(Myro.init "sim")

(define do
    (lambda (command)
        (cond
            ((eq? command 'left) (Myro.turnLeft 1 1))
            ((eq? command 'forward)(Myro.forward 1 1))
            ((eq? command 'show)(Myro.show (Myro.takePicture)))
            )))

(for-each (lambda (n)
            (for-each (lambda (move) (do move)) '(left forward show)))
          (range 4))
(using "Myro")
(Myro.init "sim")

(define do
    (lambda (command)
        (cond
            ((eq? command 'left) (Myro.turnLeft 1 1))
            ((eq? command 'forward)(Myro.forward 1 1))
            )))

(for-each (lambda (n)
            (for-each (lambda (move) (do move)) '(left forward)))
          (range 4))
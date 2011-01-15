(define fact1
    "Factorial for Scheme"
    (lambda (n)
        (if (eq? n 1)
            1
            (* n (fact1 (- n 1))))))

(define! fact2
    "Factorial seen by the DLR languages; can't run"
    (lambda (n)
        (if (eq? n 1)
            1
            (* n (fact2 (- n 1))))))

(define! fact3
  "Factorial available and runnable by DLR languages"
  (func
    (lambda (n)
        (if (eq? n 1)
            1
            (* n (fact3 (- n 1)))))))


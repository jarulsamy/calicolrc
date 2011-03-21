;; Scheme Function Examples

;; Create a function that will add 1 to a given number:

(define add1
    (lambda (x)
        (+ x 1)))

;; Create a function that will add two numbers together:

(define addem
    (lambda (x y)
        (+ x y)))

;; Call both of the functions, and display the results:

(print (add1 4))
(print (addem 42 8))


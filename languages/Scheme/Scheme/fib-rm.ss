(load "transformer-macros.ss")

;;----------------------------------------------------------------------

;; global registers
(define pc 'undefined)
(define fields_reg 'undefined)
(define final_reg 'undefined)
(define k_reg 'undefined)
(define n_reg 'undefined)
(define value_reg 'undefined)

(define make-cont
  (lambda args (return* (cons 'continuation args))))

(define*
  apply-cont
  (lambda () (return* (apply (cadr k_reg) (cddr k_reg)))))

(define <cont-1>
  (lambda (v1 k)
    (set! value_reg (+ v1 value_reg))
    (set! k_reg k)
    (set! pc apply-cont)))

(define <cont-2>
  (lambda (n k)
    (set! k_reg (make-cont <cont-1> value_reg k))
    (set! n_reg (- n 2))
    (set! pc fib-cps)))

(define <cont-3>
  (lambda () (set! final_reg value_reg) (set! pc #f)))

(define fib
  (lambda (n)
    (set! k_reg REP-k)
    (set! n_reg n)
    (set! pc fib-cps)))

(define*
  fib-cps
  (lambda ()
    (if (= n_reg 1)
        (begin (set! value_reg 1) (set! pc apply-cont))
        (if (= n_reg 2)
            (begin (set! value_reg 1) (set! pc apply-cont))
            (begin
              (set! k_reg (make-cont <cont-2> n_reg k_reg))
              (set! n_reg (- n_reg 1))
              (set! pc fib-cps))))))

(define REP-k (make-cont <cont-3>))

;; the trampoline
(define trampoline
  (lambda ()
    (if pc
        (begin (pc) (return* (trampoline)))
        (return* final_reg))))

(define run
  (lambda (setup . args)
    (apply setup args)
    (return* (trampoline))))


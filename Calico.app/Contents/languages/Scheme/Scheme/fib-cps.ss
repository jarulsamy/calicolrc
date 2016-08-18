(load "transformer-macros.ss")

(define* fib-cps
  (lambda (n k)
    (cond
      ((= n 1) (k 1))
      ((= n 2) (k 1))
      (else (fib-cps (- n 1)
	      (lambda-cont (v1)
		(fib-cps (- n 2)
		  (lambda-cont (v2)
		    (k (+ v1 v2))))))))))

(define fib
  (lambda (n)
    (fib-cps n REP-k)))

(define REP-k
  (lambda-cont (v)
    (halt* v)))

;; (run fib 5)

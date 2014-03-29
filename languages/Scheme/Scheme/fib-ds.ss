(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; continuation datatype

(define make-cont (lambda args (cons 'continuation args)))

(define*
  apply-cont
  (lambda (k value) (apply+ (cadr k) value (cddr k))))

(define+
  <cont-1>
  (lambda (value fields)
    (let ((v1 (car fields)) (k (cadr fields)))
      (apply-cont k (+ v1 value)))))

(define+
  <cont-2>
  (lambda (value fields)
    (let ((n (car fields)) (k (cadr fields)))
      (fib-cps (- n 2) (make-cont <cont-1> value k)))))

(define+
  <cont-3>
  (lambda (value fields) (let () (halt* value))))

;;----------------------------------------------------------------------
;; main program

(define*
  fib-cps
  (lambda (n k)
    (cond
      ((= n 1) (apply-cont k 1))
      ((= n 2) (apply-cont k 1))
      (else (fib-cps (- n 1) (make-cont <cont-2> n k))))))

(define fib (lambda (n) (fib-cps n REP-k)))

(define REP-k (make-cont <cont-3>))


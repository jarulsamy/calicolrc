(define odd? 'undefined)
(define even? 'undefined)

(letrec
    ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
     (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
  (set! odd? odd)
  (set! even? even))

;;---------------------------------------------------------------------
;; collect is like list comprehension in Python

(define-syntax collect
  [(collect ?exp for ?var in ?list)
   (filter-map (lambda (?var) ?exp) (lambda (?var) #t) ?list)]
  [(collect ?exp for ?var in ?list if ?condition)
   (filter-map (lambda (?var) ?exp) (lambda (?var) ?condition) ?list)])

(define filter-map
  (lambda (f pred? values)
    (if (null? values)
      '()
      (if (pred? (car values))
	  (cons (f (car values)) (filter-map f pred? (cdr values)))
	  (filter-map f pred? (cdr values))))))

(define-syntax time 
  [(time ?exp) (let ((start (current-time)))
		 ?exp
		 (- (current-time) start))])

;;---------------------------------------------------------------------
;; for loops

(define-syntax for
  [(for ?exp times do . ?bodies)
   (for-repeat ?exp (lambda () . ?bodies))]
  [(for ?var in ?exp do . ?bodies)
   (for-iterate1 ?exp (lambda (?var) . ?bodies))]
  [(for ?var at (?i) in ?exp do . ?bodies)
   (for-iterate2 0 ?exp (lambda (?var ?i) . ?bodies))]
  [(for ?var at (?i ?j . ?rest) in ?exp do . ?bodies)
   (for ?var at (?i) in ?exp do
     (for ?var at (?j . ?rest) in ?var do . ?bodies))])

(define for-repeat
  (lambda (n f)
    (if (< n 1)
      'done
      (begin
	(f)
	(for-repeat (- n 1) f)))))

(define for-iterate1
  (lambda (values f)
    (if (null? values)
      'done
      (begin
	(f (car values))
	(for-iterate1 (cdr values) f)))))

(define for-iterate2
  (lambda (i values f)
    (if (null? values)
      'done
      (begin
	(f (car values) i)
	(for-iterate2 (+ i 1) (cdr values) f)))))

(define matrix2d
  '((10 20)
    (30 40)
    (50 60)
    (70 80)))

(define matrix3d
  '(((10 20 30) (40 50 60))
    ((70 80 90) (100 110 120))
    ((130 140 150) (160 170 180))
    ((190 200 210) (220 230 240))))

;;---------------------------------------------------------------------
;; streams

(define-syntax scons
  [(scons ?x ?y) (cons ?x (lambda () ?y))])

(define scar car)

(define scdr
  (lambda (s)
    (let ((result ((cdr s))))
      (set-cdr! s (lambda () result))
      result)))

(define first
  (lambda (n s)
    (if (= n 0)
      '()
      (cons (scar s) (first (- n 1) (scdr s))))))

(define nth
  (lambda (n s)
    (if (= n 0)
      (scar s)
      (nth (- n 1) (scdr s)))))

(define smap
  (lambda (f s)
    (scons (f (scar s)) (smap f (scdr s)))))

(define ones (scons 1 ones))

(define nats (scons 0 (combine nats + ones)))

(define combine
  (lambda (s1 op s2)
    (scons (op (scar s1) (scar s2)) (combine (scdr s1) op (scdr s2)))))

(define fibs (scons 1 (scons 1 (combine fibs + (scdr fibs)))))

(define facts (scons 1 (combine facts * (scdr nats))))

(define ! (lambda (n) (nth n facts)))

;;---------------------------------------------------------------------
;; example interaction:

;; ==> (load "examples.ss")
;; ok
;; ==> (odd? 43)
;; #t

;; other examples:

;; ==> (list (odd? 42) (even? 42) (odd? 43) (even? 43))

;; ==> (collect (* n n) for n in (range 10))
;; ==> (collect (* n n) for n in (range 5 20 3))
;; ==> (collect (* n n) for n in (range 10) if (> n 5))

;; ==> (for 5 times do (print 'hello))
;; ==> (for sym in '(a b c d) do (print sym) (newline))
;; ==> (for n in (range 10 20 2) do
;;            (print n))

;; ==> (for n at (i j) in matrix2d do
;;            (print (list n 'coords: i j)))
;; ==> (for n at (i j k) in matrix3d do
;;            (print (list n 'coords: i j k)))

;; ==> (! 5)"
;; ==> (nth 10 facts)
;; ==> (nth 20 fibs)
;; ==> (first 30 fibs)

(define test-all
  (lambda ()
    (print (list (odd? 42) (even? 42) (odd? 43) (even? 43)))
    (print (collect (* n n) for n in (range 10)))
    (print (collect (* n n) for n in (range 5 20 3)))
    (print (collect (* n n) for n in (range 10) if (> n 5)))
    (print (for 5 times do (print 'hello)))
    (print (for sym in '(a b c d) do (print sym) (newline)))
    (print (for n in (range 10 20 2) do (print n)))
    (print (for n at (i j) in matrix2d do (print (list n 'coords: i j))))
    (print (for n at (i j k) in matrix3d do (print (list n 'coords: i j k))))
    (print (! 5))
    (print (nth 10 facts))
    (print (nth 20 fibs))
    (print (first 30 fibs))
    (test-mu-lambda)
    (test-define)
    (test-call/cc)
    (test-loop)
    (test-macros)
    ))

(define verify
  (lambda (answer exp)
    (print "Testing ")
    (print exp)
    (if (equal? exp answer)
      (print 'passed)
      (begin
	(display "failed: ")
	(display answer)
	(newline)))))

(define test-mu-lambda
  (lambda ()
    (print "testing mu lambda")
    (verify '(1 2 3 4 5)
      ((lambda x x) 1 2 3 4 5))
    (verify '(1 (2 3 4 5))
      ((lambda (x . y) (list x y)) 1 2 3 4 5))
    (verify '(1 2 (3 4 5))
      ((lambda (a b . z) (list a b z)) 1 2 3 4 5))
    (verify '(1 2 (3))
      ((lambda (a b . z) (list a b z)) 1 2 3))
    (verify '(1 2 ())
      ((lambda (a b . z) (list a b z)) 1 2))
    (verify "not enough arguments given"
      (try ((lambda (a b . z) (list a b z)) 1)
	       (catch e e "not enough arguments given")))
    ))

(define test-define
  (lambda ()
    (print "testing define")
    (define f1 (lambda (a b c) (list a b c)))
    (define (f2) (list 42))
    (define (f3 . x) (list x))
    (define (f4 a b c . x) (list a b c x))
    (define (f5 a b c x) (list a b c x))
    (verify '((1 2 3) (42) ((1 2 3)) (1 2 3 (4 5)) (1 2 3 4))
      (list (f1 1 2 3) (f2) (f3 1 2 3) (f4 1 2 3 4 5) (f5 1 2 3 4)))))

(define test-call/cc
  (lambda ()
    (print "testing call/cc")
    (verify 40
      (* 10 (call/cc (lambda (k) 4))))
    (verify 40
      (* 10 (call/cc (lambda (k) (+ 1 (k 4))))))
    (verify 50
      (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (j (k 5))))))))))
    (verify 60
      (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (k (j 5))))))))))))

(define test-try
  (lambda ()
    (print "testing try")
    (verify 3
      (try 3))
    (verify 3
      (try 3 (finally (print 'yes) 4)))
    (verify 'yes
      (try (raise 'yes) (catch e e)))
    (verify 'yes
      (try (try (raise 'yes)) (catch e e)))
    (verify 'oops
      (try (try (begin (print 'one) (raise 'oops) (print 'two))) (catch e e)))
    (verify 40
      (* 10 (try (begin (print 'one) (raise 'oops) (print 'two))
            (catch ex (print 3) 4))))
    (verify 50
      (* 10 (try (begin (print 'one) (print 'two) 5)
            (catch ex (print 3) 4))))
    (verify 40
      (* 10 (try (begin (print 'one) (raise 'oops) 5)
            (catch ex (print (list 'ex: ex)) 4))))
    (verify 'oops
      (try (* 10 (try (begin (print 'one) (raise 'oops) 5)
            (catch ex (print (list 'ex: ex)) (raise ex) 4))) (catch e e)))
    (verify 'oops
      (try (* 10 (try (begin (print 'one) (raise 'oops) 5)
              (catch ex (print (list 'ex: ex)) (raise ex) 4)
              (finally (print 'two) 7))) (catch e e)))
    (verify 77
      (try (* 10 (try (begin (print 'one) (raise 'oops) 5)
		      (catch ex (print (list 'ex: ex)) (raise 'bar) 4)))
	   (catch x (print 'hello) 77)))
    (verify 3
      (try 3 (finally (print 'hi) 4)))
    (verify 'ok
      (define div (lambda (x y) (if (= y 0) (raise "division by zero") (/ x y)))))
    (verify 5
      (div 10 2))
    (verify "division by zero"
      (try (div 10 0) (catch e e)))
    (verify "division by zero"
      (try (let ((x (try (div 10 0)))) x) (catch e e)))
    (verify 5
      (let ((x (try (div 10 2) (catch e -1)))) x))
    (verify -1
      (let ((x (try (div 10 0) (catch e -1)))) x))
    (verify 5
      (let ((x (try (div 10 2) (catch e -1) (finally (print 'closing-files) 42))))  x))
    (verify -1
      (let ((x (try (div 10 0) (catch e -1) (finally (print 'closing-files) 42))))  x))
    (verify 5
      (let ((x (try (div 10 2) (finally (print 'closing-files) 42))))  x))
    (verify 'foo
      (try (let ((x (try (div 10 0) (catch e -1 (raise 'foo)) (finally (print 'closing-files) 42))))  x) (catch e e)))
    (verify 'ack
      (try (let ((x (try (div 10 0)
                (catch e -1 (raise 'foo))
                (finally (print 'closing-files) (raise 'ack) 42))))
       x) (catch e e)))
    (verify 99
      (try (let ((x (try (div 10 0)
                     (catch e -1 (raise 'foo))
                     (finally (print 'closing-files) (raise 'ack) 42))))
            x)
       (catch e (if (equal? e 'ack) 99 (raise 'doug)))
       (finally (print 'closing-outer-files))))
    (verify 'doug
      (try (try (let ((x (try (div 10 0)
                     (catch e -1 (raise 'foo))
                     (finally (print 'closing-files) (raise 'ack) 42))))
            x)
       (catch e (if (equal? e 'foo) 99 (raise 'doug)))
       (finally (print 'closing-outer-files))) (catch e e)))
    ))

(define test-loop
  (lambda ()
    (print "testing loop")
    (try (let loop ((n 5))
            (print n)
            (if (= n 0)
                (raise 'blastoff!))
            (loop (- n 1)))
       (catch e e))))

(define (test-macros)
  (print "testing macros")
  (verify #t
    (let ((bool 5))
      (or (= bool 4) (= bool 5))))
  (verify 6
    (let ((bool 5))
      (or (= bool 4) 6)))
  (verify #f
    (let ((bool 5))
      (and (= bool 5) (> bool 0) (= bool 4))))
  (verify 5
    (let ((r 5))
      (case 'banana
	(apple 'no)
	((cherry banana) 1 2 r)
	(else 'no))))
  (verify '((6) orange 5)
    (let ((r 5))
      (record-case (cons 'banana (cons 'orange (cons (* 2 3) '())))
	(apple (a b c) (list c b a r))
	((cherry banana) (a . b) (list b a r))
	((orange) () 'no)
	(else 2 3 4)))))

(test-all)

;; Calico Scheme interpreter
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")

;;----------------------------------------------------------------------------
;; Interpreter with support for choose

(load "environments-cps.ss")
(load "parser-cps.ss")

(define *need-newline* #f)

(define pretty-print-prim
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print (if (procedure-object? arg) '<procedure> arg))))

(define procedure-object?
  (lambda (x)
    (or (procedure? x) (and (pair? x) (eq? (car x) 'procedure)))))

(define newline-prim
  (lambda ()
    (set! *need-newline* #f)
    (newline)))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

(define display-prim
  (lambda (x)
    (let ((s (format "~a" x)))  ;; must use ~a, not ~s, to handle embedded newlines properly
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s))))

(define scheme-REP-k
  (lambda-cont2 (v fail)
    (if (not (eq? v '<void>))
	(pretty-print-prim v))
    (if *need-newline* (newline))
    (read-eval-print fail)))

(define scheme-REP-handler
  (lambda-handler2 (e fail)
    (REP-k `(uncaught exception: ,e) fail)))

(define scheme-REP-fail
  (lambda-fail ()
    (REP-k "no more choices" REP-fail)))

(define start
  (lambda ()
    ;; start with fresh environments
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (read-eval-print REP-fail)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print REP-fail)))

(define read-line
  (lambda (prompt)
    (printf prompt)
    (let ((input (read)))
      (format "~s" input))))

;; because read-line uses (read), it can only read a single sexp at a
;; time. it always returns a string version of its input. if the input
;; is the list (+ 2 3), the string "(+ 2 3)" is returned; if the input
;; is the string "apple", the string "\"apple\"" is returned; etc.
;;
;; raw-read-line is only for testing the evaluation of multiple sexps
;; at once.  the user must type the input as a string enclosed by
;; double quotes.

(define raw-read-line
  (lambda (prompt)
    (printf prompt)
    (let loop ((input (read)))
      (if (string? input)
	input
	(begin
	  (printf "Error: input must be enclosed in quotation marks.\n==> ")
	  (loop (read)))))))

(define* read-eval-print
  (lambda (fail)
    (set! load-stack '())  ;; in case a previous load encountered an error
    (let ((input (read-line "==> ")))  ;; or raw-read-line
      (scan-input input REP-handler fail
	(lambda-cont2 (tokens fail)
	  (read-and-eval-sexps tokens toplevel-env REP-handler fail REP-k))))))

(define* read-and-eval-sexps
  (lambda (tokens env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '<void> fail)
      (read-sexp tokens handler fail
	(lambda-cont3 (datum tokens-left fail)
	  (parse datum handler fail
	    (lambda-cont2 (exp fail)
	      (m exp env handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-sexps tokens-left env handler fail k)))))))))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
    (cases expression exp
      (lit-exp (datum) (k datum fail))
      (var-exp (id) (lookup-value id env handler fail k))
      (func-exp (exp) (m exp env handler fail
                        (lambda-cont2 (f fail)
                          (k (dlr-func f) fail))))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding var env handler fail
	      (lambda-cont2 (binding fail)
		(let ((old-value (binding-value binding)))
		  (set-binding-value! binding rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-binding-value! binding old-value) (fail))))
		    (k '<void> new-fail))))))))
      (define-exp (var docstring rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding-in-first-frame var env handler fail
	      (lambda-cont2 (binding fail)
		(set-binding-value! binding rhs-value)
		(set-binding-docstring! binding docstring)
		;; definitions should occur only at top level, so no need to undo
		(k '<void> fail))))))
      (define!-exp (var docstring rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (set-global-value! var rhs-value)
	    (set-global-docstring! var docstring)
	    (k '<void> fail))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler fail
	  (lambda-cont2 (binding fail)
	    (set-binding-value! binding (make-pattern-macro clauses))
	    (k '<void> fail))))
      (begin-exp (exps) (eval-sequence exps env handler fail k))
      (lambda-exp (formals body)
	(k (closure formals body env) fail))
      (mu-lambda-exp (formals runt body)
	(k (mu-closure formals runt body env) fail))
      (try-catch-exp (body cvar cexps)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler fail k)))
      (try-finally-exp (body fexps)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail)))))))
      (try-catch-finally-exp (body cvar cexps fexps)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler fail
	     (lambda-cont2 (v fail)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler fail
		 (lambda-cont2 (v2 fail) (k v fail)))))))
      (raise-exp (exp)
	(m exp env handler fail
	  ;; TODO: pass in more info to handler (k, env) to support resume, etc.
	  (lambda-cont2 (e fail) (handler e fail))))
      (dict-exp (pairs)
	(k (list 'dict pairs) fail))
      (help-exp (var)
	(if (reserved-keyword? var)
	  (k (format "~a is a keyword" var) fail)
	  (lookup-binding var env handler fail
	    (lambda-cont2 (binding fail)
	      (k (binding-docstring binding) fail)))))
      (choose-exp (exps)
	(eval-choices exps env handler fail k))
      (app-exp (operator operands)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(if (dlr-exp? proc)
		  (k (dlr-apply proc args) fail)
		  (proc args env handler fail k)))))))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (lambda-handler2 (e fail)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	;;(printf "executing catch block~%")
	(eval-sequence cexps new-env handler fail k)))))

(define try-finally-handler
  (lambda (fexps env handler)
    (lambda-handler2 (e fail)
      ;;(printf "executing finally block~%")
      (eval-sequence fexps env handler fail
	(lambda-cont2 (v fail)
	  ;;(printf "propagating ~a exception~%" e)
	  (handler e fail))))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (lambda-handler2 (e fail)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e))))
	(let ((catch-handler (try-finally-handler fexps env handler)))
	  ;;(printf "executing catch block~%")
	  (eval-sequence cexps new-env catch-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail))))))))))

(define* eval-choices
  (lambda (exps env handler fail k)
    (if (null? exps)
      ;; no more choices, so backtrack to previous choice point
      (fail)
      (let ((new-fail (lambda-fail () (eval-choices (cdr exps) env handler fail k))))
	;; if new-fail is invoked, it will try the next choice
	(m (car exps) env handler new-fail k)))))

(define closure
  (lambda (formals body env)
    (lambda-proc (args env2 handler fail k2)
      (if (= (length args) (length formals))
	(m body (extend env formals args) handler fail k2)
	(handler "incorrect number of arguments" fail)))))

(define mu-closure
  (lambda (formals runt body env)
    (lambda-proc (args env2 handler fail k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (m body new-env handler fail k2))
	(handler "not enough arguments given" fail)))))

(define* m*
  (lambda (exps env handler fail k)
    (if (null? exps)
      (k '() fail)
      (m (car exps) env handler fail
	(lambda-cont2 (v1 fail)
	  (m* (cdr exps) env handler fail
	    (lambda-cont2 (v2 fail)
	      (k (cons v1 v2) fail))))))))

(define* eval-sequence
  (lambda (exps env handler fail k)
    (m (car exps) env handler fail
       (lambda-cont2 (result fail)
	 (if (null? (cdr exps))
	   (k result fail)
	   (eval-sequence (cdr exps) env handler fail k))))))

(define make-initial-env-extended
  (lambda (env)
    ;; this is here as a hook for extending environments in C# etc.
    env))

(define length-prim
  (lambda-proc (args env2 handler fail k2)
    (if (= (length args) 1)
      (length-loop (car args) 0 (car args) handler fail k2)
      (handler "incorrect number of arguments to procedure length" fail))))

(define* length-loop
  (lambda (x sum ls handler fail k2)
    (cond
      ((null? x) (k2 sum fail))
      ((not (pair? x)) (handler (format "~a is not a proper list" ls) fail))
      (else (length-loop (cdr x) (+ sum 1) ls handler fail k2)))))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
     (make-initial-environment
      (list 'exit 'eval 'parse 'parse-string 'apply 'sqrt 'print 'display 'newline 'load 'length
	    'null? 'cons 'car 'cdr 'cadr 'caddr 'list '+ '- '* '/ '< '> '= 'abs 'equal? 'eq? 'memq 'member 'range
	    'set-car! 'set-cdr! 'import 'get 'call-with-current-continuation 'call/cc 'abort 'require 'cut
	    'reverse 'append 'list->vector 'dir 'current-time 'map 'for-each 'env
	    'using 'not 'printf 'vector 'vector-set! 'vector-ref 'make-vector)
      (list
	;; exit
        (lambda-proc (args env2 handler fail k2)
	  (halt* '(exiting the interpreter)))
	;; eval
	(lambda-proc (args env2 handler fail k2)
	  (parse (car args) handler fail
	    (lambda-cont2 (exp fail)
	      (m exp toplevel-env handler fail k2))))   ;; use toplevel-env here?
	;; parse
	(lambda-proc (args env2 handler fail k2)
	  (parse (car args) handler fail k2))
	;; parse-string
	(lambda-proc (args env2 handler fail k2)
	  (scan-input (car args) handler fail
	    (lambda-cont2 (tokens fail)
	      (read-sexp tokens handler fail
		(lambda-cont3 (datum tokens-left fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (parse datum handler fail k2)
		    (handler (format "tokens left over at line ~a, char ~a" 
				     (get-line-count (first tokens-left))
				     (get-char-count (first tokens-left)))
			     fail)))))))
	;; apply
	(lambda-proc (args env2 handler fail k2)
	  (let ((proc (car args))
		(proc-args (cadr args)))
	    (proc proc-args env2 handler fail k2)))
;; FIX: need to check each fixed-arity primitive for correct number of args before calling
	;; sqrt
	(lambda-proc (args env2 handler fail k2) (k2 (apply sqrt args) fail))
	;; print
	(lambda-proc (args env2 handler fail k2) (for-each pretty-print-prim args) (k2 '<void> fail))
	;; display
	(lambda-proc (args env2 handler fail k2) (apply display-prim args) (k2 '<void> fail))
	;; newline
	(lambda-proc (args env2 handler fail k2) (newline-prim) (k2 '<void> fail))
	;; load
	(lambda-proc (args env2 handler fail k2)
	   (load-file (car args) toplevel-env handler fail k2))
	;; length
	length-prim
	;; null?
	(lambda-proc (args env2 handler fail k2) (k2 (apply null? args) fail))
	;; cons
	(lambda-proc (args env2 handler fail k2) (k2 (apply cons args) fail))
	;; car
	(lambda-proc (args env2 handler fail k2) (k2 (apply car args) fail))
	;; cdr
	(lambda-proc (args env2 handler fail k2) (k2 (apply cdr args) fail))
	;; cadr
	(lambda-proc (args env2 handler fail k2) (k2 (apply cadr args) fail))
	;; caddr
	(lambda-proc (args env2 handler fail k2) (k2 (apply caddr args) fail))
	;; list
	(lambda-proc (args env2 handler fail k2) (k2 args fail))
	;; +
	(lambda-proc (args env2 handler fail k2) (k2 (apply + args) fail))
	;; - 
	(lambda-proc (args env2 handler fail k2) (k2 (apply - args) fail))
	;; *
	(lambda-proc (args env2 handler fail k2) (k2 (apply * args) fail))
	;; /
	(lambda-proc (args env2 handler fail k2)
          (cond
            ((= (length args) 1)
             (if (= (car args) 0)
                 (handler "division by zero" fail)
                 (k2 (apply / args) fail)))
            ((>= (length args) 2)
             (if (= (cadr args) 0)
                 (handler "division by zero" fail)
                 (k2 (apply / args) fail)))
            (else (handler "not enough args to /" fail))))
	;; <
	(lambda-proc (args env2 handler fail k2) (k2 (apply < args) fail))
	;; >
	(lambda-proc (args env2 handler fail k2) (k2 (apply > args) fail))
	;; =
	(lambda-proc (args env2 handler fail k2) (k2 (apply = args) fail))
	;; abs
	(lambda-proc (args env2 handler fail k2) (k2 (apply abs args) fail))
	;; equal?
	(lambda-proc (args env2 handler fail k2)
	  (if (= (length args) 2)
	    (equal-objects? (car args) (cadr args)
	      (lambda-cont (bool) (k2 bool fail)))
	    (handler "incorrect number of arguments to procedure equal?" fail)))
	;; eq?
	(lambda-proc (args env2 handler fail k2) (k2 (apply eq? args) fail))
	;; memq
	(lambda-proc (args env2 handler fail k2) (k2 (apply memq args) fail))
	;; member
	(lambda-proc (args env2 handler fail k2)
	  (if (= (length args) 2)
	    (member-prim (car args) (cadr args) (cadr args) handler fail k2)
	    (handler "incorrect number of arguments to procedure member" fail)))
	;; range
	(lambda-proc (args env2 handler fail k2) (k2 (apply range args) fail))
	;; set-car!
	(lambda-proc (args env2 handler fail k2) (k2 (apply set-car! args) fail))
	;; set-cdr
	(lambda-proc (args env2 handler fail k2) (k2 (apply set-cdr! args) fail))
	;; import
	(lambda-proc (args env2 handler fail k2) (import-primitive args env2 handler fail k2))
	;; get
	(lambda-proc (args env2 handler fail k2) (get-primitive args env2 handler fail k2))
	;; call/cc
	(lambda-proc (args env2 handler fail k2) (call/cc-primitive (car args) env2 handler fail k2))
	;; call/cc
	(lambda-proc (args env2 handler fail k2) (call/cc-primitive (car args) env2 handler fail k2))
	;; abort
	(lambda-proc (args env2 handler fail k2)
	  (if (null? args)
	    (REP-k '<void> fail)
	    (REP-k (car args) fail)))
	;; require
	(lambda-proc (args env2 handler fail k2)
	  (if (true? (car args))
	    (k2 'ok fail)
	    (fail)))
	;; cut
	(lambda-proc (args env2 handler fail k2) (k2 'ok REP-fail))
	;; reverse
	(lambda-proc (args env2 handler fail k2) (k2 (apply reverse args) fail))
	;; append
	(lambda-proc (args env2 handler fail k2) (k2 (apply append args) fail))
	;; list->vector
	(lambda-proc (args env2 handler fail k2) (k2 (apply make-vector args) fail))
	;; dir
	(lambda-proc (args env2 handler fail k2) (k2 (dir args env2) fail))
	;; current-time
	(lambda-proc (args env2 handler fail k2) (k2 (get-current-time) fail))
	;; map
	(lambda-proc (args env2 handler fail k2)
	  (map-prim (car args) (cdr args) env2 handler fail k2))
	;; for-each
	(lambda-proc (args env2 handler fail k2)
	  (for-each-prim (car args) (cdr args) env2 handler fail k2))
	;; env
	(lambda-proc (args env2 handler fail k2) (k2 env2 fail))
	;; using (not defined in scheme-scheme)
	(lambda-proc (args env2 handler fail k2) (k2 (using-prim args env2) fail))
	;; not
	(lambda-proc (args env2 handler fail k2) (k2 (not (car args)) fail))
	;; printf
	(lambda-proc (args env2 handler fail k2) (apply printf-prim args) (k2 '<void> fail))
        ;; vector
	(lambda-proc (args env2 handler fail k2) (k2 (make-vector args) fail))
        ;; vector-set!
	(lambda-proc (args env2 handler fail k2) (k2 (vector-set! (car args) (cadr args) (caddr args)) fail))
        ;; vector-ref
	(lambda-proc (args env2 handler fail k2) (k2 (apply vector-ref args) fail))
        ;; make-vector
	(lambda-proc (args env2 handler fail k2) (k2 (make-vector-size (car args)) fail))
	)))))

(define toplevel-env (make-toplevel-env))

(define* equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
	   (and (boolean? x) (boolean? y) (eq? x y))
	   (and (symbol? x) (symbol? y) (eq? x y))
	   (and (number? x) (number? y) (= x y))
	   (and (char? x) (char? y) (char=? x y))
	   (and (string? x) (string? y) (string=? x y)))
       (k #t))
      ((and (pair? x) (pair? y))
       (equal-objects? (car x) (car y)
	 (lambda-cont (bool)
	   (if bool
	     (equal-objects? (cdr x) (cdr y) k)
	     (k #f)))))
      ((and (vector? x) (vector? y) (= (vector-length x) (vector-length y)))
       (equal-vectors? x y (- (vector-length x) 1) k))
      (else (k #f)))))

(define* equal-vectors?
  (lambda (v1 v2 i k)
    (if (< i 0)
      (k #t)
      (equal-objects? (vector-ref v1 i) (vector-ref v2 i)
	(lambda-cont (bool)
	  (if bool
	    (equal-vectors? v1 v2 (- i 1) k)
	    (k #f)))))))

(define* member-prim
  (lambda (x ls orig-ls handler fail k)
    (cond
      ((null? ls) (k #f fail))
      ((not (pair? ls)) (handler (format "improper list ~a" orig-ls) fail))
      (else (equal-objects? x (car ls)
	      (lambda-cont (bool)
		(if bool
		  (k ls fail)
		  (member-prim x (cdr ls) orig-ls handler fail k))))))))

;; supports procedures of any number of arguments
(define* map-prim
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args))
              (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler fail k))
            (else (mapN proc list-args env handler fail k)))))))

(define* iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define* iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env handler fail
            (lambda-cont2 (v fail)
              (iterate-continue proc iterator env handler fail k)))))))

(define* iterate-collect
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-collect-continue proc iterator env handler fail k))))

(define* iterate-collect-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env handler fail
            (lambda-cont2 (v1 fail)
              (iterate-collect-continue proc iterator env handler fail
                (lambda-cont2 (v2 fail)
                  (k (cons v1 v2) fail)))))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list))
      (cons (car arg-list) (listify (cdr arg-list))))
     ((vector? (car arg-list))
      (cons (my-vector->list (car arg-list)) (listify (cdr arg-list))))
     ((string? (car arg-list))
      (cons (string->list (car arg-list)) (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map" 
		  (get_type (car arg-list))))))) ;; get_type is defined in C#

;; for improved efficiency
(define* map1
  (lambda (proc list1 env handler fail k)
    (if (null? list1)
      (k '() fail)
      (if (dlr-exp? proc)
	(map1 proc (cdr list1) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (list (car list1))) v2)
	       fail)))
	(proc (list (car list1)) env handler fail
	  (lambda-cont2 (v1 fail)
	    (map1 proc (cdr list1) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

;; for improved efficiency
(define* map2
  (lambda (proc list1 list2 env handler fail k)
    (if (null? list1)
      (k '() fail)
      (if (dlr-exp? proc)
	(map2 proc (cdr list1) (cdr list2) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (list (car list1) (car list2))) v2)
	       fail)))
	(proc (list (car list1) (car list2)) env handler fail
	  (lambda-cont2 (v1 fail)
	    (map2 proc (cdr list1) (cdr list2) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

(define* mapN
  (lambda (proc lists env handler fail k)
    (if (null? (car lists))
      (k '() fail)
      (if (dlr-exp? proc)
	(mapN proc (map cdr lists) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (map car lists)) v2)
	      fail)))
	(proc (map car lists) env handler fail
	  (lambda-cont2 (v1 fail)
	    (mapN proc (map cdr lists) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

(define* for-each-prim
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
      (iterate proc (car lists) env handler fail k)
      (let ((arg-list (listify lists)))
	(if (null? (car arg-list))
	  (k '<void> fail)
	  (if (dlr-exp? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-prim proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-prim proc (map cdr arg-list) env handler fail k)))))))))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now)
		     1000000000))))))

(define* get-primitive
  (lambda (args env handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env handler fail
	(lambda-cont2 (v fail)
	  (cond
	    ((null? (cdr args)) (k v fail))
	    ((not (environment? v)) (handler (format "~a is not a module" sym) fail))
	    (else (get-primitive (cdr args) v handler fail k))))))))

;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define* import-primitive
  (lambda (args env handler fail k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler fail k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler fail
	      (lambda-cont2 (binding fail)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  (load-file filename module handler fail k)))))))))

(define* call/cc-primitive
  (lambda (proc env handler fail k)
    (let ((fake-k (lambda-proc (args env2 handler fail k2) (k (car args) fail))))
      (if (dlr-exp? proc)
	  (k (dlr-apply proc (list fake-k)) fail)
	  (proc (list fake-k) env handler fail k)))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

(define dir
  (lambda (args env)
    (sort symbol<? (if (null? args)
		       (flatten
			 (append (get-reserved-keywords)
				 (map get-variables-from-frame (frames macro-env))
				 (map get-variables-from-frame (frames env))))
		       (get-variables-from-frame (car (frames (car args))))))))

(define get-variables-from-frame
  (lambda (frame) 
    (map binding-variable frame)))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
	  (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define load-stack '())

(define* load-file
  (lambda (filename env handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k '<void> fail))
      ((not (string? filename))
       (handler (format "filename is not a string: ~a" filename) fail))
      ((not (file-exists? filename))
       (handler (format "file does not exist: ~a" filename) fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-sexps tokens env handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k '<void> fail)))))))))

(define* load-files
  (lambda (filenames env handler fail k)
    (if (null? filenames)
      (k '<void> fail)
      (load-file (car filenames) env handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env handler fail k))))))

;; help is now a special form
;;(define* help-prim
;;  (lambda (var env handler fail k)
;;    (lookup-binding var env handler fail
;;      (lambda-cont2 (binding fail)
;;	(k (binding-docstring binding) fail)))))

(define range
  (lambda args
    (letrec
	((range
	  (lambda (n end step acc)
	    (if (>= n end)
	      (reverse acc)
	      (range (+ n step) end step (cons n acc))))))
      (cond
	((null? (cdr args)) (range 0 (car args) 1 '()))
	((null? (cddr args)) (range (car args) (cadr args) 1 '()))
	(else (range (car args) (cadr args) (caddr args) '()))))))
	
(define make-vector list->vector) ;; ignored in C#

;; not used
(define Main
  (lambda filenames
    (printf "Calico Scheme (0.2)\n")
    (printf "(c) 2009-2011, IPRE\n")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (set! load-stack '())
    ;; in the register machine, this call just sets up the registers
    (load-files filenames toplevel-env REP-handler REP-fail REP-k)
    ;; starts the computation after registers are set up
    (trampoline)))

;;------------------------------------------------------------------------
;; C# support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 handler fail k2)
      (k2 (apply* external-function-object args) fail))))

(define REP-k
  (lambda-cont2 (result fail)
    (set! last-fail fail)
    (halt* result)))

(define REP-fail
  (lambda-fail ()
    (halt* "no more choices")))

(define REP-handler
  (lambda-handler2 (e fail)
    (set! last-fail fail)
    (halt* (list 'exception e))))

(define last-fail REP-fail)

;; not used yet
(define reinitialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (set! load-stack '())
    (set! last-fail REP-fail)))

(define execute
  (lambda (input)
    (set! load-stack '())
    (scan-input input REP-handler last-fail
      (lambda-cont2 (tokens fail)
	(read-and-eval-sexps tokens toplevel-env REP-handler fail REP-k)))
    (trampoline)))

(define execute-file
  (lambda (filename)
    (set! load-stack '())
    (load-file filename toplevel-env REP-handler last-fail REP-k)
    (trampoline)))

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input try-parse-handler REP-fail
      (lambda-cont2 (tokens fail)
	(parse-sexps tokens try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

;;------------------------------------------------------------------------
;; for manual testing only in scheme


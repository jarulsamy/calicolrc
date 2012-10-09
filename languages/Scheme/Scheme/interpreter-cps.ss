;; Calico Scheme interpreter with support for choose
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")
(load "environments-cps.ss")
(load "parser-cps.ss")

;;----------------------------------------------------------------------------
;; enables interpreter to be run directly in Petite Chez Scheme, independently of C#

;; dummy versions of functions used in C# code
(define dlr-exp? (lambda (x) #f))
(define dlr-apply apply)
(define dlr-func (lambda (x) x))
(define dlr-env-contains (lambda (x) #f))
(define dlr-env-lookup (lambda (x) #f))
(define dlr-object? (lambda (x) #f))
(define dlr-lookup-components (lambda (x y) #f))
(define set-global-value! (lambda (var x) #f))
(define set-global-docstring! (lambda (var x) #f))
(define printf-prim printf)
(define using-prim (lambda ignore #f))
(define iterator? (lambda ignore #f))
(define get_type (lambda (x) 'unknown))

;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------
;; read-eval-print loop

(define start
  (lambda ()
    ;; start with fresh environments
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (read-eval-print-loop)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop)))

(define* read-eval-print-loop
  (lambda ()
    (let ((input (raw-read-line "==> ")))  ;; read-line or raw-read-line
      ;; execute gets redefined as execute-rm in the scheme register machine
      (let ((result (execute input 'stdin)))
	(if (not (void? result))
	    (safe-print result))
	(if *need-newline*
	  (newline))
	(if (end-of-session? result)
	  (halt* 'goodbye)
	  (read-eval-print-loop))))))

(define REP-k
  (lambda-cont2 (v fail)
    (set! *last-fail* fail)
    (halt* v)))

(define REP-handler
  (lambda-handler2 (e fail)
    (set! *last-fail* fail)
    (halt* (list 'exception e))))

(define REP-fail
  (lambda-fail ()
    (halt* "no more choices")))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define exception?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'exception))))

(define execute-string
  (lambda (input)
    (execute input 'stdin)))

(define execute-file
  (lambda (filename)
    (execute (read-content filename) filename)))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (let ((result (scan-input input src REP-handler *last-fail* REP-k)))
      (if (exception? result)
	result
	(begin
	  (set! *tokens-left* result)
	  (if (token-type? (first *tokens-left*) 'end-marker)
	    void-value
	    (execute-loop src)))))))

(define execute-loop
  (lambda (src)
    (let ((result (execute-next-expression src)))
      (if (or (exception? result)
	      (end-of-session? result)
	      (token-type? (first *tokens-left*) 'end-marker))
	  result
	  (execute-loop src)))))

(define execute-next-expression
  (lambda (src)
    (read-asexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;; not used
(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (set! *last-fail* REP-fail)))

;;----------------------------------------------------------------------------
;; for register machine only

(define execute-string-rm
  (lambda (input)
    (execute-rm input 'stdin)))

(define execute-file-rm
  (lambda (filename)
    (execute-rm (read-content filename) filename)))

(define execute-rm
  (lambda (input src)
    (set! load-stack '())
    (scan-input input src REP-handler *last-fail* REP-k)
    (let ((result (trampoline)))
      (if (exception? result)
	result
	(begin
	  (set! *tokens-left* result)
	  (if (token-type? (first *tokens-left*) 'end-marker)
	    void-value
	    (execute-loop-rm src)))))))

(define execute-loop-rm
  (lambda (src)
    (execute-next-expression src)
    (let ((result (trampoline)))
      (if (or (exception? result)
	      (end-of-session? result)
	      (token-type? (first *tokens-left*) 'end-marker))
	result
	(execute-loop-rm src)))))

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input 'stdin try-parse-handler *last-fail*
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens 'stdin try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

;;----------------------------------------------------------------------------
;; old read-eval-print loop

;; redefined as REP-k when no-csharp-support.ss is loaded
;;(define scheme-REP-k
;;  (lambda-cont2 (v fail)
;;    (if (not (void? v))
;;	(safe-print v))
;;    (if *need-newline*
;;      (newline))
;;    (read-eval-print fail)))

;; redefined as REP-handler when no-csharp-support.ss is loaded
;;(define scheme-REP-handler
;;  (lambda-handler2 (e fail)
;;    (REP-k `(uncaught exception: ,e) fail)))

;; redefined as REP-fail when no-csharp-support.ss is loaded
;;(define scheme-REP-fail
;;  (lambda-fail ()
;;    (REP-k "no more choices" REP-fail)))

;;(define start
;;  (lambda ()
;;    ;; start with fresh environments
;;    (set! toplevel-env (make-toplevel-env))
;;    (set! macro-env (make-macro-env^))
;;    (read-eval-print REP-fail)))

;; avoids reinitializing environments on startup (useful for crash recovery)
;;(define restart
;;  (lambda ()
;;    (printf "Restarting...\n")
;;    (read-eval-print REP-fail)))

;;(define* read-eval-print
;;  (lambda (fail)
;;    (set! load-stack '())  ;; in case a previous load encountered an error
;;    (let ((input (raw-read-line "==> ")))  ;; read-line or raw-read-line
;;      (scan-input input 'stdin REP-handler fail
;;	(lambda-cont2 (tokens fail)
;;	  (read-and-eval-asexps tokens 'stdin toplevel-env REP-handler fail REP-k))))))

;;----------------------------------------------------------------------------

(define* read-and-eval-asexps
  (lambda (tokens src env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-asexp tokens src handler fail
	(lambda-cont4 (datum end tokens-left fail)
	  (aparse datum handler fail
	    (lambda-cont2 (exp fail)
	      (m exp env handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-asexps tokens-left src env handler fail k)))))))))))

(define *tracing-on?* #t)

(define make-debugging-k
  (lambda (exp k)
    (if (not *tracing-on?*)
	k
	(lambda-cont2 (v fail)
	  (handle-debug-info exp v)
	  (k v fail)))))

(define highlight-expression
  (lambda (exp)
    ;; call: (function 1 2 3) 
    ;;          ["filename.ss" at line 13 column 4]
    (printf "call: ~s~%" (aunparse exp))
    (let ((info (rac exp)))
      (if (not (eq? info 'none))
	  (printf "['~a' at line ~a column ~a]~%"
		  (get-srcfile info)
		  (get-start-line info)
		  (get-start-char info))))))

(define handle-debug-info
  (lambda (exp result)
    (printf "~s evaluates to ~a~%" (aunparse exp) result)))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
   (if *tracing-on?* (highlight-expression exp))
   (let ((k (make-debugging-k exp k)))   ;; need to reindent
    (cases aexpression exp
      (lit-aexp (datum info) (k datum fail))
      (var-aexp (id info) (lookup-value id env info handler fail k))
      (func-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (dlr-func proc) fail))))
      (if-aexp (test-exp then-exp else-exp info)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (assign-aexp (var rhs-exp var-info info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding var env var-info handler fail
	      (lambda-cont2 (binding fail)
		(let ((old-value (binding-value binding)))
		  (set-binding-value! binding rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-binding-value! binding old-value) (fail))))
		    (k void-value new-fail))))))))
      (define-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding-in-first-frame var env handler fail
	      (lambda-cont2 (binding fail)
		(set-binding-value! binding rhs-value)
		(set-binding-docstring! binding docstring)
		;; definitions should occur only at top level, so no need to undo
		(k void-value fail))))))
      (define!-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (set-global-value! var rhs-value)
	    (set-global-docstring! var docstring)
	    (k void-value fail))))
      (define-syntax-aexp (name clauses aclauses info)
	(lookup-binding-in-first-frame name macro-env handler fail
	  (lambda-cont2 (binding fail)
	    (set-binding-value! binding (make-pattern-macro^ clauses aclauses))
	    (k void-value fail))))
      (begin-aexp (exps info)
	(eval-sequence exps env handler fail k))
      (lambda-aexp (formals bodies info)
	(k (closure formals bodies env) fail))
      (mu-lambda-aexp (formals runt bodies info)
	(k (mu-closure formals runt bodies env) fail))
      (trace-lambda-aexp (name formals bodies info)
	  (k (trace-closure name formals bodies env) fail))
      (mu-trace-lambda-aexp (name formals runt bodies info)
	(k (mu-trace-closure name formals runt bodies env) fail))
      (try-catch-aexp (body cvar cexps info)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler fail k)))
      (try-finally-aexp (body fexps info)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail)))))))
      (try-catch-finally-aexp (body cvar cexps fexps info)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler fail
	     (lambda-cont2 (v fail)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler fail
		 (lambda-cont2 (v2 fail) (k v fail)))))))
      (raise-aexp (exp info)
	(m exp env handler fail
	  ;; TODO: pass in more info to handler (k, env) to support resume, etc.
	  (lambda-cont2 (e fail) (handler e fail))))
      (dict-aexp (pairs info)
	(k (list 'dict pairs) fail))
      (help-aexp (var var-info info)
	(if (reserved-keyword? var)
	  (k (format "~a is a keyword" var) fail)
	  (lookup-binding var env var-info handler fail
	    (lambda-cont2 (binding fail)
	      (k (binding-docstring binding) fail)))))
      (choose-aexp (exps info)
	(eval-choices exps env handler fail k))
      (app-aexp (operator operands info)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(cond
		  ((dlr-exp? proc) (k (dlr-apply proc args) fail))
		  ((procedure-object? proc) (proc args env info handler fail k))
		  (else (runtime-error (format "attempt to apply non-procedure ~a" proc)
				       info handler fail))))))))
      (else (error 'm "bad abstract syntax: ~s" exp)))))
)

(define* runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (format "runtime error: ~a" msg) fail)
      (let ((src (get-srcfile info))
	    (line (get-start-line info))
	    (char (get-start-char info)))
	(handler (format "runtime error: ~a ~a" msg (where-at line char src)) fail)))))

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

(define _closure-depth 0)

(define get-closure-depth
  (lambda ()
    _closure-depth
    ))

(define increment-closure-depth
  (lambda ()
    (set! _closure-depth (+ _closure-depth 1))
    ))

(define decrement-closure-depth
  (lambda ()
    (set! _closure-depth (- _closure-depth 1))
    ))

(define repeat
  ;; turns a list of char into a string
  (lambda (item times)
    (if (= times 0)
	'()
	(cons item (repeat item (- times 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (= (length args) (length formals))
	  (begin
	    (printf "~scall: ~s~%" (apply string-append (repeat " |" (get-closure-depth))) (cons name args))
	    (increment-closure-depth)
	    (eval-sequence bodies (extend env formals args) handler fail 
		 (lambda-cont2 (v fail)
	            (decrement-closure-depth)
	            (printf "~sreturn: ~s~%" (apply string-append (repeat " |" (get-closure-depth))) v)
		    (k2 v fail))))
	  (runtime-error "incorrect number of arguments in application" info handler fail)))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (lambda-proc (args env2 info handler fail k2)
       (if (>= (length args) (length formals))
	   (let ((new-env
		  (extend env
			  (cons runt formals)
			  (cons (list-tail args (length formals))
				(list-head args (length formals))))))
	     (printf "~scall: ~s~%" (apply string-append (repeat " |" (get-closure-depth))) (cons name args))
	     (increment-closure-depth)
	     (eval-sequence bodies new-env handler fail
		(lambda-cont2 (v fail)
		    (decrement-closure-depth)
		    (printf "~sreturn: ~s~%" (apply string-append (repeat " |" (get-closure-depth))) v)
		    (k2 v fail))))
	   (runtime-error "not enough arguments in application" info handler fail)))))

(define closure
  (lambda (formals bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (= (length args) (length formals))
	(eval-sequence bodies (extend env formals args) handler fail k2)
	(runtime-error "incorrect number of arguments in application" info handler fail)))))

(define mu-closure
  (lambda (formals runt bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (eval-sequence bodies new-env handler fail k2))
	(runtime-error "not enough arguments in application" info handler fail)))))

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

;;----------------------------------------------------------------------------
;; Primitives

(define length-one?
  (lambda (ls)
    (and (not (null? ls)) (null? (cdr ls)))))

(define length-two?
  (lambda (ls)
    (and (not (null? ls)) (not (null? (cdr ls))) (null? (cddr ls)))))

(define length-at-least?
  (lambda (n ls)
    (cond
      ((< n 1) #t)
      ((or (null? ls) (not (pair? ls))) #f)
      (else (length-at-least? (- n 1) (cdr ls))))))

(define all-numeric?
  (lambda (ls)
    (or (null? ls)
	(and (number? (car ls))
	     (all-numeric? (cdr ls))))))

(define all-char?
  (lambda (ls)
    (or (null? ls)
	(and (char? (car ls))
	     (all-char? (cdr ls))))))

;; void
(define void-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 void-value fail)))

(define void-value '<void>)

(define void?
  (lambda (x) (eq? x void-value)))

;; exit
(define exit-prim
  (lambda-proc (args env2 info handler fail k2)
    (halt* end-of-session)))

(define end-of-session?
  (lambda (x) (eq? x end-of-session)))

(define end-of-session '(exiting the interpreter))

;; eval
(define eval-prim
  (lambda-proc (args env2 info handler fail k2)
    (reannotate-cps (car args)
      (lambda-cont (adatum)
	(aparse adatum handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env handler fail k2)))))))

;; parse
(define parse-prim
  (lambda-proc (args env2 info handler fail k2)
    (reannotate-cps (car args)
      (lambda-cont (adatum)
	(aparse adatum handler fail k2)))))

;; string-length
(define string-length-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string-length" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-length called on non-string argument" info handler fail))
      (else (k2 (apply string-length args) fail)))))

;; string-ref
(define string-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to string-ref" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-ref called with non-string first argument" info handler fail))
      ((not (number? (cadr args)))
       (runtime-error "string-ref called with non-numberic second argument" info handler fail))
      (else (k2 (apply string-ref args) fail)))))

;; unparse
(define unparse-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car args)) fail)))

;; parse-string
(define parse-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-asexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (aparse adatum handler fail k2)
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; read-string
(define read-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-asexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k2 adatum fail)
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; apply
(define apply-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((proc (car args))
	  (proc-args (cadr args)))
      (proc proc-args env2 info handler fail k2))))

;; sqrt
(define sqrt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to sqrt" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "sqrt called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply sqrt args) fail)))))

;; print
(define print-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each safe-print args)
    (k2 void-value fail)))

;; string
(define string-prim 
  ;; turns a list of char into a string
  (lambda-proc (args env2 info handler fail k2)
     ;;(k2 (apply string-append (map (lambda (c) (format "~s" c)) args)) fail)))
     (k2 (apply char->string args) fail)))

;; substring
(define substring-prim 
  ;; (substring "string" start stop)
  (lambda-proc (args env2 info handler fail k2)
     (k2 (substring (car args) (cadr args) (caddr args)) fail))) 

;; number->string
(define number->string-prim 
  ;; given a number, returns those digits as a string
  (lambda-proc (args env2 info handler fail k2)
     (k2 (number->string (car args)) fail)))

;; assv
(define assv-prim 
  ;; given 'a '((b 1) (a 2)) returns (a 2)
  (lambda-proc (args env2 info handler fail k2)
     (k2 (assv (car args) (cadr args)) fail)))
;; memv
(define memv-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (memv (car args) (cadr args)) fail)))

(define safe-print
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print (make-safe arg))))

(define make-safe
  (lambda (x)
    (cond
      ((procedure-object? x) '<procedure>)
      ((environment-object? x) '<environment>)
      ((pair? x) (cons (make-safe (car x)) (make-safe (cdr x))))
      ((vector? x) (list->vector (make-safe (vector->list x))))
      (else x))))

(define procedure-object?
  (lambda (x)
    (or (procedure? x) (and (pair? x) (eq? (car x) 'procedure)))))

(define environment-object?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

;; display
;; fix: why is this so complicated?
(define display-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((s (format "~a" (car args))))  ;; must use ~a, not ~s, to handle embedded newlines properly
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s)
      (k2 void-value fail))))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

;; newline
(define newline-prim
  (lambda-proc (args env2 info handler fail k2)
    (set! *need-newline* #f)
    (newline)
    (k2 void-value fail)))

(define *need-newline* #f)

;; load
(define load-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-one? args))
       (runtime-error "incorrect number of arguments to load" info handler fail)
       (load-file (car args) toplevel-env info handler fail k2))))

(define load-stack '())

(define* load-file
  (lambda (filename env info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (runtime-error (format "filename ~a is not a string" filename) info handler fail))
      ((not (file-exists? filename))
       (runtime-error (format "attempted to load nonexistent file ~a" filename) info handler fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-asexps tokens filename env handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* load-files
  (lambda (filenames env info handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (load-file (car filenames) env info handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env info handler fail k))))))

;; length
(define length-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-one? args))
      (runtime-error "incorrect number of arguments to length" info handler fail)
      (length-loop (car args) 0 (car args) info handler fail k2))))

(define* length-loop
  (lambda (x sum ls info handler fail k2)
    (cond
      ((null? x) (k2 sum fail))
      ((not (pair? x))
       (runtime-error (format "length called on improper list ~s" ls) info handler fail))
      (else (length-loop (cdr x) (+ sum 1) ls info handler fail k2)))))

;; symbol?
(define symbol?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error
         (format "incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument" args)
         info handler fail))
      (else (k2 (apply symbol? args) fail)))))

;; number?
(define number?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to number?" info handler fail))
      (else (k2 (apply number? args) fail)))))

;; boolean?
(define boolean?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to boolean?" info handler fail))
      (else (k2 (apply boolean? args) fail)))))

;; string?
(define string?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string?" info handler fail))
      (else (k2 (apply string? args) fail)))))

;; char?
(define char?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char?" info handler fail))
      (else (k2 (apply char? args) fail)))))

;; char=?
(define char=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to char=?" info handler fail))
      (else (k2 (apply char=? args) fail)))))

;; char-whitespace?
(define char-whitespace?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-whitespace?" info handler fail))
      (else (k2 (apply char-whitespace? args) fail)))))

;; char-alphabetic?
(define char-alphabetic?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-alphabetic?" info handler fail))
      (else (k2 (apply char-alphabetic? args) fail)))))

;; char-numeric?
(define char-numeric?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-numeric?" info handler fail))
      (else (k2 (apply char-numeric? args) fail)))))

;; null?
(define null?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to null?" info handler fail))
      (else (k2 (apply null? args) fail)))))

;; pair?
(define pair?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to pair?" info handler fail))
      (else (k2 (apply pair? args) fail)))))

;; cons
(define cons-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to cons" info handler fail))
      (else (k2 (apply cons args) fail)))))

;; car
(define car-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
;;      ((not (length-one? args))
;;       (runtime-error "incorrect number of arguments to car" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "car called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply car args) fail)))))

;; cdr
(define cdr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
;;      ((not (length-one? args))
;;       (runtime-error "incorrect number of arguments to cdr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdr args) fail)))))

;; cadr
(define cadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
;;      ((not (length-one? args))
;;       (runtime-error "incorrect number of arguments to cadr" info handler fail))
      ((not (length-at-least? 2 (car args)))
       (runtime-error (format "cadr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply cadr args) fail)))))

;; caddr
(define caddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
;;      ((not (length-one? args))
;;       (runtime-error "incorrect number of arguments to caddr" info handler fail))
      ((not (length-at-least? 3 (car args)))
       (runtime-error (format "caddr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply caddr args) fail)))))

;; list
(define list-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 args fail)))

;; +
(define plus-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (all-numeric? args))
      (runtime-error "+ called on non-numeric argument(s)" info handler fail)
      (k2 (apply + args) fail))))

;; -
(define minus-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args)
       (runtime-error "incorrect number of arguments to -" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "- called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply - args) fail)))))

;; *
(define times-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (all-numeric? args))
      (runtime-error "* called on non-numeric argument(s)" info handler fail)
      (k2 (apply * args) fail))))

;; /
(define divide-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args)
       (runtime-error "incorrect number of arguments to /" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "/ called on non-numeric argument(s)" info handler fail))
      ((member 0 (cdr args))
       (runtime-error "division by zero" info handler fail))
      (else (k2 (apply / args) fail)))))

;; modulo
(define modulo-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to %" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "% called on non-numeric argument(s)" info handler fail))
      ((= 0 (caddr args))
       (runtime-error "modulo by zero" info handler fail))
      (else (k2 (apply modulo args) fail)))))

;; <
(define lt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "< called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply < args) fail)))))

;; >
(define gt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "> called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply > args) fail)))))

;; <=
(define lt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <=" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "<= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply <= args) fail)))))

;; >=
(define gt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >=" info handler fail))
      ((not (all-numeric? args))
       (runtime-error ">= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply >= args) fail)))))

;; =
(define equal-sign-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to =" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply = args) fail)))))

;; abs
(define abs-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to abs" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "abs called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply abs args) fail)))))

;; equal?
(define equal?-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to equal?" info handler fail)
      (equal-objects? (car args) (cadr args)
	(lambda-cont (bool) (k2 bool fail))))))

(define* equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
	   ;; (eq? x y) would be easier, but Eq doesn't work correctly for bools in Scheme.cs:
	   (and (boolean? x) (boolean? y) (or (and x y) (and (not x) (not y))))
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

;; eq?
(define eq?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to eq?" info handler fail))
      (else (k2 (apply eq? args) fail)))))

;; memq
(define memq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to memq" info handler fail))
      (else (k2 (apply memq args) fail)))))

;; member
(define member-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to member" info handler fail)
      (member-loop (car args) (cadr args) (cadr args) info handler fail k2))))

(define* member-loop
  (lambda (x y ls info handler fail k)
    (cond
      ((null? y) (k #f fail))
      ((not (pair? y))
       (runtime-error (format "member called on improper list ~s" ls) info handler fail))
      (else (equal-objects? x (car y)
	      (lambda-cont (bool)
		(if bool
		  (k y fail)
		  (member-loop x (cdr y) ls info handler fail k))))))))

;; range
(define range-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((or (null? args) (length-at-least? 4 args))
       (runtime-error "incorrect number of arguments to range" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "range called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply range args) fail)))))

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
	
;; set-car!
(define set-car!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-car!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-car! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-car! args) fail)))))

;; set-cdr!
(define set-cdr!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-cdr!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-cdr! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-cdr! args) fail)))))

;; import
;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define import-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((filename (car args)))
      (if (null? (cdr args))
	(load-file filename env2 'none handler fail k2)
	(let ((module-name (cadr args)))
	  (lookup-binding-in-first-frame module-name env2 handler fail
	    (lambda-cont2 (binding fail)
	      (let ((module (extend env2 '() '())))
		(set-binding-value! binding module)
		(load-file filename module 'none handler fail k2)))))))))

;; get
(define get-prim
  (lambda-proc (args env2 info handler fail k2)
    (get-primitive args env2 info handler fail k2)))

(define* get-primitive
  (lambda (args env info handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env 'none handler fail
	(lambda-cont2 (v fail)
	  (cond
	    ((null? (cdr args)) (k v fail))
	    ((not (environment? v))
	     (runtime-error (format "invalid module ~a" sym) info handler fail))
	    (else (get-primitive (cdr args) v info handler fail k))))))))

;; call/cc
(define call/cc-prim
  (lambda-proc (args env info handler fail k)
    (if (not (length-one? args))
      (runtime-error "incorrect number of arguments to call/cc" info handler fail)
      (let ((proc (car args)))
	(if (not (procedure-object? proc))
	  (runtime-error "call/cc called with non-procedure" info handler fail)
	  (let ((fake-k (lambda-proc (args env2 info handler fail k2) (k (car args) fail))))
	    (if (dlr-exp? proc)
	      (k (dlr-apply proc (list fake-k)) fail)
	      (proc (list fake-k) env info handler fail k))))))))

;; abort
(define abort-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (null? args)
      (REP-k void-value fail)
      (REP-k (car args) fail))))

;; require
(define require-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to require" info handler fail))
      ((true? (car args)) (k2 'ok fail))
      (else (fail)))))

;; cut
(define cut-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (null? args))
      (runtime-error "incorrect number of arguments to cut" info handler fail)
      (k2 'ok REP-fail))))

;; reverse
(define reverse-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to reverse" info handler fail))
      ((not (list? args))
       (runtime-error (format "reverse called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply reverse args) fail)))))

;; append
(define append-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
;;      ((not (length-two? args))
;;       (runtime-error "incorrect number of arguments to append" info handler fail))
;;      ((not (list? (car args)))
;;       (runtime-error (format "append called on incorrect list structure ~s" (car args)) info handler fail))
      (else (append-all args (lambda-cont (v) (k2 v fail)))))))
;;      (else (k2 (apply append args) fail)))))

(define* append2
  (lambda (ls1 ls2 k)
    (if (null? ls1)
      (k ls2)
      (append2 (cdr ls1) ls2
	(lambda-cont (v)
	  (k (cons (car ls1) v)))))))

(define* append-all
  (lambda (lists k)
    (cond
      ((null? lists) (k '()))
      ((null? (cdr lists)) (k (car lists)))
      (else (append-all (cdr lists)
	      (lambda-cont (ls)
		(append2 (car lists) ls k)))))))

;; string->number
(define string->number-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->number" info handler fail))
      (else (k2 (apply string->number args) fail)))))

;; string=?
(define string=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to string=?" info handler fail))
     (else (k2 (apply string=? args) fail)))))

;; list->vector
(define list-to-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->vector" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->vector called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply list->vector args) fail)))))

;; list->string
(define list->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->string" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->string called on incorrect list structure ~s" (car args)) info handler fail))
      ((not (all-char? (car args)))
       (runtime-error (format "list->string called on non-char list ~s" (car args)) info handler fail))
      (else (k2 (apply list->string args) fail)))))

;; dir
(define dir-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (dir args env2) fail)))

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

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

;; current-time
(define current-time-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (get-current-time) fail)))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now) 1000000000))))))

;; map
(define map-prim
  (lambda-proc (args env2 info handler fail k2)
    (map-primitive (car args) (cdr args) env2 handler fail k2)))

;; supports procedures of any number of arguments
(define* map-primitive
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args))
              (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler fail k))
            (else (mapN proc list-args env handler fail k)))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list))
      (cons (car arg-list) (listify (cdr arg-list))))
     ((vector? (car arg-list))
      (cons (vector->list (car arg-list)) (listify (cdr arg-list))))
     ((string? (car arg-list))
      (cons (string->list (car arg-list)) (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map" 
		  (get_type (car arg-list))))))) ;; get_type is defined in C#

(define* iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define* iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env 'none handler fail
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
          (proc (list item) env 'none handler fail
            (lambda-cont2 (v1 fail)
              (iterate-collect-continue proc iterator env handler fail
                (lambda-cont2 (v2 fail)
                  (k (cons v1 v2) fail)))))))))

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
	(proc (list (car list1)) env 'none handler fail
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
	(proc (list (car list1) (car list2)) env 'none handler fail
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
	(proc (map car lists) env 'none handler fail
	  (lambda-cont2 (v1 fail)
	    (mapN proc (map cdr lists) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

;; for-each
(define for-each-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each-primitive (car args) (cdr args) env2 handler fail k2)))

(define* for-each-primitive
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
      (iterate proc (car lists) env handler fail k)
      (let ((arg-list (listify lists)))
	(if (null? (car arg-list))
	  (k void-value fail)
	  (if (dlr-exp? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-primitive proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env 'none handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-primitive proc (map cdr arg-list) env handler fail k)))))))))

;; end
(define env-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 env2 fail)))

;; using (not defined in scheme)
(define using-primitive
  (lambda-proc (args env2 info handler fail k2)
    (k2 (using-prim args env2) fail)))

;; not
(define not-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to not" info handler fail))
      (else (k2 (not (car args)) fail)))))

;; printf (not defined in scheme)
(define printf-primitive
  (lambda-proc (args env2 info handler fail k2)
    (apply printf-prim args)
    (k2 void-value fail)))

;; vector
(define vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (list->vector args) fail)))

;; vector-set!
(define vector-set!-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (vector-set! (car args) (cadr args) (caddr args)) fail)))

;; vector-ref
(define vector-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply vector-ref args) fail)))

;; make-vector
(define make-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply make-vector args) fail)))

;; error
(define error-prim
  (lambda-proc (args env2 info handler fail k2)
    (let* ((location (format "Error in ~a: " (car args)))
	   (message (string-append location (apply format (cdr args)))))
      (runtime-error message info handler fail))))

;; list-ref
(define list-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to list-ref" info handler fail))
      (else (k2 (apply list-ref args) fail)))))

;; Add new procedures above here!
;; Then, add NAME to env
;; Then, add NAME_proc to Scheme.cs

;; this is here as a hook for extending environments in C# etc.
(define make-initial-env-extended
  (lambda (env) env))

(define make-toplevel-env
  (lambda ()
    (let ((primitives 
	   (list
	    (list '* times-prim)
	    (list '+ plus-prim)
	    (list '- minus-prim)
	    (list '/ divide-prim)
	    (list '< lt-prim)
	    (list '<= lt-or-eq-prim)
	    (list '= equal-sign-prim)
	    (list '=? equal-sign-prim)
	    (list '> gt-prim)
	    (list '>= gt-or-eq-prim)
	    (list 'abort abort-prim)
	    (list 'abs abs-prim)
	    (list 'append append-prim)
	    (list 'apply apply-prim)
	    (list 'assv assv-prim)
	    (list 'boolean? boolean?-prim)
	    (list 'caddr caddr-prim)
	    (list 'cadr cadr-prim)
	    (list 'call-with-current-continuation call/cc-prim)
	    (list 'call/cc call/cc-prim)
	    (list 'car car-prim)
	    (list 'cdr cdr-prim)
	    (list 'char? char?-prim)
	    (list 'char=? char=?-prim)
	    (list 'char-whitespace? char-whitespace?-prim)
	    (list 'char-alphabetic? char-alphabetic?-prim)
	    (list 'char-numeric? char-numeric?-prim)
	    (list 'cons cons-prim)
	    (list 'current-time current-time-prim)
	    (list 'cut cut-prim)
	    (list 'dir dir-prim)
	    (list 'display display-prim)
	    (list 'env env-prim)
	    (list 'eq? eq?-prim)
	    (list 'equal? equal?-prim)
	    (list 'error error-prim)
	    (list 'eval eval-prim)
	    (list 'exit exit-prim)
	    (list 'for-each for-each-prim)
	    (list 'get get-prim)
	    (list 'import import-prim)
	    (list 'length length-prim)
	    (list 'list list-prim)
	    (list 'list->vector list-to-vector-prim)
	    (list 'list->string list->string-prim)
	    (list 'list-ref list-ref-prim)
	    (list 'load load-prim)
	    (list 'make-vector make-vector-prim)
	    (list 'map map-prim)
	    (list 'member member-prim)
	    (list 'memq memq-prim)
	    (list 'memv memv-prim)
	    (list 'modulo modulo-prim)
	    (list 'newline newline-prim)
	    (list 'not not-prim)
	    (list 'null? null?-prim)
	    (list 'number->string number->string-prim)
	    (list 'number? number?-prim)
	    (list 'pair? pair?-prim)
	    (list 'parse parse-prim)
	    (list 'parse-string parse-string-prim)
	    (list 'print print-prim)
	    (list 'printf printf-primitive)
	    (list 'range range-prim)
	    (list 'read-string read-string-prim)
	    (list 'require require-prim)
	    (list 'reverse reverse-prim)
	    (list 'set-car! set-car!-prim)
	    (list 'set-cdr! set-cdr!-prim)
	    (list 'sqrt sqrt-prim)
	    (list 'string string-prim)
	    (list 'string-length string-length-prim)
	    (list 'string-ref string-ref-prim)
	    (list 'string? string?-prim)
	    (list 'string->number string->number-prim)
	    (list 'string=? string=?-prim)
	    (list 'substring substring-prim)
	    (list 'symbol? symbol?-prim)
	    (list 'unparse unparse-prim)
	    (list 'using using-primitive)
	    (list 'vector vector-prim)
	    (list 'vector-ref vector-ref-prim)
	    (list 'vector-set! vector-set!-prim)
	    (list 'void void-prim)
	    )))
      (make-initial-env-extended
       (make-initial-environment
	(map car primitives)
	(map cadr primitives))))))
	
(define toplevel-env (make-toplevel-env))

;;------------------------------------------------------------------------
;; C# support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 info handler fail k2)
      (k2 (apply* external-function-object args) fail))))

;; not used
(define Main
  (lambda filenames
    (printf "Calico Scheme (0.2)\n")
    (printf "(c) 2009-2011, IPRE\n")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    ;; in the register machine, this call just sets up the registers
    (load-files filenames toplevel-env 'none REP-handler REP-fail REP-k)
    ;; starts the computation after registers are set up
    (trampoline)))

;; temporary - remove before transforming to C#
;;(load "no-csharp-support.ss")

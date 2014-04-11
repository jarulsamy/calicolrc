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
;; to run the scheme data structure machine within Petite:
;; % petite pjscheme-ds.ss
;; > (start)

;;----------------------------------------------------------------------------
;; to run the scheme register machine within Petite:
;; % petite pjscheme-rm.ss
;; > (run start-rm)

;;----------------------------------------------------------------------------
;; used by scheme CPS, DS, RM, and C# RM code

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

;;----------------------------------------------------------------------------
;; used only by scheme CPS, DS, and RM code

;; dummy versions of functions defined in C# code
(define-native dlr-proc? (lambda (x) #f))
(define-native dlr-apply apply)
(define-native dlr-func (lambda (x) x))
(define-native callback (lambda args #f))
(define-native dlr-env-contains (lambda (x) #f))
(define-native dlr-env-lookup (lambda (x) #f))
(define-native dlr-object? (lambda (x) #f))
(define-native dlr-lookup-components (lambda (x y) #f))
(define-native set-global-value! (lambda (var x) #f))
(define-native set-global-docstring! (lambda (var x) #f))
(define-native using (lambda ignore #f))
(define-native iterator? (lambda ignore #f))
(define-native get_type (lambda (x) 'unknown))

(define use-lexical-address
  (lambda args
    (cond 
     ((null? args) *use-lexical-address*)
     (else 
      (begin (set! *use-lexical-address* (true? (car args)))
	     void-value)))))

(define-native read-line
  (lambda (prompt)
    (printf prompt)
    (format "~s" (read))))

;; because read-line uses (read), it can only read a single sexp at a
;; time. it always returns a string version of its input. if the input
;; is the list (+ 2 3), the string "(+ 2 3)" is returned; if the input
;; is the string "apple", the string "\"apple\"" is returned; etc.
;;
;; read-line-test is only for testing the evaluation of multiple sexps
;; at once.  the user must type the input as a string enclosed by
;; double quotes.

(define read-line-test ;; redefine this to read-line to test
  (lambda (prompt)
    (printf prompt)
    (let loop ((input (read)))
      (if (string? input)
	  input
	  (begin
	    (printf "Error: input must be enclosed in quotation marks.\n==> ")
	    (loop (read)))))))

;;----------------------------------------------------------------------------
;; used only by scheme CPS and DS code

(define start
  (lambda ()
    ;; start with fresh environments
    (initialize-globals)
    (read-eval-print-loop)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop)))

(define read-eval-print-loop
  (lambda ()
    (let ((input (read-line "==> ")))  
      ;; execute gets redefined as execute-rm when no-csharp-support.ss is loaded
      (let ((result (execute input 'stdin)))
	(if (not (void? result))
	    (if (exception? result)
		(handle-exception result)
		(safe-print result)))
	(if *need-newline*
	  (newline))
	(if (end-of-session? result)
	  (halt* 'goodbye)
	  (read-eval-print-loop))))))

(define handle-exception
  (lambda (exc)
    ;; (exception ("ReadError" "cannot represent 1/0" stdin 1 1 ()))
    (let ((stack (cadddr (cddr (cadr exc))))
	  (message (cadr (cadr exc)))
	  (error-type (car (cadr exc))))
      (printf "~%Traceback (most recent call last):~%")
      (while (not (null? stack))
	     (display (format-exception-line (car stack)))
	     (set! stack (cdr stack)))
      (printf "~a: ~a~%" error-type message))))

(define format-exception-line
  (lambda (line)
    (let ((filename (car line))
	  (line-number (cadr line))
	  (column-number (caddr line)))
      (if (= (length line) 3)
	  (format "  File \"~a\", line ~a, col ~a~%" filename line-number column-number)
	  (format "  File \"~a\", line ~a, col ~a, in ~a~%" filename line-number column-number (cadddr line))))))

(define execute-string
  (lambda (input)
    (execute input 'stdin)))

(define execute-file
  (lambda (filename)
    (execute (read-content filename) filename)))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (initialize-execute!)
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
    (read-sexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum (initial-contours toplevel-env) REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;;----------------------------------------------------------------------------
;; used only by scheme RM code

(define start-rm
  (lambda ()
    ;; start with fresh environments
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (read-eval-print-loop-rm)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart-rm
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop-rm)))

(define read-eval-print-loop-rm
  (lambda ()
    (let ((input (read-line "==> ")))  
      (let ((result (execute-rm input 'stdin)))
	(while (not (end-of-session? result))
	   (cond 
	    ((exception? result) (handle-exception result))
	    ((not (void? result))
	     (begin 
	       (if *need-newline* (newline))
	       (safe-print result))))
	   (set! input (read-line "==> "))  
	   (set! result (execute-rm input 'stdin)))
	'goodbye))))

;;----------------------------------------------------------------------------
;; used only by scheme RM and C# RM code

(define execute-string-rm
  (lambda (input)
    (execute-rm input 'stdin)))

(define execute-file-rm
  (lambda (filename)
    (execute-rm (read-content filename) filename)))

(define execute-rm
  (lambda (input src)
    (set! load-stack '())
    (initialize-execute!)
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
    (execute-next-expression-rm src)
    (let ((result (trampoline)))
      (if (or (exception? result)
	      (end-of-session? result)
	      (token-type? (first *tokens-left*) 'end-marker))
	result
	(execute-loop-rm src)))))

(define execute-next-expression-rm
  (lambda (src)
    (read-sexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum (initial-contours toplevel-env) REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;;----------------------------------------------------------------------------
;; used only by C# RM code

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input 'stdin try-parse-handler *last-fail*
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens 'stdin (initial-contours toplevel-env) try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (initialize-execute!)
    (set! *last-fail* REP-fail)))

;;----------------------------------------------------------------------------

(define *tracing-on?* #f)

(define make-debugging-k
  (lambda (exp k)
    (lambda-cont2 (v fail)
      (handle-debug-info exp v)
      (k v fail))))

(define highlight-expression
  (lambda (exp)
    ;; call: (function 1 2 3) 
    ;;          ["filename.ss" at line 13 column 4]
    (printf "call: ~s~%" (aunparse exp))
    (let ((info (rac exp)))
      (if (not (eq? info 'none))
	  (printf "['~a', line ~a, col ~a]~%"
		  (get-srcfile info)
		  (get-start-line info)
		  (get-start-char info))))))

(define handle-debug-info
  (lambda (exp result)
    (printf "~s => ~a~%" (aunparse exp) (make-safe result))))

(define *stack-trace* '(()))

(define *use-stack-trace* #t)

(define get-use-stack-trace
  (lambda ()
    *use-stack-trace*))

(define set-use-stack-trace!
  (lambda (value)
    (set! *use-stack-trace* (true? value))))

(define initialize-stack-trace!
  (lambda ()
    (set-car! *stack-trace* '())))

(define initialize-execute!
  (lambda () 
    (set! _closure_depth  0)
    (set! _trace_pause #f)
    (initialize-stack-trace!)))

(define push-stack-trace!
  (lambda (exp)
    ;;(printf "~a: ~a\n" 'push exp)
    ;; FIXME: limit size of stack!
    (set-car! *stack-trace* (cons exp (car *stack-trace*)))))

(define pop-stack-trace!
  (lambda (exp)
    ;;(printf "~a: ~a\n" 'pop exp)
    (if (not (null? (car *stack-trace*)))
	(set-car! *stack-trace* (cdr (car *stack-trace*))))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
   (if *tracing-on?* (highlight-expression exp))
   (let ((k (if *tracing-on?* (make-debugging-k exp k) k)))
    (cases aexpression exp
      (lit-aexp (datum info) (k datum fail))
      (var-aexp (id info)
	(lookup-value id env info handler fail k))
      (lexical-address-aexp (depth offset id info)
	(lookup-value-by-lexical-address depth offset (frames env) fail k))
      (func-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (dlr-func proc) fail))))
      (callback-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (callback proc) fail))))
      (if-aexp (test-exp then-exp else-exp info)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (assign-aexp (var rhs-exp var-info info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-variable var env var-info handler fail
	      (lambda-cont2 (var fail)
		(let ((old-value (dlr-env-lookup var)))
		  ;; need to undo the assignment if we back up
		  (set-global-value! var rhs-value)
		  (let ((new-fail (lambda-fail () (set-global-value! var old-value) (fail))))
		    (k void-value new-fail))))
	      (lambda-cont3 (dlr-obj components fail) ;; dlr-obj is Myro, components is (Myro robot)
		(let ((old-value (get-external-member dlr-obj components)))
		  (set-external-member! dlr-obj components rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-external-member! dlr-obj components old-value) (fail))))
		    (k void-value new-fail))))
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
	    (if (procedure-object? rhs-value)
		(set-global-value! var (dlr-func rhs-value))
		(set-global-value! var rhs-value))
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
      (choose-aexp (exps info)
	(eval-choices exps env handler fail k))
      (app-aexp (operator operands info)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(if *use-stack-trace* (push-stack-trace! exp))
		(cond
		  ((dlr-proc? proc) 
		   (let ((result (dlr-apply proc args)))
		     (if *use-stack-trace* (pop-stack-trace! exp))
		     (k result fail)))
		  ((procedure-object? proc) 
		   (if *use-stack-trace*
		       (proc args env info handler fail 
			  (lambda-cont2 (v2 fail)
			     (pop-stack-trace! exp)
			     (k v2 fail)))
		       (proc args env info handler fail k)))
		  (else (runtime-error (format "attempt to apply non-procedure '~a'" proc)
				       info handler fail))))))))
      (else (error 'm "bad abstract syntax: '~s'" exp))))))

(define make-exception
  (lambda (exception message source line column)
    (list exception message source line column (make-stack-trace))))

(define make-stack-trace
  (lambda ()
    (let ((trace (car *stack-trace*)))
      (reverse (map format-stack-trace trace)))))

(define get-procedure-name
  (lambda (aexp)
    (if (macro-derived-source-info? aexp)
	(rac (get-source-info aexp))
	(cases aexpression aexp
	  (app-aexp (operator operands info)
	    (cases aexpression operator
	      (lexical-address-aexp (depth offset id info) id)
	      (var-aexp (id info) id)
	      (lambda-aexp (formals bodies info) `(lambda ,formals ...))
	      (mu-lambda-aexp (formals runt bodies info) `(lambda ,(append formals runt) ...))
	      (trace-lambda-aexp (name formals bodies info) name)
	      (mu-trace-lambda-aexp (name formals runt bodies info) name)
	      (else 'application)))
	  (else 'unknown)))))

(define format-stack-trace
  (lambda (exp)
    (let ((info (rac exp)))
      (if (eq? info 'none)
	  'macro-generated-exp
	  (list (get-srcfile info)
		(get-start-line info)
		(get-start-char info)
		(get-procedure-name exp))))))

(define* runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (make-exception "RunTimeError" msg 'none 'none 'none) fail)
      (let ((src (get-srcfile info))
	    (line_number (get-start-line info))
	    (char_number (get-start-char info)))
	(handler (make-exception "RunTimeError" msg src line_number char_number) fail)))))

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
    (if (null? (cdr exps))
      (m (car exps) env handler fail k)
      (m (car exps) env handler fail
	(lambda-cont2 (result fail)
	  (eval-sequence (cdr exps) env handler fail k))))))

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

(define make-trace-depth-string
  (lambda (level)
    (if (= level 0)
      ""
      (string-append " |" (make-trace-depth-string (- level 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
	(if (= (length args) (length formals))
	  (begin
	    (printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name args))
	    ;;(printf "k: ~a\n" (make-safe-continuation k2))
	    (set! trace-depth (+ trace-depth 1))
	    (eval-sequence bodies (extend env formals args) handler fail 
	      (lambda-cont2 (v fail)
		(set! trace-depth (- trace-depth 1))
		(printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		(k2 v fail))))
	  (runtime-error "incorrect number of arguments in application" info handler fail))))))

;; experimental
(define-native make-safe-continuation
  (lambda (k)
    (cond
      ((not (pair? k)) '<???>)
      ((eq? (car k) 'fail-continuation) '<fail>)
      ((memq (car k) '(handler handler2)) '<handler>)
      ((memq (car k) '(continuation continuation2 continuation3 continuation4))
       (cons (cadr k) (map make-safe-continuation (filter continuation-object? (cddr k)))))
      (else '<???>))))

;; experimental
(define continuation-object?
  (lambda (x)
    (and (pair? x) (memq (car x) '(continuation continuation2 continuation3 continuation4)))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
	(if (>= (length args) (length formals))
	  (let ((new-env
		  (extend env
		    (cons runt formals)
		    (cons (list-tail args (length formals))
			  (list-head args (length formals))))))
	    (printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name args))
	    (set! trace-depth (+ trace-depth 1))
	    (eval-sequence bodies new-env handler fail
	      (lambda-cont2 (v fail)
		(set! trace-depth (- trace-depth 1))
		(printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		(k2 v fail))))
	  (runtime-error "not enough arguments in application" info handler fail))))))

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

;; zero?
(define zero?-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (= (car args) 0) fail)))

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
    (cond
      ((length-one? args)  ;; petite uses toplevel env
       (annotate-cps (car args) 'none
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours toplevel-env) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp toplevel-env handler fail k2))))))
      ((length-two? args)
       (annotate-cps (car args) 'none
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours (cadr args)) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp (cadr args) handler fail k2))))))
      (else (runtime-error "incorrect number of arguments to eval" info handler fail)))))
	       
;; eval-ast
(define eval-ast-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to eval-ast" info handler fail))
      ((not (list? (car args)))  ;; is there a better test for exp?  aexpression?
       (runtime-error "eval-ast called on non-abstract syntax tree argument" info handler fail))
      (else (m (car args) toplevel-env handler fail k2)))))  ;; petite uses toplevel env

;; parse
(define parse-prim
  (lambda-proc (args env2 info handler fail k2)
    (annotate-cps (car args) 'none
      (lambda-cont (adatum)
        (aparse adatum (initial-contours toplevel-env) handler fail k2)))))  ;; was env2

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
    (k2 (aunparse (car args)) fail)))   ;; aunparse should be in CPS

;; unparse-procedure
(define unparse-procedure-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car (caddr (car args)))) fail)))  ;; aunparse should be in CPS

;; parse-string
(define parse-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (aparse adatum (initial-contours toplevel-env) handler fail k2)  ;; was env2
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; read-string
(define read-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens 'stdin handler fail
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

;; odd?
(define odd?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to odd?" info handler fail))
      (else (k2 (odd? (car args)) fail)))))

;; even?
(define even?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to even?" info handler fail))
      (else (k2 (even? (car args)) fail)))))

;; quotient
(define quotient-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to quotient" info handler fail))
      (else (k2 (apply quotient args) fail)))))

;; remainder
(define remainder-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to remainder" info handler fail))
      (else (k2 (apply remainder args) fail)))))

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
     (k2 (apply string args) fail)))

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
    (if (not (length-at-least? 1 args))
	(runtime-error "incorrect number of arguments to load" info handler fail)
	(load-files args toplevel-env info handler fail k2))))  ;; petite uses toplevel env

(define load-stack '())

(define* load-file
  (lambda (filename env2 info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (runtime-error (format "filename '~a' is not a string" filename) info handler fail))
      ((not (file-exists? filename))
       (runtime-error (format "attempted to load nonexistent file '~a'" filename) info handler fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-asexps tokens filename env2 handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* read-and-eval-asexps
  (lambda (tokens src env2 handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (datum end tokens-left fail)
	  (aparse datum (initial-contours env2) handler fail  ;; was env2
	    (lambda-cont2 (exp fail)
	      (m exp env2 handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-asexps tokens-left src env2 handler fail k)))))))))))

(define* load-files
  (lambda (filenames env2 info handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (load-file (car filenames) env2 info handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env2 info handler fail k))))))

;; length
(define length-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (length-one? args)
      (length-loop (car args) 0 (car args) info handler fail k2)
      (runtime-error "incorrect number of arguments to length" info handler fail))))

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
      ((or (not (char? (car args))) (not (char? (cadr args))))
       (runtime-error "char=? requires arguments of type char" info handler fail))
      (else (k2 (apply char=? args) fail)))))

;; char-whitespace?
(define char-whitespace?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-whitespace?" info handler fail))
      (else (k2 (apply char-whitespace? args) fail)))))

;; char->integer
(define char->integer-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char->integer" info handler fail))
      (else (k2 (apply char->integer args) fail)))))

;; integer->char
(define integer->char-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to integer->char" info handler fail))
      (else (k2 (apply integer->char args) fail)))))

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
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to car" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "car called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply car args) fail)))))

;; cdr
(define cdr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdr args) fail)))))

;; cadr
(define cadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadr" info handler fail))
      ((not (length-at-least? 2 (car args)))
       (runtime-error (format "cadr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply cadr args) fail)))))

;; caddr
(define caddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caddr" info handler fail))
      ((not (length-at-least? 3 (car args)))
       (runtime-error (format "caddr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply caddr args) fail)))))

(define caaaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaaar args) fail)))))

(define caaadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaadr args) fail)))))

(define caaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaar args) fail)))))

(define caadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caadar args) fail)))))

(define caaddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaddr args) fail)))))

(define caadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caadr args) fail)))))

(define caar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caar args) fail)))))

(define cadaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadaar args) fail)))))

(define cadadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadadr args) fail)))))

(define cadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadar args) fail)))))

(define caddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caddar args) fail)))))

(define cadddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadddr args) fail)))))

(define cdaaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaaar args) fail)))))

(define cdaadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaadr args) fail)))))

(define cdaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaar args) fail)))))

(define cdadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdadar args) fail)))))

(define cdaddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaddr args) fail)))))

(define cdadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdadr args) fail)))))

(define cdar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdar args) fail)))))

(define cddaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddaar args) fail)))))

(define cddadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddadr args) fail)))))

(define cddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddar args) fail)))))

(define cdddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdddar args) fail)))))

(define cddddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddddr args) fail)))))

(define cdddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdddr args) fail)))))

(define cddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddr args) fail)))))

;; list
(define list-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 args fail)))

;; make-set
(define make-set-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to set" info handler fail))
      (else (make-set (car args) env2 info handler fail k2)))))
    
(define* make-set
  (lambda (lst env2 info handler fail k2)
    (if (null? lst)
      (k2 lst fail)
      (make-set (cdr lst) env2 info handler fail
	(lambda-cont2 (v fail)
	  (if (member (car lst) v)
	    (k2 v fail)
	    (k2 (cons (car lst) v) fail)))))))

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
      ((= (cadr args) 0)
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
	   (and (eq? x void-value) (eq? y void-value))
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

(define snoc-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply snoc args) fail)))

(define rac-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply rac args) fail)))

(define rdc-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply rdc args) fail)))

(define-native range
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
(define import-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((filename (car args)))
      (if (null? (cdr args))
	(load-file filename env2 'none handler fail k2)
	(let ((module-name (cadr args)))
	  (lookup-binding-in-first-frame module-name env2 handler fail
	    (lambda-cont2 (binding fail)
	      (let ((module (make-toplevel-env)))
		(set-binding-value! binding module)
		(load-file filename module 'none handler fail k2)))))))))

;; get-stack-trace-prim
(define get-stack-trace-prim
  (lambda-proc (args env2 info handler fail k)
    (k (car *stack-trace*) fail)))

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
	     (runtime-error (format "invalid module '~a'" sym) info handler fail))
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
	    (if (dlr-proc? proc)
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
    (append-all args info handler fail k2)))
;;    (cond
;;      ((not (length-two? args))
;;       (runtime-error "incorrect number of arguments to append" info handler fail))
;;      ((not (list? (car args)))
;;       (runtime-error (format "append called on incorrect list structure ~s" (car args)) info handler fail))
;;      (else (append-all args (lambda-cont (v) (k2 v fail)))))))
;;      (else (k2 (apply append args) fail)))))

(define* append2
  (lambda (ls1 ls2 fail k2)
    (if (null? ls1)
      (k2 ls2 fail)
      (append2 (cdr ls1) ls2 fail
	(lambda-cont2 (v fail)
	  (k2 (cons (car ls1) v) fail))))))

(define* append-all
  (lambda (lists info handler fail k2)
    (cond
      ((null? lists) (k2 '() fail))
      ((null? (cdr lists)) (k2 (car lists) fail))
      ((not (list? (car lists)))
       (runtime-error (format "append called on incorrect list structure ~s" (car lists)) info handler fail))
      (else (append-all (cdr lists) info handler fail
	      (lambda-cont2 (ls fail)
		(append2 (car lists) ls fail k2)))))))

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
(define list->vector-prim
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

(define char->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char->string" info handler fail))
      ((not (char? (car args)))
       (runtime-error (format "char->string called on non-char item ~s" (car args)) info handler fail))
      (else (k2 (apply char->string args) fail)))))


(define string->list-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->list" info handler fail))
      ((not (string? (car args)))
       (runtime-error (format "string->list called on non-string item ~s" (car args)) info handler fail))
      (else (k2 (apply string->list args) fail)))))


(define string->symbol-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->symbol" info handler fail))
      ((not (string? (car args)))
       (runtime-error (format "string->symbol called on non-string item ~s" (car args)) info handler fail))
      (else (k2 (apply string->symbol args) fail)))))


(define symbol->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to symbol->string" info handler fail))
      ((not (symbol? (car args)))
       (runtime-error (format "symbol->string called on non-symbol item ~s" (car args)) info handler fail))
      (else (k2 (apply symbol->string args) fail)))))

(define vector->list-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to vector->list" info handler fail))
      ((not (vector? (car args)))
       (runtime-error (format "vector->list called on incorrect vector structure ~s" (car args)) info handler fail))
      (else (k2 (apply vector->list args) fail)))))

;; dir
(define dir-prim
  (lambda-proc (args env2 info handler fail k2)
    (make-set (directory args env2) env2 info handler fail k2)))

(define directory
  (lambda (args env)
    (if (or (null? args) (environment? (car args)))
	(sort symbol<? (if (null? args)
			   (append (get-variables-from-frames (frames macro-env))
				   (get-variables-from-frames (frames env)))
			   (get-variables-from-frames (frames (car args)))))
	(get-external-members (car args)))))

(define get-variables-from-frame
  (lambda (frame)
    (cadr frame)))

(define get-variables-from-frames
  (lambda (frames) 
    (flatten (map get-variables-from-frame frames))))

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
      (if (dlr-proc? proc)
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
      (if (dlr-proc? proc)
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
      (if (dlr-proc? proc)
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
	  (if (dlr-proc? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-primitive proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env 'none handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-primitive proc (map cdr arg-list) env handler fail k)))))))))

;; format
(define format-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((< (length args) 1)
       (runtime-error "incorrect number of arguments to format" info handler fail))
      (else (k2 (apply format args) fail)))))

;; env
(define current-environment-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 env2 fail)))

;; using (not defined in scheme)
(define using-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (using args env2) fail)))

;; not
(define not-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to not" info handler fail))
      (else (k2 (not (true? (car args))) fail)))))

;; printf 
(define printf-prim
  (lambda-proc (args env2 info handler fail k2)
    (apply printf args)
    (k2 void-value fail)))

;; vector
(define vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply vector_native args) fail)))

(define-native vector_native
  (lambda args
    (apply vector args)))

;; vector-set!
(define vector-set!-prim
  (lambda-proc (args env2 info handler fail k2)
    (vector-set! (car args) (cadr args) (caddr args))
    (k2 void-value fail)))

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
    (cond 
      ((not (length-at-least? 1 args))
       (runtime-error "incorrect number of arguments to 'error' (should at least 1)" info handler fail))
      (else
       (let* ((location (format "Error in '~a': " (car args)))
	      (message (string-append location (apply format (cdr args)))))
	 (runtime-error message info handler fail))))))

;; list-ref
(define list-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to list-ref" info handler fail))
      (else (k2 (apply list-ref args) fail)))))

;; current-directory
(define current-directory-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args) (k2 (current-directory) fail))
      ((length-one? args)
       (if (string? (car args))
	   (k2 (current-directory (car args)) fail)
	   (runtime-error "directory must be a string" info handler fail)))
      (else (runtime-error "incorrect number of arguments to current-directory" info handler fail)))))

(define round-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (number? (car args)))
       (k2 (round (car args)) fail))
      (else
       (runtime-error "round requires exactly one number" info handler fail)))))

(define use-stack-trace-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (boolean? (car args)))
       (begin
	 (set-use-stack-trace! (car args))
	 (k2 void-value fail)))
      ((null? args)
       (k2 *use-stack-trace* fail))
      (else
       (runtime-error "use-stack-trace requires exactly one boolean or nothing" info handler fail)))))

(define use-tracing-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (boolean? (car args)))
       (begin
	 (set! *tracing-on?* (true? (car args)))
	 (k2 void-value fail)))
      ((null? args)
       (k2 *tracing-on?* fail))
      (else
       (runtime-error "use-tracing requires exactly one boolean or nothing" info handler fail)))))

(define eqv?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to eqv?" info handler fail))
     (else (k2 (apply eqv? args) fail)))))

(define vector?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to vector?" info handler fail))
     (else (k2 (apply vector? args) fail)))))

(define atom?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to atom?" info handler fail))
     (else (k2 (apply atom? args) fail)))))

(define iter?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to iter?" info handler fail))
     (else (k2 (apply iter? args) fail)))))

(define list?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to list?" info handler fail))
     (else (k2 (apply list? args) fail)))))

(define procedure?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to procedure?" info handler fail))
     (else (k2 (apply procedure? args) fail)))))

(define string<?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to string<?" info handler fail))
     (else (k2 (apply string<? args) fail)))))

(define float-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to float" info handler fail))
      (else (k2 (apply float args) fail)))))

(define globals-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (null? args))
       (runtime-error "incorrect number of arguments to globals" info handler fail))
      (else (k2 (apply globals args) fail)))))

(define int-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to int" info handler fail))
      (else (k2 (apply int args) fail)))))

(define apply-with-keywords-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 1 args))
       (runtime-error "incorrect number of arguments to apply-with-keywords" info handler fail))
      (else (k2 (apply apply-with-keywords args) fail)))))

(define assq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to assq" info handler fail))
      (else (k2 (apply assq args) fail)))))

(define dict-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      (else (k2 (apply dict args) fail)))))

(define property-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to property" info handler fail))
      (else (k2 (apply property args) fail)))))

(define rational-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to rational" info handler fail))
      (else (k2 (apply / args) fail)))))

(define reset-toplevel-env-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (null? args))
       (runtime-error "incorrect number of arguments to reset-toplevel-env" info handler fail))
      (else (k2 (apply reset-toplevel-env args) fail)))))

(define sort-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to sort" info handler fail))
      (else (k2 (apply sort args) fail)))))

(define string-append-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to string-append" info handler fail))
      (else (k2 (apply string-append args) fail)))))

(define string-split-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string-split" info handler fail))
      (else (k2 (apply string-split args) fail)))))

(define symbol-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to symbol" info handler fail))
      (else (k2 (apply make-symbol args) fail)))))

(define typeof-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to typeof" info handler fail))
      (else (k2 (apply type args) fail)))))
 
(define use-lexical-address-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (apply use-lexical-address args) fail)))

;; -----------------------------------------------------
;; To add a new primitive:
;; -----------------------------------------------------
;; 1. Add new NAME-prim primitive procedures above here
;; 2. add (list 'NAME NAME-prim) to toplevel-env, below
;; 3. add NAME to Scheme.xx implementation
;; 4. if you use map or apply on it internally, and the
;;    the implementation language cannot pass functions
;;    as arguments, then add NAME_proc to Scheme.xx 
;; -----------------------------------------------------

(define make-toplevel-env
  (lambda ()
    (let ((primitives 
	   (list
	    (list '* times-prim)
	    (list '+ plus-prim)
	    (list '- minus-prim)
	    (list '/ divide-prim)
	    (list '% modulo-prim)
	    (list '< lt-prim)
	    (list '<= lt-or-eq-prim)
	    (list '= equal-sign-prim)
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
	    (list 'caaaar caaaar-prim)
	    (list 'caaadr caaadr-prim)
	    (list 'caaar caaar-prim)
	    (list 'caadar caadar-prim)
	    (list 'caaddr caaddr-prim)
	    (list 'caadr caadr-prim)
	    (list 'caar caar-prim)
	    (list 'cadaar cadaar-prim)
	    (list 'cadadr cadadr-prim)
	    (list 'cadar cadar-prim)
	    (list 'caddar caddar-prim)
	    (list 'cadddr cadddr-prim)
	    (list 'cdaaar cdaaar-prim)
	    (list 'cdaadr cdaadr-prim)
	    (list 'cdaar cdaar-prim)
	    (list 'cdadar cdadar-prim)
	    (list 'cdaddr cdaddr-prim)
	    (list 'cdadr cdadr-prim)
	    (list 'cdar cdar-prim)
	    (list 'cddaar cddaar-prim)
	    (list 'cddadr cddadr-prim)
	    (list 'cddar cddar-prim)
	    (list 'cdddar cdddar-prim)
	    (list 'cddddr cddddr-prim)
	    (list 'cdddr cdddr-prim)
	    (list 'cddr cddr-prim)
	    (list 'char? char?-prim)
	    (list 'char=? char=?-prim)
	    (list 'char-whitespace? char-whitespace?-prim)
	    (list 'char-alphabetic? char-alphabetic?-prim)
	    (list 'char-numeric? char-numeric?-prim)
	    (list 'char->integer char->integer-prim)
	    (list 'cons cons-prim)
	    (list 'current-time current-time-prim)
	    (list 'cut cut-prim)
	    (list 'dir dir-prim)
	    (list 'display display-prim)
	    (list 'current-environment current-environment-prim)
	    (list 'eq? eq?-prim)
	    (list 'equal? equal?-prim)
	    (list 'error error-prim)
	    (list 'eval eval-prim)
	    (list 'eval-ast eval-ast-prim)
	    (list 'exit exit-prim)
	    (list 'for-each for-each-prim)
	    (list 'format format-prim)
	    (list 'get get-prim)
	    (list 'get-stack-trace get-stack-trace-prim)
	    (list 'import import-prim)
	    (list 'integer->char integer->char-prim)
	    (list 'length length-prim)
	    (list 'list list-prim)
	    (list 'list->vector list->vector-prim)
	    (list 'list->string list->string-prim)
	    (list 'list-ref list-ref-prim)
	    (list 'load load-prim)
	    (list 'make-set make-set-prim)
	    (list 'make-vector make-vector-prim)
	    (list 'map map-prim)
	    (list 'member member-prim)
	    (list 'memq memq-prim)
	    (list 'memv memv-prim)
	    (list 'newline newline-prim)
	    (list 'not not-prim)
	    (list 'null? null?-prim)
	    (list 'number->string number->string-prim)
	    (list 'number? number?-prim)
	    (list 'pair? pair?-prim)
	    (list 'parse parse-prim)
	    (list 'parse-string parse-string-prim)
	    (list 'print print-prim)
	    (list 'printf printf-prim)
	    (list 'range range-prim)
	    (list 'read-string read-string-prim)
	    (list 'require require-prim)
	    (list 'reverse reverse-prim)
	    (list 'set-car! set-car!-prim)
	    (list 'set-cdr! set-cdr!-prim)
	    (list 'snoc snoc-prim)
	    (list 'rac rac-prim)
	    (list 'rdc rdc-prim)
	    (list 'sqrt sqrt-prim)
	    (list 'odd? odd?-prim)
	    (list 'even? even?-prim)
	    (list 'quotient quotient-prim)
	    (list 'remainder remainder-prim)
	    (list 'string string-prim)
	    (list 'string-length string-length-prim)
	    (list 'string-ref string-ref-prim)
	    (list 'string? string?-prim)
	    (list 'string->number string->number-prim)
	    (list 'string=? string=?-prim)
	    (list 'substring substring-prim)
	    (list 'symbol? symbol?-prim)
	    (list 'unparse unparse-prim)    ;; unparse should be in CPS
	    (list 'unparse-procedure unparse-procedure-prim)  ;; unparse should be in CPS
	    (list 'using using-prim)
	    (list 'use-stack-trace use-stack-trace-prim)
	    (list 'vector vector-prim)
	    (list 'vector-ref vector-ref-prim)
	    (list 'vector-set! vector-set!-prim)
	    (list 'void void-prim)
	    (list 'zero? zero?-prim)
	    (list 'current-directory current-directory-prim)
	    (list 'cd current-directory-prim)
	    (list 'round round-prim)
	    (list 'char->string char->string-prim)
	    (list 'string->list string->list-prim)
	    (list 'string->symbol string->symbol-prim)
	    (list 'symbol->string symbol->string-prim)
	    (list 'vector->list vector->list-prim)
	    (list 'eqv? eqv?-prim)
	    (list 'vector? vector?-prim)
	    (list 'atom? atom?-prim)
	    (list 'iter? iter?-prim)
	    (list 'list? list?-prim)
	    (list 'procedure? procedure?-prim)
	    (list 'string<? string<?-prim)
 	    (list 'float float-prim)
 	    (list 'globals globals-prim)
 	    (list 'int int-prim)
 	    (list 'apply-with-keywords apply-with-keywords-prim)
 	    (list 'assq assq-prim)
 	    (list 'dict dict-prim)
 	    (list 'property property-prim)
 	    (list 'rational rational-prim)
 	    (list 'reset-toplevel-env reset-toplevel-env-prim)
 	    (list 'sort sort-prim)
 	    (list 'string-append string-append-prim)
 	    (list 'string-split string-split-prim)
 	    (list 'symbol symbol-prim)
 	    (list 'typeof typeof-prim)
 	    (list 'use-lexical-address use-lexical-address-prim)
	    (list 'use-tracing use-tracing-prim)
	    )))
      (make-initial-env-extended (map car primitives) (map cadr primitives)))))

;; this is here as a hook for extending environments in C# etc.
(define-native make-initial-env-extended
  (lambda (names procs)
    (make-initial-environment names procs)))

(define reset-toplevel-env
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    void-value))

(define toplevel-env 'undefined)

;;------------------------------------------------------------------------
;; C# support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 info handler fail k2)
      (k2 (apply* external-function-object args) fail))))

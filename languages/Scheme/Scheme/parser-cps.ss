;; Calico Scheme parser
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")

;;--------------------------------------------------------------------------
;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader-cps.ss")
(load "unifier-cps.ss")

;; The core grammar
;;
;; <exp> ::= <literal>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (define <var> <exp>)
;;         | (define <var> <docstring> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (<exp> <exp> ...)
;;         | (try <body> (catch <var> <exp> ...))
;;         | (try <body> (finally <exp> ...))
;;         | (try <body> (catch <var> <exp> ...) (finally <exp> ...))
;;         | (raise <exp>)
;;         | (dict (<exp> <exp>) ...)
;;         | (help <var>)
;;         | (choose <exp> ...)

(define-datatype expression expression?
  (lit-exp
   (datum anything?))
  (var-exp
    (id symbol?))
  (func-exp
    (exp expression?))
  (if-exp
   (test-exp expression?)
   (then-exp expression?)
   (else-exp expression?))
  (assign-exp
    (var symbol?)
    (rhs-exp expression?))
  (define-exp
    (id symbol?)
    (docstring string?)
    (rhs-exp expression?))
  (define!-exp
    (id symbol?)
    (docstring string?)
    (rhs-exp expression?))
  (define-syntax-exp
    (keyword symbol?)
    (clauses (list-of (list-of pattern?))))
  (begin-exp
    (exps (list-of expression?)))
  (lambda-exp
    (formals (list-of symbol?))
    (body expression?))
  (mu-lambda-exp
    (formals (list-of symbol?))
    (runt symbol?)
    (body expression?))
  (app-exp
    (operator expression?)
    (operands (list-of expression?)))
  (try-catch-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?)))
  (try-finally-exp
    (body expression?)
    (finally-exps (list-of expression?)))
  (try-catch-finally-exp
    (body expression?)
    (catch-var symbol?)
    (catch-exps (list-of expression?))
    (finally-exps (list-of expression?)))
  (raise-exp
    (exp expression?))
  (dict-exp
    (pairs (list-of (list-of expression?))))
  (help-exp
    (var symbol?))
  (choose-exp
    (exps (list-of expression?)))
  )

;;--------------------------------------------------------------------------
;; Macro support

(load "environments-cps.ss")

(define syntactic-sugar?
  (lambda (datum)
    (and (pair? datum)
	 (symbol? (car datum))
	 (true? (search-env macro-env (car datum))))))

(define make-pattern-macro
  (lambda (clauses)
    (cons 'pattern-macro clauses)))

(define macro-clauses
  (lambda (macro)
    (cdr macro)))

(define pattern-macro?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'pattern-macro))))

(define* expand-once
  (lambda (datum handler fail k)
    (lookup-value (car datum) macro-env handler fail
      (lambda-cont2 (macro fail)
	(if (pattern-macro? macro)
	  (process-macro-clauses (macro-clauses macro) datum handler fail k)
	  ;; macro transformer functions take 1-arg continuations:
	  (macro datum
	    (lambda-cont (expansion)
	      (k expansion fail))))))))

(define* process-macro-clauses
  (lambda (clauses datum handler fail k)
    (if (null? clauses)
      (handler (format "no matching clause found for ~a" datum) fail)
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses)))
	(unify-patterns left-pattern datum
	  (lambda-cont (subst)
	    (if subst
	      (instantiate right-pattern subst (lambda-cont (v) (k v fail)))
	      (process-macro-clauses (cdr clauses) datum handler fail k))))))))

(define mit-define-transformer
  (lambda-macro (datum k)
    (let ((name (caadr datum))
	  (formals (cdadr datum))
	  (bodies (cddr datum)))
      (k `(define ,name (lambda ,formals ,@bodies))))))

(define and-transformer
  (lambda-macro (datum k)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) (k '#t))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(if ,(car exps) (and ,@(cdr exps)) #f)))))))

;; avoids variable capture
(define or-transformer
  (lambda-macro (datum k)
    (let ((exps (cdr datum)))
      (cond
	((null? exps) (k '#f))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(let ((bool ,(car exps))
			(else-code (lambda () (or ,@(cdr exps)))))
		    (if bool bool (else-code)))))))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer
  (lambda-macro (datum k)
    (let ((clauses (cdr datum)))
      (if (null? clauses)
	(error 'cond-transformer "bad concrete syntax: ~a" datum)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null? first-clause) (not (list? first-clause)))
	    (error 'cond-transformer "bad concrete syntax: ~a" datum)
	    (let ((test-exp (car first-clause))
		  (then-exps (cdr first-clause)))
	      (cond
		((eq? test-exp 'else)
		 (cond
		   ((null? then-exps) (error 'cond-transformer "bad concrete syntax: (~a)" 'else))
		   ((null? (cdr then-exps)) (k (car then-exps)))
		   (else (k `(begin ,@then-exps)))))
		((null? then-exps)
		 (if (null? other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@other-clauses))))
			 (if bool bool (else-code))))))
		((null? other-clauses)
		 (if (null? (cdr then-exps))
		   (k `(if ,test-exp ,(car then-exps)))
		   (k `(if ,test-exp (begin ,@then-exps)))))
		((null? (cdr then-exps))
		 (k `(if ,test-exp ,(car then-exps) (cond ,@other-clauses))))
		(else (k `(if ,test-exp (begin ,@then-exps) (cond ,@other-clauses))))))))))))

(define let-transformer
  (lambda-macro (datum k)
    (if (symbol? (cadr datum))
      ;; named let
      (let* ((name (cadr datum))
	     (bindings (caddr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cdddr datum)))
	(k `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
      ;; ordinary let
      (let* ((bindings (cadr datum))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cddr datum)))
	(k `((lambda ,vars ,@bodies) ,@exps))))))

(define letrec-transformer
  (lambda-macro (datum k)
    (let* ((decls (cadr datum))
	   (vars (map car decls))
	   (procs (map cadr decls))
	   (bodies (cddr datum)))
      (create-letrec-assignments vars procs
	(lambda-cont2 (bindings assigns)
	  (k `(let ,bindings ,@assigns ,@bodies)))))))

(define* create-letrec-assignments
  (lambda (vars procs k2)
    (if (null? vars)
      (k2 '() '())
      (create-letrec-assignments (cdr vars) (cdr procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car vars) 'undefined) bindings)
	      (cons `(set! ,(car vars) ,(car procs)) assigns)))))))

(define let*-transformer
  (lambda-macro (datum k)
    (let ((bindings (cadr datum))
	  (bodies (cddr datum)))
      (nest-let*-bindings bindings bodies k))))

(define* nest-let*-bindings
  (lambda (bindings bodies k)
    (if (or (null? bindings)
	    (null? (cdr bindings)))
	(k `(let ,bindings ,@bodies))
	(nest-let*-bindings (cdr bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer
  (lambda-macro (datum k)
    (let ((exp (cadr datum))
	  (clauses (cddr datum)))
      ;; if exp is a variable, no need to introduce r binding
      (if (symbol? exp)
	(case-clauses->simple-cond-clauses exp clauses
	  (lambda-cont (new-clauses)
	    (k `(cond ,@new-clauses))))
	(case-clauses->cond-clauses 'r clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses)))))))))

(define* case-clauses->simple-cond-clauses
  (lambda (var clauses k)
    (if (null? clauses)
      (k '())
      (case-clauses->simple-cond-clauses var (cdr clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car clauses)))
	    (cond
	      ((eq? (car clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol? (car clause))
	       (k (cons `((eq? ,var ',(car clause)) ,@(cdr clause)) new-clauses)))
	      (else (k (cons `((memq ,var ',(car clause)) ,@(cdr clause))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (case-clauses->cond-clauses var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr clause))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol? (car clause))
		(let ((name (car clause)))
		  (k2 (cons `(,name (lambda () ,@(cdr clause))) bindings)
		      (cons `((eq? ,var ',(car clause)) (,name)) new-clauses)))
		(let ((name (caar clause)))
		  (k2 (cons `(,name (lambda () ,@(cdr clause))) bindings)
		      (cons `((memq ,var ',(car clause)) (,name)) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer
  (lambda-macro (datum k)
    (let ((exp (cadr datum))
	  (clauses (cddr datum)))
      ;; if exp is a variable, no need to introduce r binding
      (if (symbol? exp)
	(record-case-clauses->cond-clauses exp clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ,bindings (cond ,@new-clauses)))))
	(record-case-clauses->cond-clauses 'r clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses)))))))))

(define* record-case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr clause))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol? (car clause))
		(let ((name (car clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((eq? (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((memq (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
      (list and-transformer
	    or-transformer
	    cond-transformer
	    let-transformer
	    letrec-transformer
	    let*-transformer
	    case-transformer
	    record-case-transformer))))

(define macro-env (make-macro-env))

;;--------------------------------------------------------------------------

(define* parse
  (lambda (datum handler fail k)
    (cond
      ((literal? datum) (k (lit-exp datum) fail))
      ((quote? datum) (k (lit-exp (cadr datum)) fail))
      ((quasiquote? datum)
       (expand-quasiquote (cadr datum)
	 (lambda-cont (v)
	   (parse v handler fail k))))
      ((unquote? datum) (handler (format "misplaced ~a" datum) fail))
      ((unquote-splicing? datum) (handler (format "misplaced ~a" datum) fail))
      ((symbol? datum) (k (var-exp datum) fail))
      ((syntactic-sugar? datum)
       (expand-once datum handler fail
	 (lambda-cont2 (v fail)
	   (parse v handler fail k))))
      ((if-then? datum)
       (parse (cadr datum) handler fail
	 (lambda-cont2 (v1 fail)
	   (parse (caddr datum) handler fail
	     (lambda-cont2 (v2 fail)
	       (k (if-exp v1 v2 (lit-exp #f)) fail))))))
      ((if-else? datum)
       (parse (cadr datum) handler fail
	 (lambda-cont2 (v1 fail)
	   (parse (caddr datum) handler fail
	     (lambda-cont2 (v2 fail)
	       (parse (cadddr datum) handler fail
		 (lambda-cont2 (v3 fail)
		   (k (if-exp v1 v2 v3) fail))))))))
      ((assignment? datum)
       (parse (caddr datum) handler fail
	 (lambda-cont2 (v fail)
	   (k (assign-exp (cadr datum) v) fail))))
      ((func? datum) (parse (cadr datum) handler fail
                       (lambda-cont2 (e fail)
                         (k (func-exp e) fail))))
      ((define? datum)
       (cond
	 ((mit-style? datum)
	  (mit-define-transformer datum
	    (lambda-cont (v)
	      (parse v handler fail k))))
	 ((= (length datum) 3) ;; (define <var> <body>)
	  (parse (caddr datum) handler fail
	    (lambda-cont2 (body fail)
	      (k (define-exp (cadr datum) "" body) fail))))
	 ((and (= (length datum) 4) (string? (caddr datum))) ;; (define <var> <docstring> <body>)
	  (parse (cadddr datum) handler fail
	    (lambda-cont2 (body fail)
	      (k (define-exp (cadr datum) (caddr datum) body) fail))))
	 (else (handler (format "bad concrete syntax: ~a" datum) fail))))
      ((define!? datum)
       (cond
	 ((mit-style? datum)
	  (mit-define-transformer datum
	    (lambda-cont (v)
	      (parse v handler fail k))))
	 ((= (length datum) 3) ;; (define! <var> <body>)
	  (parse (caddr datum) handler fail
	    (lambda-cont2 (body fail)
	      (k (define!-exp (cadr datum) "" body) fail))))
	 ((and (= (length datum) 4) (string? (caddr datum))) ;; (define! <var> <docstring> <body>)
	  (parse (cadddr datum) handler fail
	    (lambda-cont2 (body fail)
	      (k (define!-exp (cadr datum) (caddr datum) body) fail))))
	 (else (handler (format "bad concrete syntax: ~a" datum) fail))))
      ((define-syntax? datum)
       (k (define-syntax-exp (cadr datum) (cddr datum)) fail))
      ((begin? datum)
       (parse-all (cdr datum) handler fail
	 (lambda-cont2 (v fail)
	   (cond
	     ((null? v) (handler (format "bad concrete syntax: ~a" datum) fail))
	     ((null? (cdr v)) (k (car v) fail))
	     (else (k (begin-exp v) fail))))))
      ((lambda? datum)
       (parse (cons 'begin (cddr datum)) handler fail
	 (lambda-cont2 (body fail)
	   (if (list? (cadr datum))
	     (k (lambda-exp (cadr datum) body) fail)
	     (k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) body) fail)))))
      ((try? datum)
       (cond
	 ((= (length datum) 2)
	  ;; (try <body>)
	  (parse (try-body datum) handler fail k))
	 ((and (= (length datum) 3) (catch? (caddr datum)))
	  ;; (try <body> (catch <var> <exp> ...))
	  (parse (try-body datum) handler fail
	    (lambda-cont2 (body fail)
	      (parse-all (catch-exps (caddr datum)) handler fail
		(lambda-cont2 (cexps fail)
		  (let ((cvar (catch-var (caddr datum))))
		    (k (try-catch-exp body cvar cexps) fail)))))))
	 ((and (= (length datum) 3) (finally? (caddr datum)))
	  ;; (try <body> (finally <exp> ...))
	  (parse (try-body datum) handler fail
	    (lambda-cont2 (body fail)
	      (parse-all (finally-exps (caddr datum)) handler fail
		(lambda-cont2 (fexps fail)
		  (k (try-finally-exp body fexps) fail))))))
	 ((and (= (length datum) 4) (catch? (caddr datum)) (finally? (cadddr datum)))
	  ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	  (parse (try-body datum) handler fail
	    (lambda-cont2 (body fail)
	      (parse-all (catch-exps (caddr datum)) handler fail
		(lambda-cont2 (cexps fail)
		  (parse-all (finally-exps (cadddr datum)) handler fail
		    (lambda-cont2 (fexps fail)
		      (let ((cvar (catch-var (caddr datum))))
			(k (try-catch-finally-exp body cvar cexps fexps) fail)))))))))
	 (else (handler (format "bad try syntax: ~a" datum) fail))))
      ((raise? datum)
       (parse (cadr datum) handler fail
	 (lambda-cont2 (v fail)
	   (k (raise-exp v) fail))))
      ((dict? datum)
       (parse-pairs (cdr datum) handler fail
	 (lambda-cont2 (v1 fail)
	   (k (dict-exp v1) fail))))
      ((help? datum)
       (if (symbol? (cadr datum))
	 (k (help-exp (cadr datum)) fail)
	 (handler (format "bad concrete syntax: ~a" datum) fail)))
      ((choose? datum)
       (parse-all (cdr datum) handler fail
	 (lambda-cont2 (exps fail)
	   (k (choose-exp exps) fail))))
      ((application? datum)
       (parse (car datum) handler fail
	 (lambda-cont2 (v1 fail)
	   (parse-all (cdr datum) handler fail
	     (lambda-cont2 (v2 fail)
	       (k (app-exp v1 v2) fail))))))
      (else (handler (format "bad concrete syntax: ~a" datum) fail)))))

(define* parse-pairs
  (lambda (pairs handler fail k)
    (if (null? pairs)
      (k '() fail)
      (parse (caar pairs) handler fail
	(lambda-cont2 (a fail)
	  (parse (cadar pairs) handler fail
	    (lambda-cont2 (b fail)
              (parse-pairs (cdr pairs) handler fail
		(lambda-cont2 (results fail)
		  (k (cons (list a b) results) fail))))))))))

(define* parse-all
  (lambda (datum-list handler fail k)
    (if (null? datum-list)
      (k '() fail)
      (parse (car datum-list) handler fail
	(lambda-cont2 (a fail)
	  (parse-all (cdr datum-list) handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* expand-quasiquote
  (lambda (datum k)
    (cond
      ((vector? datum)
       (expand-quasiquote (vector->list datum)
	 (lambda-cont (ls) (k `(list->vector ,ls)))))
      ((not (pair? datum)) (k `(quote ,datum)))
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) (k `(quote ,datum)))
      ((unquote? datum) (k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
	 (k (cadr (car datum)))
	 (expand-quasiquote (cdr datum)
	   (lambda-cont (v) (k `(append ,(cadr (car datum)) ,v))))))
      ((quasiquote-list? datum)
       (expand-quasiquote-list datum
	 (lambda-cont (v)
	   (k `(list ,@v)))))
      (else
	(expand-quasiquote (car datum)
	  (lambda-cont (v1)
	    (expand-quasiquote (cdr datum)
	      (lambda-cont (v2)
		(k `(cons ,v1 ,v2))))))))))

(define* expand-quasiquote-list
  (lambda (datum k)
    (if (null? datum)
      (k '())
      (expand-quasiquote (car datum)
	(lambda-cont (v1)
	  (expand-quasiquote-list (cdr datum)
	    (lambda-cont (v2)
	       (k (cons v1 v2)))))))))

(define quasiquote-list?
  (lambda (datum)
    (or (null? datum)
	(and (pair? datum)
	     ;; doesn't handle nested quasiquotes yet
	     (not (quasiquote? datum))
	     (not (unquote? datum))
	     (not (unquote-splicing? datum))
	     ;; doesn't handle nested quasiquotes yet
	     (not (quasiquote? (car datum)))
	     (not (unquote-splicing? (car datum)))
	     (quasiquote-list? (cdr datum))))))

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((pair? (cdr formals)) (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define mit-style?
  (lambda (datum)
    (not (symbol? (cadr datum)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
	(boolean? datum)
	(char? datum)
	(string? datum)
	(vector? datum))))

(define anything?
  (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
	   (op (length datum) len)
	   (eq? (car datum) tag)))))

(define quote? (tagged-list 'quote = 2))
(define func? (tagged-list 'func = 2))
(define quasiquote? (tagged-list 'quasiquote = 2))
(define unquote? (tagged-list 'unquote = 2))
(define unquote-splicing? (tagged-list 'unquote-splicing = 2))
(define if-then? (tagged-list 'if = 3))
(define if-else? (tagged-list 'if = 4))
(define assignment? (tagged-list 'set! = 3))
(define define? (tagged-list 'define >= 3))
(define define!? (tagged-list 'define! >= 3))
(define define-syntax? (tagged-list 'define-syntax >= 3))
(define begin? (tagged-list 'begin >= 2))
(define lambda? (tagged-list 'lambda >= 3))
(define raise? (tagged-list 'raise = 2))
(define dict? (tagged-list 'dict >= 1))
(define help? (tagged-list 'help = 2))
(define choose? (tagged-list 'choose >= 1))
(define try? (tagged-list 'try >= 2))
(define try-body (lambda (x) (cadr x)))
(define catch? (tagged-list 'catch >= 3))
(define catch-var (lambda (x) (cadr x)))
(define catch-exps (lambda (x) (cddr x)))
(define finally? (tagged-list 'finally >= 2))
(define finally-exps (lambda (x) (cdr x)))

(define application?
  (lambda (datum)
    (and (list? datum)
	 (not (null? datum))
	 (not (reserved-keyword? (car datum))))))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define
	    begin cond and or let let* letrec case record-case
	    try catch finally raise dict help choose)))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
	 (memq x (get-reserved-keywords)))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(define parse-string
  (lambda (string)
    (read-datum string init-handler2 init-fail
      (lambda-cont3 (datum tokens-left fail)
	(parse datum init-handler2 init-fail init-cont2)))))

;;(define parse-file
;;  (lambda (filename)
;;    (get-parsed-sexps filename)))

(define print-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (get-parsed-sexps filename))))

(define get-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(parse-sexps tokens init-handler2 init-fail init-cont2)))))

(define* parse-sexps
  (lambda (tokens handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens handler fail
	(lambda-cont3 (datum tokens-left fail)
	  (parse datum handler fail
	    (lambda-cont2 (exp fail)
	      (parse-sexps tokens-left handler fail
		(lambda-cont2 (v fail)
		  (k (cons exp v) fail))))))))))

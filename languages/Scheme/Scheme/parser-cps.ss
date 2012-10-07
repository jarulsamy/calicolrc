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
;;         | <vector>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (func <exp>)
;;         | (define <var> <exp>)
;;         | (define <var> <docstring> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (trace-lambda name (<formal> ...) <exp> ...)
;;         | (trace-lambda name <formal> <exp> ...)
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
  (if-exp
   (test-exp expression?)
   (then-exp expression?)
   (else-exp expression?))
  (assign-exp
    (var symbol?)
    (rhs-exp expression?))
  (func-exp
    (exp expression?))
  (define-exp
    (id symbol?)
    (docstring string?)
    (rhs-exp expression?))
  (define!-exp
    (id symbol?)
    (docstring string?)
    (rhs-exp expression?))
  (define-syntax-exp
    (name symbol?)
    (clauses (list-of (list-of pattern?))))
  (begin-exp
    (exps (list-of expression?)))
  (lambda-exp
    (formals (list-of symbol?))
    (bodies (list-of expression?)))
  (mu-lambda-exp
    (formals (list-of symbol?))
    (runt symbol?)
    (bodies (list-of expression?)))
  (trace-lambda-exp
    (name symbol?)
    (formals (list-of symbol?))
    (bodies (list-of expression?)))
  (mu-trace-lambda-exp
    (name symbol?)
    (formals (list-of symbol?))
    (runt symbol?)
    (bodies (list-of expression?)))
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
    (entries (list-of (list-of expression?))))
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
    (lookup-value (car datum) macro-env 'none handler fail
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
      (parse-error "no matching clause found for" datum handler fail)
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

(define* macro-error
  (lambda (transformer-name datum)
    (error transformer-name "bad concrete syntax: ~a" datum)))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer
  (lambda-macro (datum k)
    (let ((clauses (cdr datum)))
      (if (null? clauses)
	(macro-error 'cond-transformer datum)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null? first-clause) (not (list? first-clause)))
	    (macro-error 'cond-transformer datum)
	    (let ((test-exp (car first-clause))
		  (then-exps (cdr first-clause)))
	      (cond
		((eq? test-exp 'else)
		 (cond
		   ((null? then-exps) (macro-error 'cond-transformer '(else)))
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

;;--------------------------------------------------------------------------
;; FIXME? rename parse-cps

(define* parse
  (lambda (datum handler fail k)
    (cond
      ((null? datum) (k (lit-exp datum) fail))
      ((literal? datum) (k (lit-exp datum) fail))
      ((vector? datum) (k (lit-exp datum) fail))
      ((symbol? datum) (k (var-exp datum) fail))
      ((quote? datum) (k (lit-exp (cadr datum)) fail))
      ((quasiquote? datum)
       (qq-expand-cps_ (cadr datum) 0
	 (lambda-cont (expansion)
	   (parse expansion handler fail k))))
      ((unquote? datum) (parse-error "misplaced" datum handler fail))
      ((unquote-splicing? datum) (parse-error "misplaced" datum handler fail))
      ((syntactic-sugar? datum)
       (expand-once datum handler fail
	 (lambda-cont2 (expansion fail)
	   (parse expansion handler fail k))))
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
      ((func? datum)
       (parse (cadr datum) handler fail
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
	 (else (parse-error "bad concrete syntax:" datum handler fail))))
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
	 (else (parse-error "bad concrete syntax:" datum handler fail))))
      ((define-syntax? datum)
       (k (define-syntax-exp (cadr datum) (cddr datum)) fail))
      ((begin? datum)
       (parse-all (cdr datum) handler fail
	 (lambda-cont2 (v fail)
	   (cond
	     ((null? v) (parse-error "bad concrete syntax:" datum handler fail))
	     ((null? (cdr v)) (k (car v) fail))
	     (else (k (begin-exp v) fail))))))
      ((lambda? datum)
       (parse-all (cddr datum) handler fail
	 (lambda-cont2 (bodies fail)
	   (if (list? (cadr datum))
	     (k (lambda-exp (cadr datum) bodies) fail)
	     (k (mu-lambda-exp (head (cadr datum)) (last (cadr datum)) bodies) fail)))))
      ((trace-lambda? datum)
       (parse-all (cdddr datum) handler fail
	 (lambda-cont2 (bodies fail)
	   (if (list? (caddr datum))
	     (k (trace-lambda-exp (cadr datum) (caddr datum) bodies) fail)
	     (k (mu-trace-lambda-exp (cadr datum) (head (caddr datum)) (last (caddr datum)) bodies) fail)))))
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
	 (else (parse-error "bad try syntax:" datum handler fail))))
      ((raise? datum)
       (parse (cadr datum) handler fail
	 (lambda-cont2 (v fail)
	   (k (raise-exp v) fail))))
      ((dict? datum)
       (parse-entries (cdr datum) handler fail
	 (lambda-cont2 (v1 fail)
	   (k (dict-exp v1) fail))))
      ((help? datum)
       (if (symbol? (cadr datum))
	 (k (help-exp (cadr datum)) fail)
	 (parse-error "bad concrete syntax:" datum handler fail)))
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
      (else (parse-error "bad concrete syntax:" datum handler fail)))))

(define* parse-error
  (lambda (msg datum handler fail)
    (handler (format "parse error: ~a ~s" msg datum) fail)))

;; for dicts
(define* parse-entries
  (lambda (entries handler fail k)
    (if (null? entries)
      (k '() fail)
      (parse (caar entries) handler fail
	(lambda-cont2 (a fail)
	  (parse (cadar entries) handler fail
	    (lambda-cont2 (b fail)
              (parse-entries (cdr entries) handler fail
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

;; quasiquote code goes here

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
	(string? datum))))

(define anything?
  (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
	   (op (length datum) len)
	   (eq? (car datum) tag)))))

(define quote? (tagged-list 'quote = 2))
(define quasiquote? (tagged-list 'quasiquote = 2))
(define unquote? (tagged-list 'unquote >= 2))   ;; >= for use with alan bawden's qq-expand algorithm
(define unquote-splicing? (tagged-list 'unquote-splicing >= 2))
(define if-then? (tagged-list 'if = 3))
(define if-else? (tagged-list 'if = 4))
(define assignment? (tagged-list 'set! = 3))
(define func? (tagged-list 'func = 2))
(define define? (tagged-list 'define >= 3))
(define define!? (tagged-list 'define! >= 3))
(define define-syntax? (tagged-list 'define-syntax >= 3))
(define begin? (tagged-list 'begin >= 2))
(define lambda? (tagged-list 'lambda >= 3))
;;(define trace-lambda? (tagged-list 'trace-lambda >= 4)) ;; not needed in C#
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
	 (not (eq? (memq x (get-reserved-keywords)) #f)))))

;;------------------------------------------------------------------------
;; annotated s-expressions

(define let-transformer^
  (lambda-macro (adatum k)
    (if (symbol?^ (cadr^ adatum))
      ;; named let
      (let* ((name (cadr^ adatum))
	     (bindings (caddr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cdddr^ adatum)))
	(k `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
      ;; ordinary let
      (let* ((bindings (cadr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cddr^ adatum)))
	(k `((lambda ,vars ,@bodies) ,@exps))))))

(define letrec-transformer^
  (lambda-macro (adatum k)
    (let* ((decls (cadr^ adatum))
	   (vars (map^ car^ decls))
	   (procs (map^ cadr^ decls))
	   (bodies (cddr^ adatum)))
      (create-letrec-assignments^ vars procs
	(lambda-cont2 (bindings assigns)
	  (k `(let ,bindings ,@assigns ,@bodies)))))))

(define* create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null? vars)
      (k2 '() '())
      (create-letrec-assignments^ (cdr vars) (cdr procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car vars) 'undefined) bindings)
	      (cons `(set! ,(car vars) ,(car procs)) assigns)))))))

(define mit-define-transformer^
  (lambda-macro (adatum k)
    (let ((name (car^ (cadr^ adatum)))
	  (formals (cdr^ (cadr^ adatum)))
	  (bodies (cddr^ adatum)))
      (k `(define ,name (lambda ,formals ,@bodies))))))

(define and-transformer^
  (lambda-macro (adatum k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null? exps) (k '#t))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(if ,(car exps) (and ,@(cdr exps)) #f)))))))

;; avoids variable capture
(define or-transformer^
  (lambda-macro (adatum k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null? exps) (k '#f))
	((null? (cdr exps)) (k (car exps)))
	(else (k `(let ((bool ,(car exps))
			(else-code (lambda () (or ,@(cdr exps)))))
		    (if bool bool (else-code)))))))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer^
  (lambda-macro (adatum k)
    (let ((clauses (cdr^ adatum)))
      (if (null? clauses)
	(amacro-error 'cond-transformer^ adatum)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null?^ first-clause) (not (list?^ first-clause)))
	    (amacro-error 'cond-transformer^ adatum)
	    (let ((test-exp (car^ first-clause))
		  (then-exps (cdr^ first-clause)))
	      (cond
		((eq?^ test-exp 'else)
		 (cond
		   ((null? then-exps) (amacro-error 'cond-transformer^ '(else)))
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

(define let*-transformer^
  (lambda-macro (adatum k)
    (let ((bindings (get-sexp (cadr^ adatum)))
	  (bodies (cddr^ adatum)))
      (nest-let*-bindings^ bindings bodies k))))

(define* nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null? bindings)
	    (null? (cdr bindings)))
	(k `(let ,bindings ,@bodies))
	(nest-let*-bindings^ (cdr bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer^
  (lambda-macro (adatum k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (case-clauses->cond-clauses^ 'r clauses
	  (lambda-cont2 (bindings new-clauses)
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null? clauses)
      (k '())
      (case-clauses->simple-cond-clauses^ var (cdr clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car clauses)))
	    (cond
	      ((eq?^ (car^ clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol?^ (car^ clause))
	       (k (cons `((eq? ,var ',(get-sexp (car^ clause))) ,@(cdr^ clause)) new-clauses)))
	      (else (k (cons `((memq ,var ',(get-sexp (car^ clause))) ,@(cdr^ clause))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (case-clauses->cond-clauses^ var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr^ clause))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (get-sexp (car^ clause))))
		  (k2 (cons `(,name (lambda () ,@(cdr^ clause))) bindings)
		      (cons `((eq? ,var ',(car^ clause)) (,name)) new-clauses)))
		(let ((name (get-sexp (car^ (car^ clause)))))
		  (k2 (cons `(,name (lambda () ,@(cdr^ clause))) bindings)
		      (cons `((memq ,var ',(car^ clause)) (,name)) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer^
  (lambda-macro (adatum k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)
	  (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses^ var (cdr clauses)
	(lambda-cont2 (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr^ clause))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (get-sexp (car^ clause))))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(cddr^ clause))) bindings)
		      (cons `((eq? (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (get-sexp (car^ (car^ clause)))))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(cddr^ clause))) bindings)
		      (cons `((memq (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

(define define-datatype-variant-names
  (lambda (variants)
    (cond
     ((null? variants) '())
     (else (cons (get-sexp (car^ (car variants)))
		 (define-datatype-variant-names (cdr variants)))))))

(define define-datatype-variant-tests
  (lambda (variants)
    (cond
     ((null? variants) '())
     ((null? (cdr^ (car variants))) ;; no test
      (cons '() (define-datatype-variant-tests (cdr variants))))
     (else (cons (get-sexp (cadr^ (cadr^ (car variants))))
		 (define-datatype-variant-tests (cdr variants)))))))

(define make-define-datatype-defines
  (lambda (names tests)
    (cond
     ((null? names) '())
     ((null? (car tests)) ;; no test
      (cons `(define ,(car names) 
	       (lambda args 
		 (cons ',(car names) args)))
	    (make-define-datatype-defines (cdr names) (cdr tests))))
     (else (cons `(define ,(car names) 
		    (lambda args 
		      (if (apply ,(car tests) (list (car args)))
			  (cons ',(car names) args)
			  (error ',(car names) "'~s' not a valid value" (car args)))))
		 (make-define-datatype-defines (cdr names) (cdr tests)))))))

(define define-datatype-transformer^
  (lambda-macro (adatum k)
    (let* ((datatype-name (get-sexp (cadr^ adatum)))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string datatype-name) "?"))))
      (if (not (eq?^ (cadr (cdr^ adatum)) type-tester-name))
	;; type tester function must be named type-tester-name
	(amacro-error 'define-datatype-transformer^ adatum)
	(let* ((variants (cddr (cdr^ adatum)))
	       (variant-names (define-datatype-variant-names variants))
	       (variant-tests (define-datatype-variant-tests variants))
	       (tester-def `(define ,type-tester-name
			      (lambda (x)
				(and (pair? x)
				     (not (not (memq (car x) ',variant-names))))))))
	  (k `(begin
		,tester-def
		,@(make-define-datatype-defines variant-names variant-tests))))))))

(define cases-transformer^
  (lambda-macro (adatum k)
    (let* ((type-name (get-sexp (cadr^ adatum)))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string type-name) "?")))
	   (exp (caddr^ adatum))
	   (clauses (cdddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)
	  (k `(let ((r ,exp) ,@bindings)
		(if (not (,type-tester-name r))
		  (error 'cases "~a is not a valid ~a" r ',type-name)
		  (cond ,@new-clauses)))))))))

;;(define make-macro-env
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
;;      (list and-transformer
;;	    or-transformer
;;	    cond-transformer
;;	    let-transformer
;;	    letrec-transformer
;;	    let*-transformer
;;	    case-transformer
;;	    record-case-transformer))))

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'and 
	    'or 
	    'cond 
	    'let 
	    'letrec 
	    'let* 
	    'case 
	    'record-case
	    'define-datatype 
	    'cases
	    )
      (list and-transformer^
	    or-transformer^
	    cond-transformer^
	    let-transformer^
	    letrec-transformer^
	    let*-transformer^
	    case-transformer^
	    record-case-transformer^
	    define-datatype-transformer^
	    cases-transformer^
	    ))))

(define macro-env (make-macro-env^))

(define* amacro-error
  (lambda (transformer-name adatum)
    (unannotate-cps adatum
      (lambda-cont (datum)
	(error transformer-name "bad concrete syntax: ~a" datum)))))

(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define macro-clauses^
  (lambda (macro)
    (cadr macro)))

(define macro-aclauses^
  (lambda (macro)
    (caddr macro)))

(define-datatype aexpression aexpression?
  (lit-aexp
    (datum anything?)
    (info source-info?))
  (var-aexp
    (id symbol?)
    (info source-info?))
  (if-aexp
    (test-aexp aexpression?)
    (then-aexp aexpression?)
    (else-aexp aexpression?)
    (info source-info?))
  (assign-aexp
    (var symbol?)
    (rhs-exp aexpression?)
    (var-info source-info?)
    (info source-info?))
  (func-aexp
    (exp aexpression?)
    (info source-info?))
  (define-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define!-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define-syntax-aexp
    (name symbol?)
    (clauses (list-of (list-of pattern?)))
    (aclauses list-of-asexp?)
    (info source-info?))
  (begin-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  (lambda-aexp
    (formals (list-of symbol?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-lambda-aexp
    (formals (list-of symbol?))
    (runt symbol?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (trace-lambda-aexp
    (name symbol?)
    (formals (list-of symbol?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-trace-lambda-aexp
    (name symbol?)
    (formals (list-of symbol?))
    (runt symbol?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (app-aexp
    (operator aexpression?)
    (operands (list-of aexpression?))
    (info source-info?))
  (try-catch-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (info source-info?))
  (try-finally-aexp
    (body aexpression?)
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (try-catch-finally-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (raise-aexp
    (exp aexpression?)
    (info source-info?))
  (dict-aexp
    (entries (list-of (list-of aexpression?)))
    (info source-info?))
  (help-aexp
    (var symbol?)
    (var-info source-info?)  ;; probably don't need this, but doesn't hurt
    (info source-info?))
  (choose-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  )

(define application?^
  (lambda (asexp)
    (and (list?^ asexp)
	 (not (null?^ asexp))
	 (not (reserved-keyword? (get-sexp (car^ asexp)))))))

(define mit-style?^
  (lambda (asexp)
    (not (symbol?^ (cadr^ asexp)))))

(define literal?^
  (lambda (asexp)
    (let ((s (get-sexp asexp)))
      (or (number? s) (boolean? s) (char? s) (string? s)))))

(define syntactic-sugar?^
  (lambda (asexp)
    (and (pair?^ asexp)
	 (symbol?^ (car^ asexp))
	 (true? (search-env macro-env (get-sexp (car^ asexp)))))))

(define tagged-list^
  (lambda (tag op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq? (get-sexp (car^ asexp)) tag)))))

(define quote?^ (tagged-list^ 'quote = 2))
(define quasiquote?^ (tagged-list^ 'quasiquote = 2))
(define unquote?^ (tagged-list^ 'unquote >= 2))
(define unquote-splicing?^ (tagged-list^ 'unquote-splicing >= 2))
(define if-then?^ (tagged-list^ 'if = 3))
(define if-else?^ (tagged-list^ 'if = 4))
(define assignment?^ (tagged-list^ 'set! = 3))
(define func?^ (tagged-list^ 'func = 2))
(define define?^ (tagged-list^ 'define >= 3))
(define define!?^ (tagged-list^ 'define! >= 3))
(define define-syntax?^ (tagged-list^ 'define-syntax >= 3))
(define begin?^ (tagged-list^ 'begin >= 2))
(define lambda?^ (tagged-list^ 'lambda >= 3))
(define trace-lambda?^ (tagged-list^ 'trace-lambda >= 4))
(define raise?^ (tagged-list^ 'raise = 2))
(define dict?^ (tagged-list^ 'dict >= 1))
(define help?^ (tagged-list^ 'help = 2))
(define choose?^ (tagged-list^ 'choose >= 1))
(define try?^ (tagged-list^ 'try >= 2))
(define try-body^ (lambda (x) (cadr^ x)))
(define catch?^ (tagged-list^ 'catch >= 3))
(define catch-var^ (lambda (x) (cadr^ x)))
(define catch-exps^ (lambda (x) (cddr^ x)))
(define finally?^ (tagged-list^ 'finally >= 2))
(define finally-exps^ (lambda (x) (cdr (get-sexp x))))

;; <adatum> ::= (asexp <sexp> <info>)

;; FIXME: rewrite in cps:

(define* aparse
  (lambda (adatum handler fail k)
    (let ((info (get-source-info adatum)))
      (cond
        ((null?^ adatum) (k (lit-aexp (get-sexp adatum) info) fail))
	((literal?^ adatum) (k (lit-aexp (get-sexp adatum) info) fail))
	((vector?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp v info) fail))))
	((symbol?^ adatum) (k (var-aexp (get-sexp adatum) info) fail))
	((quote?^ adatum)
	 (unannotate-cps (cadr^ adatum)
	   (lambda-cont (v)
	     (k (lit-aexp v info) fail))))
	((quasiquote?^ adatum)
	 (qq-expand-cps (cadr^ adatum) 0
	   (lambda-cont (v)
	     (reannotate-cps v
	       (lambda-cont (aexpansion)
		 (let ((info (get-source-info adatum)))
		   (if (original-source-info? adatum)
		     (aparse (replace-info aexpansion (snoc 'quasiquote info)) handler fail k)
		     (aparse (replace-info aexpansion info) handler fail k))))))))
	((unquote?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((unquote-splicing?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((syntactic-sugar?^ adatum)
	 (expand-once^ adatum handler fail
	   (lambda-cont2 (aexpansion fail)
	     (aparse aexpansion handler fail k))))
	((if-then?^ adatum)
	 (aparse (cadr^ adatum) handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) handler fail
	       (lambda-cont2 (v2 fail)
		 (k (if-aexp v1 v2 (lit-aexp #f 'none) info) fail))))))
	((if-else?^ adatum)
	 (aparse (cadr^ adatum) handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) handler fail
	       (lambda-cont2 (v2 fail)
		 (aparse (cadddr^ adatum) handler fail
		   (lambda-cont2 (v3 fail)
		     (k (if-aexp v1 v2 v3 info) fail))))))))
	((assignment?^ adatum)
	 (aparse (caddr^ adatum) handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (assign-aexp (get-sexp (cadr^ adatum)) v var-info info) fail)))))
	((func?^ adatum)
	 (aparse (cadr^ adatum) handler fail
	   (lambda-cont2 (e fail)
	     (k (func-aexp e info) fail))))
	((define?^ adatum)
	 (cond
	   ((mit-style?^ adatum)
	    (mit-define-transformer^ adatum
	      (lambda-cont (v)
		(reannotate-cps v
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define <var> <body>)
	    (aparse (caddr^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (get-sexp (cadr^ adatum)) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (get-sexp (cadr^ adatum)) (get-sexp (caddr^ adatum)) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define!?^ adatum)
	 (cond
	   ((mit-style?^ adatum)
	    (mit-define-transformer^ adatum
	      (lambda-cont (v)
		(reannotate-cps v
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define! <var> <body>)
	    (aparse (caddr^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (get-sexp (cadr^ adatum)) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define! <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (get-sexp (cadr^ adatum)) (get-sexp (caddr^ adatum)) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define-syntax?^ adatum)
	 (let ((name (get-sexp (cadr^ adatum)))
	       (aclauses (cddr^ adatum)))
	   (unannotate-cps aclauses
	     (lambda-cont (clauses)
	       (k (define-syntax-aexp name clauses aclauses info) fail)))))
	((begin?^ adatum)
	 (aparse-all (cdr^ adatum) handler fail
	   (lambda-cont2 (v fail)
	     (cond
	       ((null? v) (aparse-error "bad concrete syntax:" adatum handler fail))
	       ((null? (cdr v)) (k (car v) fail))
	       (else (k (begin-aexp v info) fail))))))
	((lambda?^ adatum)
	 (aparse-all (cddr^ adatum) handler fail
	   (lambda-cont2 (bodies fail)
	     (unannotate-cps (cadr^ adatum)
	       (lambda-cont (formals)
		 (if (list? formals)
		   (k (lambda-aexp formals bodies info) fail)
		   (k (mu-lambda-aexp (head formals) (last formals) bodies info) fail)))))))
	((trace-lambda?^ adatum)
	 (aparse-all (cdddr^ adatum) handler fail
	   (lambda-cont2 (bodies fail)
	     (unannotate-cps (caddr^ adatum)
	       (lambda-cont (formals)
		 (if (list? formals)
		   (k (trace-lambda-aexp (get-sexp (cadr^ adatum)) formals bodies info) fail)
		   (k (mu-trace-lambda-aexp (get-sexp (cadr^ adatum)) (head formals) (last formals) bodies info) fail)))))))
	((try?^ adatum)
	 ;; fix: this code is horrendously UGLY!
	 (cond
	   ((= (length^ adatum) 2)
	    ;; (try <body>)
	    (aparse (try-body^ adatum) handler fail k))
	   ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
	    ;; (try <body> (catch <var> <exp> ...))
	    (aparse (try-body^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (catch-exps^ (caddr^ adatum)) handler fail
		  (lambda-cont2 (cexps fail)
		    (let ((cvar (get-sexp (catch-var^ (caddr^ adatum)))))
		      (k (try-catch-aexp body cvar cexps info) fail)))))))
	   ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
	    ;; (try <body> (finally <exp> ...))
	    (aparse (try-body^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (finally-exps^ (caddr^ adatum)) handler fail
		  (lambda-cont2 (fexps fail)
		    (k (try-finally-aexp body fexps info) fail))))))
	   ((and (= (length^ adatum) 4) (catch?^ (caddr^ adatum)) (finally?^ (cadddr^ adatum)))
	    ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	    (aparse (try-body^ adatum) handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (catch-exps^ (caddr^ adatum)) handler fail
		  (lambda-cont2 (cexps fail)
		    (aparse-all (finally-exps^ (cadddr^ adatum)) handler fail
		      (lambda-cont2 (fexps fail)
			(let ((cvar (get-sexp (catch-var^ (caddr^ adatum)))))
			  (k (try-catch-finally-aexp body cvar cexps fexps info) fail)))))))))
	   (else (aparse-error "bad try syntax:" adatum handler fail))))
	((raise?^ adatum)
	 (aparse (cadr^ adatum) handler fail
	   (lambda-cont2 (v fail)
	     (k (raise-aexp v info) fail))))
	((dict?^ adatum)
	 (aparse-entries (cdr^ adatum) handler fail
	   (lambda-cont2 (entries fail)
	     (k (dict-aexp entries info) fail))))
	((help?^ adatum)
	 (if (symbol?^ (cadr^ adatum))
	   (let ((var (get-sexp (cadr^ adatum)))
		 (var-info (get-source-info (cadr^ adatum))))
	     (k (help-aexp var var-info info) fail))
	   (aparse-error "bad concrete syntax:" adatum handler fail)))
	((choose?^ adatum)
	 (aparse-all (cdr^ adatum) handler fail
	   (lambda-cont2 (exps fail)
	     (k (choose-aexp exps info) fail))))
	((application?^ adatum)
	 (aparse (car^ adatum) handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse-all (cdr^ adatum) handler fail
	       (lambda-cont2 (v2 fail)
		 (k (app-aexp v1 v2 info) fail))))))
	(else (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define* aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps adatum
	(lambda-cont (datum)
	  (handler (format "parse error: ~a ~s ~a" msg datum
			   (where-at (get-start-line info) (get-start-char info) (get-srcfile info)))
		   fail))))))

(define* expand-once^
  (lambda (adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (lookup-value (get-sexp (car^ adatum)) macro-env 'none handler fail
      (lambda-cont2 (macro fail)
	(if (pattern-macro? macro)
	  (process-macro-clauses^ (macro-clauses^ macro) (macro-aclauses^ macro) adatum handler fail k)
	  ;; macro transformer functions take 1-arg continuations:
	  (macro adatum
	    (lambda-cont (v)
	      (reannotate-cps v
		(lambda-cont (expansion)
		  (if (has-source-info? expansion)
		    (k expansion fail)
		    (let ((info (get-source-info adatum)))
		      (if (original-source-info? adatum)
			(let ((macro-keyword (get-sexp (car^ adatum))))
			  (k (replace-info expansion (snoc macro-keyword info)) fail))
			(k (replace-info expansion info) fail)))))))))))))

(define* process-macro-clauses^
  (lambda (clauses aclauses adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (if (null? clauses)
      (aparse-error "no matching clause found for" adatum handler fail)
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses))
	    (aleft-pattern (car^ (car aclauses)))
	    (aright-pattern (cadr^ (car aclauses))))
	(unannotate-cps adatum
	  (lambda-cont (datum)
	    (unify-patterns^ left-pattern datum aleft-pattern adatum
	      (lambda-cont (subst)
		(if subst
		  (instantiate^ right-pattern subst aright-pattern (lambda-cont2 (v av) (k av fail)))
		  (process-macro-clauses^ (cdr clauses) (cdr aclauses) adatum handler fail k))))))))))

;; for dicts
(define* aparse-entries
  (lambda (entries handler fail k)
    (if (null? entries)
      (k '() fail)
      (aparse-all (get-sexp (car entries)) handler fail
	(lambda-cont2 (a fail)
	  (aparse-entries (cdr entries) handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* aparse-all
  (lambda (adatum-list handler fail k)
    (if (null? adatum-list)
      (k '() fail)
      (aparse (car adatum-list) handler fail
	(lambda-cont2 (a fail)
	  (aparse-all (cdr adatum-list) handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* aparse-sexps
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-asexp tokens src handler fail
	(lambda-cont4 (adatum end tokens-left fail)
	  (aparse adatum handler fail
	    (lambda-cont2 (exp fail)
	      (aparse-sexps tokens-left src handler fail
		(lambda-cont2 (v fail)
		  (k (cons exp v) fail))))))))))

;;--------------------------------------------------------------------------------------------
;; quasiquote expansion
;;
;; based on Appendix B of Alan Bawden's paper "Quasiquotation in Lisp", with some optimizations
;;
;; this version matches the functionality of Petite's quasiquote expander

;; for testing only
(define qqtest
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (let ((adatum (aread-string s)))
	(qq-expand-cps (cadr^ adatum) 0
	  (lambda-cont (v)
	    (reannotate-cps v
	      (lambda-cont (aexpansion)
		(replace-info aexpansion (snoc 'quasiquote (get-source-info adatum)))))))))))

;; expands annotated code
(define* qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (^cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (^cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car^ ax) ,v)))))
	 ((and (unquote?^ ax) (not (null? (cdr^ ax))) (null? (cddr^ ax))) (k (cadr^ ax)))
	 (else (k `(quote ,ax))))) ;; illegal
      ((vector?^ ax)
       (qq-expand-cps (retag (vector->list^ ax) 'none) depth
	 (lambda-cont (v)
	   (k `(list->vector ,v)))))
      ((not (pair?^ ax)) (k `',ax))
      ((null? (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (^cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands annotated code
(define* qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (^cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (^cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car^ ax) ,v))))))
	 ((unquote?^ ax) (k `(list . ,(^cdr^ ax))))
	 ((null? (cddr^ ax)) (k (cadr^ ax)))
	 (else (k `(append . ,(^cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair?^ ax)) (k `'(,ax)))
      ((null? (cdr^ ax))
       (qq-expand-list-cps (car^ ax) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (^cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;; expands unannotated code
(define* qq-expand-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car x) ,v)))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (k (cadr x)))
	 (else (k `(quote ,x))))) ;; illegal
      ((vector? x)
       (qq-expand-cps_ (vector->list x) depth
	 (lambda-cont (v)
	   (k `(list->vector ,v)))))
      ((not (pair? x)) (k `',x))
      ((null? (cdr x)) (qq-expand-list-cps_ (car x) depth k))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands unannotated code
(define* qq-expand-list-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car x) ,v))))))
	 ((unquote? x) (k `(list . ,(cdr x))))
	 ((null? (cddr x)) (k (cadr x)))
	 (else (k `(append . ,(cdr x))))))
      ((vector? x)
       (qq-expand-cps_ x depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair? x)) (k `'(,x)))
      ((null? (cdr x))
       (qq-expand-list-cps_ (car x) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;;------------------------------------------------------------------------
;; C# does not have "cases", so this is not useful there
;; This is the un-annotated version anyway

(define* unparse
  (lambda (exp handler fail k)
    (cases expression exp
      (lit-exp (datum)
	(cond
	  ((literal? datum) (k datum fail))
	  ((vector? datum) (k datum fail))
	  (else (k `(quote ,datum) fail))))
      (var-exp (id) (k id fail))
      (if-exp (test-exp then-exp else-exp)
	      (unparse test-exp handler fail
		  (lambda-cont (v1)
		      (unparse then-exp handler fail
			  (lambda-cont (v2)
			     (unparse else-exp handler fail
				(lambda-cont (v3)
				   (k `(if ,v1 ,v2 ,v3)))))))))
      (assign-exp (var rhs-exp)
        (unparse rhs-exp handler fail
           (lambda-cont (v)
	      (k `(set! ,var ,v) fail))))
      (func-exp (exp)
        (unparse exp handler fail
           (lambda-cont (v)
 	       (k `(func ,v) fail))))
      (define-exp (id docstring rhs-exp)
        (unparse rhs-exp handler fail
           (lambda-cont (v)
	       (if (string=? docstring "")
		   (k `(define ,id ,v) fail)
		   (k `(define ,id ,docstring ,v) fail)))))
      (define!-exp (id docstring rhs-exp)
        (unparse rhs-exp handler fail
           (lambda-cont (v)
	       (if (string=? docstring "")
		   (k `(define! ,id ,v) fail)
		   (k `(define! ,id ,docstring ,v) fail)))))
      (define-syntax-exp (name clauses)
	(k `(define-syntax ,name ,@clauses) fail))
      (begin-exp (exps)
	(unparse-exps exps handler fail
          (lambda-cont (v)
	     (k `(begin ,@v) fail))))
      (lambda-exp (formals bodies)
        (unparse-exps bodies handler fail
           (lambda-cont (v)
	      (k `(lambda ,formals ,@v) fail))))
      (mu-lambda-exp (formals runt bodies)
        (unparse-exps bodies handler fail
           (lambda-cont (v)
   	      (k `(lambda (,@formals . ,runt) ,@v) fail)))) 
      (trace-lambda-exp (name formals bodies)
	 (unparse-exps bodies handler fail
	    (lambda-cont (v)
	       (k `(trace-lambda ,name ,formals ,v) fail))))
      (mu-trace-lambda-exp (name formals runt bodies)
	 (unparse-exps bodies handler fail
	    (lambda-cont (v)
	       (k `(trace-lambda ,name (,@formals . ,runt) ,@v) fail)))) 
      (app-exp (operator operands)
	 (unparse-exps operands handler fail
	    (lambda-cont (v2)
     	       (unparse-exps operator handler fail
	          (lambda-cont (v1)
	              (k `(,v1 ,@v2) fail))))))
      (try-catch-exp (body catch-var catch-exps)
	 (unparse-exps catch-exps handler fail
	    (lambda-cont (v1)
	       (unparse body handler fail
	          (lambda-cont (v2)
 	             (k `(try ,v2 (catch ,catch-var ,@v1)) fail))))))
      (try-finally-exp (body finally-exps)
	 (unparse-exps finally-exps handler fail
	    (lambda-cont (v1)
	       (unparse body handler fail
	          (lambda-cont (v2)
	              (k `(try ,v2 (finally ,@v1)) fail))))))
      (try-catch-finally-exp (body catch-var catch-exps finally-exps)
	 (unparse-exps catch-exps handler fail
	    (lambda-cont (v2)
   	       (unparse-exps finally-exps handler fail
	          (lambda-cont (v1)
		     (unparse body handler fail
			 (lambda-cont (v3)
			     (k `(try ,v3
				      (catch ,catch-var ,@v2)
				      (finally ,@v1)) fail))))))))
      (raise-exp (exp)
	 (unparse exp handler fail
	    (lambda-cont (v)
	       (k `(raise ,v) fail))))
;;      (dict-exp (entries)
;;	`(dict ,@(map (lambda (b) `(,(unparse (car b)) ,(unparse (cadr b)))) entries)))
      (help-exp (var)
	(k `(help ,var) fail))
      (choose-exp (exps)
	 (unparse-exps exps handler fail
	    (lambda-cont (v)
	       (k `(choose ,@v) fail))))
      (else (error 'unparse "bad abstract syntax: ~s" exp)))))

(define* unparse-exps 
  (lambda (exps handler fail k)
    (if (null? exps)
	(k '() fail)
	(unparse (car exps) handle fail
	    (lambda (v1)
	       (unparse-exps (cdr exps) handler fail
	           (lambda-cont (v2)
		      (k (cons v1 v2) fail))))))))

(define aunparse ;; rewritten not to use cases so that we can use it in C#
  (lambda (aexp)
    (let ((ae (car aexp)))
      (cond
        ((eq? ae 'lit-aexp)
         (let ((datum (cadr aexp)))
           (cond
             ((literal? datum) datum)
             ((vector? datum) datum)
             (else `(quote ,datum)))))
        ((eq? ae 'var-aexp)
         (let ((id (cadr aexp))
               ;;(info (caddr aexp))
	       )
           id))
        ((eq? ae 'if-aexp)
         (let ((test-aexp (cadr aexp))
               (then-aexp (caddr aexp))
               (else-aexp (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           `(if ,(aunparse test-aexp) ,(aunparse then-aexp) ,(aunparse else-aexp))))
        ((eq? ae 'assign-aexp)
         (let ((var (cadr aexp))
               (rhs-exp (caddr aexp))
               ;;(var-info (cadddr aexp))
               ;;(info (cadddr aexp))
	       )
           `(set! ,var ,(aunparse rhs-exp))))
        ((eq? ae 'func-aexp)
         (let ((exp (cadr aexp))
               ;;(info (caddr aexp))
	       )
           `(func ,(aunparse exp))))
        ((eq? ae 'define-aexp)
         (let ((id (cadr aexp))
               (docstring (caddr aexp))
               (rhs-exp (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           (if (string=? docstring "")
               `(define ,id ,(aunparse rhs-exp))
               `(define ,id ,docstring ,(aunparse rhs-exp)))))
        ((eq? ae 'define!-aexp)
         (let ((id (cadr aexp))
               (docstring (caddr aexp))
               (rhs-exp (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           (if (string=? docstring "")
               `(define! ,id ,(aunparse rhs-exp))
               `(define! ,id ,docstring ,(aunparse rhs-exp)))))
        ((eq? ae 'define-syntax-aexp)
         (let ((name (cadr aexp))
               (clauses (caddr aexp))
               ;;(aclauses (cadddr aexp))
               ;;(info  (car (cddddr aexp)))
	       )
           `(define-syntax ,name ,@clauses)))
        ((eq? ae 'begin-aexp)
         (let ((exps (cadr aexp))
               ;;(info (caddr aexp))
	       )
           `(begin ,@(map aunparse exps))))
        ((eq? ae 'lambda-aexp)
         (let ((formals (cadr aexp))
               (bodies (caddr aexp))
               ;;(info (caddr aexp))
	       )
           `(lambda ,formals ,@(map aunparse bodies))))
        ((eq? ae 'mu-lambda-aexp)
         (let ((formals (cadr aexp))
               (runt (caddr aexp))
               (bodies (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           `(lambda (,@formals . ,runt) ,@(map aunparse bodies))))
        ((eq? ae 'trace-lambda-aexp)
         (let ((name (cadr aexp))
               (formals (caddr aexp))
               (bodies (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           `(trace-lambda ,name ,formals ,@(map aunparse bodies))))
      ((eq? ae 'mu-trace-lambda-aexp)
       (let ((name (cadr aexp))
             (formals (caddr aexp))
             (runt (cadddr aexp))
             (bodies (car (cddddr aexp)))
             ;;(info (cadr (cddddr aexp)))
	     )
         `(trace-lambda ,name (,@formals . ,runt) ,@(map aunparse bodies))))
        ((eq? ae 'app-aexp)
         (let ((operator (cadr aexp))
               (operands (caddr aexp))
               ;;(info (cadddr aexp))
	       )
           `(,(aunparse operator) ,@(map aunparse operands))))
        ((eq? ae 'try-catch-aexp)
         (let ((body (cadr aexp))
               (catch-var (caddr aexp))
               (catch-exps (cadddr aexp))
               ;;(info (car (cddddr aexp)))
	       )
           `(try ,(aunparse body) (catch ,catch-var ,@(map aunparse catch-exps)))))
        ((eq? ae 'try-finally-aexp)
         (let ((body (cadr aexp))
               (finally-exps (caddr aexp))
               ;;(info (cadddr aexp))
	       )
           `(try ,(aunparse body) (finally ,@(map aunparse finally-exps)))))
        ((eq? ae 'try-catch-finally-aexp)
         (let ((body (cadr aexp))
               (catch-var (caddr aexp))
               (catch-exps (cadddr aexp))
               (finally-exps (car (cddddr aexp)))
               ;;(info (cadr (cddddr aexp)))
	       )
           `(try ,(aunparse body)
              (catch ,catch-var ,@(map aunparse catch-exps))
              (finally ,@(map aunparse finally-exps)))))
        ((eq? ae 'raise-aexp)
         (let ((exp (cadr aexp))
               ;;(info (caddr aexp))
	       )
           `(raise ,(aunparse exp))))
;;        ((eq? ae 'dict-aexp)
;;         (let ((entries (cadr aexp))
;;               (info (caddr aexp)))
;;           `(dict ,@(map (lambda (b) `(,(aunparse (car b)) ,(aunparse (cadr b)))) entries))))
        ((eq? ae 'help-aexp)
         (let ((var (cadr aexp))
               ;;(var-info (caddr aexp))
               ;;(info (cadddr aexp))
	       )
           `(help ,var)))
        ((eq? ae 'choose-aexp)
         (let ((exps (cadr aexp))
               ;;(info (caddr aexp))
	       )
           `(choose ,@(map aunparse exps))))
        (else (error 'unparse "bad concrete syntax: ~s" aexp))
        ))))

(define expand-macro
  (lambda (transformer sexp)
    (transformer sexp init-cont)))

(define parse-string
  (lambda (string)
    (read-datum string 'stdin init-handler2 init-fail
      (lambda-cont3 (datum tokens-left fail)
	(parse datum init-handler2 init-fail init-cont2)))))

(define parse-file
  (lambda (filename)
    (get-parsed-sexps filename)))

(define print-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (get-parsed-sexps filename))))

(define get-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(parse-sexps tokens filename init-handler2 init-fail init-cont2)))))

(define* parse-sexps
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
	(lambda-cont3 (datum tokens-left fail)
	  (parse datum handler fail
	    (lambda-cont2 (exp fail)
	      (parse-sexps tokens-left src handler fail
		(lambda-cont2 (v fail)
		  (k (cons exp v) fail))))))))))

(define aparse-string
  (lambda (string)
    (aread-datum string 'stdin init-handler2 init-fail
      (lambda-cont3 (adatum tokens-left fail)
	(aparse adatum init-handler2 init-fail init-cont2)))))

(define aparse-file
  (lambda (filename)
    (aget-parsed-sexps filename)))

(define aprint-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (aget-parsed-sexps filename))))

(define aget-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens filename init-handler2 init-fail init-cont2)))))

;;--------------------------------------------------------------------------------------------
;; not used - for possible future reference

;; based on Alan Bawden's paper "Quasiquotation in Lisp"

;; for testing only
(define qq1
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand1 (cadr datum)))))

;; non-CPS Appendix A algorithm, with some optimizations
(define qq-expand1
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1 (qq-expand1 (cadr x))))
      ((unquote? x) (cadr x))
      ((unquote-splicing? x) `(quote ,x))  ;; illegal
      ((pair? x)
       (cond
	 ((null? (cdr x)) (qq-expand1-list (car x)))
	 ((unquote? (car x)) `(cons ,(cadr (car x)) ,(qq-expand1 (cdr x))))
	 (else `(append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x))))))
      ((vector? x) `(list->vector ,(qq-expand1 (vector->list x))))
      (else `(quote ,x)))))

(define qq-expand1-list
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1-list (qq-expand1 (cadr x))))
      ((unquote? x) `(list ,(cadr x)))
      ((unquote-splicing? x) (cadr x))
      ((pair? x)
       (cond
	 ((null? (cdr x)) `(list ,(qq-expand1-list (car x))))
	 ((unquote? (car x)) `(list (cons ,(cadr (car x)) ,(qq-expand1 (cdr x)))))
	 (else `(list (append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x)))))))
      ((vector? x) `(list ,(qq-expand1 x)))
      (else `(quote (,x))))))

;; for testing only
(define qq2
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand2 (cadr datum) 0))))

;; non-CPS Appendix B algorithm, with some optimizations
(define qq-expand2
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr x))
	 (else `(quote ,x))))  ;; illegal
      ((vector? x) `(list->vector ,(qq-expand2 (vector->list x) depth)))
      ((not (pair? x)) `',x)
      ((null? (cdr x)) (qq-expand2-list (car x) depth))
      (else `(append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth))))))

(define qq-expand2-list
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(list (cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1)))))
	 ((unquote? x) `(list . ,(cdr x)))
	 ((null? (cddr x)) (cadr x))
	 (else `(append . ,(cdr x)))))
      ((vector? x) `(list ,(qq-expand2 x depth)))
      ((not (pair? x)) `'(,x))
      ((null? (cdr x)) `(list ,(qq-expand2-list (car x) depth)))
      (else `(list (append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth)))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1))))
;;	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr^ ax))
;;	 (else `(quote ,ax))))  ;; illegal
;;      ((vector? x) `(list->vector ,(qq-expand (vector->list x) (retag (vector->list^ ax) 'none) depth)))
;;      ((not (pair? x)) `',ax)
;;      ((null? (cdr x)) (qq-expand-list (car x) (car^ ax) depth))
;;      (else `(append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand-list
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1)))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(list (cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1)))))
;;	 ((unquote? x) `(list . ,(^cdr^ ax)))
;;	 ((null? (cddr x)) (cadr^ ax))
;;	 (else `(append . ,(^cdr^ ax)))))
;;      ((vector? x) `(list ,(qq-expand x ax depth)))
;;      ((not (pair? x)) `'(,ax))
;;      ((null? (cdr x)) `(list ,(qq-expand-list (car x) (car^ ax) depth)))
;;      (else `(list (append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth)))))))

;;--------------------------------------------------------------------------------------------
;; my version - not correct

;;(define* expand-quasiquote
;;  (lambda (adatum k)
;;    (cond
;;      ((vector?^ adatum)
;;       (expand-quasiquote^ (vector->list^ adatum)
;;	 (lambda-cont (ls) (k `(list->vector ,ls)))))
;;      ((not (pair?^ adatum)) (k `(quote ,adatum)))
;;      ;; doesn't handle nested quasiquotes yet
;;      ((quasiquote?^ adatum) (k `(quote ,adatum)))
;;      ((unquote?^ adatum) (k (cadr^ adatum)))
;;      ((unquote-splicing? (car^ adatum))
;;       (if (null? (cdr^ adatum))
;;	 (k (cadr^ (car^ adatum)))
;;	 (expand-quasiquote^ (^cdr^ adatum)
;;	   (lambda-cont (v) (k `(append ,(cadr^ (car^ adatum)) ,v))))))
;;      ((quasiquote-list?^ adatum)
;;       (expand-quasiquote-list^ (get-sexp adatum)
;;	 (lambda-cont (v)
;;	   (k `(list ,@v)))))
;;      (else
;;	(expand-quasiquote^ (car^ adatum)
;;	  (lambda-cont (v1)
;;	    (expand-quasiquote^ (^cdr^ adatum)
;;	      (lambda-cont (v2)
;;		(k `(cons ,v1 ,v2))))))))))

;;(define* expand-quasiquote-list^  ;; maps expand-quasiquote to an arbitrary flat list
;;  (lambda (asexps k)
;;    (if (null? asexps)
;;      (k '())
;;      (expand-quasiquote^ (car asexps)
;;	(lambda-cont (v1)
;;	  (expand-quasiquote-list^ (cdr asexps)
;;	    (lambda-cont (v2)
;;	      (k (cons v1 v2)))))))))

;;(define quasiquote-list?^
;;  (lambda (adatum)
;;    (or (null?^ adatum)
;;	(and (pair?^ adatum)
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ adatum))
;;	     (not (unquote?^ adatum))
;;	     (not (unquote-splicing?^ adatum))
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ (car^ adatum)))
;;	     (not (unquote-splicing?^ (car^ adatum)))
;;	     (quasiquote-list?^ (^cdr^ adatum))))))

;;--------------------------------------------------------------------------------------------
;; temporary - stuff for testing

;;(define a 'apple)
;;(define b 'banana)
;;(define c '(cherry orange))
;;(define m 2)
;;(define n 3)
;;(define abc '(a b c))

;;;;(define parseexp (lambda (e) (parse e init-handler2 init-fail init-cont2)))

;;;(define-syntax for ((for ?exp times do . ?bodies) (for-repeat ?exp (lambda () . ?bodies))))

;;(define for-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "for-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'for-macro "huh?")))))

;;(define collect-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "collect-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'collect-macro "huh?")))))

;;(define for-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "for-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'for-macro^ "huh?")))))

;;(define collect-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "collect-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'collect-macro^ "huh?")))))

;;(define make-macro-env
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'for 'collect)
;;      (list and-transformer
;;	    or-transformer
;;	    cond-transformer
;;	    let-transformer
;;	    letrec-transformer
;;	    let*-transformer
;;	    case-transformer
;;	    record-case-transformer
;;	    (for-macro)      ;; for testing only
;;	    (collect-macro)  ;; for testing only
;;))))

;;(define make-macro-env^
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case) ;; 'for 'collect)
;;      (list and-transformer^
;;	    or-transformer^
;;	    cond-transformer^
;;	    let-transformer^
;;	    letrec-transformer^
;;	    let*-transformer^
;;	    case-transformer^
;;	    record-case-transformer^
;;;;	    (for-macro^)      ;; for testing only
;;;;	    (collect-macro^)  ;; for testing only
;;))))

;;(define macro-env (make-macro-env^))

;;;;(define macro-env 'undefined)

;;;;(define pmacro (lookup-value 'for macro-env init-handler2 init-fail init-cont2))

;;(define check
;;    (lambda (f)
;;      (let ((exps #f) (aexps #f))
;;        (set! macro-env (make-macro-env))
;;        (set! exps (parse-file f))
;;        (set! macro-env (make-macro-env^))
;;        (set! aexps (aparse-file f))
;;        (equal? (map unparse exps) (map aunparse aexps)))))

(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

(load "petite-init.ss")
(load "define-datatype.ss")

(define-datatype expression expression?
 (lit-exp (datum anything?)) (var-exp (id symbol?))
 (func-exp (exp expression?))
 (if-exp
   (test-exp expression?)
   (then-exp expression?)
   (else-exp expression?))
 (assign-exp (var symbol?) (rhs-exp expression?))
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
 (begin-exp (exps (list-of expression?)))
 (lambda-exp (formals (list-of symbol?)) (body expression?))
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
 (raise-exp (exp expression?))
 (dict-exp (pairs (list-of (list-of expression?))))
 (help-exp (var symbol?))
 (choose-exp (exps (list-of expression?))))

;;----------------------------------------------------------------------
;; continuation datatype

(define make-cont (lambda args (cons 'continuation args)))

(define*
  apply-cont
  (lambda (k value)
    (record-case (cdr k)
      (<cont-1> () (halt* value))
      (<cont-2> (fail k) (apply-cont2 k value fail))
      (<cont-3> (clauses datum right-pattern handler fail k)
       (if value
           (instantiate
             right-pattern
             value
             (make-cont '<cont-2> fail k))
           (process-macro-clauses (cdr clauses) datum handler fail k)))
      (<cont-4> (bindings k)
       (apply-cont k `(let (,(car bindings)) ,value)))
      (<cont-5> (k) (apply-cont k `(cond ,@value)))
      (<cont-6> (clauses var k)
       (let ((clause (car clauses)))
         (cond
           ((eq? (car clause) 'else)
            (apply-cont k (cons clause value)))
           ((symbol? (car clause))
            (apply-cont
              k
              (cons `((eq? ,var ',(car clause)) ,@(cdr clause)) value)))
           (else
            (apply-cont
              k
              (cons
                `((memq ,var ',(car clause)) ,@(cdr clause))
                value))))))
      (<cont-7> (handler fail k) (parse value handler fail k))
      (<cont-8> (v1 k) (apply-cont k `(cons ,v1 ,value)))
      (<cont-9> (datum k)
       (expand-quasiquote
         (cdr datum)
         (make-cont '<cont-8> value k)))
      (<cont-10> (k) (apply-cont k `(list ,@value)))
      (<cont-11> (datum k)
       (apply-cont k `(append ,(cadr (car datum)) ,value)))
      (<cont-12> (k) (apply-cont k `(list->vector ,value)))
      (<cont-13> (v1 k) (apply-cont k (cons v1 value)))
      (<cont-14> (datum k)
       (expand-quasiquote-list
         (cdr datum)
         (make-cont '<cont-13> value k)))
      (<cont-15> (fail k2) (apply-cont2 k2 value fail))
      (<cont-16> (x y k)
       (if value
           (equal-objects? (cdr x) (cdr y) k)
           (apply-cont k #f)))
      (<cont-17> (i v1 v2 k)
       (if value
           (equal-vectors? v1 v2 (- i 1) k)
           (apply-cont k #f)))
      (<cont-18> (ls orig-ls x handler fail k)
       (if value
           (apply-cont2 k ls fail)
           (member-prim x (cdr ls) orig-ls handler fail k)))
      (<cont-19> (pattern var k)
       (if value (apply-cont k #t) (occurs? var (cdr pattern) k)))
      (<cont-20> (p1 p2 k)
       (if value
           (apply-cont k #f)
           (apply-cont k (make-sub 'unit p1 p2))))
      (<cont-21> (s-car k)
       (if (not value)
           (apply-cont k #f)
           (apply-cont k (make-sub 'composite s-car value))))
      (<cont-22> (new-cdr1 s-car k)
       (unify-patterns
         new-cdr1
         value
         (make-cont '<cont-21> s-car k)))
      (<cont-23> (pair2 s-car k)
       (instantiate
         (cdr pair2)
         s-car
         (make-cont '<cont-22> value s-car k)))
      (<cont-24> (pair1 pair2 k)
       (if (not value)
           (apply-cont k #f)
           (instantiate
             (cdr pair1)
             value
             (make-cont '<cont-23> pair2 value k))))
      (<cont-25> (a k) (apply-cont k (cons a value)))
      (<cont-26> (pattern s k)
       (instantiate
         (cdr pattern)
         s
         (make-cont '<cont-25> value k)))
      (<cont-27> (s2 k) (instantiate value s2 k))
      (else (error 'apply-cont "bad continuation: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation2 datatype

(define make-cont2 (lambda args (cons 'continuation2 args)))

(define*
  apply-cont2
  (lambda (k value1 value2)
    (record-case (cdr k)
      (<cont2-1> (token k)
       (apply-cont2 k (cons token value1) value2))
      (<cont2-2> (chars k)
       (apply-cont3
         k
         (append value1 (list read-line-count read-char-count))
         chars
         value2))
      (<cont2-3> () (halt* value1))
      (<cont2-4> (handler k)
       (read-sexp
         value1
         handler
         value2
         (make-cont3 '<cont3-10> handler k)))
      (<cont2-5> ()
       (print-unparsed-sexps
         value1
         init-handler2
         init-fail
         init-cont2))
      (<cont2-6> (k)
       (apply-cont2 k (binding-value value1) value2))
      (<cont2-7> (variable env handler k)
       (if (dlr-env-contains variable)
           (apply-cont2 k (dlr-env-lookup variable) value2)
           (if value1
               (lookup-variable-components value1 "" env handler value2 k)
               (apply-handler2
                 handler
                 (format "unbound variable ~a" variable)
                 value2))))
      (<cont2-8> (components path var handler k)
       (if (null? (cdr components))
           (apply-cont2 k value1 value2)
           (let ((result (binding-value value1))
                 (new-path (if (string=? path "")
                               (format "~a" var)
                               (format "~a.~a" path var))))
             (if (not (environment? result))
                 (if (dlr-object? result)
                     (apply-cont2
                       k
                       (dlr-lookup-components result (cdr components))
                       value2)
                     (apply-handler2
                       handler
                       (format "~a is not a module" new-path)
                       value2))
                 (lookup-variable-components (cdr components) new-path
                   result handler value2 k)))))
      (<cont2-9> (datum handler k)
       (if (pattern-macro? value1)
           (process-macro-clauses (macro-clauses value1) datum handler
             value2 k)
           (apply-macro value1 datum (make-cont '<cont-2> value2 k))))
      (<cont2-10> (bodies k)
       (apply-cont k `(let ,value1 ,@value2 ,@bodies)))
      (<cont2-11> (procs vars k2)
       (apply-cont2
         k2
         (cons `(,(car vars) 'undefined) value1)
         (cons `(set! ,(car vars) ,(car procs)) value2)))
      (<cont2-12> (exp k)
       (apply-cont k `(let ((r ,exp) ,@value1) (cond ,@value2))))
      (<cont2-13> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq? (car clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr clause))) value1)
               (cons '(else (else-code)) value2))
             (if (symbol? (car clause))
                 (let ((name (car clause)))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr clause))) value1)
                     (cons `((eq? ,var ',(car clause)) (,name)) value2)))
                 (let ((name (caar clause)))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr clause))) value1)
                     (cons
                       `((memq ,var ',(car clause)) (,name))
                       value2)))))))
      (<cont2-14> (k)
       (apply-cont k `(let ,value1 (cond ,@value2))))
      (<cont2-15> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq? (car clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr clause))) value1)
               (cons `(else (else-code)) value2))
             (if (symbol? (car clause))
                 (let ((name (car clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr clause) ,@(cddr clause)))
                       value1)
                     (cons
                       `((eq? (car ,var) ',(car clause))
                          (apply ,name (cdr ,var)))
                       value2)))
                 (let ((name (caar clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr clause) ,@(cddr clause)))
                       value1)
                     (cons
                       `((memq (car ,var) ',(car clause))
                          (apply ,name (cdr ,var)))
                       value2)))))))
      (<cont2-16> (v1 k)
       (apply-cont2 k (app-exp v1 value1) value2))
      (<cont2-17> (datum handler k)
       (parse-all
         (cdr datum)
         handler
         value2
         (make-cont2 '<cont2-16> value1 k)))
      (<cont2-18> (k) (apply-cont2 k (choose-exp value1) value2))
      (<cont2-19> (k) (apply-cont2 k (dict-exp value1) value2))
      (<cont2-20> (k) (apply-cont2 k (raise-exp value1) value2))
      (<cont2-21> (cexps datum body k)
       (let ((cvar (catch-var (caddr datum))))
         (apply-cont2
           k
           (try-catch-finally-exp body cvar cexps value1)
           value2)))
      (<cont2-22> (datum body handler k)
       (parse-all
         (finally-exps (cadddr datum))
         handler
         value2
         (make-cont2 '<cont2-21> value1 datum body k)))
      (<cont2-23> (datum handler k)
       (parse-all
         (catch-exps (caddr datum))
         handler
         value2
         (make-cont2 '<cont2-22> datum value1 handler k)))
      (<cont2-24> (body k)
       (apply-cont2 k (try-finally-exp body value1) value2))
      (<cont2-25> (datum handler k)
       (parse-all
         (finally-exps (caddr datum))
         handler
         value2
         (make-cont2 '<cont2-24> value1 k)))
      (<cont2-26> (datum body k)
       (let ((cvar (catch-var (caddr datum))))
         (apply-cont2 k (try-catch-exp body cvar value1) value2)))
      (<cont2-27> (datum handler k)
       (parse-all
         (catch-exps (caddr datum))
         handler
         value2
         (make-cont2 '<cont2-26> datum value1 k)))
      (<cont2-28> (datum k)
       (if (list? (cadr datum))
           (apply-cont2 k (lambda-exp (cadr datum) value1) value2)
           (apply-cont2
             k
             (mu-lambda-exp
               (head (cadr datum))
               (last (cadr datum))
               value1)
             value2)))
      (<cont2-29> (datum handler k)
       (cond
         ((null? value1)
          (apply-handler2
            handler
            (format "bad concrete syntax: ~a" datum)
            value2))
         ((null? (cdr value1)) (apply-cont2 k (car value1) value2))
         (else (apply-cont2 k (begin-exp value1) value2))))
      (<cont2-30> (datum k)
       (apply-cont2
         k
         (define!-exp (cadr datum) (caddr datum) value1)
         value2))
      (<cont2-31> (datum k)
       (apply-cont2 k (define!-exp (cadr datum) "" value1) value2))
      (<cont2-32> (datum k)
       (apply-cont2
         k
         (define-exp (cadr datum) (caddr datum) value1)
         value2))
      (<cont2-33> (datum k)
       (apply-cont2 k (define-exp (cadr datum) "" value1) value2))
      (<cont2-34> (k) (apply-cont2 k (func-exp value1) value2))
      (<cont2-35> (datum k)
       (apply-cont2 k (assign-exp (cadr datum) value1) value2))
      (<cont2-36> (v1 v2 k)
       (apply-cont2 k (if-exp v1 v2 value1) value2))
      (<cont2-37> (datum v1 handler k)
       (parse
         (cadddr datum)
         handler
         value2
         (make-cont2 '<cont2-36> v1 value1 k)))
      (<cont2-38> (datum handler k)
       (parse
         (caddr datum)
         handler
         value2
         (make-cont2 '<cont2-37> datum value1 handler k)))
      (<cont2-39> (v1 k)
       (apply-cont2 k (if-exp v1 value1 (lit-exp #f)) value2))
      (<cont2-40> (datum handler k)
       (parse
         (caddr datum)
         handler
         value2
         (make-cont2 '<cont2-39> value1 k)))
      (<cont2-41> (handler k) (parse value1 handler value2 k))
      (<cont2-42> (a b k)
       (apply-cont2 k (cons (list a b) value1) value2))
      (<cont2-43> (a pairs handler k)
       (parse-pairs
         (cdr pairs)
         handler
         value2
         (make-cont2 '<cont2-42> a value1 k)))
      (<cont2-44> (pairs handler k)
       (parse
         (cadar pairs)
         handler
         value2
         (make-cont2 '<cont2-43> value1 pairs handler k)))
      (<cont2-45> (a k) (apply-cont2 k (cons a value1) value2))
      (<cont2-46> (datum-list handler k)
       (parse-all
         (cdr datum-list)
         handler
         value2
         (make-cont2 '<cont2-45> value1 k)))
      (<cont2-47> ()
       (parse-sexps value1 init-handler2 init-fail init-cont2))
      (<cont2-48> (exp k)
       (apply-cont2 k (cons exp value1) value2))
      (<cont2-49> (tokens-left handler k)
       (parse-sexps
         tokens-left
         handler
         value2
         (make-cont2 '<cont2-48> value1 k)))
      (<cont2-50> ()
       (if (not (eq? value1 '<void>)) (pretty-print-prim value1))
       (if *need-newline* (newline))
       (read-eval-print value2))
      (<cont2-51> ()
       (read-and-eval-sexps value1 toplevel-env REP-handler value2
         REP-k))
      (<cont2-52> (tokens-left env handler k)
       (if (token-type? (first tokens-left) 'end-marker)
           (apply-cont2 k value1 value2)
           (read-and-eval-sexps tokens-left env handler value2 k)))
      (<cont2-53> (tokens-left env handler k)
       (m value1 env handler value2
          (make-cont2 '<cont2-52> tokens-left env handler k)))
      (<cont2-54> (args env handler k)
       (if (dlr-exp? value1)
           (apply-cont2 k (dlr-apply value1 args) value2)
           (apply-proc value1 args env handler value2 k)))
      (<cont2-55> (operator env handler k)
       (m operator env handler value2
          (make-cont2 '<cont2-54> value1 env handler k)))
      (<cont2-56> (k)
       (apply-cont2 k (binding-docstring value1) value2))
      (<cont2-57> (handler)
       (apply-handler2 handler value1 value2))
      (<cont2-58> (v k) (apply-cont2 k v value2))
      (<cont2-59> (fexps env handler k)
       (eval-sequence fexps env handler value2
         (make-cont2 '<cont2-58> value1 k)))
      (<cont2-60> (clauses k)
       (set-binding-value! value1 (make-pattern-macro clauses))
       (apply-cont2 k '<void> value2))
      (<cont2-61> (docstring var k)
       (set-global-value! var value1)
       (set-global-docstring! var docstring)
       (apply-cont2 k '<void> value2))
      (<cont2-62> (docstring rhs-value k)
       (set-binding-value! value1 rhs-value)
       (set-binding-docstring! value1 docstring)
       (apply-cont2 k '<void> value2))
      (<cont2-63> (docstring var env handler k)
       (lookup-binding-in-first-frame var env handler value2
         (make-cont2 '<cont2-62> docstring value1 k)))
      (<cont2-64> (rhs-value k)
       (let ((old-value (binding-value value1)))
         (set-binding-value! value1 rhs-value)
         (let ((new-fail (make-fail
                           '<fail-3>
                           value1
                           old-value
                           value2)))
           (apply-cont2 k '<void> new-fail))))
      (<cont2-65> (var env handler k)
       (lookup-binding var env handler value2
         (make-cont2 '<cont2-64> value1 k)))
      (<cont2-66> (else-exp then-exp env handler k)
       (if value1
           (m then-exp env handler value2 k)
           (m else-exp env handler value2 k)))
      (<cont2-67> (k) (apply-cont2 k (dlr-func value1) value2))
      (<cont2-68> (e handler) (apply-handler2 handler e value2))
      (<cont2-69> (v1 k) (apply-cont2 k (cons v1 value1) value2))
      (<cont2-70> (exps env handler k)
       (m* (cdr exps) env handler value2
           (make-cont2 '<cont2-69> value1 k)))
      (<cont2-71> (exps env handler k)
       (if (null? (cdr exps))
           (apply-cont2 k value1 value2)
           (eval-sequence (cdr exps) env handler value2 k)))
      (<cont2-72> (handler k2)
       (read-sexp
         value1
         handler
         value2
         (make-cont3 '<cont3-15> handler k2)))
      (<cont2-73> (handler k2)
       (m value1 toplevel-env handler value2 k2))
      (<cont2-74> (iterator proc env handler k)
       (iterate-continue proc iterator env handler value2 k))
      (<cont2-75> (iterator proc env handler k)
       (iterate-collect-continue proc iterator env handler value2
         (make-cont2 '<cont2-69> value1 k)))
      (<cont2-76> (list1 proc env handler k)
       (map1 proc (cdr list1) env handler value2
         (make-cont2 '<cont2-69> value1 k)))
      (<cont2-77> (list1 proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (list (car list1))) value1)
         value2))
      (<cont2-78> (list1 list2 proc env handler k)
       (map2 proc (cdr list1) (cdr list2) env handler value2
         (make-cont2 '<cont2-69> value1 k)))
      (<cont2-79> (list1 list2 proc k)
       (apply-cont2
         k
         (cons
           (dlr-apply proc (list (car list1) (car list2)))
           value1)
         value2))
      (<cont2-80> (lists proc env handler k)
       (mapN proc (map cdr lists) env handler value2
         (make-cont2 '<cont2-69> value1 k)))
      (<cont2-81> (lists proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (map car lists)) value1)
         value2))
      (<cont2-82> (arg-list proc env handler k)
       (for-each-prim proc (map cdr arg-list) env handler value2
         k))
      (<cont2-83> (args sym handler k)
       (cond
         ((null? (cdr args)) (apply-cont2 k value1 value2))
         ((not (environment? value1))
          (apply-handler2
            handler
            (format "~a is not a module" sym)
            value2))
         (else (get-primitive (cdr args) value1 handler value2 k))))
      (<cont2-84> (filename env handler k)
       (let ((module (extend env '() '())))
         (set-binding-value! value1 module)
         (load-file filename module handler value2 k)))
      (<cont2-85> (k)
       (if (null? load-stack)
           (printf "WARNING: empty load-stack encountered!\n")
           (set! load-stack (cdr load-stack)))
       (apply-cont2 k '<void> value2))
      (<cont2-86> (env handler k)
       (read-and-eval-sexps value1 env handler value2
         (make-cont2 '<cont2-85> k)))
      (<cont2-87> (filenames env handler k)
       (load-files (cdr filenames) env handler value2 k))
      (<cont2-88> ()
       (read-and-eval-sexps value1 toplevel-env init-handler2
         value2 init-cont2))
      (<cont2-89> ()
       (parse-sexps value1 init-handler2 value2 init-cont2))
      (else (error 'apply-cont2 "bad continuation2: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation3 datatype

(define make-cont3 (lambda args (cons 'continuation3 args)))

(define*
  apply-cont3
  (lambda (k value1 value2 value3)
    (record-case (cdr k)
      (<cont3-1> (handler k)
       (if (token-type? value1 'end-marker)
           (apply-cont2 k (list value1) value3)
           (scan-input-loop
             value2
             handler
             value3
             (make-cont2 '<cont2-1> value1 k))))
      (<cont3-2> (k)
       (apply-cont3 k (list->vector value1) value2 value3))
      (<cont3-3> (keyword k)
       (apply-cont3 k (list keyword value1) value2 value3))
      (<cont3-4> (sexp1 k)
       (apply-cont3 k (cons sexp1 value1) value2 value3))
      (<cont3-5> (expected-terminator handler k)
       (read-sexp-sequence value2 expected-terminator handler
         value3 (make-cont3 '<cont3-4> value1 k)))
      (<cont3-6> (expected-terminator handler k)
       (close-sexp-sequence value1 value2 expected-terminator
         handler value3 k))
      (<cont3-7> (handler k)
       (read-vector
         value2
         handler
         value3
         (make-cont3 '<cont3-4> value1 k)))
      (<cont3-8> () (halt* (cons value1 value2)))
      (<cont3-9> () (halt* value1))
      (<cont3-10> (handler k)
       (if (token-type? (first value2) 'end-marker)
           (apply-cont3 k value1 value2 value3)
           (apply-handler2
             handler
             (format
               "tokens left over at line ~a, char ~a"
               (get-line-count (first value2))
               (get-char-count (first value2)))
             value3)))
      (<cont3-11> (handler k)
       (pretty-print value1)
       (print-unparsed-sexps value2 handler value3 k))
      (<cont3-12> ()
       (parse value1 init-handler2 init-fail init-cont2))
      (<cont3-13> (handler k)
       (parse
         value1
         handler
         value3
         (make-cont2 '<cont2-49> value2 handler k)))
      (<cont3-14> (env handler k)
       (parse
         value1
         handler
         value3
         (make-cont2 '<cont2-53> value2 env handler k)))
      (<cont3-15> (handler k2)
       (if (token-type? (first value2) 'end-marker)
           (parse value1 handler value3 k2)
           (apply-handler2
             handler
             (format
               "tokens left over at line ~a, char ~a"
               (get-line-count (first value2))
               (get-char-count (first value2)))
             value3)))
      (else (error 'apply-cont3 "bad continuation3: ~a" k)))))

;;----------------------------------------------------------------------
;; fail-continuation datatype

(define make-fail
  (lambda args (cons 'fail-continuation args)))

(define*
  apply-fail
  (lambda (fail)
    (record-case (cdr fail)
      (<fail-1> () (halt* "no more choices"))
      (<fail-2> () (apply-cont2 REP-k "no more choices" REP-fail))
      (<fail-3> (binding old-value fail)
       (set-binding-value! binding old-value)
       (apply-fail fail))
      (<fail-4> (exps env handler fail k)
       (eval-choices (cdr exps) env handler fail k))
      (else (error
             'apply-fail
             "bad fail-continuation: ~a"
             fail)))))

;;----------------------------------------------------------------------
;; handler datatype

(define make-handler (lambda args (cons 'handler args)))

(define*
  apply-handler
  (lambda (handler exception)
    (record-case (cdr handler)
      (<handler-1> () (halt* (list 'exception exception)))
      (else (error 'apply-handler "bad handler: ~a" handler)))))

;;----------------------------------------------------------------------
;; handler2 datatype

(define make-handler2 (lambda args (cons 'handler2 args)))

(define*
  apply-handler2
  (lambda (handler exception fail)
    (record-case (cdr handler)
      (<handler2-1> () (halt* (list 'exception exception)))
      (<handler2-2> ()
       (apply-cont2 REP-k `(uncaught exception: ,exception) fail))
      (<handler2-3> (cexps cvar env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (eval-sequence cexps new-env handler fail k)))
      (<handler2-4> (fexps env handler)
       (eval-sequence fexps env handler fail
         (make-cont2 '<cont2-68> exception handler)))
      (<handler2-5> (cexps cvar fexps env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (let ((catch-handler (try-finally-handler
                                fexps
                                env
                                handler)))
           (eval-sequence cexps new-env catch-handler fail
             (make-cont2 '<cont2-59> fexps env handler k)))))
      (else (error 'apply-handler2 "bad handler2: ~a" handler)))))

;;----------------------------------------------------------------------
;; procedure datatype

(define make-proc (lambda args (cons 'procedure args)))

(define*
  apply-proc
  (lambda (proc args env2 handler fail k2)
    (record-case (cdr proc)
      (<proc-1> (formals body env)
       (if (= (length args) (length formals))
           (m body (extend env formals args) handler fail k2)
           (apply-handler2
             handler
             "incorrect number of arguments"
             fail)))
      (<proc-2> (formals runt body env)
       (if (>= (length args) (length formals))
           (let ((new-env (extend
                            env
                            (cons runt formals)
                            (cons
                              (list-tail args (length formals))
                              (list-head args (length formals))))))
             (m body new-env handler fail k2))
           (apply-handler2 handler "not enough arguments given" fail)))
      (<proc-3> ()
       (if (= (length args) 1)
           (length-loop (car args) 0 (car args) handler fail k2)
           (apply-handler2
             handler
             "incorrect number of arguments to procedure length"
             fail)))
      (<proc-4> ()
       (apply-cont2 k2 (make-vector-size (car args)) fail))
      (<proc-5> () (apply-cont2 k2 (apply vector-ref args) fail))
      (<proc-6> ()
       (apply-cont2
         k2
         (vector-set! (car args) (cadr args) (caddr args))
         fail))
      (<proc-7> () (apply-cont2 k2 (make-vector args) fail))
      (<proc-8> ()
       (apply printf-prim args)
       (apply-cont2 k2 '<void> fail))
      (<proc-9> () (apply-cont2 k2 (not (car args)) fail))
      (<proc-10> () (apply-cont2 k2 (using-prim args env2) fail))
      (<proc-11> () (apply-cont2 k2 env2 fail))
      (<proc-12> ()
       (for-each-prim (car args) (cdr args) env2 handler fail k2))
      (<proc-13> ()
       (map-prim (car args) (cdr args) env2 handler fail k2))
      (<proc-14> () (apply-cont2 k2 (get-current-time) fail))
      (<proc-15> () (apply-cont2 k2 (dir args env2) fail))
      (<proc-16> ()
       (apply-cont2 k2 (apply make-vector args) fail))
      (<proc-17> () (apply-cont2 k2 (apply append args) fail))
      (<proc-18> () (apply-cont2 k2 (apply reverse args) fail))
      (<proc-19> () (apply-cont2 k2 'ok REP-fail))
      (<proc-20> ()
       (if (true? (car args))
           (apply-cont2 k2 'ok fail)
           (apply-fail fail)))
      (<proc-21> ()
       (if (null? args)
           (apply-cont2 REP-k '<void> fail)
           (apply-cont2 REP-k (car args) fail)))
      (<proc-22> ()
       (call/cc-primitive (car args) env2 handler fail k2))
      (<proc-23> () (get-primitive args env2 handler fail k2))
      (<proc-24> () (import-primitive args env2 handler fail k2))
      (<proc-25> () (apply-cont2 k2 (apply set-cdr! args) fail))
      (<proc-26> () (apply-cont2 k2 (apply set-car! args) fail))
      (<proc-27> () (apply-cont2 k2 (apply range args) fail))
      (<proc-28> ()
       (if (= (length args) 2)
           (member-prim (car args) (cadr args) (cadr args) handler fail
             k2)
           (apply-handler2
             handler
             "incorrect number of arguments to procedure member"
             fail)))
      (<proc-29> () (apply-cont2 k2 (apply memq args) fail))
      (<proc-30> () (apply-cont2 k2 (apply eq? args) fail))
      (<proc-31> ()
       (if (= (length args) 2)
           (equal-objects?
             (car args)
             (cadr args)
             (make-cont '<cont-15> fail k2))
           (apply-handler2
             handler
             "incorrect number of arguments to procedure equal?"
             fail)))
      (<proc-32> () (apply-cont2 k2 (apply abs args) fail))
      (<proc-33> () (apply-cont2 k2 (apply = args) fail))
      (<proc-34> () (apply-cont2 k2 (apply > args) fail))
      (<proc-35> () (apply-cont2 k2 (apply < args) fail))
      (<proc-36> ()
       (cond
         ((= (length args) 1)
          (if (= (car args) 0)
              (apply-handler2 handler "division by zero" fail)
              (apply-cont2 k2 (apply / args) fail)))
         ((>= (length args) 2)
          (if (= (cadr args) 0)
              (apply-handler2 handler "division by zero" fail)
              (apply-cont2 k2 (apply / args) fail)))
         (else
          (apply-handler2 handler "not enough args to /" fail))))
      (<proc-37> () (apply-cont2 k2 (apply * args) fail))
      (<proc-38> () (apply-cont2 k2 (apply - args) fail))
      (<proc-39> () (apply-cont2 k2 (apply + args) fail))
      (<proc-40> () (apply-cont2 k2 args fail))
      (<proc-41> () (apply-cont2 k2 (apply caddr args) fail))
      (<proc-42> () (apply-cont2 k2 (apply cadr args) fail))
      (<proc-43> () (apply-cont2 k2 (apply cdr args) fail))
      (<proc-44> () (apply-cont2 k2 (apply car args) fail))
      (<proc-45> () (apply-cont2 k2 (apply cons args) fail))
      (<proc-46> () (apply-cont2 k2 (apply null? args) fail))
      (<proc-47> ()
       (load-file (car args) toplevel-env handler fail k2))
      (<proc-48> () (newline-prim) (apply-cont2 k2 '<void> fail))
      (<proc-49> ()
       (apply display-prim args)
       (apply-cont2 k2 '<void> fail))
      (<proc-50> ()
       (for-each pretty-print-prim args)
       (apply-cont2 k2 '<void> fail))
      (<proc-51> () (apply-cont2 k2 (apply sqrt args) fail))
      (<proc-52> ()
       (let ((proc (car args)) (proc-args (cadr args)))
         (apply-proc proc proc-args env2 handler fail k2)))
      (<proc-53> ()
       (scan-input
         (car args)
         handler
         fail
         (make-cont2 '<cont2-72> handler k2)))
      (<proc-54> () (parse (car args) handler fail k2))
      (<proc-55> ()
       (parse
         (car args)
         handler
         fail
         (make-cont2 '<cont2-73> handler k2)))
      (<proc-56> () (halt* '(exiting the interpreter)))
      (<proc-57> (k) (apply-cont2 k (car args) fail))
      (<proc-58> (external-function-object)
       (apply-cont2
         k2
         (apply* external-function-object args)
         fail))
      (else (error 'apply-proc "bad procedure: ~a" proc)))))

;;----------------------------------------------------------------------
;; macro-transformer datatype

(define make-macro
  (lambda args (cons 'macro-transformer args)))

(define*
  apply-macro
  (lambda (macro datum k)
    (record-case (cdr macro)
      (<macro-1> ()
       (let ((name (caadr datum))
             (formals (cdadr datum))
             (bodies (cddr datum)))
         (apply-cont k `(define ,name (lambda ,formals ,@bodies)))))
      (<macro-2> ()
       (let ((exps (cdr datum)))
         (cond
           ((null? exps) (apply-cont k '#t))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont k `(if ,(car exps) (and ,@(cdr exps)) #f))))))
      (<macro-3> ()
       (let ((exps (cdr datum)))
         (cond
           ((null? exps) (apply-cont k '#f))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont
              k
              `(let ((bool ,(car exps))
                     (else-code (lambda () (or ,@(cdr exps)))))
                 (if bool bool (else-code))))))))
      (<macro-4> ()
       (let ((clauses (cdr datum)))
         (if (null? clauses)
             (error 'cond-transformer "bad concrete syntax: ~a" datum)
             (let ((first-clause (car clauses))
                   (other-clauses (cdr clauses)))
               (if (or (null? first-clause) (not (list? first-clause)))
                   (error 'cond-transformer
                     "bad concrete syntax: ~a"
                     datum)
                   (let ((test-exp (car first-clause))
                         (then-exps (cdr first-clause)))
                     (cond
                       ((eq? test-exp 'else)
                        (cond
                          ((null? then-exps)
                           (error 'cond-transformer
                             "bad concrete syntax: (~a)"
                             'else))
                          ((null? (cdr then-exps))
                           (apply-cont k (car then-exps)))
                          (else (apply-cont k `(begin ,@then-exps)))))
                       ((null? then-exps)
                        (if (null? other-clauses)
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)) (if bool bool)))
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)
                                     (else-code (lambda ()
                                                  (cond ,@other-clauses))))
                                 (if bool bool (else-code))))))
                       ((null? other-clauses)
                        (if (null? (cdr then-exps))
                            (apply-cont k `(if ,test-exp ,(car then-exps)))
                            (apply-cont
                              k
                              `(if ,test-exp (begin ,@then-exps)))))
                       ((null? (cdr then-exps))
                        (apply-cont
                          k
                          `(if ,test-exp
                               ,(car then-exps)
                               (cond ,@other-clauses))))
                       (else
                        (apply-cont
                          k
                          `(if ,test-exp
                               (begin ,@then-exps)
                               (cond ,@other-clauses)))))))))))
      (<macro-5> ()
       (if (symbol? (cadr datum))
           (let* ((name (cadr datum))
                  (bindings (caddr datum))
                  (vars (map car bindings))
                  (exps (map cadr bindings))
                  (bodies (cdddr datum)))
             (apply-cont
               k
               `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
           (let* ((bindings (cadr datum))
                  (vars (map car bindings))
                  (exps (map cadr bindings))
                  (bodies (cddr datum)))
             (apply-cont k `((lambda ,vars ,@bodies) ,@exps)))))
      (<macro-6> ()
       (let* ((decls (cadr datum))
              (vars (map car decls))
              (procs (map cadr decls))
              (bodies (cddr datum)))
         (create-letrec-assignments
           vars
           procs
           (make-cont2 '<cont2-10> bodies k))))
      (<macro-7> ()
       (let ((bindings (cadr datum)) (bodies (cddr datum)))
         (nest-let*-bindings bindings bodies k)))
      (<macro-8> ()
       (let ((exp (cadr datum)) (clauses (cddr datum)))
         (if (symbol? exp)
             (case-clauses->simple-cond-clauses
               exp
               clauses
               (make-cont '<cont-5> k))
             (case-clauses->cond-clauses
               'r
               clauses
               (make-cont2 '<cont2-12> exp k)))))
      (<macro-9> ()
       (let ((exp (cadr datum)) (clauses (cddr datum)))
         (if (symbol? exp)
             (record-case-clauses->cond-clauses
               exp
               clauses
               (make-cont2 '<cont2-14> k))
             (record-case-clauses->cond-clauses
               'r
               clauses
               (make-cont2 '<cont2-12> exp k)))))
      (else (error
             'apply-macro
             "bad macro-transformer: ~a"
             macro)))))

;;----------------------------------------------------------------------
;; main program

(define 1st (lambda (n) (string-ref chars-to-scan n)))

(define remaining (lambda (n) (+ 1 n)))

(define*
  scan-input
  (lambda (input handler fail k)
    (set! read-char-count 0)
    (set! read-line-count 1)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 handler fail k)))

(define*
  scan-input-loop
  (lambda (chars handler fail k)
    (apply-action '(goto start-state) '() chars handler fail
      (make-cont3 '<cont3-1> handler k))))

(define*
  apply-action
  (lambda (action buffer chars handler fail k)
    (record-case action
      (shift (next)
       (begin
         (set! read-char-count (+ read-char-count 1))
         (apply-action next (cons (1st chars) buffer)
           (remaining chars) handler fail k)))
      (replace (new-char next)
       (apply-action next (cons new-char buffer) (remaining chars)
         handler fail k))
      (drop-newline (next)
       (begin
         (set! read-line-count (+ read-line-count 1))
         (set! read-char-count 0)
         (apply-action next buffer (remaining chars) handler fail
           k)))
      (drop (next)
       (begin
         (set! read-char-count (+ read-char-count 1))
         (apply-action next buffer (remaining chars) handler fail
           k)))
      (goto (state)
       (let ((action (apply-state state (1st chars))))
         (if (eq? action 'error)
             (scan-error chars handler fail)
             (apply-action action buffer chars handler fail k))))
      (emit (token-type)
       (convert-buffer-to-token token-type buffer handler fail
         (make-cont2 '<cont2-2> chars k)))
      (else (error 'apply-action "invalid action: ~a" action)))))

(define*
  scan-error
  (lambda (chars handler fail)
    (let ((c (1st chars)))
      (if (char=? c #\nul)
          (apply-handler2
            handler
            (format
              "scan error: unexpected end of input at line ~a, char ~a"
              read-line-count
              read-char-count)
            fail)
          (apply-handler2
            handler
            (format
              "scan error: unexpected character ~a encountered at line ~a, char ~a"
              c
              read-line-count
              read-char-count)
            fail)))))

(define*
  convert-buffer-to-token
  (lambda (token-type buffer handler fail k)
    (let ((buffer (reverse buffer)))
      (case token-type
        (integer
         (apply-cont2 k (list 'integer (list->string buffer)) fail))
        (decimal
         (apply-cont2 k (list 'decimal (list->string buffer)) fail))
        (rational
         (apply-cont2 k (list 'rational (list->string buffer)) fail))
        (identifier
         (apply-cont2
           k
           (list 'identifier (string->symbol (list->string buffer)))
           fail))
        (boolean
         (apply-cont2
           k
           (list
             'boolean
             (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))
           fail))
        (character
         (apply-cont2 k (list 'character (car buffer)) fail))
        (named-character
         (let ((name (list->string buffer)))
           (cond
             ((string=? name "nul")
              (apply-cont2 k (list 'character #\nul) fail))
             ((string=? name "space")
              (apply-cont2 k (list 'character #\space) fail))
             ((string=? name "tab")
              (apply-cont2 k (list 'character #\tab) fail))
             ((string=? name "newline")
              (apply-cont2 k (list 'character #\newline) fail))
             ((string=? name "linefeed")
              (apply-cont2 k (list 'character #\newline) fail))
             ((string=? name "backspace")
              (apply-cont2 k (list 'character #\backspace) fail))
             ((string=? name "return")
              (apply-cont2 k (list 'character #\return) fail))
             ((string=? name "page")
              (apply-cont2 k (list 'character #\page) fail))
             (else
              (apply-handler2
                handler
                (format
                  "invalid character name '~a' at line ~a, char ~a"
                  name
                  read-line-count
                  read-char-count)
                fail)))))
        (string
         (apply-cont2 k (list 'string (list->string buffer)) fail))
        (else (apply-cont2 k (list token-type) fail))))))

(define token-type?
  (lambda (token class) (eq? (car token) class)))

(define get-line-count (lambda (token) (rac (rdc token))))

(define get-char-count (lambda (token) (rac token)))

(define rac
  (lambda (lyst)
    (cond
      ((null? (cdr lyst)) (car lyst))
      (else (rac (cdr lyst))))))

(define rdc
  (lambda (lyst)
    (cond
      ((null? (cdr lyst)) '())
      (else (cons (car lyst) (rdc (cdr lyst)))))))

(define char-delimiter?
  (lambda (c)
    (or (char-whitespace? c)
        (char=? c #\')
        (char=? c #\()
        (char=? c #\[)
        (char=? c #\))
        (char=? c #\])
        (char=? c #\")
        (char=? c #\;)
        (char=? c #\#)
        (char=? c #\nul))))

(define char-initial?
  (lambda (c)
    (or (char-alphabetic? c)
        (char=? c #\!)
        (char=? c #\$)
        (char=? c #\%)
        (char=? c #\&)
        (char=? c #\*)
        (char=? c #\/)
        (char=? c #\:)
        (char=? c #\<)
        (char=? c #\=)
        (char=? c #\>)
        (char=? c #\?)
        (char=? c #\^)
        (char=? c #\_)
        (char=? c #\~))))

(define char-special-subsequent?
  (lambda (c)
    (or (char=? c #\+)
        (char=? c #\-)
        (char=? c #\@)
        (char=? c #\.))))

(define char-subsequent?
  (lambda (c)
    (or (char-initial? c)
        (char-numeric? c)
        (char-special-subsequent? c))))

(define char-sign?
  (lambda (c) (or (char=? c #\+) (char=? c #\-))))

(define char-boolean?
  (lambda (c)
    (or (char=? c #\t)
        (char=? c #\T)
        (char=? c #\f)
        (char=? c #\F))))

(define apply-state
  (lambda (state c)
    (case state
      (start-state
       (cond
         ((char=? c #\newline) '(drop-newline (goto start-state)))
         ((char-whitespace? c) '(drop (goto start-state)))
         ((char=? c #\;) '(drop (goto comment-state)))
         ((char=? c #\() '(drop (emit lparen)))
         ((char=? c #\[) '(drop (emit lbracket)))
         ((char=? c #\)) '(drop (emit rparen)))
         ((char=? c #\]) '(drop (emit rbracket)))
         ((char=? c #\') '(drop (emit apostrophe)))
         ((char=? c #\`) '(drop (emit backquote)))
         ((char=? c #\,) '(drop (goto comma-state)))
         ((char=? c #\#) '(drop (goto hash-prefix-state)))
         ((char=? c #\") '(drop (goto string-state)))
         ((char-initial? c) '(shift (goto identifier-state)))
         ((char-sign? c) '(shift (goto signed-state)))
         ((char=? c #\.) '(shift (goto decimal-point-state)))
         ((char-numeric? c) '(shift (goto whole-number-state)))
         ((char=? c #\nul) '(drop (emit end-marker)))
         (else 'error)))
      (comment-state
       (cond
         ((char=? c #\newline) '(drop-newline (goto start-state)))
         ((char=? c #\nul) '(goto start-state))
         (else '(drop (goto comment-state)))))
      (comma-state
       (cond
         ((char=? c #\@) '(drop (emit comma-at)))
         (else '(emit comma))))
      (hash-prefix-state
       (cond
         ((char-boolean? c) '(shift (emit boolean)))
         ((char=? c #\\) '(drop (goto character-state)))
         ((char=? c #\() '(drop (emit lvector)))
         (else 'error)))
      (character-state
       (cond
         ((char-alphabetic? c)
          '(shift (goto alphabetic-character-state)))
         ((not (char=? c #\nul)) '(shift (emit character)))
         (else 'error)))
      (alphabetic-character-state
       (cond
         ((char-alphabetic? c) '(shift (goto named-character-state)))
         (else '(emit character))))
      (named-character-state
       (cond
         ((char-delimiter? c) '(emit named-character))
         (else '(shift (goto named-character-state)))))
      (string-state
       (cond
         ((char=? c #\") '(drop (emit string)))
         ((char=? c #\\) '(drop (goto string-escape-state)))
         ((char=? c #\nul) 'error)
         (else '(shift (goto string-state)))))
      (string-escape-state
       (cond
         ((char=? c #\") '(shift (goto string-state)))
         ((char=? c #\\) '(shift (goto string-state)))
         ((char=? c #\b) '(replace #\backspace (goto string-state)))
         ((char=? c #\f) '(replace #\page (goto string-state)))
         ((char=? c #\n) '(replace #\newline (goto string-state)))
         ((char=? c #\t) '(replace #\tab (goto string-state)))
         ((char=? c #\r) '(replace #\return (goto string-state)))
         (else 'error)))
      (identifier-state
       (cond
         ((char-subsequent? c) '(shift (goto identifier-state)))
         ((char-delimiter? c) '(emit identifier))
         (else 'error)))
      (signed-state
       (cond
         ((char-numeric? c) '(shift (goto whole-number-state)))
         ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
         ((char-delimiter? c) '(emit identifier))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (decimal-point-state
       (cond
         ((char-numeric? c) '(shift (goto fractional-number-state)))
         ((char-delimiter? c) '(emit dot))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (signed-decimal-point-state
       (cond
         ((char-numeric? c) '(shift (goto fractional-number-state)))
         ((char-delimiter? c) '(emit identifier))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (whole-number-state
       (cond
         ((char-numeric? c) '(shift (goto whole-number-state)))
         ((char=? c #\.) '(shift (goto fractional-number-state)))
         ((char=? c #\/) '(shift (goto rational-number-state)))
         ((or (char=? c #\e) (char=? c #\E))
          '(shift (goto suffix-state)))
         ((char-delimiter? c) '(emit integer))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (fractional-number-state
       (cond
         ((char-numeric? c) '(shift (goto fractional-number-state)))
         ((or (char=? c #\e) (char=? c #\E))
          '(shift (goto suffix-state)))
         ((char-delimiter? c) '(emit decimal))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (rational-number-state
       (cond
         ((char-numeric? c) '(shift (goto rational-number-state*)))
         ((char-delimiter? c) '(emit identifier))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (rational-number-state*
       (cond
         ((char-numeric? c) '(shift (goto rational-number-state*)))
         ((char-delimiter? c) '(emit rational))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (suffix-state
       (cond
         ((char-sign? c) '(shift (goto signed-exponent-state)))
         ((char-numeric? c) '(shift (goto exponent-state)))
         ((char-delimiter? c) '(emit identifier))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (signed-exponent-state
       (cond
         ((char-numeric? c) '(shift (goto exponent-state)))
         ((char-delimiter? c) '(emit identifier))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (exponent-state
       (cond
         ((char-numeric? c) '(shift (goto exponent-state)))
         ((char-delimiter? c) '(emit decimal))
         ((char-subsequent? c) '(shift (goto identifier-state)))
         (else 'error)))
      (else (error 'apply-state "invalid state: ~a" state)))))

(define first (lambda (x) (car x)))

(define rest-of (lambda (x) (cdr x)))

(define string->integer (lambda (str) (string->number str)))

(define string->decimal (lambda (str) (string->number str)))

(define string->rational
  (lambda (str) (string->number str)))

(define true? (lambda (v) (if v #t #f)))

(define*
  read-sexp
  (lambda (tokens handler fail k)
    (record-case (first tokens)
      (integer (str)
       (apply-cont3 k (string->integer str) (rest-of tokens) fail))
      (decimal (str)
       (apply-cont3 k (string->decimal str) (rest-of tokens) fail))
      (rational (str)
       (let ((num (string->rational str)))
         (if (true? num)
             (apply-cont3 k num (rest-of tokens) fail)
             (apply-handler2
               handler
               (format
                 "cannot represent ~a at line ~a, char ~a"
                 str
                 (get-line-count (first tokens))
                 (get-char-count (first tokens)))
               fail))))
      (boolean (bool) (apply-cont3 k bool (rest-of tokens) fail))
      (character (char)
       (apply-cont3 k char (rest-of tokens) fail))
      (string (str) (apply-cont3 k str (rest-of tokens) fail))
      (identifier (id) (apply-cont3 k id (rest-of tokens) fail))
      (apostrophe ()
       (read-abbreviation tokens 'quote handler fail k))
      (backquote ()
       (read-abbreviation tokens 'quasiquote handler fail k))
      (comma ()
       (read-abbreviation tokens 'unquote handler fail k))
      (comma-at ()
       (read-abbreviation tokens 'unquote-splicing handler fail k))
      (lparen ()
       (let ((tokens (rest-of tokens)))
         (if (token-type? (first tokens) 'dot)
             (read-error tokens handler fail)
             (read-sexp-sequence tokens 'rparen handler fail k))))
      (lbracket ()
       (let ((tokens (rest-of tokens)))
         (if (token-type? (first tokens) 'dot)
             (read-error tokens handler fail)
             (read-sexp-sequence tokens 'rbracket handler fail k))))
      (lvector ()
       (read-vector
         (rest-of tokens)
         handler
         fail
         (make-cont3 '<cont3-2> k)))
      (else (read-error tokens handler fail)))))

(define*
  read-abbreviation
  (lambda (tokens keyword handler fail k)
    (read-sexp
      (rest-of tokens)
      handler
      fail
      (make-cont3 '<cont3-3> keyword k))))

(define*
  read-sexp-sequence
  (lambda (tokens expected-terminator handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator handler
         fail k))
      (dot ()
       (read-sexp
         (rest-of tokens)
         handler
         fail
         (make-cont3 '<cont3-6> expected-terminator handler k)))
      (else (read-sexp
             tokens
             handler
             fail
             (make-cont3 '<cont3-5> expected-terminator handler k))))))

(define*
  close-sexp-sequence
  (lambda (sexp tokens expected-terminator handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
         ((token-type? (first tokens) expected-terminator)
          (apply-cont3 k sexp (rest-of tokens) fail))
         ((eq? expected-terminator 'rparen)
          (apply-handler2
            handler
            (format
              "parenthesized list terminated by bracket at line ~a, char ~a"
              (get-line-count (first tokens))
              (get-char-count (first tokens)))
            fail))
         ((eq? expected-terminator 'rbracket)
          (apply-handler2
            handler
            (format
              "bracketed list terminated by parenthesis at line ~a, char ~a"
              (get-line-count (first tokens))
              (get-char-count (first tokens)))
            fail))))
      (else (read-error tokens handler fail)))))

(define*
  read-vector
  (lambda (tokens handler fail k)
    (record-case (first tokens)
      (rparen () (apply-cont3 k '() (rest-of tokens) fail))
      (else (read-sexp
             tokens
             handler
             fail
             (make-cont3 '<cont3-7> handler k))))))

(define*
  read-error
  (lambda (tokens handler fail)
    (let ((token (first tokens))
          (where (if (null? load-stack)
                     ""
                     (format " in ~a" (car load-stack)))))
      (if (token-type? token 'end-marker)
          (apply-handler2
            handler
            (format
              "read error: unexpected end of input at line ~a, char ~a~a"
              (get-line-count token)
              (get-char-count token)
              where)
            fail)
          (apply-handler2
            handler
            (format
              "read error: unexpected token ~a encountered at line ~a, char ~a~a"
              (car token) (get-line-count token) (get-char-count token)
              where)
            fail)))))

(define read-content
  (lambda (filename)
    (apply
      string
      (call-with-input-file
        filename
        (lambda (port)
          (let loop ((char (read-char port)))
            (if (eof-object? char)
                '()
                (cons char (loop (read-char port))))))))))

(define read-next-sexp
  (lambda (tokens)
    (read-sexp
      tokens
      init-handler2
      init-fail
      (make-cont3 '<cont3-8>))))

(define scan-string
  (lambda (input)
    (scan-input input init-handler2 init-fail init-cont2)))

(define scan-file
  (lambda (filename)
    (scan-input
      (read-content filename)
      init-handler2
      init-fail
      init-cont2)))

(define read-string
  (lambda (input)
    (read-datum input init-handler2 init-fail init-cont3)))

(define*
  read-datum
  (lambda (input handler fail k)
    (scan-input
      input
      handler
      fail
      (make-cont2 '<cont2-4> handler k))))

(define read-file
  (lambda (filename)
    (scan-input
      (read-content filename)
      init-handler2
      init-fail
      (make-cont2 '<cont2-5>))))

(define*
  print-unparsed-sexps
  (lambda (tokens handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k 'done fail)
        (read-sexp
          tokens
          handler
          fail
          (make-cont3 '<cont3-11> handler k)))))

(define make-binding
  (lambda (variable value) (list variable "" value)))

(define binding-variable (lambda (binding) (car binding)))

(define binding-docstring (lambda (binding) (cadr binding)))

(define binding-value (lambda (binding) (caddr binding)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-car! (cdr binding) docstring)))

(define set-binding-value!
  (lambda (binding value) (set-car! (cddr binding) value)))

(define make-frame
  (lambda (variables values)
    (map make-binding variables values)))

(define first-binding (lambda (frame) (car frame)))

(define rest-of-bindings (lambda (frame) (cdr frame)))

(define empty-frame? (lambda (frame) (null? frame)))

(define search-frame
  (lambda (frame variable)
    (cond
      ((empty-frame? frame) #f)
      ((eq? (binding-variable (first-binding frame)) variable)
       (first-binding frame))
      (else (search-frame (rest-of-bindings frame) variable)))))

(define environment?
  (lambda (x) (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda () (cons 'environment '(()))))

(define make-initial-environment
  (lambda (vars vals)
    (cons 'environment (list (make-frame vars vals)))))

(define first-frame (lambda (env) (cadr env)))

(define frames (lambda (env) (cdr env)))

(define set-first-frame!
  (lambda (env new-frame) (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values)
    (cons
      'environment
      (cons (make-frame variables values) (cdr env)))))

(define search-env
  (lambda (env variable) (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
        #f
        (let ((binding (search-frame (car frames) variable)))
          (if binding
              binding
              (search-frames (cdr frames) variable))))))

(define*
  lookup-value
  (lambda (variable env handler fail k)
    (lookup-binding variable env handler fail
      (make-cont2 '<cont2-6> k))))

(define*
  lookup-binding
  (lambda (variable env handler fail k)
    (let ((binding (search-env env variable)))
      (if binding
          (apply-cont2 k binding fail)
          (split-variable
            variable
            fail
            (make-cont2 '<cont2-7> variable env handler k))))))

(define*
  lookup-binding-in-first-frame
  (lambda (var env handler fail k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
            (apply-cont2 k binding fail)
            (let ((new-binding (make-binding var 'undefined)))
              (let ((new-frame (cons new-binding frame)))
                (set-first-frame! env new-frame)
                (apply-cont2 k new-binding fail))))))))

(define*
  lookup-variable-components
  (lambda (components path env handler fail k)
    (let ((var (car components)))
      (lookup-module-binding var env path handler fail
        (make-cont2 '<cont2-8> components path var handler k)))))

(define*
  lookup-module-binding
  (lambda (var env path handler fail k)
    (let ((binding (search-env env var)))
      (cond
        (binding (apply-cont2 k binding fail))
        ((dlr-env-contains var)
         (apply-cont2 k (dlr-env-lookup var) fail))
        ((string=? path "")
         (apply-handler2
           handler
           (format "unbound module '~a'" var)
           fail))
        (else
         (apply-handler2
           handler
           (format "unbound variable '~a' in module '~a'" var path)
           fail))))))

(define*
  split-variable
  (lambda (variable fail k)
    (let ((strings (group
                     (string->list (symbol->string variable))
                     #\.)))
      (if (or (member "" strings) (= (length strings) 1))
          (apply-cont2 k #f fail)
          (apply-cont2 k (map string->symbol strings) fail)))))

(define group
  (lambda (chars delimiter)
    (letrec ((position (lambda (chars)
                         (if (char=? (car chars) delimiter)
                             0
                             (+ 1 (position (cdr chars))))))
             (group (lambda (chars)
                      (cond
                        ((null? chars) '())
                        ((not (member delimiter chars))
                         (list (apply string chars)))
                        (else
                         (let ((n (position chars)))
                           (cons
                             (apply string (list-head chars n))
                             (group (cdr (list-tail chars n))))))))))
      (group chars))))

(define syntactic-sugar?
  (lambda (datum)
    (and (pair? datum)
         (symbol? (car datum))
         (true? (search-env macro-env (car datum))))))

(define make-pattern-macro
  (lambda (clauses) (cons 'pattern-macro clauses)))

(define macro-clauses (lambda (macro) (cdr macro)))

(define pattern-macro?
  (lambda (x) (and (pair? x) (eq? (car x) 'pattern-macro))))

(define*
  expand-once
  (lambda (datum handler fail k)
    (lookup-value (car datum) macro-env handler fail
      (make-cont2 '<cont2-9> datum handler k))))

(define*
  process-macro-clauses
  (lambda (clauses datum handler fail k)
    (if (null? clauses)
        (apply-handler2
          handler
          (format "no matching clause found for ~a" datum)
          fail)
        (let ((left-pattern (caar clauses))
              (right-pattern (cadar clauses)))
          (unify-patterns
            left-pattern
            datum
            (make-cont '<cont-3> clauses datum right-pattern handler
              fail k))))))

(define*
  create-letrec-assignments
  (lambda (vars procs k2)
    (if (null? vars)
        (apply-cont2 k2 '() '())
        (create-letrec-assignments
          (cdr vars)
          (cdr procs)
          (make-cont2 '<cont2-11> procs vars k2)))))

(define*
  nest-let*-bindings
  (lambda (bindings bodies k)
    (if (or (null? bindings) (null? (cdr bindings)))
        (apply-cont k `(let ,bindings ,@bodies))
        (nest-let*-bindings
          (cdr bindings)
          bodies
          (make-cont '<cont-4> bindings k)))))

(define*
  case-clauses->simple-cond-clauses
  (lambda (var clauses k)
    (if (null? clauses)
        (apply-cont k '())
        (case-clauses->simple-cond-clauses
          var
          (cdr clauses)
          (make-cont '<cont-6> clauses var k)))))

(define*
  case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (case-clauses->cond-clauses
          var
          (cdr clauses)
          (make-cont2 '<cont2-13> clauses var k2)))))

(define*
  record-case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (record-case-clauses->cond-clauses
          var
          (cdr clauses)
          (make-cont2 '<cont2-15> clauses var k2)))))

(define make-macro-env
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case)
      (list and-transformer or-transformer cond-transformer
        let-transformer letrec-transformer let*-transformer
        case-transformer record-case-transformer))))

(define*
  parse
  (lambda (datum handler fail k)
    (cond
      ((literal? datum) (apply-cont2 k (lit-exp datum) fail))
      ((quote? datum) (apply-cont2 k (lit-exp (cadr datum)) fail))
      ((quasiquote? datum)
       (expand-quasiquote
         (cadr datum)
         (make-cont '<cont-7> handler fail k)))
      ((unquote? datum)
       (apply-handler2 handler (format "misplaced ~a" datum) fail))
      ((unquote-splicing? datum)
       (apply-handler2 handler (format "misplaced ~a" datum) fail))
      ((symbol? datum) (apply-cont2 k (var-exp datum) fail))
      ((syntactic-sugar? datum)
       (expand-once
         datum
         handler
         fail
         (make-cont2 '<cont2-41> handler k)))
      ((if-then? datum)
       (parse
         (cadr datum)
         handler
         fail
         (make-cont2 '<cont2-40> datum handler k)))
      ((if-else? datum)
       (parse
         (cadr datum)
         handler
         fail
         (make-cont2 '<cont2-38> datum handler k)))
      ((assignment? datum)
       (parse
         (caddr datum)
         handler
         fail
         (make-cont2 '<cont2-35> datum k)))
      ((func? datum)
       (parse
         (cadr datum)
         handler
         fail
         (make-cont2 '<cont2-34> k)))
      ((define? datum)
       (cond
         ((mit-style? datum)
          (apply-macro
            mit-define-transformer
            datum
            (make-cont '<cont-7> handler fail k)))
         ((= (length datum) 3)
          (parse
            (caddr datum)
            handler
            fail
            (make-cont2 '<cont2-33> datum k)))
         ((and (= (length datum) 4) (string? (caddr datum)))
          (parse
            (cadddr datum)
            handler
            fail
            (make-cont2 '<cont2-32> datum k)))
         (else
          (apply-handler2
            handler
            (format "bad concrete syntax: ~a" datum)
            fail))))
      ((define!? datum)
       (cond
         ((mit-style? datum)
          (apply-macro
            mit-define-transformer
            datum
            (make-cont '<cont-7> handler fail k)))
         ((= (length datum) 3)
          (parse
            (caddr datum)
            handler
            fail
            (make-cont2 '<cont2-31> datum k)))
         ((and (= (length datum) 4) (string? (caddr datum)))
          (parse
            (cadddr datum)
            handler
            fail
            (make-cont2 '<cont2-30> datum k)))
         (else
          (apply-handler2
            handler
            (format "bad concrete syntax: ~a" datum)
            fail))))
      ((define-syntax? datum)
       (apply-cont2
         k
         (define-syntax-exp (cadr datum) (cddr datum))
         fail))
      ((begin? datum)
       (parse-all
         (cdr datum)
         handler
         fail
         (make-cont2 '<cont2-29> datum handler k)))
      ((lambda? datum)
       (parse
         (cons 'begin (cddr datum))
         handler
         fail
         (make-cont2 '<cont2-28> datum k)))
      ((try? datum)
       (cond
         ((= (length datum) 2)
          (parse (try-body datum) handler fail k))
         ((and (= (length datum) 3) (catch? (caddr datum)))
          (parse
            (try-body datum)
            handler
            fail
            (make-cont2 '<cont2-27> datum handler k)))
         ((and (= (length datum) 3) (finally? (caddr datum)))
          (parse
            (try-body datum)
            handler
            fail
            (make-cont2 '<cont2-25> datum handler k)))
         ((and (= (length datum) 4)
               (catch? (caddr datum))
               (finally? (cadddr datum)))
          (parse
            (try-body datum)
            handler
            fail
            (make-cont2 '<cont2-23> datum handler k)))
         (else
          (apply-handler2
            handler
            (format "bad try syntax: ~a" datum)
            fail))))
      ((raise? datum)
       (parse
         (cadr datum)
         handler
         fail
         (make-cont2 '<cont2-20> k)))
      ((dict? datum)
       (parse-pairs
         (cdr datum)
         handler
         fail
         (make-cont2 '<cont2-19> k)))
      ((help? datum)
       (if (symbol? (cadr datum))
           (apply-cont2 k (help-exp (cadr datum)) fail)
           (apply-handler2
             handler
             (format "bad concrete syntax: ~a" datum)
             fail)))
      ((choose? datum)
       (parse-all
         (cdr datum)
         handler
         fail
         (make-cont2 '<cont2-18> k)))
      ((application? datum)
       (parse
         (car datum)
         handler
         fail
         (make-cont2 '<cont2-17> datum handler k)))
      (else
       (apply-handler2
         handler
         (format "bad concrete syntax: ~a" datum)
         fail)))))

(define*
  parse-pairs
  (lambda (pairs handler fail k)
    (if (null? pairs)
        (apply-cont2 k '() fail)
        (parse
          (caar pairs)
          handler
          fail
          (make-cont2 '<cont2-44> pairs handler k)))))

(define*
  parse-all
  (lambda (datum-list handler fail k)
    (if (null? datum-list)
        (apply-cont2 k '() fail)
        (parse
          (car datum-list)
          handler
          fail
          (make-cont2 '<cont2-46> datum-list handler k)))))

(define*
  expand-quasiquote
  (lambda (datum k)
    (cond
      ((vector? datum)
       (expand-quasiquote
         (vector->list datum)
         (make-cont '<cont-12> k)))
      ((not (pair? datum)) (apply-cont k `',datum))
      ((quasiquote? datum) (apply-cont k `',datum))
      ((unquote? datum) (apply-cont k (cadr datum)))
      ((unquote-splicing? (car datum))
       (if (null? (cdr datum))
           (apply-cont k (cadr (car datum)))
           (expand-quasiquote
             (cdr datum)
             (make-cont '<cont-11> datum k))))
      ((quasiquote-list? datum)
       (expand-quasiquote-list datum (make-cont '<cont-10> k)))
      (else
       (expand-quasiquote
         (car datum)
         (make-cont '<cont-9> datum k))))))

(define*
  expand-quasiquote-list
  (lambda (datum k)
    (if (null? datum)
        (apply-cont k '())
        (expand-quasiquote
          (car datum)
          (make-cont '<cont-14> datum k)))))

(define quasiquote-list?
  (lambda (datum)
    (or (null? datum)
        (and (pair? datum)
             (not (quasiquote? datum))
             (not (unquote? datum))
             (not (unquote-splicing? datum))
             (not (quasiquote? (car datum)))
             (not (unquote-splicing? (car datum)))
             (quasiquote-list? (cdr datum))))))

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((pair? (cdr formals))
       (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define mit-style?
  (lambda (datum) (not (symbol? (cadr datum)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
        (boolean? datum)
        (char? datum)
        (string? datum)
        (vector? datum))))

(define anything? (lambda (datum) #t))

(define tagged-list
  (lambda (tag op len)
    (lambda (datum)
      (and (list? datum)
           (op (length datum) len)
           (eq? (car datum) tag)))))

(define try-body (lambda (x) (cadr x)))

(define catch-var (lambda (x) (cadr x)))

(define catch-exps (lambda (x) (cddr x)))

(define finally-exps (lambda (x) (cdr x)))

(define application?
  (lambda (datum)
    (and (list? datum)
         (not (null? datum))
         (not (reserved-keyword? (car datum))))))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define begin
      cond and or let let* letrec case record-case try catch
      finally raise dict help choose)))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x) (memq x (get-reserved-keywords)))))

(define parse-string
  (lambda (string)
    (read-datum
      string
      init-handler2
      init-fail
      (make-cont3 '<cont3-12>))))

(define print-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (get-parsed-sexps filename))))

(define get-parsed-sexps
  (lambda (filename)
    (scan-input
      (read-content filename)
      init-handler2
      init-fail
      (make-cont2 '<cont2-47>))))

(define*
  parse-sexps
  (lambda (tokens handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k '() fail)
        (read-sexp
          tokens
          handler
          fail
          (make-cont3 '<cont3-13> handler k)))))

(define pretty-print-prim
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print
      (if (procedure-object? arg) '<procedure> arg))))

(define procedure-object?
  (lambda (x)
    (or (procedure? x)
        (and (pair? x) (eq? (car x) 'procedure)))))

(define newline-prim
  (lambda () (set! *need-newline* #f) (newline)))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

(define display-prim
  (lambda (x)
    (let ((s (format "~a" x)))
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s))))

(define start
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (read-eval-print REP-fail)))

(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print REP-fail)))

(define read-line
  (lambda (prompt)
    (printf prompt)
    (let ((input (read))) (format "~s" input))))

(define raw-read-line
  (lambda (prompt)
    (printf prompt)
    (let loop ((input (read)))
      (if (string? input)
          input
          (begin
            (printf
              "Error: input must be enclosed in quotation marks.\n==> ")
            (loop (read)))))))

(define*
  read-eval-print
  (lambda (fail)
    (set! load-stack '())
    (let ((input-string (read-line "==> ")))
      (scan-input
        input-string
        REP-handler
        fail
        (make-cont2 '<cont2-51>)))))

(define*
  read-and-eval-sexps
  (lambda (tokens env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k '<void> fail)
        (read-sexp
          tokens
          handler
          fail
          (make-cont3 '<cont3-14> env handler k)))))

(define*
  m
  (lambda (exp env handler fail k)
    (cases expression exp
     (lit-exp (datum) (apply-cont2 k datum fail))
     (var-exp (id) (lookup-value id env handler fail k))
     (func-exp
       (exp)
       (m exp env handler fail (make-cont2 '<cont2-67> k)))
     (if-exp
       (test-exp then-exp else-exp)
       (m test-exp env handler fail
          (make-cont2 '<cont2-66> else-exp then-exp env handler k)))
     (assign-exp
       (var rhs-exp)
       (m rhs-exp env handler fail
          (make-cont2 '<cont2-65> var env handler k)))
     (define-exp
       (var docstring rhs-exp)
       (m rhs-exp env handler fail
          (make-cont2 '<cont2-63> docstring var env handler k)))
     (define!-exp
       (var docstring rhs-exp)
       (m rhs-exp env handler fail
          (make-cont2 '<cont2-61> docstring var k)))
     (define-syntax-exp
       (keyword clauses)
       (lookup-binding-in-first-frame keyword macro-env handler
         fail (make-cont2 '<cont2-60> clauses k)))
     (begin-exp (exps) (eval-sequence exps env handler fail k))
     (lambda-exp
       (formals body)
       (apply-cont2 k (closure formals body env) fail))
     (mu-lambda-exp
       (formals runt body)
       (apply-cont2 k (mu-closure formals runt body env) fail))
     (try-catch-exp
       (body cvar cexps)
       (let ((new-handler (try-catch-handler cvar cexps env handler
                            k)))
         (m body env new-handler fail k)))
     (try-finally-exp
       (body fexps)
       (let ((new-handler (try-finally-handler fexps env handler)))
         (m body env new-handler fail
            (make-cont2 '<cont2-59> fexps env handler k))))
     (try-catch-finally-exp
       (body cvar cexps fexps)
       (let ((new-handler (try-catch-finally-handler cvar cexps
                            fexps env handler k)))
         (m body env new-handler fail
            (make-cont2 '<cont2-59> fexps env handler k))))
     (raise-exp
       (exp)
       (m exp env handler fail (make-cont2 '<cont2-57> handler)))
     (dict-exp (pairs) (apply-cont2 k (list 'dict pairs) fail))
     (help-exp
       (var)
       (if (reserved-keyword? var)
           (apply-cont2 k (format "~a is a keyword" var) fail)
           (lookup-binding var env handler fail
             (make-cont2 '<cont2-56> k))))
     (choose-exp (exps) (eval-choices exps env handler fail k))
     (app-exp
       (operator operands)
       (m* operands env handler fail
           (make-cont2 '<cont2-55> operator env handler k)))
     (else (error 'm "bad abstract syntax: ~a" exp)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (make-handler2 '<handler2-3> cexps cvar env handler k)))

(define try-finally-handler
  (lambda (fexps env handler)
    (make-handler2 '<handler2-4> fexps env handler)))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (make-handler2 '<handler2-5> cexps cvar fexps env handler
      k)))

(define*
  eval-choices
  (lambda (exps env handler fail k)
    (if (null? exps)
        (apply-fail fail)
        (let ((new-fail (make-fail '<fail-4> exps env handler fail
                          k)))
          (m (car exps) env handler new-fail k)))))

(define closure
  (lambda (formals body env)
    (make-proc '<proc-1> formals body env)))

(define mu-closure
  (lambda (formals runt body env)
    (make-proc '<proc-2> formals runt body env)))

(define*
  m*
  (lambda (exps env handler fail k)
    (if (null? exps)
        (apply-cont2 k '() fail)
        (m (car exps) env handler fail
           (make-cont2 '<cont2-70> exps env handler k)))))

(define*
  eval-sequence
  (lambda (exps env handler fail k)
    (m (car exps) env handler fail
       (make-cont2 '<cont2-71> exps env handler k))))

(define make-initial-env-extended (lambda (env) env))

(define*
  length-loop
  (lambda (x sum ls handler fail k2)
    (cond
      ((null? x) (apply-cont2 k2 sum fail))
      ((not (pair? x))
       (apply-handler2
         handler
         (format "~a is not a proper list" ls)
         fail))
      (else (length-loop (cdr x) (+ sum 1) ls handler fail k2)))))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
      (make-initial-environment
        (list 'exit 'eval 'parse 'parse-string 'apply 'sqrt 'print
         'display 'newline 'load 'length 'null? 'cons 'car 'cdr 'cadr
         'caddr 'list '+ '- '* '/ '< '> '= 'abs 'equal? 'eq? 'memq
         'member 'range 'set-car! 'set-cdr! 'import 'get
         'call-with-current-continuation 'call/cc 'abort 'require
         'cut 'reverse 'append 'list->vector 'dir 'current-time 'map
         'for-each 'env 'using 'not 'printf 'vector 'vector-set!
         'vector-ref 'make-vector)
        (list (make-proc '<proc-56>) (make-proc '<proc-55>)
         (make-proc '<proc-54>) (make-proc '<proc-53>)
         (make-proc '<proc-52>) (make-proc '<proc-51>)
         (make-proc '<proc-50>) (make-proc '<proc-49>)
         (make-proc '<proc-48>) (make-proc '<proc-47>) length-prim
         (make-proc '<proc-46>) (make-proc '<proc-45>)
         (make-proc '<proc-44>) (make-proc '<proc-43>)
         (make-proc '<proc-42>) (make-proc '<proc-41>)
         (make-proc '<proc-40>) (make-proc '<proc-39>)
         (make-proc '<proc-38>) (make-proc '<proc-37>)
         (make-proc '<proc-36>) (make-proc '<proc-35>)
         (make-proc '<proc-34>) (make-proc '<proc-33>)
         (make-proc '<proc-32>) (make-proc '<proc-31>)
         (make-proc '<proc-30>) (make-proc '<proc-29>)
         (make-proc '<proc-28>) (make-proc '<proc-27>)
         (make-proc '<proc-26>) (make-proc '<proc-25>)
         (make-proc '<proc-24>) (make-proc '<proc-23>)
         (make-proc '<proc-22>) (make-proc '<proc-22>)
         (make-proc '<proc-21>) (make-proc '<proc-20>)
         (make-proc '<proc-19>) (make-proc '<proc-18>)
         (make-proc '<proc-17>) (make-proc '<proc-16>)
         (make-proc '<proc-15>) (make-proc '<proc-14>)
         (make-proc '<proc-13>) (make-proc '<proc-12>)
         (make-proc '<proc-11>) (make-proc '<proc-10>)
         (make-proc '<proc-9>) (make-proc '<proc-8>)
         (make-proc '<proc-7>) (make-proc '<proc-6>)
         (make-proc '<proc-5>) (make-proc '<proc-4>))))))

(define*
  equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
           (and (boolean? x) (boolean? y) (eq? x y))
           (and (symbol? x) (symbol? y) (eq? x y))
           (and (number? x) (number? y) (= x y))
           (and (char? x) (char? y) (char=? x y))
           (and (string? x) (string? y) (string=? x y)))
       (apply-cont k #t))
      ((and (pair? x) (pair? y))
       (equal-objects?
         (car x)
         (car y)
         (make-cont '<cont-16> x y k)))
      ((and (vector? x)
            (vector? y)
            (= (vector-length x) (vector-length y)))
       (equal-vectors? x y (- (vector-length x) 1) k))
      (else (apply-cont k #f)))))

(define*
  equal-vectors?
  (lambda (v1 v2 i k)
    (if (< i 0)
        (apply-cont k #t)
        (equal-objects?
          (vector-ref v1 i)
          (vector-ref v2 i)
          (make-cont '<cont-17> i v1 v2 k)))))

(define*
  member-prim
  (lambda (x ls orig-ls handler fail k)
    (cond
      ((null? ls) (apply-cont2 k #f fail))
      ((not (pair? ls))
       (apply-handler2
         handler
         (format "improper list ~a" orig-ls)
         fail))
      (else
       (equal-objects?
         x
         (car ls)
         (make-cont '<cont-18> ls orig-ls x handler fail k))))))

(define*
  map-prim
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args)) (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2)
             (map2 proc (car list-args) (cadr list-args) env handler fail
               k))
            (else (mapN proc list-args env handler fail k)))))))

(define*
  iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define*
  iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (apply-cont2 k '() fail)
          (apply-proc proc (list item) env handler fail
            (make-cont2 '<cont2-74> iterator proc env handler k))))))

(define*
  iterate-collect
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-collect-continue proc iterator env handler fail
        k))))

(define*
  iterate-collect-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (apply-cont2 k '() fail)
          (apply-proc proc (list item) env handler fail
            (make-cont2 '<cont2-75> iterator proc env handler k))))))

(define listify
  (lambda (arg-list)
    (cond
      ((null? arg-list) '())
      ((list? (car arg-list))
       (cons (car arg-list) (listify (cdr arg-list))))
      ((vector? (car arg-list))
       (cons
         (my-vector->list (car arg-list))
         (listify (cdr arg-list))))
      ((string? (car arg-list))
       (cons
         (string->list (car arg-list))
         (listify (cdr arg-list))))
      (else
       (error 'map
         "cannot use object type '~a' in map"
         (get_type (car arg-list)))))))

(define*
  map1
  (lambda (proc list1 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (map1 proc (cdr list1) env handler fail
              (make-cont2 '<cont2-77> list1 proc k))
            (apply-proc proc (list (car list1)) env handler fail
              (make-cont2 '<cont2-76> list1 proc env handler k))))))

(define*
  map2
  (lambda (proc list1 list2 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (map2 proc (cdr list1) (cdr list2) env handler fail
              (make-cont2 '<cont2-79> list1 list2 proc k))
            (apply-proc proc (list (car list1) (car list2)) env handler
              fail
              (make-cont2 '<cont2-78> list1 list2 proc env handler k))))))

(define*
  mapN
  (lambda (proc lists env handler fail k)
    (if (null? (car lists))
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (mapN proc (map cdr lists) env handler fail
              (make-cont2 '<cont2-81> lists proc k))
            (apply-proc proc (map car lists) env handler fail
              (make-cont2 '<cont2-80> lists proc env handler k))))))

(define*
  for-each-prim
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
        (iterate proc (car lists) env handler fail k)
        (let ((arg-list (listify lists)))
          (if (null? (car arg-list))
              (apply-cont2 k '<void> fail)
              (if (dlr-exp? proc)
                  (begin
                    (dlr-apply proc (map car arg-list))
                    (for-each-prim proc (map cdr arg-list) env handler fail
                      k))
                  (apply-proc proc (map car arg-list) env handler fail
                    (make-cont2 '<cont2-82> arg-list proc env handler
                      k))))))))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
         (inexact (/ (time-nanosecond now) 1000000000))))))

(define*
  get-primitive
  (lambda (args env handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env handler fail
        (make-cont2 '<cont2-83> args sym handler k)))))

(define*
  import-primitive
  (lambda (args env handler fail k)
    (let ((filename (car args)))
      (if (null? (cdr args))
          (load-file filename env handler fail k)
          (let ((module-name (cadr args)))
            (lookup-binding-in-first-frame module-name env handler fail
              (make-cont2 '<cont2-84> filename env handler k)))))))

(define*
  call/cc-primitive
  (lambda (proc env handler fail k)
    (let ((fake-k (make-proc '<proc-57> k)))
      (if (dlr-exp? proc)
          (apply-cont2 k (dlr-apply proc (list fake-k)) fail)
          (apply-proc proc (list fake-k) env handler fail k)))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

(define dir
  (lambda (args env)
    (sort
      symbol<?
      (if (null? args)
          (flatten
            (append
              (get-reserved-keywords)
              (map get-variables-from-frame (frames macro-env))
              (map get-variables-from-frame (frames env))))
          (get-variables-from-frame (car (frames (car args))))))))

(define get-variables-from-frame
  (lambda (frame) (map binding-variable frame)))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
          (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define*
  load-file
  (lambda (filename env handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (apply-cont2 k '<void> fail))
      ((not (string? filename))
       (apply-handler2
         handler
         (format "filename is not a string: ~a" filename)
         fail))
      ((not (file-exists? filename))
       (apply-handler2
         handler
         (format "file does not exist: ~a" filename)
         fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input
         (read-content filename)
         handler
         fail
         (make-cont2 '<cont2-86> env handler k))))))

(define*
  load-files
  (lambda (filenames env handler fail k)
    (if (null? filenames)
        (apply-cont2 k '<void> fail)
        (load-file (car filenames) env handler fail
          (make-cont2 '<cont2-87> filenames env handler k)))))

(define range
  (lambda args
    (letrec ((range (lambda (n end step acc)
                      (if (>= n end)
                          (reverse acc)
                          (range (+ n step) end step (cons n acc))))))
      (cond
        ((null? (cdr args)) (range 0 (car args) 1 '()))
        ((null? (cddr args)) (range (car args) (cadr args) 1 '()))
        (else (range (car args) (cadr args) (caddr args) '()))))))

(define make-external-proc
  (lambda (external-function-object)
    (make-proc '<proc-58> external-function-object)))

(define Main
  (lambda filenames
    (printf "Calico Scheme (0.2)\n")
    (printf "(c) 2009-2011, IPRE\n")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (set! load-stack '())
    (load-files filenames toplevel-env REP-handler REP-fail
      REP-k)
    (trampoline)))

(define execute
  (lambda (string)
    (set! load-stack '())
    (scan-input
      string
      init-handler2
      init-fail
      (make-cont2 '<cont2-88>))
    (trampoline)))

(define execute-file
  (lambda (filename)
    (set! load-stack '())
    (load-file filename toplevel-env init-handler2 init-fail
      init-cont2)
    (trampoline)))

(define try-parse-string
  (lambda (string)
    (scan-input
      string
      init-handler2
      init-fail
      (make-cont2 '<cont2-89>))
    (trampoline)))

(define pattern?
  (lambda (x)
    (or (null? x)
        (number? x)
        (boolean? x)
        (symbol? x)
        (and (pair? x) (pattern? (car x)) (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
         (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x)) (not (pair? x)))))

(define*
  occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (apply-cont k #f))
      ((pattern-variable? pattern)
       (apply-cont k (equal? var pattern)))
      (else
       (occurs?
         var
         (car pattern)
         (make-cont '<cont-19> pattern var k))))))

(define*
  unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
           (apply-cont k (make-sub 'unit p1 p2))
           (occurs? p1 p2 (make-cont '<cont-20> p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2))
       (apply-cont k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (apply-cont k #f)))))

(define*
  unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns
      (car pair1)
      (car pair2)
      (make-cont '<cont-24> pair1 pair2 k))))

(define*
  instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (apply-cont k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate
         (car pattern)
         s
         (make-cont '<cont-26> pattern s k)))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

(define make-sub (lambda args (cons 'substitution args)))

(define*
  apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (apply-cont k var))
      (unit (new-var new-pattern)
       (if (equal? var new-var)
           (apply-cont k new-pattern)
           (apply-cont k var)))
      (composite (s1 s2)
       (apply-sub s1 var (make-cont '<cont-27> s2 k)))
      (else (error 'apply-sub "bad substitution: ~a" s)))))

(define chars-to-scan 'undefined)

(define read-line-count 'undefined)

(define read-char-count 'undefined)

(define init-cont (make-cont '<cont-1>))

(define init-cont2 (make-cont2 '<cont2-3>))

(define init-cont3 (make-cont3 '<cont3-9>))

(define init-handler (make-handler '<handler-1>))

(define init-handler2 (make-handler2 '<handler2-1>))

(define init-fail (make-fail '<fail-1>))

(define mit-define-transformer (make-macro '<macro-1>))

(define and-transformer (make-macro '<macro-2>))

(define or-transformer (make-macro '<macro-3>))

(define cond-transformer (make-macro '<macro-4>))

(define let-transformer (make-macro '<macro-5>))

(define letrec-transformer (make-macro '<macro-6>))

(define let*-transformer (make-macro '<macro-7>))

(define case-transformer (make-macro '<macro-8>))

(define record-case-transformer (make-macro '<macro-9>))

(define macro-env (make-macro-env))

(define quote? (tagged-list 'quote = 2))

(define func? (tagged-list 'func = 2))

(define quasiquote? (tagged-list 'quasiquote = 2))

(define unquote? (tagged-list 'unquote = 2))

(define unquote-splicing?
  (tagged-list 'unquote-splicing = 2))

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

(define catch? (tagged-list 'catch >= 3))

(define finally? (tagged-list 'finally >= 2))

(define *need-newline* #f)

(define REP-k (make-cont2 '<cont2-50>))

(define REP-handler (make-handler2 '<handler2-2>))

(define REP-fail (make-fail '<fail-2>))

(define load-stack '())

(define length-prim (make-proc '<proc-3>))

(define toplevel-env (make-toplevel-env))

(define make-vector list->vector)


(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

(load "petite-init.ss")
(load "define-datatype.ss")

(define-datatype aexpression aexpression?
 (lit-aexp (datum anything?) (info source-info?))
 (var-aexp (id symbol?) (info source-info?))
 (lexical-address-aexp
   (depth number?)
   (offset number?)
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
 (func-aexp (exp aexpression?) (info source-info?))
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
   (clauses (list-of define-syntax-clause?))
   (aclauses list-of-define-syntax-clauses?^)
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
 (mu-trace-lambda-aexp (name symbol?) (formals (list-of symbol?)) (runt symbol?)
   (bodies (list-of aexpression?)) (info source-info?))
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
 (try-catch-finally-aexp (body aexpression?) (catch-var symbol?)
   (catch-exps (list-of aexpression?))
   (finally-exps (list-of aexpression?)) (info source-info?))
 (raise-aexp (exp aexpression?) (info source-info?))
 (choose-aexp
   (exps (list-of aexpression?))
   (info source-info?)))

;;----------------------------------------------------------------------
;; continuation datatype

(define make-cont (lambda args (cons 'continuation args)))

(define*
  apply-cont
  (lambda (k value)
    (record-case (cdr k)
      (<cont-1> (chars fail k) (apply-cont3 k value chars fail))
      (<cont-2> (v1 info k)
       (apply-cont k (list pair-tag v1 value info)))
      (<cont-3> (x info k)
       (annotate-cps
         (cdr x)
         'none
         (make-cont '<cont-2> value info k)))
      (<cont-4> (k) (apply-cont k (list->vector value)))
      (<cont-5> (v1 k) (apply-cont k (cons v1 value)))
      (<cont-6> (x k)
       (unannotate-cps (cdr x) (make-cont '<cont-5> value k)))
      (<cont-7> (x k)
       (unannotate-cps (caddr x) (make-cont '<cont-5> value k)))
      (<cont-8> (end tokens-left fail k)
       (apply-cont4 k value end tokens-left fail))
      (<cont-9> (end tokens fail k)
       (apply-cont4 k value end (rest-of tokens) fail))
      (<cont-10> (src start tokens handler fail k)
       (read-sexp (rest-of tokens) src handler fail
         (make-cont4 '<cont4-3> src start value k)))
      (<cont-11> () (halt* value))
      (<cont-12> (adatum senv info handler fail k)
       (let ((formals-list (if (list? value)
                               value
                               (cons (last value) (head value))))
             (name (untag-atom^ (cadr^ adatum))))
         (aparse-all (cdddr^ adatum) (cons formals-list senv) handler
           fail (make-cont2 '<cont2-16> name value info k))))
      (<cont-13> (adatum senv info handler fail k)
       (let ((formals-list (if (list? value)
                               value
                               (cons (last value) (head value)))))
         (aparse-all (cddr^ adatum) (cons formals-list senv) handler
           fail (make-cont2 '<cont2-17> value info k))))
      (<cont-14> (aclauses name info fail k)
       (apply-cont2
         k
         (define-syntax-aexp name value aclauses info)
         fail))
      (<cont-15> (senv info handler fail k)
       (aparse (replace-info value info) senv handler fail k))
      (<cont-16> (senv info handler fail k)
       (annotate-cps
         value
         'none
         (make-cont '<cont-15> senv info handler fail k)))
      (<cont-17> (adatum senv info handler fail k)
       (if (original-source-info? adatum)
           (aparse (replace-info value (snoc 'quasiquote info)) senv
             handler fail k)
           (aparse (replace-info value info) senv handler fail k)))
      (<cont-18> (adatum senv info handler fail k)
       (annotate-cps
         value
         'none
         (make-cont '<cont-17> adatum senv info handler fail k)))
      (<cont-19> (info fail k)
       (apply-cont2 k (lit-aexp (cadr value) info) fail))
      (<cont-20> (info fail k)
       (apply-cont2 k (lit-aexp value info) fail))
      (<cont-21> (msg info handler fail)
       (apply-handler2
         handler
         (make-exception "ParseError" (format "~s ~a" msg value) (get-srcfile info)
           (get-start-line info) (get-start-char info))
         fail))
      (<cont-22> (bindings k)
       (apply-cont k `(let ((unquote (car^ bindings))) ,value)))
      (<cont-23> (clauses var k)
       (let ((clause (car^ clauses)))
         (cond
           ((eq?^ (car^ clause) 'else)
            (apply-cont k (cons clause value)))
           ((symbol?^ (car^ clause))
            (apply-cont
              k
              (cons
                `((eq? ,var ',(car^ clause)) ,@(at^ (cdr^ clause)))
                value)))
           (else
            (apply-cont
              k
              (cons
                `((memq ,var ',(car^ clause)) ,@(at^ (cdr^ clause)))
                value))))))
      (<cont-24> (fields name k2)
       (let ((constructor-def `(define (unquote name)
                                 (lambda args
                                   (if (= (length args) ,(length^ fields))
                                       ,value
                                       (error ',name
                                         "wrong number of arguments"))))))
         (apply-cont2 k2 name constructor-def)))
      (<cont-25> (cdrs fields name k)
       (apply-cont
         k
         `(if (,(cadar^ fields) (car ,cdrs))
              ,value
              (error ',name
                "~a is not of type ~a"
                (car ,cdrs)
                ',(cadar^ fields)))))
      (<cont-26> (adatum macro-keyword fail k)
       (if (has-source-info? value)
           (apply-cont2 k value fail)
           (let ((info (get-source-info adatum)))
             (if (original-source-info? adatum)
                 (apply-cont2
                   k
                   (replace-info value (snoc macro-keyword info))
                   fail)
                 (apply-cont2 k (replace-info value info) fail)))))
      (<cont-27> (adatum macro-keyword fail k)
       (annotate-cps
         value
         'none
         (make-cont '<cont-26> adatum macro-keyword fail k)))
      (<cont-28> (aclauses
                  adatum
                  clauses
                  right-apattern
                  right-pattern
                  handler
                  fail
                  k)
       (if value
           (instantiate^
             right-pattern
             value
             right-apattern
             (make-cont2 '<cont2-44> fail k))
           (process-macro-clauses^ (cdr clauses) (cdr^ aclauses) adatum
             handler fail k)))
      (<cont-29> (aclauses
                  adatum
                  clauses
                  left-apattern
                  left-pattern
                  right-apattern
                  right-pattern
                  handler
                  fail
                  k)
       (unify-patterns^ left-pattern value left-apattern adatum
         (make-cont '<cont-28> aclauses adatum clauses right-apattern
           right-pattern handler fail k)))
      (<cont-30> (v1 k) (apply-cont k `(append ,v1 ,value)))
      (<cont-31> (ax depth k)
       (qq-expand-cps
         (cdr^ ax)
         depth
         (make-cont '<cont-30> value k)))
      (<cont-32> (k) (apply-cont k `(list->vector ,value)))
      (<cont-33> (depth k)
       (qq-expand-cps value depth (make-cont '<cont-32> k)))
      (<cont-34> (ax k) (apply-cont k `(cons ',(car^ ax) ,value)))
      (<cont-35> (k) (apply-cont k `(cons 'quasiquote ,value)))
      (<cont-36> (v1 k)
       (apply-cont k `(list (append ,v1 ,value))))
      (<cont-37> (ax depth k)
       (qq-expand-cps
         (cdr^ ax)
         depth
         (make-cont '<cont-36> value k)))
      (<cont-38> (k) (apply-cont k `(list ,value)))
      (<cont-39> (ax k)
       (apply-cont k `(list (cons ',(car^ ax) ,value))))
      (<cont-40> (k)
       (apply-cont k `(list (cons 'quasiquote ,value))))
      (<cont-41> (args handler fail k2)
       (aparse value (initial-contours (cadr args)) handler fail
         (make-cont2 '<cont2-69> args handler k2)))
      (<cont-42> (handler fail k2)
       (aparse value (initial-contours toplevel-env) handler fail
         (make-cont2 '<cont2-70> handler k2)))
      (<cont-43> (handler fail k2)
       (aparse value (initial-contours toplevel-env) handler fail
         k2))
      (<cont-44> (fail k2) (apply-cont2 k2 value fail))
      (<cont-45> (x y k)
       (if value
           (equal-objects? (cdr x) (cdr y) k)
           (apply-cont k #f)))
      (<cont-46> (i v1 v2 k)
       (if value
           (equal-vectors? v1 v2 (- i 1) k)
           (apply-cont k #f)))
      (<cont-47> (ls x y info handler fail k)
       (if value
           (apply-cont2 k y fail)
           (member-loop x (cdr y) ls info handler fail k)))
      (<cont-48> (pattern var k)
       (if value (apply-cont k #t) (occurs? var (cdr pattern) k)))
      (<cont-49> (ap2 p1 p2 k)
       (if value
           (apply-cont k #f)
           (apply-cont k (make-sub 'unit p1 p2 ap2))))
      (<cont-50> (s-car k)
       (if (not value)
           (apply-cont k #f)
           (apply-cont k (make-sub 'composite s-car value))))
      (<cont-51> (apair1 apair2 pair1 pair2 k)
       (if (not value)
           (apply-cont k #f)
           (instantiate^
             (cdr pair1)
             value
             (cdr^ apair1)
             (make-cont2 '<cont2-93> apair2 pair2 value k))))
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
      (<cont2-2> () (halt* value1))
      (<cont2-3> (k)
       (apply-cont2 k (binding-value value1) value2))
      (<cont2-4> (k)
       (apply-cont2 k (dlr-env-lookup value1) value2))
      (<cont2-5> (v1 info k)
       (apply-cont2 k (app-aexp v1 value1 info) value2))
      (<cont2-6> (adatum senv info handler k)
       (aparse-all (cdr^ adatum) senv handler value2
         (make-cont2 '<cont2-5> value1 info k)))
      (<cont2-7> (info k)
       (apply-cont2 k (choose-aexp value1 info) value2))
      (<cont2-8> (info k)
       (apply-cont2 k (raise-aexp value1 info) value2))
      (<cont2-9> (adatum cexps body info k)
       (let ((cvar (catch-var^ adatum)))
         (apply-cont2
           k
           (try-catch-finally-aexp body cvar cexps value1 info)
           value2)))
      (<cont2-10> (adatum senv body info handler k)
       (aparse-all (try-catch-finally-exps^ adatum) senv handler
         value2 (make-cont2 '<cont2-9> adatum value1 body info k)))
      (<cont2-11> (adatum senv info handler k)
       (aparse-all (catch-exps^ adatum) senv handler value2
         (make-cont2 '<cont2-10> adatum senv value1 info handler k)))
      (<cont2-12> (body info k)
       (apply-cont2 k (try-finally-aexp body value1 info) value2))
      (<cont2-13> (adatum senv info handler k)
       (aparse-all (try-finally-exps^ adatum) senv handler value2
         (make-cont2 '<cont2-12> value1 info k)))
      (<cont2-14> (adatum body info k)
       (let ((cvar (catch-var^ adatum)))
         (apply-cont2
           k
           (try-catch-aexp body cvar value1 info)
           value2)))
      (<cont2-15> (adatum senv info handler k)
       (aparse-all (catch-exps^ adatum) senv handler value2
         (make-cont2 '<cont2-14> adatum value1 info k)))
      (<cont2-16> (name formals info k)
       (if (list? formals)
           (apply-cont2
             k
             (trace-lambda-aexp name formals value1 info)
             value2)
           (apply-cont2
             k
             (mu-trace-lambda-aexp name (head formals) (last formals)
               value1 info)
             value2)))
      (<cont2-17> (formals info k)
       (if (list? formals)
           (apply-cont2 k (lambda-aexp formals value1 info) value2)
           (apply-cont2
             k
             (mu-lambda-aexp (head formals) (last formals) value1 info)
             value2)))
      (<cont2-18> (info k)
       (apply-cont2 k (begin-aexp value1 info) value2))
      (<cont2-19> (adatum info k)
       (apply-cont2
         k
         (define!-aexp
           (define-var^ adatum)
           (define-docstring^ adatum)
           value1
           info)
         value2))
      (<cont2-20> (adatum info k)
       (apply-cont2
         k
         (define!-aexp (define-var^ adatum) "" value1 info)
         value2))
      (<cont2-21> (adatum info k)
       (apply-cont2
         k
         (define-aexp
           (define-var^ adatum)
           (define-docstring^ adatum)
           value1
           info)
         value2))
      (<cont2-22> (adatum info k)
       (apply-cont2
         k
         (define-aexp (define-var^ adatum) "" value1 info)
         value2))
      (<cont2-23> (info k)
       (apply-cont2 k (func-aexp value1 info) value2))
      (<cont2-24> (adatum info k)
       (let ((var-info (get-source-info (cadr^ adatum))))
         (apply-cont2
           k
           (assign-aexp
             (untag-atom^ (cadr^ adatum))
             value1
             var-info
             info)
           value2)))
      (<cont2-25> (v1 v2 info k)
       (apply-cont2 k (if-aexp v1 v2 value1 info) value2))
      (<cont2-26> (adatum senv v1 info handler k)
       (aparse (cadddr^ adatum) senv handler value2
         (make-cont2 '<cont2-25> v1 value1 info k)))
      (<cont2-27> (adatum senv info handler k)
       (aparse (caddr^ adatum) senv handler value2
         (make-cont2 '<cont2-26> adatum senv value1 info handler k)))
      (<cont2-28> (v1 info k)
       (apply-cont2
         k
         (if-aexp v1 value1 (lit-aexp #f 'none) info)
         value2))
      (<cont2-29> (adatum senv info handler k)
       (aparse (caddr^ adatum) senv handler value2
         (make-cont2 '<cont2-28> value1 info k)))
      (<cont2-30> (senv handler k)
       (aparse value1 senv handler value2 k))
      (<cont2-31> (a k) (apply-cont2 k (cons a value1) value2))
      (<cont2-32> (adatum-list senv handler k)
       (aparse-all (cdr^ adatum-list) senv handler value2
         (make-cont2 '<cont2-31> value1 k)))
      (<cont2-33> (v1 k) (apply-cont2 k (cons v1 value1) value2))
      (<cont2-34> (senv src tokens-left handler k)
       (aparse-sexps tokens-left src senv handler value2
         (make-cont2 '<cont2-33> value1 k)))
      (<cont2-35> (bodies k)
       (apply-cont
         k
         `(let (unquote value1) ,@value2 ,@(at^ bodies))))
      (<cont2-36> (procs vars k2)
       (apply-cont2
         k2
         (cons `(,(car^ vars) 'undefined) value1)
         (cons `(set! ,(car^ vars) ,(car^ procs)) value2)))
      (<cont2-37> (exp k)
       (apply-cont
         k
         `(let ((r ,exp) (unquote-splicing value1))
            (cond (unquote-splicing value2)))))
      (<cont2-38> (clauses var k2)
       (let ((clause (car^ clauses)))
         (if (eq?^ (car^ clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) value1)
               (cons '(else (else-code)) value2))
             (if (symbol?^ (car^ clause))
                 (let ((name (car^ clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda () ,@(at^ (cdr^ clause))))
                       value1)
                     (cons `((eq? ,var ',(car^ clause)) (,name)) value2)))
                 (let ((name (caar^ clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda () ,@(at^ (cdr^ clause))))
                       value1)
                     (cons
                       `((memq ,var ',(car^ clause)) (,name))
                       value2)))))))
      (<cont2-39> (clauses var k2)
       (let ((clause (car^ clauses)))
         (if (eq?^ (car^ clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) value1)
               (cons `(else (else-code)) value2))
             (if (symbol?^ (car^ clause))
                 (let ((name (car^ clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name
                          (lambda (unquote (cadr^ clause))
                            ,@(at^ (cddr^ clause))))
                       value1)
                     (cons
                       `((eq? (car ,var) ',(car^ clause))
                          (apply ,name (cdr ,var)))
                       value2)))
                 (let ((name (caar^ clause)))
                   (apply-cont2
                     k2
                     (cons
                       `(,name
                          (lambda (unquote (cadr^ clause))
                            ,@(at^ (cddr^ clause))))
                       value1)
                     (cons
                       `((memq (car ,var) ',(car^ clause))
                          (apply ,name (cdr ,var)))
                       value2)))))))
      (<cont2-40> (type-tester-name k)
       (let ((tester-def `(define (unquote type-tester-name)
                            (lambda (x)
                              (and (pair? x)
                                   (not (not (memq (car x) ',value1))))))))
         (apply-cont k `(begin ,tester-def ,@value2))))
      (<cont2-41> (def name k2)
       (apply-cont2 k2 (cons name value1) (cons def value2)))
      (<cont2-42> (variants k2)
       (make-dd-variant-constructors^
         (cdr^ variants)
         (make-cont2 '<cont2-41> value2 value1 k2)))
      (<cont2-43> (exp type-name type-tester-name k)
       (apply-cont
         k
         `(let ((r ,exp) (unquote-splicing value1))
            (if (not (,type-tester-name r))
                (error 'cases "~a is not a valid ~a" r ',type-name)
                (cond (unquote-splicing value2))))))
      (<cont2-44> (fail k) (apply-cont2 k value2 fail))
      (<cont2-45> () (set! *last-fail* value2) (halt* value1))
      (<cont2-46> ()
       (m value1 toplevel-env REP-handler value2 REP-k))
      (<cont2-47> () (halt* #t))
      (<cont2-48> ()
       (aparse-sexps value1 'stdin (initial-contours toplevel-env)
         try-parse-handler value2 (make-cont2 '<cont2-47>)))
      (<cont2-49> (exp k)
       (handle-debug-info exp value1)
       (apply-cont2 k value1 value2))
      (<cont2-50> (exp k)
       (pop-stack-trace exp)
       (apply-cont2 k value1 value2))
      (<cont2-51> (args exp env info handler k)
       (if *use-stack-trace* (push-stack-trace exp))
       (cond
         ((dlr-proc? value1)
          (let ((result (dlr-apply value1 args)))
            (if *use-stack-trace* (pop-stack-trace exp))
            (apply-cont2 k result value2)))
         ((procedure-object? value1)
          (if *use-stack-trace*
              (apply-proc value1 args env info handler value2
                (make-cont2 '<cont2-50> exp k))
              (apply-proc value1 args env info handler value2 k)))
         (else
          (runtime-error
            (format "attempt to apply non-procedure '~a'" value1)
            info
            handler
            value2))))
      (<cont2-52> (exp operator env info handler k)
       (m operator env handler value2
          (make-cont2 '<cont2-51> value1 exp env info handler k)))
      (<cont2-53> (handler)
       (apply-handler2 handler value1 value2))
      (<cont2-54> (v k) (apply-cont2 k v value2))
      (<cont2-55> (fexps env handler k)
       (eval-sequence fexps env handler value2
         (make-cont2 '<cont2-54> value1 k)))
      (<cont2-56> (aclauses clauses k)
       (set-binding-value!
         value1
         (make-pattern-macro^ clauses aclauses))
       (apply-cont2 k void-value value2))
      (<cont2-57> (docstring var k)
       (set-global-value! var value1)
       (set-global-docstring! var docstring)
       (apply-cont2 k void-value value2))
      (<cont2-58> (docstring rhs-value k)
       (set-binding-value! value1 rhs-value)
       (set-binding-docstring! value1 docstring)
       (apply-cont2 k void-value value2))
      (<cont2-59> (docstring var env handler k)
       (lookup-binding-in-first-frame var env handler value2
         (make-cont2 '<cont2-58> docstring value1 k)))
      (<cont2-60> (rhs-value k)
       (let ((old-value (binding-value value1)))
         (set-binding-value! value1 rhs-value)
         (let ((new-fail (make-fail
                           '<fail-2>
                           value1
                           old-value
                           value2)))
           (apply-cont2 k void-value new-fail))))
      (<cont2-61> (rhs-value k)
       (let ((old-value (dlr-env-lookup value1)))
         (set-global-value! value1 rhs-value)
         (let ((new-fail (make-fail
                           '<fail-4>
                           old-value
                           value1
                           value2)))
           (apply-cont2 k void-value new-fail))))
      (<cont2-62> (var var-info env handler k)
       (lookup-variable var env var-info handler value2
         (make-cont2 '<cont2-61> value1 k)
         (make-cont3 '<cont3-4> value1 k)
         (make-cont2 '<cont2-60> value1 k)))
      (<cont2-63> (else-exp then-exp env handler k)
       (if value1
           (m then-exp env handler value2 k)
           (m else-exp env handler value2 k)))
      (<cont2-64> (k) (apply-cont2 k (dlr-func value1) value2))
      (<cont2-65> (exps env handler k)
       (m* (cdr exps) env handler value2
           (make-cont2 '<cont2-33> value1 k)))
      (<cont2-66> (exps env handler k)
       (eval-sequence (cdr exps) env handler value2 k))
      (<cont2-67> (e handler) (apply-handler2 handler e value2))
      (<cont2-68> (trace-depth k2)
       (set! trace-depth (- trace-depth 1))
       (printf
         "~areturn: ~s~%"
         (make-trace-depth-string trace-depth)
         value1)
       (apply-cont2 k2 value1 value2))
      (<cont2-69> (args handler k2)
       (m value1 (cadr args) handler value2 k2))
      (<cont2-70> (handler k2)
       (m value1 toplevel-env handler value2 k2))
      (<cont2-71> (handler k2)
       (read-sexp value1 'stdin handler value2
         (make-cont4 '<cont4-11> handler k2)))
      (<cont2-72> (handler k2)
       (read-sexp value1 'stdin handler value2
         (make-cont4 '<cont4-12> handler k2)))
      (<cont2-73> (k)
       (if (null? load-stack)
           (printf "WARNING: empty load-stack encountered!\n")
           (set! load-stack (cdr load-stack)))
       (apply-cont2 k void-value value2))
      (<cont2-74> (filename env2 handler k)
       (read-and-eval-asexps value1 filename env2 handler value2
         (make-cont2 '<cont2-73> k)))
      (<cont2-75> (src tokens-left env2 handler k)
       (if (token-type? (first tokens-left) 'end-marker)
           (apply-cont2 k value1 value2)
           (read-and-eval-asexps tokens-left src env2 handler value2
             k)))
      (<cont2-76> (src tokens-left env2 handler k)
       (m value1 env2 handler value2
          (make-cont2 '<cont2-75> src tokens-left env2 handler k)))
      (<cont2-77> (filenames env2 info handler k)
       (load-files (cdr filenames) env2 info handler value2 k))
      (<cont2-78> (lst k2)
       (if (member (car lst) value1)
           (apply-cont2 k2 value1 value2)
           (apply-cont2 k2 (cons (car lst) value1) value2)))
      (<cont2-79> (filename handler k2)
       (let ((module (make-toplevel-env)))
         (set-binding-value! value1 module)
         (load-file filename module 'none handler value2 k2)))
      (<cont2-80> (args sym info handler k)
       (cond
         ((null? (cdr args)) (apply-cont2 k value1 value2))
         ((not (environment? value1))
          (runtime-error
            (format "invalid module '~a'" sym)
            info
            handler
            value2))
         (else
          (get-primitive (cdr args) value1 info handler value2 k))))
      (<cont2-81> (ls1 k2)
       (apply-cont2 k2 (cons (car ls1) value1) value2))
      (<cont2-82> (lists k2)
       (append2 (car lists) value1 value2 k2))
      (<cont2-83> (iterator proc env handler k)
       (iterate-continue proc iterator env handler value2 k))
      (<cont2-84> (iterator proc env handler k)
       (iterate-collect-continue proc iterator env handler value2
         (make-cont2 '<cont2-33> value1 k)))
      (<cont2-85> (list1 proc env handler k)
       (map1 proc (cdr list1) env handler value2
         (make-cont2 '<cont2-33> value1 k)))
      (<cont2-86> (list1 proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (list (car list1))) value1)
         value2))
      (<cont2-87> (list1 list2 proc env handler k)
       (map2 proc (cdr list1) (cdr list2) env handler value2
         (make-cont2 '<cont2-33> value1 k)))
      (<cont2-88> (list1 list2 proc k)
       (apply-cont2
         k
         (cons
           (dlr-apply proc (list (car list1) (car list2)))
           value1)
         value2))
      (<cont2-89> (lists proc env handler k)
       (mapN proc (map cdr lists) env handler value2
         (make-cont2 '<cont2-33> value1 k)))
      (<cont2-90> (lists proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (map car lists)) value1)
         value2))
      (<cont2-91> (arg-list proc env handler k)
       (for-each-primitive proc (map cdr arg-list) env handler
         value2 k))
      (<cont2-92> (new-acdr1 new-cdr1 s-car k)
       (unify-patterns^ new-cdr1 value1 new-acdr1 value2
         (make-cont '<cont-50> s-car k)))
      (<cont2-93> (apair2 pair2 s-car k)
       (instantiate^
         (cdr pair2)
         s-car
         (cdr^ apair2)
         (make-cont2 '<cont2-92> value2 value1 s-car k)))
      (<cont2-94> (a aa ap k2)
       (apply-cont2
         k2
         (cons a value1)
         (cons^ aa value2 (get-source-info ap))))
      (<cont2-95> (ap pattern s k2)
       (instantiate^
         (cdr pattern)
         s
         (cdr^ ap)
         (make-cont2 '<cont2-94> value1 value2 ap k2)))
      (<cont2-96> (s2 k2) (instantiate^ value1 s2 value2 k2))
      (else (error 'apply-cont2 "bad continuation2: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation3 datatype

(define make-cont3 (lambda args (cons 'continuation3 args)))

(define*
  apply-cont3
  (lambda (k value1 value2 value3)
    (record-case (cdr k)
      (<cont3-1> (src handler k)
       (if (token-type? value1 'end-marker)
           (apply-cont2 k (list value1) value3)
           (scan-input-loop value2 src handler value3
             (make-cont2 '<cont2-1> value1 k))))
      (<cont3-2> () (halt* value1))
      (<cont3-3> (k)
       (apply-cont2 k (get-external-member value1 value2) value3))
      (<cont3-4> (rhs-value k)
       (let ((old-value (get-external-member value1 value2)))
         (set-external-member! value1 value2 rhs-value)
         (let ((new-fail (make-fail '<fail-3> value2 value1 old-value
                           value3)))
           (apply-cont2 k void-value new-fail))))
      (else (error 'apply-cont3 "bad continuation3: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation4 datatype

(define make-cont4 (lambda args (cons 'continuation4 args)))

(define*
  apply-cont4
  (lambda (k value1 value2 value3 value4)
    (record-case (cdr k)
      (<cont4-1> (src start k)
       (annotate-cps
         (list->vector value1)
         (make-info src start value2)
         (make-cont '<cont-8> value2 value3 value4 k)))
      (<cont4-2> (src start k)
       (annotate-cps
         value1
         (make-info src start value2)
         (make-cont '<cont-8> value2 value3 value4 k)))
      (<cont4-3> (src start v k)
       (annotate-cps
         (list v value1)
         (make-info src start value2)
         (make-cont '<cont-8> value2 value3 value4 k)))
      (<cont4-4> (sexp1 k)
       (apply-cont4 k (cons sexp1 value1) value2 value3 value4))
      (<cont4-5> (src handler k)
       (read-vector-sequence value3 src handler value4
         (make-cont4 '<cont4-4> value1 k)))
      (<cont4-6> (expected-terminator sexp1 src handler k)
       (close-sexp-sequence (cons sexp1 value1) value3
         expected-terminator src handler value4 k))
      (<cont4-7> (expected-terminator src handler k)
       (if (token-type? (first value3) 'dot)
           (read-sexp (rest-of value3) src handler value4
             (make-cont4 '<cont4-6> expected-terminator value1 src
               handler k))
           (read-sexp-sequence value3 expected-terminator src handler
             value4 (make-cont4 '<cont4-4> value1 k))))
      (<cont4-8> () (halt* value1))
      (<cont4-9> (senv src handler k)
       (aparse value1 senv handler value4
         (make-cont2 '<cont2-34> senv src value3 handler k)))
      (<cont4-10> ()
       (set! *tokens-left* value3)
       (aparse value1 (initial-contours toplevel-env) REP-handler
         value4 (make-cont2 '<cont2-46>)))
      (<cont4-11> (handler k2)
       (if (token-type? (first value3) 'end-marker)
           (aparse value1 (initial-contours toplevel-env) handler
             value4 k2)
           (read-error "tokens left over" value3 'stdin handler
             value4)))
      (<cont4-12> (handler k2)
       (if (token-type? (first value3) 'end-marker)
           (apply-cont2 k2 value1 value4)
           (read-error "tokens left over" value3 'stdin handler
             value4)))
      (<cont4-13> (src env2 handler k)
       (aparse value1 (initial-contours env2) handler value4
         (make-cont2 '<cont2-76> src value3 env2 handler k)))
      (else (error 'apply-cont4 "bad continuation4: ~a" k)))))

;;----------------------------------------------------------------------
;; fail-continuation datatype

(define make-fail
  (lambda args (cons 'fail-continuation args)))

(define*
  apply-fail
  (lambda (fail)
    (record-case (cdr fail)
      (<fail-1> () (halt* "no more choices"))
      (<fail-2> (binding old-value fail)
       (set-binding-value! binding old-value)
       (apply-fail fail))
      (<fail-3> (components dlr-obj old-value fail)
       (set-external-member! dlr-obj components old-value)
       (apply-fail fail))
      (<fail-4> (old-value var fail)
       (set-global-value! var old-value)
       (apply-fail fail))
      (<fail-5> (exps env handler fail k)
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
       (set! *last-fail* fail)
       (halt* (list 'exception exception)))
      (<handler2-3> () (halt* #f))
      (<handler2-4> (cexps cvar env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (eval-sequence cexps new-env handler fail k)))
      (<handler2-5> (fexps env handler)
       (eval-sequence fexps env handler fail
         (make-cont2 '<cont2-67> exception handler)))
      (<handler2-6> (cexps cvar fexps env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (let ((catch-handler (try-finally-handler
                                fexps
                                env
                                handler)))
           (eval-sequence cexps new-env catch-handler fail
             (make-cont2 '<cont2-55> fexps env handler k)))))
      (else (error 'apply-handler2 "bad handler2: ~a" handler)))))

;;----------------------------------------------------------------------
;; procedure datatype

(define make-proc (lambda args (cons 'procedure args)))

(define*
  apply-proc
  (lambda (proc args env2 info handler fail k2)
    (record-case (cdr proc)
      (<proc-1> (bodies formals env)
       (if (= (length args) (length formals))
           (eval-sequence bodies (extend env formals args) handler fail
             k2)
           (runtime-error
             "incorrect number of arguments in application"
             info
             handler
             fail)))
      (<proc-2> (bodies formals runt env)
       (if (>= (length args) (length formals))
           (let ((new-env (extend
                            env
                            (cons runt formals)
                            (cons
                              (list-tail args (length formals))
                              (list-head args (length formals))))))
             (eval-sequence bodies new-env handler fail k2))
           (runtime-error
             "not enough arguments in application"
             info
             handler
             fail)))
      (<proc-3> (bodies name trace-depth formals env)
       (if (= (length args) (length formals))
           (begin
             (printf
               "~acall: ~s~%"
               (make-trace-depth-string trace-depth)
               (cons name args))
             (set! trace-depth (+ trace-depth 1))
             (eval-sequence bodies (extend env formals args) handler fail
               (make-cont2 '<cont2-68> trace-depth k2)))
           (runtime-error
             "incorrect number of arguments in application"
             info
             handler
             fail)))
      (<proc-4> (bodies name trace-depth formals runt env)
       (if (>= (length args) (length formals))
           (let ((new-env (extend
                            env
                            (cons runt formals)
                            (cons
                              (list-tail args (length formals))
                              (list-head args (length formals))))))
             (printf
               "~acall: ~s~%"
               (make-trace-depth-string trace-depth)
               (cons name args))
             (set! trace-depth (+ trace-depth 1))
             (eval-sequence bodies new-env handler fail
               (make-cont2 '<cont2-68> trace-depth k2)))
           (runtime-error
             "not enough arguments in application"
             info
             handler
             fail)))
      (<proc-5> () (apply-cont2 k2 void-value fail))
      (<proc-6> () (apply-cont2 k2 (= (car args) 0) fail))
      (<proc-7> () (halt* end-of-session))
      (<proc-8> ()
       (cond
         ((length-one? args)
          (annotate-cps
            (car args)
            'none
            (make-cont '<cont-42> handler fail k2)))
         ((length-two? args)
          (annotate-cps
            (car args)
            'none
            (make-cont '<cont-41> args handler fail k2)))
         (else
          (runtime-error
            "incorrect number of arguments to eval"
            info
            handler
            fail))))
      (<proc-9> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to eval-ast"
            info
            handler
            fail))
         ((not (list? (car args)))
          (runtime-error
            "eval-ast called on non-abstract syntax tree argument"
            info
            handler
            fail))
         (else (m (car args) toplevel-env handler fail k2))))
      (<proc-10> ()
       (annotate-cps
         (car args)
         'none
         (make-cont '<cont-43> handler fail k2)))
      (<proc-11> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to string-length"
            info
            handler
            fail))
         ((not (string? (car args)))
          (runtime-error
            "string-length called on non-string argument"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string-length args) fail))))
      (<proc-12> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to string-ref"
            info
            handler
            fail))
         ((not (string? (car args)))
          (runtime-error
            "string-ref called with non-string first argument"
            info
            handler
            fail))
         ((not (number? (cadr args)))
          (runtime-error
            "string-ref called with non-numberic second argument"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string-ref args) fail))))
      (<proc-13> () (apply-cont2 k2 (aunparse (car args)) fail))
      (<proc-14> ()
       (apply-cont2 k2 (aunparse (car (caddr (car args)))) fail))
      (<proc-15> ()
       (scan-input (car args) 'stdin handler fail
         (make-cont2 '<cont2-71> handler k2)))
      (<proc-16> ()
       (scan-input (car args) 'stdin handler fail
         (make-cont2 '<cont2-72> handler k2)))
      (<proc-17> ()
       (let ((proc (car args)) (proc-args (cadr args)))
         (apply-proc proc proc-args env2 info handler fail k2)))
      (<proc-18> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to sqrt"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "sqrt called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply sqrt args) fail))))
      (<proc-19> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to odd?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (odd? (car args)) fail))))
      (<proc-20> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to even?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (even? (car args)) fail))))
      (<proc-21> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to quotient"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply quotient args) fail))))
      (<proc-22> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to remainder"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply remainder args) fail))))
      (<proc-23> ()
       (for-each safe-print args)
       (apply-cont2 k2 void-value fail))
      (<proc-24> ()
       (apply-cont2 k2 (apply char->string args) fail))
      (<proc-25> ()
       (apply-cont2
         k2
         (substring (car args) (cadr args) (caddr args))
         fail))
      (<proc-26> ()
       (apply-cont2 k2 (number->string (car args)) fail))
      (<proc-27> ()
       (apply-cont2 k2 (assv (car args) (cadr args)) fail))
      (<proc-28> ()
       (apply-cont2 k2 (memv (car args) (cadr args)) fail))
      (<proc-29> ()
       (let ((s (format "~a" (car args))))
         (set! *need-newline* (true? (not (ends-with-newline? s))))
         (display s)
         (apply-cont2 k2 void-value fail)))
      (<proc-30> ()
       (set! *need-newline* #f)
       (newline)
       (apply-cont2 k2 void-value fail))
      (<proc-31> ()
       (if (not (length-one? args))
           (runtime-error
             "incorrect number of arguments to load"
             info
             handler
             fail)
           (load-file (car args) toplevel-env info handler fail k2)))
      (<proc-32> ()
       (if (length-one? args)
           (length-loop (car args) 0 (car args) info handler fail k2)
           (runtime-error
             "incorrect number of arguments to length"
             info
             handler
             fail)))
      (<proc-33> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            (format
              "incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument"
              args)
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply symbol? args) fail))))
      (<proc-34> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to number?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply number? args) fail))))
      (<proc-35> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to boolean?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply boolean? args) fail))))
      (<proc-36> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to string?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string? args) fail))))
      (<proc-37> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to char?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply char? args) fail))))
      (<proc-38> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to char=?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply char=? args) fail))))
      (<proc-39> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to char-whitespace?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply char-whitespace? args) fail))))
      (<proc-40> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to char-alphabetic?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply char-alphabetic? args) fail))))
      (<proc-41> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to char-numeric?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply char-numeric? args) fail))))
      (<proc-42> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to null?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply null? args) fail))))
      (<proc-43> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to pair?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply pair? args) fail))))
      (<proc-44> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to cons"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply cons args) fail))))
      (<proc-45> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to car"
            info
            handler
            fail))
         ((not (pair? (car args)))
          (runtime-error
            (format "car called on non-pair ~s" (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply car args) fail))))
      (<proc-46> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to cdr"
            info
            handler
            fail))
         ((not (pair? (car args)))
          (runtime-error
            (format "cdr called on non-pair ~s" (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply cdr args) fail))))
      (<proc-47> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to cadr"
            info
            handler
            fail))
         ((not (length-at-least? 2 (car args)))
          (runtime-error
            (format
              "cadr called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply cadr args) fail))))
      (<proc-48> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to caddr"
            info
            handler
            fail))
         ((not (length-at-least? 3 (car args)))
          (runtime-error
            (format
              "caddr called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply caddr args) fail))))
      (<proc-49> () (apply-cont2 k2 args fail))
      (<proc-50> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to set"
            info
            handler
            fail))
         (else (make-set (car args) env2 info handler fail k2))))
      (<proc-51> ()
       (if (not (all-numeric? args))
           (runtime-error
             "+ called on non-numeric argument(s)"
             info
             handler
             fail)
           (apply-cont2 k2 (apply + args) fail)))
      (<proc-52> ()
       (cond
         ((null? args)
          (runtime-error
            "incorrect number of arguments to -"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "- called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply - args) fail))))
      (<proc-53> ()
       (if (not (all-numeric? args))
           (runtime-error
             "* called on non-numeric argument(s)"
             info
             handler
             fail)
           (apply-cont2 k2 (apply * args) fail)))
      (<proc-54> ()
       (cond
         ((null? args)
          (runtime-error
            "incorrect number of arguments to /"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "/ called on non-numeric argument(s)"
            info
            handler
            fail))
         ((member 0 (cdr args))
          (runtime-error "division by zero" info handler fail))
         (else (apply-cont2 k2 (apply / args) fail))))
      (<proc-55> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to %"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "% called on non-numeric argument(s)"
            info
            handler
            fail))
         ((= (cadr args) 0)
          (runtime-error "modulo by zero" info handler fail))
         (else (apply-cont2 k2 (apply modulo args) fail))))
      (<proc-56> ()
       (cond
         ((not (length-at-least? 2 args))
          (runtime-error
            "incorrect number of arguments to <"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "< called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply < args) fail))))
      (<proc-57> ()
       (cond
         ((not (length-at-least? 2 args))
          (runtime-error
            "incorrect number of arguments to >"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "> called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply > args) fail))))
      (<proc-58> ()
       (cond
         ((not (length-at-least? 2 args))
          (runtime-error
            "incorrect number of arguments to <="
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "<= called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply <= args) fail))))
      (<proc-59> ()
       (cond
         ((not (length-at-least? 2 args))
          (runtime-error
            "incorrect number of arguments to >="
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            ">= called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply >= args) fail))))
      (<proc-60> ()
       (cond
         ((not (length-at-least? 2 args))
          (runtime-error
            "incorrect number of arguments to ="
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "= called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply = args) fail))))
      (<proc-61> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to abs"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "abs called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply abs args) fail))))
      (<proc-62> ()
       (if (not (length-two? args))
           (runtime-error
             "incorrect number of arguments to equal?"
             info
             handler
             fail)
           (equal-objects?
             (car args)
             (cadr args)
             (make-cont '<cont-44> fail k2))))
      (<proc-63> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to eq?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply eq? args) fail))))
      (<proc-64> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to memq"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply memq args) fail))))
      (<proc-65> ()
       (if (not (length-two? args))
           (runtime-error
             "incorrect number of arguments to member"
             info
             handler
             fail)
           (member-loop (car args) (cadr args) (cadr args) info handler
             fail k2)))
      (<proc-66> ()
       (cond
         ((or (null? args) (length-at-least? 4 args))
          (runtime-error
            "incorrect number of arguments to range"
            info
            handler
            fail))
         ((not (all-numeric? args))
          (runtime-error
            "range called on non-numeric argument(s)"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply range args) fail))))
      (<proc-67> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to set-car!"
            info
            handler
            fail))
         ((not (pair? (car args)))
          (runtime-error
            (format "set-car! called on non-pair ~s" (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply set-car! args) fail))))
      (<proc-68> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to set-cdr!"
            info
            handler
            fail))
         ((not (pair? (car args)))
          (runtime-error
            (format "set-cdr! called on non-pair ~s" (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply set-cdr! args) fail))))
      (<proc-69> ()
       (let ((filename (car args)))
         (if (null? (cdr args))
             (load-file filename env2 'none handler fail k2)
             (let ((module-name (cadr args)))
               (lookup-binding-in-first-frame module-name env2 handler fail
                 (make-cont2 '<cont2-79> filename handler k2))))))
      (<proc-70> () (apply-cont2 k2 (car *stack-trace*) fail))
      (<proc-71> ()
       (get-primitive args env2 info handler fail k2))
      (<proc-72> (k) (apply-cont2 k (car args) fail))
      (<proc-73> ()
       (if (not (length-one? args))
           (runtime-error
             "incorrect number of arguments to call/cc"
             info
             handler
             fail)
           (let ((proc (car args)))
             (if (not (procedure-object? proc))
                 (runtime-error
                   "call/cc called with non-procedure"
                   info
                   handler
                   fail)
                 (let ((fake-k (make-proc '<proc-72> k2)))
                   (if (dlr-proc? proc)
                       (apply-cont2 k2 (dlr-apply proc (list fake-k)) fail)
                       (apply-proc proc (list fake-k) env2 info handler
                         fail k2)))))))
      (<proc-74> ()
       (if (null? args)
           (apply-cont2 REP-k void-value fail)
           (apply-cont2 REP-k (car args) fail)))
      (<proc-75> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to require"
            info
            handler
            fail))
         ((true? (car args)) (apply-cont2 k2 'ok fail))
         (else (apply-fail fail))))
      (<proc-76> ()
       (if (not (null? args))
           (runtime-error
             "incorrect number of arguments to cut"
             info
             handler
             fail)
           (apply-cont2 k2 'ok REP-fail)))
      (<proc-77> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to reverse"
            info
            handler
            fail))
         ((not (list? args))
          (runtime-error
            (format
              "reverse called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply reverse args) fail))))
      (<proc-78> () (append-all args info handler fail k2))
      (<proc-79> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to string->number"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string->number args) fail))))
      (<proc-80> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to string=?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string=? args) fail))))
      (<proc-81> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to list->vector"
            info
            handler
            fail))
         ((not (list? (car args)))
          (runtime-error
            (format
              "list->vector called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply list->vector args) fail))))
      (<proc-82> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to list->string"
            info
            handler
            fail))
         ((not (list? (car args)))
          (runtime-error
            (format
              "list->string called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         ((not (all-char? (car args)))
          (runtime-error
            (format
              "list->string called on non-char list ~s"
              (car args))
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply list->string args) fail))))
      (<proc-83> ()
       (make-set (dir args env2) env2 info handler fail k2))
      (<proc-84> () (apply-cont2 k2 (get-current-time) fail))
      (<proc-85> ()
       (map-primitive (car args) (cdr args) env2 handler fail k2))
      (<proc-86> ()
       (for-each-primitive (car args) (cdr args) env2 handler fail
         k2))
      (<proc-87> () (apply-cont2 k2 env2 fail))
      (<proc-88> () (apply-cont2 k2 (using-prim args env2) fail))
      (<proc-89> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to not"
            info
            handler
            fail))
         (else (apply-cont2 k2 (not (car args)) fail))))
      (<proc-90> ()
       (apply printf-prim args)
       (apply-cont2 k2 void-value fail))
      (<proc-91> ()
       (apply-cont2 k2 (apply vector_native args) fail))
      (<proc-92> ()
       (apply-cont2
         k2
         (vector-set! (car args) (cadr args) (caddr args))
         fail))
      (<proc-93> () (apply-cont2 k2 (apply vector-ref args) fail))
      (<proc-94> ()
       (apply-cont2 k2 (apply make-vector args) fail))
      (<proc-95> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to 'error' (should be 2)"
            info
            handler
            fail))
         (else
          (let* ((location (format "Error in '~a': " (car args)))
                 (message (string-append
                            location
                            (apply format (cdr args)))))
            (runtime-error message info handler fail)))))
      (<proc-96> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to list-ref"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply list-ref args) fail))))
      (<proc-97> ()
       (cond
         ((null? args) (apply-cont2 k2 (current-directory) fail))
         ((length-one? args)
          (if (string? (car args))
              (apply-cont2 k2 (current-directory (car args)) fail)
              (runtime-error
                "directory must be a string"
                info
                handler
                fail)))
         (else
          (runtime-error
            "incorrect number of arguments to current-directory"
            info
            handler
            fail))))
      (<proc-98> (external-function-object)
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
  (lambda (macro datum handler fail k)
    (record-case (cdr macro)
      (<macro-1> ()
       (if (symbol?^ (cadr^ datum))
           (let* ((name (cadr^ datum))
                  (bindings (caddr^ datum))
                  (vars (map^ car^ bindings))
                  (exps (map^ cadr^ bindings))
                  (bodies (cdddr^ datum)))
             (apply-cont
               k
               `(letrec ((,name (lambda (unquote vars) ,@(at^ bodies))))
                  (,name ,@(at^ exps)))))
           (let* ((bindings (cadr^ datum))
                  (vars (map^ car^ bindings))
                  (exps (map^ cadr^ bindings))
                  (bodies (cddr^ datum)))
             (apply-cont
               k
               `((lambda (unquote vars) ,@(at^ bodies)) ,@(at^ exps))))))
      (<macro-2> ()
       (let* ((decls (cadr^ datum))
              (vars (map^ car^ decls))
              (procs (map^ cadr^ decls))
              (bodies (cddr^ datum)))
         (create-letrec-assignments^
           vars
           procs
           (make-cont2 '<cont2-35> bodies k))))
      (<macro-3> ()
       (let ((name (caadr^ datum))
             (formals (cdadr^ datum))
             (bodies (cddr^ datum)))
         (apply-cont
           k
           `(define (unquote name)
              (lambda (unquote formals) ,@(at^ bodies))))))
      (<macro-4> ()
       (let ((exps (cdr^ datum)))
         (cond
           ((null?^ exps) (apply-cont k '#t))
           ((null?^ (cdr^ exps)) (apply-cont k (car^ exps)))
           (else
            (apply-cont
              k
              `(if ,(car^ exps) (and ,@(at^ (cdr^ exps))) #f))))))
      (<macro-5> ()
       (let ((exps (cdr^ datum)))
         (cond
           ((null?^ exps) (apply-cont k '#f))
           ((null?^ (cdr^ exps)) (apply-cont k (car^ exps)))
           (else
            (apply-cont
              k
              `(let ((bool ,(car^ exps))
                     (else-code (lambda () (or ,@(at^ (cdr^ exps))))))
                 (if bool bool (else-code))))))))
      (<macro-6> ()
       (let ((clauses (cdr^ datum)))
         (if (null?^ clauses)
             (amacro-error "empty (cond) expression" datum handler fail)
             (let ((first-clause (car^ clauses))
                   (other-clauses (cdr^ clauses)))
               (if (or (null?^ first-clause) (not (list?^ first-clause)))
                   (amacro-error
                     "improper cond clause"
                     first-clause
                     handler
                     fail)
                   (let ((test-exp (car^ first-clause))
                         (then-exps (cdr^ first-clause)))
                     (cond
                       ((eq?^ test-exp 'else)
                        (cond
                          ((null?^ then-exps)
                           (amacro-error
                             "improper else clause"
                             first-clause
                             handler
                             fail))
                          ((null?^ (cdr^ then-exps))
                           (apply-cont k (car^ then-exps)))
                          (else
                           (apply-cont k `(begin ,@(at^ then-exps))))))
                       ((null?^ then-exps)
                        (if (null?^ other-clauses)
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)) (if bool bool)))
                            (apply-cont
                              k
                              `(let ((bool ,test-exp)
                                     (else-code (lambda ()
                                                  (cond
                                                    (unquote-splicing
                                                     (at^ other-clauses))))))
                                 (if bool bool (else-code))))))
                       ((eq?^ (car^ then-exps) '=>)
                        (cond
                          ((null?^ (cdr^ then-exps))
                           (amacro-error
                             "improper => clause"
                             first-clause
                             handler
                             fail))
                          ((null?^ other-clauses)
                           (apply-cont
                             k
                             `(let ((bool ,test-exp)
                                    (th (lambda () ,(cadr^ then-exps))))
                                (if bool ((th) bool)))))
                          (else
                           (apply-cont
                             k
                             `(let ((bool ,test-exp)
                                    (th (lambda () ,(cadr^ then-exps)))
                                    (else-code (lambda ()
                                                 (cond
                                                   (unquote-splicing
                                                    (at^ other-clauses))))))
                                (if bool ((th) bool) (else-code)))))))
                       ((null?^ other-clauses)
                        (if (null?^ (cdr^ then-exps))
                            (apply-cont
                              k
                              `(if ,test-exp ,(car^ then-exps)))
                            (apply-cont
                              k
                              `(if ,test-exp (begin ,@(at^ then-exps))))))
                       ((null?^ (cdr^ then-exps))
                        (apply-cont
                          k
                          `(if ,test-exp
                               ,(car^ then-exps)
                               (cond
                                 (unquote-splicing (at^ other-clauses))))))
                       (else
                        (apply-cont
                          k
                          `(if ,test-exp
                               (begin ,@(at^ then-exps))
                               (cond
                                 (unquote-splicing
                                  (at^ other-clauses)))))))))))))
      (<macro-7> ()
       (let ((bindings (cadr^ datum)) (bodies (cddr^ datum)))
         (nest-let*-bindings^ bindings bodies k)))
      (<macro-8> ()
       (let ((exp (cadr^ datum)) (clauses (cddr^ datum)))
         (case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-37> exp k))))
      (<macro-9> ()
       (let ((exp (cadr^ datum)) (clauses (cddr^ datum)))
         (record-case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-37> exp k))))
      (<macro-10> ()
       (let* ((datatype-name (cadr^ datum))
              (type-tester-name (string->symbol
                                  (string-append
                                    (symbol->string^ datatype-name)
                                    "?"))))
         (if (not (eq?^ (caddr^ datum) type-tester-name))
             (amacro-error
               (format
                 "datatype tester predicate not named ~a"
                 type-tester-name)
               (caddr^ datum)
               handler
               fail)
             (let ((variants (cdddr^ datum)))
               (make-dd-variant-constructors^
                 variants
                 (make-cont2 '<cont2-40> type-tester-name k))))))
      (<macro-11> ()
       (let* ((type-name (cadr^ datum))
              (type-tester-name (string->symbol
                                  (string-append
                                    (symbol->string^ type-name)
                                    "?")))
              (exp (caddr^ datum))
              (clauses (cdddr^ datum)))
         (record-case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-43> exp type-name type-tester-name k))))
      (else (error
             'apply-macro
             "bad macro-transformer: ~a"
             macro)))))

;;----------------------------------------------------------------------
;; main program

(define next-avail
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining (lambda (n) (+ 1 n)))

(define initialize-scan-counters
  (lambda ()
    (set! scan-line 1)
    (set! scan-char 1)
    (set! scan-position 1)
    (set! last-scan-line scan-line)
    (set! last-scan-char scan-char)
    (set! last-scan-position scan-position)))

(define increment-scan-counters
  (lambda (chars)
    (set! last-scan-line scan-line)
    (set! last-scan-char scan-char)
    (set! last-scan-position scan-position)
    (cond
      ((char=? (next-avail chars) #\newline)
       (set! scan-line (+ 1 scan-line))
       (set! scan-char 1))
      (else (set! scan-char (+ 1 scan-char))))
    (set! scan-position (+ 1 scan-position))))

(define mark-token-start
  (lambda ()
    (set! token-start-line scan-line)
    (set! token-start-char scan-char)
    (set! token-start-position scan-position)))

(define*
  scan-input
  (lambda (input src handler fail k)
    (initialize-scan-counters)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 src handler fail k)))

(define*
  scan-input-loop
  (lambda (chars src handler fail k)
    (apply-action '(goto start-state) '() chars src handler fail
      (make-cont3 '<cont3-1> src handler k))))

(define*
  apply-action
  (lambda (action buffer chars src handler fail k)
    (record-case action
      (shift (next)
       (increment-scan-counters chars)
       (apply-action next (cons (next-avail chars) buffer)
         (remaining chars) src handler fail k))
      (replace (new-char next)
       (increment-scan-counters chars)
       (apply-action next (cons new-char buffer) (remaining chars)
         src handler fail k))
      (drop (next)
       (increment-scan-counters chars)
       (apply-action next buffer (remaining chars) src handler fail
         k))
      (goto (state)
       (if (eq? state 'token-start-state) (mark-token-start))
       (let ((action (apply-state state (next-avail chars))))
         (if (eq? action 'error)
             (unexpected-char-error chars src handler fail)
             (apply-action action buffer chars src handler fail k))))
      (emit (token-type)
       (convert-buffer-to-token token-type buffer src handler fail
         (make-cont '<cont-1> chars fail k)))
      (else (error 'apply-action "invalid action: ~a" action)))))

(define*
  scan-error
  (lambda (msg line char src handler fail)
    (apply-handler2
      handler
      (make-exception "ScanError" msg src line char)
      fail)))

(define*
  unexpected-char-error
  (lambda (chars src handler fail)
    (let ((c (next-avail chars)))
      (if (char=? c #\nul)
          (scan-error "unexpected end of input" scan-line scan-char
            src handler fail)
          (scan-error
            (format "unexpected character '~a' encountered" c) scan-line
            scan-char src handler fail)))))

(define*
  convert-buffer-to-token
  (lambda (token-type buffer src handler fail k)
    (let ((buffer (reverse buffer)))
      (case token-type
        (end-marker (apply-cont k (make-token1 'end-marker)))
        (integer
         (apply-cont k (make-token2 'integer (list->string buffer))))
        (decimal
         (apply-cont k (make-token2 'decimal (list->string buffer))))
        (rational
         (apply-cont
           k
           (make-token2 'rational (list->string buffer))))
        (identifier
         (apply-cont
           k
           (make-token2
             'identifier
             (string->symbol (list->string buffer)))))
        (boolean
         (apply-cont
           k
           (make-token2
             'boolean
             (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character
         (apply-cont k (make-token2 'character (car buffer))))
        (named-character
         (let ((name (list->string buffer)))
           (cond
             ((string=? name "nul")
              (apply-cont k (make-token2 'character #\nul)))
             ((string=? name "space")
              (apply-cont k (make-token2 'character #\space)))
             ((string=? name "tab")
              (apply-cont k (make-token2 'character #\tab)))
             ((string=? name "newline")
              (apply-cont k (make-token2 'character #\newline)))
             ((string=? name "linefeed")
              (apply-cont k (make-token2 'character #\newline)))
             ((string=? name "backspace")
              (apply-cont k (make-token2 'character #\backspace)))
             ((string=? name "return")
              (apply-cont k (make-token2 'character #\return)))
             ((string=? name "page")
              (apply-cont k (make-token2 'character #\page)))
             (else
              (scan-error (format "invalid character name #\\~a" name)
                token-start-line token-start-char src handler fail)))))
        (string
         (apply-cont k (make-token2 'string (list->string buffer))))
        (else (apply-cont k (make-token1 token-type)))))))

(define make-token1
  (lambda (token-type)
    (let ((start (list
                   token-start-line
                   token-start-char
                   token-start-position))
          (end (list
                 last-scan-line
                 last-scan-char
                 last-scan-position)))
      (if (eq? token-type 'end-marker)
          (list token-type end end)
          (list token-type start end)))))

(define make-token2
  (lambda (token-type token-info)
    (list
      token-type
      token-info
      (list
        token-start-line
        token-start-char
        token-start-position)
      (list last-scan-line last-scan-char last-scan-position))))

(define token-type?
  (lambda (token class) (eq? (car token) class)))

(define get-token-start (lambda (token) (rac (rdc token))))

(define get-token-end (lambda (token) (rac token)))

(define get-token-start-line
  (lambda (token) (car (get-token-start token))))

(define get-token-start-char
  (lambda (token) (cadr (get-token-start token))))

(define get-token-start-pos
  (lambda (token) (caddr (get-token-start token))))

(define rac
  (lambda (ls)
    (cond ((null? (cdr ls)) (car ls)) (else (rac (cdr ls))))))

(define rdc
  (lambda (ls)
    (cond
      ((null? (cdr ls)) '())
      (else (cons (car ls) (rdc (cdr ls)))))))

(define snoc
  (lambda (x ls)
    (cond
      ((null? ls) (list x))
      (else (cons (car ls) (snoc x (cdr ls)))))))

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
         ((char-whitespace? c) '(drop (goto start-state)))
         ((char=? c #\;) '(drop (goto comment-state)))
         ((char=? c #\nul) '(drop (emit end-marker)))
         (else '(goto token-start-state))))
      (token-start-state
       (cond
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
         (else 'error)))
      (comment-state
       (cond
         ((char=? c #\newline) '(drop (goto start-state)))
         ((char=? c #\nul) '(drop (emit end-marker)))
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
         ((not (char=? c #\nul)) '(shift (goto string-state)))
         (else 'error)))
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

(define aatom?
  (lambda (x) (and (pair? x) (eq? (car x) atom-tag))))

(define apair?
  (lambda (x) (and (pair? x) (eq? (car x) pair-tag))))

(define annotated?
  (lambda (x)
    (and (pair? x)
         (or (eq? (car x) atom-tag) (eq? (car x) pair-tag)))))

(define untag-atom^ (lambda (aatom) (cadr aatom)))

(define atom?^ (lambda (asexp) (eq? (car asexp) atom-tag)))

(define pair?^ (lambda (asexp) (eq? (car asexp) pair-tag)))

(define null?^
  (lambda (asexp)
    (and (atom?^ asexp) (null? (untag-atom^ asexp)))))

(define symbol?^
  (lambda (asexp)
    (and (atom?^ asexp) (symbol? (untag-atom^ asexp)))))

(define string?^
  (lambda (asexp)
    (and (atom?^ asexp) (string? (untag-atom^ asexp)))))

(define vector?^
  (lambda (asexp)
    (and (atom?^ asexp) (vector? (untag-atom^ asexp)))))

(define car^ (lambda (asexp) (cadr asexp)))

(define cdr^ (lambda (asexp) (caddr asexp)))

(define cadr^ (lambda (asexp) (car^ (cdr^ asexp))))

(define cdar^ (lambda (asexp) (cdr^ (car^ asexp))))

(define caar^ (lambda (asexp) (car^ (car^ asexp))))

(define cddr^ (lambda (asexp) (cdr^ (cdr^ asexp))))

(define cdddr^ (lambda (asexp) (cdr^ (cdr^ (cdr^ asexp)))))

(define caddr^ (lambda (asexp) (car^ (cdr^ (cdr^ asexp)))))

(define cdadr^ (lambda (asexp) (cdr^ (car^ (cdr^ asexp)))))

(define cadar^ (lambda (asexp) (car^ (cdr^ (car^ asexp)))))

(define caadr^ (lambda (asexp) (car^ (car^ (cdr^ asexp)))))

(define cadddr^
  (lambda (asexp) (car^ (cdr^ (cdr^ (cdr^ asexp))))))

(define eq?^ (lambda (asexp sym) (eq? (cadr asexp) sym)))

(define vector->list^
  (lambda (asexp) (vector->list (cadr asexp))))

(define symbol->string^
  (lambda (asexp) (symbol->string (cadr asexp))))

(define list?^
  (lambda (asexp)
    (or (null?^ asexp)
        (and (pair?^ asexp) (list?^ (caddr asexp))))))

(define at^
  (lambda (alist)
    (if (null?^ alist)
        '()
        (cons (car^ alist) (at^ (cdr^ alist))))))

(define length^
  (lambda (asexp)
    (cond
      ((null?^ asexp) 0)
      (else (+ 1 (length^ (cdr^ asexp)))))))

(define cons^ (lambda (a b info) (list pair-tag a b info)))

(define*
  annotate-cps
  (lambda (x info k)
    (cond
      ((not *reader-generates-annotated-sexps?*) (apply-cont k x))
      ((annotated? x) (apply-cont k x))
      ((pair? x)
       (annotate-cps (car x) 'none (make-cont '<cont-3> x info k)))
      (else (apply-cont k (list atom-tag x info))))))

(define*
  unannotate-cps
  (lambda (x k)
    (cond
      ((aatom? x) (unannotate-cps (cadr x) k))
      ((apair? x)
       (unannotate-cps (cadr x) (make-cont '<cont-7> x k)))
      ((pair? x)
       (unannotate-cps (car x) (make-cont '<cont-6> x k)))
      ((vector? x)
       (unannotate-cps (vector->list x) (make-cont '<cont-4> k)))
      (else (apply-cont k x)))))

(define make-info
  (lambda (src start end) (cons src (append start end))))

(define replace-info
  (lambda (asexp new-info)
    (if (atom?^ asexp)
        (list atom-tag (cadr asexp) new-info)
        (list pair-tag (cadr asexp) (caddr asexp) new-info))))

(define get-srcfile (lambda (info) (car info)))

(define get-start-line (lambda (info) (cadr info)))

(define get-start-char (lambda (info) (caddr info)))

(define get-start-pos (lambda (info) (cadddr info)))

(define get-end-line (lambda (info) (car (cddddr info))))

(define get-end-char (lambda (info) (cadr (cddddr info))))

(define get-end-pos (lambda (info) (caddr (cddddr info))))

(define get-source-info (lambda (asexp) (rac asexp)))

(define source-info?
  (lambda (x) (or (eq? x 'none) (list? x))))

(define has-source-info?
  (lambda (asexp) (not (eq? (get-source-info asexp) 'none))))

(define original-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
         (= (length (get-source-info asexp)) 7))))

(define first (lambda (x) (car x)))

(define rest-of (lambda (x) (cdr x)))

(define string->integer (lambda (str) (string->number str)))

(define string->decimal (lambda (str) (string->number str)))

(define string->rational
  (lambda (str) (string->number str)))

(define true? (lambda (v) (if v #t #f)))

(define*
  unexpected-token-error
  (lambda (tokens src handler fail)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
          (read-error "unexpected end of input" tokens src handler
            fail)
          (read-error
            (format "unexpected '~a' encountered" (car token)) tokens
            src handler fail)))))

(define*
  read-error
  (lambda (msg tokens src handler fail)
    (let ((token (first tokens)))
      (apply-handler2
        handler
        (make-exception "ReadError" msg src
          (get-token-start-line token) (get-token-start-char token))
        fail))))

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

(define*
  read-sexp
  (lambda (tokens src handler fail k)
    (let ((start (get-token-start (first tokens)))
          (end (get-token-end (first tokens))))
      (record-case (first tokens)
        (integer (str)
         (annotate-cps
           (string->integer str)
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (decimal (str)
         (annotate-cps
           (string->decimal str)
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (rational (str)
         (let ((num (string->rational str)))
           (if (true? num)
               (annotate-cps
                 num
                 (make-info src start end)
                 (make-cont '<cont-9> end tokens fail k))
               (read-error (format "cannot represent ~a" str) tokens src
                 handler fail))))
        (boolean (bool)
         (annotate-cps
           bool
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (character (char)
         (annotate-cps
           char
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (string (str)
         (annotate-cps
           str
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (identifier (id)
         (annotate-cps
           id
           (make-info src start end)
           (make-cont '<cont-9> end tokens fail k)))
        (apostrophe ()
         (read-abbreviation tokens 'quote src handler fail k))
        (backquote ()
         (read-abbreviation tokens 'quasiquote src handler fail k))
        (comma ()
         (read-abbreviation tokens 'unquote src handler fail k))
        (comma-at ()
         (read-abbreviation tokens 'unquote-splicing src handler fail
           k))
        (lparen ()
         (let ((tokens (rest-of tokens)))
           (read-sexp-sequence tokens 'rparen src handler fail
             (make-cont4 '<cont4-2> src start k))))
        (lbracket ()
         (let ((tokens (rest-of tokens)))
           (read-sexp-sequence tokens 'rbracket src handler fail
             (make-cont4 '<cont4-2> src start k))))
        (lvector ()
         (read-vector-sequence (rest-of tokens) src handler fail
           (make-cont4 '<cont4-1> src start k)))
        (else (unexpected-token-error tokens src handler fail))))))

(define*
  read-abbreviation
  (lambda (tokens keyword src handler fail k)
    (let ((start (get-token-start (first tokens)))
          (keyword-end (get-token-end (first tokens))))
      (annotate-cps
        keyword
        (make-info src start keyword-end)
        (make-cont '<cont-10> src start tokens handler fail k)))))

(define*
  read-vector-sequence
  (lambda (tokens src handler fail k)
    (record-case (first tokens)
      (rparen ()
       (close-sexp-sequence '() tokens 'rparen src handler fail k))
      (dot ()
       (read-error "unexpected dot (.)" tokens src handler fail))
      (else (read-sexp
             tokens
             src
             handler
             fail
             (make-cont4 '<cont4-5> src handler k))))))

(define*
  read-sexp-sequence
  (lambda (tokens expected-terminator src handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator src
         handler fail k))
      (dot ()
       (read-error "unexpected dot (.)" tokens src handler fail))
      (else (read-sexp
             tokens
             src
             handler
             fail
             (make-cont4 '<cont4-7> expected-terminator src handler
               k))))))

(define*
  close-sexp-sequence
  (lambda (sexps tokens expected-terminator src handler fail
           k)
    (let ((end (get-token-end (first tokens))))
      (record-case (first tokens)
        ((rparen rbracket) ()
         (cond
           ((token-type? (first tokens) expected-terminator)
            (apply-cont4 k sexps end (rest-of tokens) fail))
           ((eq? expected-terminator 'rparen)
            (read-error "parenthesized list terminated by bracket"
              tokens src handler fail))
           ((eq? expected-terminator 'rbracket)
            (read-error "bracketed list terminated by parenthesis"
              tokens src handler fail))))
        (else (unexpected-token-error tokens src handler fail))))))

(define make-binding (lambda (value) (cons value "")))

(define binding-value (lambda (binding) (car binding)))

(define binding-docstring (lambda (binding) (cdr binding)))

(define set-binding-value!
  (lambda (binding value) (set-car! binding value)))

(define set-binding-docstring!
  (lambda (binding docstring) (set-cdr! binding docstring)))

(define make-frame
  (lambda (variables values)
    (list (list->vector (map make-binding values)) variables)))

(define empty-frame? (lambda (frame) (null? (cadr frame))))

(define frame-bindings (lambda (frame) (car frame)))

(define environment?
  (lambda (x) (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda () (list 'environment (make-frame '() '()))))

(define make-initial-environment
  (lambda (vars vals)
    (list 'environment (make-frame vars vals))))

(define first-frame (lambda (env) (cadr env)))

(define first-frame-vars
  (lambda (env) (cadr (first-frame env))))

(define initial-contours
  (lambda (env) (cdr (first-frame env))))

(define frames (lambda (env) (cdr env)))

(define add-binding
  (lambda (new-var new-binding frame)
    (let ((bindings (vector->list (car frame)))
          (vars (cadr frame)))
      (list
        (list->vector (append bindings (list new-binding)))
        (append vars (list new-var))))))

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

(define in-first-frame?
  (lambda (var env)
    (true? (memq var (first-frame-vars env)))))

(define get-first-frame-value
  (lambda (var env)
    (binding-value (search-frame (first-frame env) var))))

(define*
  lookup-value-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (apply-cont2
        k
        (binding-value (vector-ref bindings offset))
        fail))))

(define*
  lookup-binding-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (apply-cont2 k (vector-ref bindings offset) fail))))

(define*
  lookup-value
  (lambda (var env var-info handler fail k)
    (lookup-variable var env var-info handler fail (make-cont2 '<cont2-4> k)
      (make-cont3 '<cont3-3> k) (make-cont2 '<cont2-3> k))))

(define*
  lookup-variable
  (lambda (var env var-info handler fail gk dk sk)
    (let ((binding (search-env env var)))
      (if binding
          (apply-cont2 sk binding fail)
          (let ((components (split-variable var)))
            (cond
              ((and (null? (cdr components))
                    (dlr-env-contains (car components)))
               (apply-cont2 gk (car components) fail))
              ((and (not (null? (cdr components)))
                    (dlr-env-contains (car components))
                    (dlr-object-contains
                      (dlr-env-lookup (car components))
                      components))
               (apply-cont3
                 dk
                 (dlr-env-lookup (car components))
                 components
                 fail))
              ((null? (cdr components))
               (runtime-error
                 (format "unbound variable '~a'" var)
                 var-info
                 handler
                 fail))
              (else
               (lookup-variable-components components "" env var-info
                 handler fail dk sk))))))))

(define*
  lookup-variable-components
  (lambda (components path module var-info handler fail dk sk)
    (let* ((var (car components))
           (binding (search-env module var)))
      (cond
        (binding
         (if (null? (cdr components))
             (apply-cont2 sk binding fail)
             (let ((value (binding-value binding))
                   (new-path (if (string=? path "")
                                 (format "~a" var)
                                 (format "~a.~a" path var))))
               (cond
                 ((environment? value)
                  (lookup-variable-components (cdr components) new-path
                    value var-info handler fail dk sk))
                 ((dlr-object-contains value components)
                  (apply-cont3 dk value components fail))
                 (else
                  (runtime-error
                    (format "'~a' is not a module" new-path)
                    var-info
                    handler
                    fail))))))
        ((string=? path "")
         (runtime-error
           (format "unbound module '~a'" var)
           var-info
           handler
           fail))
        (else
         (runtime-error
           (format "unbound variable '~a' in module '~a'" var path)
           var-info
           handler
           fail))))))

(define*
  lookup-binding-in-first-frame
  (lambda (var env handler fail k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
            (apply-cont2 k binding fail)
            (let ((new-binding (make-binding 'undefined)))
              (let ((new-frame (add-binding var new-binding frame)))
                (set-first-frame! env new-frame)
                (apply-cont2 k new-binding fail))))))))

(define split-variable
  (lambda (var)
    (let ((strings (string-split (symbol->string var) #\.)))
      (if (member "" strings) '() (map string->symbol strings)))))

(define string-split
  (lambda (s delimiter-char)
    (letrec ((position (lambda (chars)
                         (if (char=? (car chars) delimiter-char)
                             0
                             (+ 1 (position (cdr chars))))))
             (split (lambda (chars)
                      (cond
                        ((null? chars) '())
                        ((not (member delimiter-char chars))
                         (list (apply string chars)))
                        (else
                         (let ((n (position chars)))
                           (cons
                             (apply string (list-head chars n))
                             (split (cdr (list-tail chars n))))))))))
      (split (string->list s)))))

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

(define anything? (lambda (datum) #t))

(define application?^
  (lambda (asexp)
    (and (list?^ asexp)
         (not (null?^ asexp))
         (not (reserved-keyword? (untag-atom^ (car^ asexp)))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
         (not (eq? (memq x (get-reserved-keywords)) #f)))))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define begin
      cond and or let let* letrec case record-case try catch
      finally raise define-syntax choose define-datatype cases
      trace-lambda)))

(define mit-style-define?^
  (lambda (asexp) (not (symbol?^ (cadr^ asexp)))))

(define literal?
  (lambda (datum)
    (or (number? datum)
        (boolean? datum)
        (null? datum)
        (char? datum)
        (string? datum))))

(define literal?^
  (lambda (asexp)
    (and (eq? (car asexp) atom-tag)
         (or (number? (untag-atom^ asexp))
             (boolean? (untag-atom^ asexp))
             (null? (untag-atom^ asexp))
             (char? (untag-atom^ asexp))
             (string? (untag-atom^ asexp))))))

(define syntactic-sugar?^
  (lambda (asexp)
    (and (pair?^ asexp)
         (symbol?^ (car^ asexp))
         (in-first-frame? (untag-atom^ (car^ asexp)) macro-env))))

(define define-var^ (lambda (x) (untag-atom^ (cadr^ x))))

(define define-docstring^
  (lambda (x) (untag-atom^ (caddr^ x))))

(define try-body^ (lambda (x) (cadr^ x)))

(define catch-var^
  (lambda (x) (untag-atom^ (cadr^ (caddr^ x)))))

(define catch-exps^ (lambda (x) (cddr^ (caddr^ x))))

(define try-finally-exps^ (lambda (x) (cdr^ (caddr^ x))))

(define try-catch-finally-exps^
  (lambda (x) (cdr^ (cadddr^ x))))

(define*
  aparse
  (lambda (adatum senv handler fail k)
    (let ((info (get-source-info adatum)))
      (cond
        ((literal?^ adatum)
         (apply-cont2 k (lit-aexp (untag-atom^ adatum) info) fail))
        ((symbol?^ adatum)
         (if *use-lexical-address*
             (get-lexical-address (untag-atom^ adatum) senv 0 info fail
               k)
             (apply-cont2 k (var-aexp (untag-atom^ adatum) info) fail)))
        ((vector?^ adatum)
         (unannotate-cps adatum (make-cont '<cont-20> info fail k)))
        ((quote?^ adatum)
         (unannotate-cps adatum (make-cont '<cont-19> info fail k)))
        ((quasiquote?^ adatum)
         (qq-expand-cps
           (cadr^ adatum)
           0
           (make-cont '<cont-18> adatum senv info handler fail k)))
        ((unquote?^ adatum)
         (aparse-error "misplaced" adatum handler fail))
        ((unquote-splicing?^ adatum)
         (aparse-error "misplaced" adatum handler fail))
        ((syntactic-sugar?^ adatum)
         (expand-once^
           adatum
           handler
           fail
           (make-cont2 '<cont2-30> senv handler k)))
        ((if-then?^ adatum)
         (aparse (cadr^ adatum) senv handler fail
           (make-cont2 '<cont2-29> adatum senv info handler k)))
        ((if-else?^ adatum)
         (aparse (cadr^ adatum) senv handler fail
           (make-cont2 '<cont2-27> adatum senv info handler k)))
        ((assignment?^ adatum)
         (aparse (caddr^ adatum) senv handler fail
           (make-cont2 '<cont2-24> adatum info k)))
        ((func?^ adatum)
         (aparse (cadr^ adatum) senv handler fail
           (make-cont2 '<cont2-23> info k)))
        ((define?^ adatum)
         (cond
           ((mit-style-define?^ adatum)
            (apply-macro mit-define-transformer^ adatum handler fail
              (make-cont '<cont-16> senv info handler fail k)))
           ((= (length^ adatum) 3)
            (aparse (caddr^ adatum) senv handler fail
              (make-cont2 '<cont2-22> adatum info k)))
           ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum)))
            (aparse (cadddr^ adatum) senv handler fail
              (make-cont2 '<cont2-21> adatum info k)))
           (else
            (aparse-error "bad concrete syntax:" adatum handler fail))))
        ((define!?^ adatum)
         (cond
           ((mit-style-define?^ adatum)
            (apply-macro mit-define-transformer^ adatum handler fail
              (make-cont '<cont-16> senv info handler fail k)))
           ((= (length^ adatum) 3)
            (aparse (caddr^ adatum) senv handler fail
              (make-cont2 '<cont2-20> adatum info k)))
           ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum)))
            (aparse (cadddr^ adatum) senv handler fail
              (make-cont2 '<cont2-19> adatum info k)))
           (else
            (aparse-error "bad concrete syntax:" adatum handler fail))))
        ((define-syntax?^ adatum)
         (let ((name (define-var^ adatum)) (aclauses (cddr^ adatum)))
           (unannotate-cps
             aclauses
             (make-cont '<cont-14> aclauses name info fail k))))
        ((begin?^ adatum)
         (cond
           ((null?^ (cdr^ adatum))
            (aparse-error "bad concrete syntax:" adatum handler fail))
           ((null?^ (cddr^ adatum))
            (aparse (cadr^ adatum) senv handler fail k))
           (else
            (aparse-all (cdr^ adatum) senv handler fail
              (make-cont2 '<cont2-18> info k)))))
        ((lambda?^ adatum)
         (unannotate-cps
           (cadr^ adatum)
           (make-cont '<cont-13> adatum senv info handler fail k)))
        ((trace-lambda?^ adatum)
         (unannotate-cps
           (caddr^ adatum)
           (make-cont '<cont-12> adatum senv info handler fail k)))
        ((try?^ adatum)
         (cond
           ((= (length^ adatum) 2)
            (aparse (try-body^ adatum) senv handler fail k))
           ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
            (aparse (try-body^ adatum) senv handler fail
              (make-cont2 '<cont2-15> adatum senv info handler k)))
           ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
            (aparse (try-body^ adatum) senv handler fail
              (make-cont2 '<cont2-13> adatum senv info handler k)))
           ((and (= (length^ adatum) 4)
                 (catch?^ (caddr^ adatum))
                 (finally?^ (cadddr^ adatum)))
            (aparse (try-body^ adatum) senv handler fail
              (make-cont2 '<cont2-11> adatum senv info handler k)))
           (else
            (aparse-error "bad try syntax:" adatum handler fail))))
        ((raise?^ adatum)
         (aparse (cadr^ adatum) senv handler fail
           (make-cont2 '<cont2-8> info k)))
        ((choose?^ adatum)
         (aparse-all (cdr^ adatum) senv handler fail
           (make-cont2 '<cont2-7> info k)))
        ((application?^ adatum)
         (aparse (car^ adatum) senv handler fail
           (make-cont2 '<cont2-6> adatum senv info handler k)))
        (else
         (aparse-error
           "bad concrete syntax:"
           adatum
           handler
           fail))))))

(define*
  aparse-all
  (lambda (adatum-list senv handler fail k)
    (if (null?^ adatum-list)
        (apply-cont2 k '() fail)
        (aparse (car^ adatum-list) senv handler fail
          (make-cont2 '<cont2-32> adatum-list senv handler k)))))

(define*
  aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps
        adatum
        (make-cont '<cont-21> msg info handler fail)))))

(define*
  aparse-sexps
  (lambda (tokens src senv handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k '() fail)
        (read-sexp tokens src handler fail
          (make-cont4 '<cont4-9> senv src handler k)))))

(define*
  get-lexical-address
  (lambda (id senv depth info fail k)
    (cond
      ((null? senv) (apply-cont2 k (var-aexp id info) fail))
      ((memq id (car senv))
       (get-lexical-address-offset id (car senv) depth 0 info fail
         k))
      (else
       (get-lexical-address id (cdr senv) (+ depth 1) info fail
         k)))))

(define*
  get-lexical-address-offset
  (lambda (id contours depth offset info fail k)
    (if (eq? (car contours) id)
        (apply-cont2
          k
          (lexical-address-aexp depth offset id info)
          fail)
        (get-lexical-address-offset id (cdr contours) depth
          (+ offset 1) info fail k))))

(define*
  create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null?^ vars)
        (apply-cont2 k2 '() '())
        (create-letrec-assignments^
          (cdr^ vars)
          (cdr^ procs)
          (make-cont2 '<cont2-36> procs vars k2)))))

(define*
  amacro-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (apply-handler2
        handler
        (make-exception "MacroError" msg (get-start-line info)
          (get-srcfile info) (get-start-char info))
        fail))))

(define*
  nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null?^ bindings) (null?^ (cdr^ bindings)))
        (apply-cont k `(let (unquote bindings) ,@(at^ bodies)))
        (nest-let*-bindings^
          (cdr^ bindings)
          bodies
          (make-cont '<cont-22> bindings k)))))

(define*
  case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null?^ clauses)
        (apply-cont k '())
        (case-clauses->simple-cond-clauses^
          var
          (cdr^ clauses)
          (make-cont '<cont-23> clauses var k)))))

(define*
  case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
        (apply-cont2 k2 '() '())
        (case-clauses->cond-clauses^
          var
          (cdr^ clauses)
          (make-cont2 '<cont2-38> clauses var k2)))))

(define*
  record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
        (apply-cont2 k2 '() '())
        (record-case-clauses->cond-clauses^
          var
          (cdr^ clauses)
          (make-cont2 '<cont2-39> clauses var k2)))))

(define*
  make-dd-variant-constructors^
  (lambda (variants k2)
    (if (null?^ variants)
        (apply-cont2 k2 '() '())
        (make-dd-variant-constructor^
          (car^ variants)
          (make-cont2 '<cont2-42> variants k2)))))

(define*
  make-dd-variant-constructor^
  (lambda (variant k2)
    (let ((name (car^ variant)) (fields (cdr^ variant)))
      (verify-dd-constructor-fields^
        name
        fields
        'args
        (make-cont '<cont-24> fields name k2)))))

(define*
  verify-dd-constructor-fields^
  (lambda (name fields cdrs k)
    (if (null?^ fields)
        (apply-cont k `(cons ',name args))
        (verify-dd-constructor-fields^
          name
          (cdr^ fields)
          `(cdr ,cdrs)
          (make-cont '<cont-25> cdrs fields name k)))))

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case
        'define-datatype 'cases)
      (list and-transformer^ or-transformer^ cond-transformer^
        let-transformer^ letrec-transformer^ let*-transformer^
        case-transformer^ record-case-transformer^
        define-datatype-transformer^ cases-transformer^))))

(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define pattern-macro?
  (lambda (x) (and (pair? x) (eq? (car x) 'pattern-macro))))

(define macro-clauses (lambda (macro) (cadr macro)))

(define macro-aclauses (lambda (macro) (caddr macro)))

(define define-syntax-clause?
  (lambda (x)
    (and (list? x)
         (= (length x) 2)
         (pattern? (car x))
         (pattern? (cadr x)))))

(define define-syntax-clause?^
  (lambda (x)
    (and (list?^ x)
         (= (length^ x) 2)
         (apattern? (car^ x))
         (apattern? (cadr^ x)))))

(define apattern?
  (lambda (x)
    (or (aatom? x)
        (and (apair? x)
             (apattern? (cadr x))
             (apattern? (caddr x))))))

(define list-of-define-syntax-clauses?^
  (lambda (alist)
    (or (null?^ alist)
        (and (define-syntax-clause?^ (car^ alist))
             (list-of-define-syntax-clauses?^ (cdr^ alist))))))

(define*
  expand-once^
  (lambda (adatum handler fail k)
    (let ((macro-keyword (untag-atom^ (car^ adatum))))
      (let ((macro (get-first-frame-value
                     macro-keyword
                     macro-env)))
        (if (pattern-macro? macro)
            (process-macro-clauses^ (macro-clauses macro)
              (macro-aclauses macro) adatum handler fail k)
            (apply-macro macro adatum handler fail
              (make-cont '<cont-27> adatum macro-keyword fail k)))))))

(define*
  process-macro-clauses^
  (lambda (clauses aclauses adatum handler fail k)
    (if (null? clauses)
        (aparse-error
          "no matching clause found for"
          adatum
          handler
          fail)
        (let ((left-pattern (caar clauses))
              (right-pattern (cadar clauses))
              (left-apattern (caar^ aclauses))
              (right-apattern (cadar^ aclauses)))
          (unannotate-cps
            adatum
            (make-cont '<cont-29> aclauses adatum clauses left-apattern
              left-pattern right-apattern right-pattern handler fail
              k))))))

(define*
  qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps
         (cdr^ ax)
         (+ depth 1)
         (make-cont '<cont-35> k)))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
         ((> depth 0)
          (qq-expand-cps
            (cdr^ ax)
            (- depth 1)
            (make-cont '<cont-34> ax k)))
         ((and (unquote?^ ax)
               (not (null?^ (cdr^ ax)))
               (null?^ (cddr^ ax)))
          (apply-cont k (cadr^ ax)))
         (else (apply-cont k `',ax))))
      ((vector?^ ax)
       (annotate-cps
         (vector->list^ ax)
         'none
         (make-cont '<cont-33> depth k)))
      ((not (pair?^ ax)) (apply-cont k `',ax))
      ((null?^ (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-31> ax depth k))))))

(define*
  qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps
         (cdr^ ax)
         (+ depth 1)
         (make-cont '<cont-40> k)))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
         ((> depth 0)
          (qq-expand-cps
            (cdr^ ax)
            (- depth 1)
            (make-cont '<cont-39> ax k)))
         ((unquote?^ ax) (apply-cont k `(list . ,(cdr^ ax))))
         ((null?^ (cddr^ ax)) (apply-cont k (cadr^ ax)))
         (else (apply-cont k `(append . ,(cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth (make-cont '<cont-38> k)))
      ((not (pair?^ ax)) (apply-cont k `'(,ax)))
      ((null?^ (cdr^ ax))
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-38> k)))
      (else
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-37> ax depth k))))))

(define aunparse
  (lambda (aexp)
    (cases aexpression aexp
     (lit-aexp
       (datum info)
       (cond
         ((literal? datum) datum)
         ((vector? datum) datum)
         (else `',datum)))
     (var-aexp (id info) id)
     (lexical-address-aexp (depth offset id info) id)
     (if-aexp
       (test-aexp then-aexp else-aexp info)
       `(if ,(aunparse test-aexp)
            ,(aunparse then-aexp)
            ,(aunparse else-aexp)))
     (assign-aexp
       (var rhs-exp var-info info)
       `(set! ,var ,(aunparse rhs-exp)))
     (func-aexp (exp info) `(func ,(aunparse exp)))
     (define-aexp
       (id docstring rhs-exp info)
       (if (string=? docstring "")
           `(define (unquote id) ,(aunparse rhs-exp))
           `(define (unquote id) ,docstring ,(aunparse rhs-exp))))
     (define!-aexp
       (id docstring rhs-exp info)
       (if (string=? docstring "")
           `(define! ,id ,(aunparse rhs-exp))
           `(define! ,id ,docstring ,(aunparse rhs-exp))))
     (define-syntax-aexp
       (name clauses aclauses info)
       `(define-syntax (unquote name) ,@clauses))
     (begin-aexp (exps info) `(begin ,@(map aunparse exps)))
     (lambda-aexp
       (formals bodies info)
       `(lambda (unquote formals) ,@(map aunparse bodies)))
     (mu-lambda-aexp
       (formals runt bodies info)
       `(lambda (,@formals . ,runt) ,@(map aunparse bodies)))
     (app-aexp
       (operator operands info)
       `(,(aunparse operator) ,@(map aunparse operands)))
     (try-catch-aexp
       (body catch-var catch-exps info)
       `(try ,(aunparse body)
             (catch ,catch-var ,@(map aunparse catch-exps))))
     (try-finally-aexp
       (body finally-exps info)
       `(try ,(aunparse body)
             (finally ,@(map aunparse finally-exps))))
     (try-catch-finally-aexp
       (body catch-var catch-exps finally-exps info)
       `(try ,(aunparse body)
             (catch ,catch-var ,@(map aunparse catch-exps))
             (finally ,@(map aunparse finally-exps))))
     (raise-aexp (exp info) `(raise ,(aunparse exp)))
     (choose-aexp (exps info) `(choose ,@(map aunparse exps)))
     (else (error 'aunparse "bad abstract syntax: ~s" aexp)))))

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

(define start
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (read-eval-print-loop)))

(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop)))

(define*
  read-eval-print-loop
  (lambda ()
    (let ((input (raw-read-line "==> ")))
      (let ((result (execute input 'stdin)))
        (if (not (void? result)) (safe-print result))
        (if *need-newline* (newline))
        (if (end-of-session? result)
            (halt* 'goodbye)
            (read-eval-print-loop))))))

(define exception?
  (lambda (x) (and (pair? x) (eq? (car x) 'exception))))

(define execute-string
  (lambda (input) (execute input 'stdin)))

(define execute-file
  (lambda (filename)
    (execute (read-content filename) filename)))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (let ((result (scan-input input src REP-handler *last-fail*
                    REP-k)))
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
      (make-cont4 '<cont4-10>))))

(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (initialize-execute)
    (set! *last-fail* REP-fail)))

(define execute-string-rm
  (lambda (input) (execute-rm input 'stdin)))

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

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input 'stdin try-parse-handler *last-fail*
      (make-cont2 '<cont2-48>))
    (trampoline)))

(define make-debugging-k
  (lambda (exp k)
    (if (not *tracing-on?*) k (make-cont2 '<cont2-49> exp k))))

(define highlight-expression
  (lambda (exp)
    (printf "call: ~s~%" (aunparse exp))
    (let ((info (rac exp)))
      (if (not (eq? info 'none))
          (printf
            "['~a' at line ~a column ~a]~%"
            (get-srcfile info)
            (get-start-line info)
            (get-start-char info))))))

(define handle-debug-info
  (lambda (exp result)
    (printf "~s evaluates to ~a~%" (aunparse exp) result)))

(define get-use-stack-trace (lambda () *use-stack-trace*))

(define set-use-stack-trace
  (lambda (value) (set! *use-stack-trace* value)))

(define initialize-stack-trace
  (lambda () (set-car! *stack-trace* '())))

(define push-stack-trace
  (lambda (exp)
    (set-car! *stack-trace* (cons exp (car *stack-trace*)))))

(define pop-stack-trace
  (lambda (exp)
    (if (not (null? (car *stack-trace*)))
        (set-car! *stack-trace* (cdr (car *stack-trace*))))))

(define*
  m
  (lambda (exp env handler fail k)
    (if *tracing-on?* (highlight-expression exp))
    (let ((k (if *tracing-on?* (make-debugging-k exp k) k)))
      (cases aexpression exp
       (lit-aexp (datum info) (apply-cont2 k datum fail))
       (var-aexp
         (id info)
         (lookup-value id env info handler fail k))
       (lexical-address-aexp
         (depth offset id info)
         (lookup-value-by-lexical-address depth offset (frames env)
           fail k))
       (func-aexp
         (exp info)
         (m exp env handler fail (make-cont2 '<cont2-64> k)))
       (if-aexp
         (test-exp then-exp else-exp info)
         (m test-exp env handler fail
            (make-cont2 '<cont2-63> else-exp then-exp env handler k)))
       (assign-aexp
         (var rhs-exp var-info info)
         (m rhs-exp env handler fail
            (make-cont2 '<cont2-62> var var-info env handler k)))
       (define-aexp
         (var docstring rhs-exp info)
         (m rhs-exp env handler fail
            (make-cont2 '<cont2-59> docstring var env handler k)))
       (define!-aexp
         (var docstring rhs-exp info)
         (m rhs-exp env handler fail
            (make-cont2 '<cont2-57> docstring var k)))
       (define-syntax-aexp
         (name clauses aclauses info)
         (lookup-binding-in-first-frame name macro-env handler fail
           (make-cont2 '<cont2-56> aclauses clauses k)))
       (begin-aexp
         (exps info)
         (eval-sequence exps env handler fail k))
       (lambda-aexp
         (formals bodies info)
         (apply-cont2 k (closure formals bodies env) fail))
       (mu-lambda-aexp
         (formals runt bodies info)
         (apply-cont2 k (mu-closure formals runt bodies env) fail))
       (trace-lambda-aexp
         (name formals bodies info)
         (apply-cont2
           k
           (trace-closure name formals bodies env)
           fail))
       (mu-trace-lambda-aexp
         (name formals runt bodies info)
         (apply-cont2
           k
           (mu-trace-closure name formals runt bodies env)
           fail))
       (try-catch-aexp
         (body cvar cexps info)
         (let ((new-handler (try-catch-handler cvar cexps env handler
                              k)))
           (m body env new-handler fail k)))
       (try-finally-aexp
         (body fexps info)
         (let ((new-handler (try-finally-handler fexps env handler)))
           (m body env new-handler fail
              (make-cont2 '<cont2-55> fexps env handler k))))
       (try-catch-finally-aexp
         (body cvar cexps fexps info)
         (let ((new-handler (try-catch-finally-handler cvar cexps
                              fexps env handler k)))
           (m body env new-handler fail
              (make-cont2 '<cont2-55> fexps env handler k))))
       (raise-aexp
         (exp info)
         (m exp env handler fail (make-cont2 '<cont2-53> handler)))
       (choose-aexp
         (exps info)
         (eval-choices exps env handler fail k))
       (app-aexp
         (operator operands info)
         (m* operands env handler fail
             (make-cont2 '<cont2-52> exp operator env info handler k)))
       (else (error 'm "bad abstract syntax: '~s'" exp))))))

(define make-exception
  (lambda (exception message source line column)
    (list exception message source line column
      (make-stack-trace))))

(define make-stack-trace
  (lambda ()
    (let ((trace (car *stack-trace*)))
      (reverse (map format-stack-trace trace)))))

(define get-procedure-name
  (lambda (exp)
    (cases aexpression exp
      (lexical-address-aexp (depth offset id info) id)
      (var-aexp (id info) id)
      (app-aexp
        (operator operands info)
        (get-procedure-name operator))
      (else 'unknown))))

(define format-stack-trace
  (lambda (exp)
    (let ((info (rac exp)))
      (list
        (get-srcfile info)
        (get-start-line info)
        (get-start-char info)
        (get-procedure-name exp)))))

(define*
  runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
        (apply-handler2
          handler
          (make-exception "RunTimeError" msg 'none 'none 'none)
          fail)
        (let ((src (get-srcfile info))
              (line (get-start-line info))
              (char (get-start-char info)))
          (apply-handler2
            handler
            (make-exception "RunTimeError" msg src line char)
            fail)))))

(define*
  m*
  (lambda (exps env handler fail k)
    (if (null? exps)
        (apply-cont2 k '() fail)
        (m (car exps) env handler fail
           (make-cont2 '<cont2-65> exps env handler k)))))

(define*
  eval-sequence
  (lambda (exps env handler fail k)
    (if (null? (cdr exps))
        (m (car exps) env handler fail k)
        (m (car exps) env handler fail
           (make-cont2 '<cont2-66> exps env handler k)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (make-handler2 '<handler2-4> cexps cvar env handler k)))

(define try-finally-handler
  (lambda (fexps env handler)
    (make-handler2 '<handler2-5> fexps env handler)))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (make-handler2 '<handler2-6> cexps cvar fexps env handler
      k)))

(define*
  eval-choices
  (lambda (exps env handler fail k)
    (if (null? exps)
        (apply-fail fail)
        (let ((new-fail (make-fail '<fail-5> exps env handler fail
                          k)))
          (m (car exps) env handler new-fail k)))))

(define closure
  (lambda (formals bodies env)
    (make-proc '<proc-1> bodies formals env)))

(define mu-closure
  (lambda (formals runt bodies env)
    (make-proc '<proc-2> bodies formals runt env)))

(define make-trace-depth-string
  (lambda (level)
    (if (= level 0)
        ""
        (string-append
          " |"
          (make-trace-depth-string (- level 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (let ((trace-depth 0))
      (make-proc '<proc-3> bodies name trace-depth formals env))))

(define continuation-object?
  (lambda (x)
    (and (pair? x)
         (memq
           (car x)
           '(continuation
              continuation2
              continuation3
              continuation4)))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (let ((trace-depth 0))
      (make-proc '<proc-4> bodies name trace-depth formals runt
        env))))

(define length-one?
  (lambda (ls) (and (not (null? ls)) (null? (cdr ls)))))

(define length-two?
  (lambda (ls)
    (and (not (null? ls))
         (not (null? (cdr ls)))
         (null? (cddr ls)))))

(define length-at-least?
  (lambda (n ls)
    (cond
      ((< n 1) #t)
      ((or (null? ls) (not (pair? ls))) #f)
      (else (length-at-least? (- n 1) (cdr ls))))))

(define all-numeric?
  (lambda (ls)
    (or (null? ls)
        (and (number? (car ls)) (all-numeric? (cdr ls))))))

(define all-char?
  (lambda (ls)
    (or (null? ls)
        (and (char? (car ls)) (all-char? (cdr ls))))))

(define void? (lambda (x) (eq? x void-value)))

(define end-of-session? (lambda (x) (eq? x end-of-session)))

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
    (or (procedure? x)
        (and (pair? x) (eq? (car x) 'procedure)))))

(define environment-object?
  (lambda (x) (and (pair? x) (eq? (car x) 'environment))))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

(define*
  load-file
  (lambda (filename env2 info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (apply-cont2 k void-value fail))
      ((not (string? filename))
       (runtime-error
         (format "filename '~a' is not a string" filename)
         info
         handler
         fail))
      ((not (file-exists? filename))
       (runtime-error
         (format "attempted to load nonexistent file '~a'" filename)
         info
         handler
         fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
         (make-cont2 '<cont2-74> filename env2 handler k))))))

(define*
  read-and-eval-asexps
  (lambda (tokens src env2 handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k void-value fail)
        (read-sexp tokens src handler fail
          (make-cont4 '<cont4-13> src env2 handler k)))))

(define*
  load-files
  (lambda (filenames env2 info handler fail k)
    (if (null? filenames)
        (apply-cont2 k void-value fail)
        (load-file (car filenames) env2 info handler fail
          (make-cont2 '<cont2-77> filenames env2 info handler k)))))

(define*
  length-loop
  (lambda (x sum ls info handler fail k2)
    (cond
      ((null? x) (apply-cont2 k2 sum fail))
      ((not (pair? x))
       (runtime-error
         (format "length called on improper list ~s" ls)
         info
         handler
         fail))
      (else
       (length-loop (cdr x) (+ sum 1) ls info handler fail k2)))))

(define*
  make-set
  (lambda (lst env2 info handler fail k2)
    (if (null? lst)
        (apply-cont2 k2 lst fail)
        (make-set (cdr lst) env2 info handler fail
          (make-cont2 '<cont2-78> lst k2)))))

(define*
  equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
           (and (boolean? x)
                (boolean? y)
                (or (and x y) (and (not x) (not y))))
           (and (symbol? x) (symbol? y) (eq? x y))
           (and (number? x) (number? y) (= x y))
           (and (char? x) (char? y) (char=? x y))
           (and (string? x) (string? y) (string=? x y)))
       (apply-cont k #t))
      ((and (pair? x) (pair? y))
       (equal-objects?
         (car x)
         (car y)
         (make-cont '<cont-45> x y k)))
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
          (make-cont '<cont-46> i v1 v2 k)))))

(define*
  member-loop
  (lambda (x y ls info handler fail k)
    (cond
      ((null? y) (apply-cont2 k #f fail))
      ((not (pair? y))
       (runtime-error
         (format "member called on improper list ~s" ls)
         info
         handler
         fail))
      (else
       (equal-objects?
         x
         (car y)
         (make-cont '<cont-47> ls x y info handler fail k))))))

(define*
  get-primitive
  (lambda (args env info handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env 'none handler fail
        (make-cont2 '<cont2-80> args sym info handler k)))))

(define*
  append2
  (lambda (ls1 ls2 fail k2)
    (if (null? ls1)
        (apply-cont2 k2 ls2 fail)
        (append2
          (cdr ls1)
          ls2
          fail
          (make-cont2 '<cont2-81> ls1 k2)))))

(define*
  append-all
  (lambda (lists info handler fail k2)
    (cond
      ((null? lists) (apply-cont2 k2 '() fail))
      ((null? (cdr lists)) (apply-cont2 k2 (car lists) fail))
      ((not (list? (car lists)))
       (runtime-error
         (format
           "append called on incorrect list structure ~s"
           (car lists))
         info
         handler
         fail))
      (else
       (append-all (cdr lists) info handler fail
         (make-cont2 '<cont2-82> lists k2))))))

(define dir
  (lambda (args env)
    (if (or (null? args) (environment? (car args)))
        (sort
          symbol<?
          (if (null? args)
              (append
                (get-variables-from-frames (frames macro-env))
                (get-variables-from-frames (frames env)))
              (get-variables-from-frames (frames (car args)))))
        (get-external-members (car args)))))

(define get-variables-from-frame
  (lambda (frame) (cadr frame)))

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

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
         (inexact (/ (time-nanosecond now) 1000000000))))))

(define*
  map-primitive
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

(define listify
  (lambda (arg-list)
    (cond
      ((null? arg-list) '())
      ((list? (car arg-list))
       (cons (car arg-list) (listify (cdr arg-list))))
      ((vector? (car arg-list))
       (cons
         (vector->list (car arg-list))
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
          (apply-proc proc (list item) env 'none handler fail
            (make-cont2 '<cont2-83> iterator proc env handler k))))))

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
          (apply-proc proc (list item) env 'none handler fail
            (make-cont2 '<cont2-84> iterator proc env handler k))))))

(define*
  map1
  (lambda (proc list1 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-proc? proc)
            (map1 proc (cdr list1) env handler fail
              (make-cont2 '<cont2-86> list1 proc k))
            (apply-proc proc (list (car list1)) env 'none handler fail
              (make-cont2 '<cont2-85> list1 proc env handler k))))))

(define*
  map2
  (lambda (proc list1 list2 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-proc? proc)
            (map2 proc (cdr list1) (cdr list2) env handler fail
              (make-cont2 '<cont2-88> list1 list2 proc k))
            (apply-proc proc (list (car list1) (car list2)) env 'none handler fail
              (make-cont2 '<cont2-87> list1 list2 proc env handler k))))))

(define*
  mapN
  (lambda (proc lists env handler fail k)
    (if (null? (car lists))
        (apply-cont2 k '() fail)
        (if (dlr-proc? proc)
            (mapN proc (map cdr lists) env handler fail
              (make-cont2 '<cont2-90> lists proc k))
            (apply-proc proc (map car lists) env 'none handler fail
              (make-cont2 '<cont2-89> lists proc env handler k))))))

(define*
  for-each-primitive
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
        (iterate proc (car lists) env handler fail k)
        (let ((arg-list (listify lists)))
          (if (null? (car arg-list))
              (apply-cont2 k void-value fail)
              (if (dlr-proc? proc)
                  (begin
                    (dlr-apply proc (map car arg-list))
                    (for-each-primitive proc (map cdr arg-list) env handler
                      fail k))
                  (apply-proc proc (map car arg-list) env 'none handler fail
                    (make-cont2 '<cont2-91> arg-list proc env handler
                      k))))))))

(define make-initial-env-extended (lambda (env) env))

(define make-toplevel-env
  (lambda ()
    (let ((primitives (list (list '* times-prim) (list '+ plus-prim)
                       (list '- minus-prim) (list '/ divide-prim)
                       (list '% modulo-prim) (list '< lt-prim)
                       (list '<= lt-or-eq-prim) (list '= equal-sign-prim)
                       (list '=? equal-sign-prim) (list '> gt-prim)
                       (list '>= gt-or-eq-prim) (list 'abort abort-prim)
                       (list 'abs abs-prim) (list 'append append-prim)
                       (list 'apply apply-prim) (list 'assv assv-prim)
                       (list 'boolean? boolean?-prim)
                       (list 'caddr caddr-prim) (list 'cadr cadr-prim)
                       (list 'call-with-current-continuation call/cc-prim)
                       (list 'call/cc call/cc-prim) (list 'car car-prim)
                       (list 'cdr cdr-prim) (list 'char? char?-prim)
                       (list 'char=? char=?-prim)
                       (list 'char-whitespace? char-whitespace?-prim)
                       (list 'char-alphabetic? char-alphabetic?-prim)
                       (list 'char-numeric? char-numeric?-prim)
                       (list 'cons cons-prim)
                       (list 'current-time current-time-prim)
                       (list 'cut cut-prim) (list 'dir dir-prim)
                       (list 'display display-prim)
                       (list 'current-environment current-environment-prim)
                       (list 'eq? eq?-prim) (list 'equal? equal?-prim)
                       (list 'error error-prim) (list 'eval eval-prim)
                       (list 'eval-ast eval-ast-prim)
                       (list 'exit exit-prim)
                       (list 'for-each for-each-prim) (list 'get get-prim)
                       (list 'get-stack-trace get-stack-trace-prim)
                       (list 'import import-prim)
                       (list 'length length-prim) (list 'list list-prim)
                       (list 'list->vector list-to-vector-prim)
                       (list 'list->string list->string-prim)
                       (list 'list-ref list-ref-prim)
                       (list 'load load-prim)
                       (list 'make-set make-set-prim)
                       (list 'make-vector make-vector-prim)
                       (list 'map map-prim) (list 'member member-prim)
                       (list 'memq memq-prim) (list 'memv memv-prim)
                       (list 'newline newline-prim) (list 'not not-prim)
                       (list 'null? null?-prim)
                       (list 'number->string number->string-prim)
                       (list 'number? number?-prim)
                       (list 'pair? pair?-prim) (list 'parse parse-prim)
                       (list 'parse-string parse-string-prim)
                       (list 'print print-prim)
                       (list 'printf printf-primitive)
                       (list 'range range-prim)
                       (list 'read-string read-string-prim)
                       (list 'require require-prim)
                       (list 'reverse reverse-prim)
                       (list 'set-car! set-car!-prim)
                       (list 'set-cdr! set-cdr!-prim)
                       (list 'sqrt sqrt-prim) (list 'odd? odd?-prim)
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
                       (list 'unparse unparse-prim)
                       (list 'unparse-procedure unparse-procedure-prim)
                       (list 'using using-primitive)
                       (list 'vector vector-prim)
                       (list 'vector-ref vector-ref-prim)
                       (list 'vector-set! vector-set!-prim)
                       (list 'void void-prim) (list 'zero? zero?-prim)
                       (list 'current-directory current-directory-prim)
                       (list 'cd current-directory-prim))))
      (make-initial-env-extended
        (make-initial-environment
          (map car primitives)
          (map cadr primitives))))))

(define make-external-proc
  (lambda (external-function-object)
    (make-proc '<proc-98> external-function-object)))

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
         (make-cont '<cont-48> pattern var k))))))

(define*
  unify-patterns^
  (lambda (p1 p2 ap1 ap2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
           (apply-cont k (make-sub 'unit p1 p2 ap2))
           (occurs? p1 p2 (make-cont '<cont-49> ap2 p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns^ p2 p1 ap2 ap1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2))
       (apply-cont k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs^ p1 p2 ap1 ap2 k))
      (else (apply-cont k #f)))))

(define*
  unify-pairs^
  (lambda (pair1 pair2 apair1 apair2 k)
    (unify-patterns^ (car pair1) (car pair2) (car^ apair1) (car^ apair2)
      (make-cont '<cont-51> apair1 apair2 pair1 pair2 k))))

(define*
  instantiate^
  (lambda (pattern s ap k2)
    (cond
      ((constant? pattern) (apply-cont2 k2 pattern ap))
      ((pattern-variable? pattern) (apply-sub^ s pattern ap k2))
      ((pair? pattern)
       (instantiate^
         (car pattern)
         s
         (car^ ap)
         (make-cont2 '<cont2-95> ap pattern s k2)))
      (else (error 'instantiate^ "bad pattern: ~a" pattern)))))

(define make-sub (lambda args (cons 'substitution args)))

(define*
  apply-sub^
  (lambda (s var avar k2)
    (record-case (cdr s)
      (empty () (apply-cont2 k2 var avar))
      (unit (new-var new-pattern new-apattern)
       (if (equal? var new-var)
           (apply-cont2 k2 new-pattern new-apattern)
           (apply-cont2 k2 var avar)))
      (composite (s1 s2)
       (apply-sub^ s1 var avar (make-cont2 '<cont2-96> s2 k2)))
      (else (error 'apply-sub^ "bad substitution: ~a" s)))))

(define chars-to-scan 'undefined)

(define scan-line 'undefined)

(define scan-char 'undefined)

(define scan-position 'undefined)

(define last-scan-line 'undefined)

(define last-scan-char 'undefined)

(define last-scan-position 'undefined)

(define token-start-line 'undefined)

(define token-start-char 'undefined)

(define token-start-position 'undefined)

(define atom-tag (box 'atom))

(define pair-tag (box 'pair))

(define-native
  map^
  (lambda (f^ asexp)
    (cond
      ((null?^ asexp) (list atom-tag '() 'none))
      (else
       (cons^ (f^ (car^ asexp)) (map^ f^ (cdr^ asexp)) 'none)))))

(define *reader-generates-annotated-sexps?* #t)

(define init-cont (make-cont '<cont-11>))

(define init-cont2 (make-cont2 '<cont2-2>))

(define init-cont3 (make-cont3 '<cont3-2>))

(define init-cont4 (make-cont4 '<cont4-8>))

(define init-handler (make-handler '<handler-1>))

(define init-handler2 (make-handler2 '<handler2-1>))

(define init-fail (make-fail '<fail-1>))

(define-native
  search-frame
  (lambda (frame var)
    (search-for-binding var (car frame) (cadr frame) 0)))

(define-native
  search-for-binding
  (lambda (var bindings variables i)
    (cond
      ((null? variables) #f)
      ((eq? (car variables) var) (vector-ref bindings i))
      (else
       (search-for-binding
         var
         bindings
         (cdr variables)
         (+ i 1))))))

(define *use-lexical-address* #t)

(define-native dlr-proc? (lambda (x) #f))

(define-native dlr-apply apply)

(define-native dlr-func (lambda (x) x))

(define-native dlr-env-contains (lambda (x) #f))

(define-native dlr-env-lookup (lambda (x) #f))

(define-native dlr-object? (lambda (x) #f))

(define-native dlr-lookup-components (lambda (x y) #f))

(define-native set-global-value! (lambda (var x) #f))

(define-native set-global-docstring! (lambda (var x) #f))

(define-native printf-prim printf)

(define-native using-prim (lambda ignore #f))

(define-native iterator? (lambda ignore #f))

(define-native get_type (lambda (x) 'unknown))

(define-native
  tagged-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
           (op (length^ asexp) len)
           (symbol?^ (car^ asexp))
           (eq?^ (car^ asexp) keyword)))))

(define quote?^ (tagged-list^ 'quote = 2))

(define quasiquote?^ (tagged-list^ 'quasiquote = 2))

(define unquote?^ (tagged-list^ 'unquote >= 2))

(define unquote-splicing?^
  (tagged-list^ 'unquote-splicing >= 2))

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

(define choose?^ (tagged-list^ 'choose >= 1))

(define try?^ (tagged-list^ 'try >= 2))

(define catch?^ (tagged-list^ 'catch >= 3))

(define finally?^ (tagged-list^ 'finally >= 2))

(define let-transformer^ (make-macro '<macro-1>))

(define letrec-transformer^ (make-macro '<macro-2>))

(define mit-define-transformer^ (make-macro '<macro-3>))

(define and-transformer^ (make-macro '<macro-4>))

(define or-transformer^ (make-macro '<macro-5>))

(define cond-transformer^ (make-macro '<macro-6>))

(define let*-transformer^ (make-macro '<macro-7>))

(define case-transformer^ (make-macro '<macro-8>))

(define record-case-transformer^ (make-macro '<macro-9>))

(define define-datatype-transformer^
  (make-macro '<macro-10>))

(define cases-transformer^ (make-macro '<macro-11>))

(define-native
  dd1
  "(define-datatype thing thing?\n     (thing0)\n     (thing1\n       (f1 thing1-field1?))\n     (thing2\n       (f1 thing2-field1?)\n       (f2 thing2-field2?))\n     (thing3\n       (f1 thing3-field1?)\n       (f2 (list-of thing3-field2?))\n       (f3 thing3-field3?)))")

(define-native
  cases1
  "(cases thing (cons x y)\n     (thing0 () b1)\n     (thing1 (f1) b1 b2 b3)\n     (thing2 (f1 f2 . f3) b1 b2 b3)\n     (thing3 args b1 b2 b3)\n     (else d1 d2 d3))")

(define-native
  dd2
  "(define-datatype expression expression?\n     (var-exp\n       (id symbol?))\n     (if-exp\n       (test-exp expression?)\n       (then-exp expression?)\n       (else-exp expression?))\n     (lambda-exp\n       (formals (list-of symbol?))\n       (bodies (list-of expression?)))\n     (app-exp\n       (operator expression?)\n       (operands (list-of expression?))))")

(define-native
  cases2
  "(cases expression exp\n     (var-exp (id info)\n       (lookup-value id env info handler fail k))\n     (if-exp (test-exp then-exp else-exp info)\n       (m test-exp env handler fail\n\t  (lambda (bool fail)\n\t    (if bool\n\t      (m then-exp env handler fail k)\n\t      (m else-exp env handler fail k)))))\n      (lambda-exp (formals bodies info)\n\t(k (closure formals bodies env) fail))\n      (app-exp (operator operands info)\n\t(m* operands env handler fail\n\t  (lambda (args fail)\n\t    (m operator env handler fail\n\t      (lambda (proc fail)\n\t\t(proc args env info handler fail k))))))\n      (else (error 'm \"bad abstract syntax: ~s\" exp)))")

(define macro-env (make-macro-env^))

(define-native dlr-proc? (lambda (x) #f))

(define-native dlr-apply apply)

(define-native dlr-func (lambda (x) x))

(define-native dlr-env-contains (lambda (x) #f))

(define-native dlr-env-lookup (lambda (x) #f))

(define-native dlr-object? (lambda (x) #f))

(define-native dlr-lookup-components (lambda (x y) #f))

(define-native set-global-value! (lambda (var x) #f))

(define-native set-global-docstring! (lambda (var x) #f))

(define-native printf-prim printf)

(define-native using-prim (lambda ignore #f))

(define-native iterator? (lambda ignore #f))

(define-native get_type (lambda (x) 'unknown))

(define REP-k (make-cont2 '<cont2-45>))

(define REP-handler (make-handler2 '<handler2-2>))

(define REP-fail (make-fail '<fail-1>))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define try-parse-handler (make-handler2 '<handler2-3>))

(define *tracing-on?* #f)

(define *stack-trace* '(()))

(define *use-stack-trace* #t)

(define-native initialize-execute (lambda () 'Ok))

(define-native
  make-safe-continuation
  (lambda (k)
    (cond
      ((not (pair? k)) '<???>)
      ((eq? (car k) 'fail-continuation) '<fail>)
      ((memq (car k) '(handler handler2)) '<handler>)
      ((memq
         (car k)
         '(continuation continuation2 continuation3 continuation4))
       (cons
         (cadr k)
         (map make-safe-continuation
              (filter continuation-object? (cddr k)))))
      (else '<???>))))

(define void-prim (make-proc '<proc-5>))

(define void-value '<void>)

(define zero?-prim (make-proc '<proc-6>))

(define exit-prim (make-proc '<proc-7>))

(define end-of-session '(exiting the interpreter))

(define eval-prim (make-proc '<proc-8>))

(define eval-ast-prim (make-proc '<proc-9>))

(define parse-prim (make-proc '<proc-10>))

(define string-length-prim (make-proc '<proc-11>))

(define string-ref-prim (make-proc '<proc-12>))

(define unparse-prim (make-proc '<proc-13>))

(define unparse-procedure-prim (make-proc '<proc-14>))

(define parse-string-prim (make-proc '<proc-15>))

(define read-string-prim (make-proc '<proc-16>))

(define apply-prim (make-proc '<proc-17>))

(define sqrt-prim (make-proc '<proc-18>))

(define odd?-prim (make-proc '<proc-19>))

(define even?-prim (make-proc '<proc-20>))

(define quotient-prim (make-proc '<proc-21>))

(define remainder-prim (make-proc '<proc-22>))

(define print-prim (make-proc '<proc-23>))

(define string-prim (make-proc '<proc-24>))

(define substring-prim (make-proc '<proc-25>))

(define number->string-prim (make-proc '<proc-26>))

(define assv-prim (make-proc '<proc-27>))

(define memv-prim (make-proc '<proc-28>))

(define display-prim (make-proc '<proc-29>))

(define newline-prim (make-proc '<proc-30>))

(define *need-newline* #f)

(define load-prim (make-proc '<proc-31>))

(define load-stack '())

(define length-prim (make-proc '<proc-32>))

(define symbol?-prim (make-proc '<proc-33>))

(define number?-prim (make-proc '<proc-34>))

(define boolean?-prim (make-proc '<proc-35>))

(define string?-prim (make-proc '<proc-36>))

(define char?-prim (make-proc '<proc-37>))

(define char=?-prim (make-proc '<proc-38>))

(define char-whitespace?-prim (make-proc '<proc-39>))

(define char-alphabetic?-prim (make-proc '<proc-40>))

(define char-numeric?-prim (make-proc '<proc-41>))

(define null?-prim (make-proc '<proc-42>))

(define pair?-prim (make-proc '<proc-43>))

(define cons-prim (make-proc '<proc-44>))

(define car-prim (make-proc '<proc-45>))

(define cdr-prim (make-proc '<proc-46>))

(define cadr-prim (make-proc '<proc-47>))

(define caddr-prim (make-proc '<proc-48>))

(define list-prim (make-proc '<proc-49>))

(define make-set-prim (make-proc '<proc-50>))

(define plus-prim (make-proc '<proc-51>))

(define minus-prim (make-proc '<proc-52>))

(define times-prim (make-proc '<proc-53>))

(define divide-prim (make-proc '<proc-54>))

(define modulo-prim (make-proc '<proc-55>))

(define lt-prim (make-proc '<proc-56>))

(define gt-prim (make-proc '<proc-57>))

(define lt-or-eq-prim (make-proc '<proc-58>))

(define gt-or-eq-prim (make-proc '<proc-59>))

(define equal-sign-prim (make-proc '<proc-60>))

(define abs-prim (make-proc '<proc-61>))

(define equal?-prim (make-proc '<proc-62>))

(define eq?-prim (make-proc '<proc-63>))

(define memq-prim (make-proc '<proc-64>))

(define member-prim (make-proc '<proc-65>))

(define range-prim (make-proc '<proc-66>))

(define-native
  range
  (lambda args
    (letrec ((range (lambda (n end step acc)
                      (if (>= n end)
                          (reverse acc)
                          (range (+ n step) end step (cons n acc))))))
      (cond
        ((null? (cdr args)) (range 0 (car args) 1 '()))
        ((null? (cddr args)) (range (car args) (cadr args) 1 '()))
        (else (range (car args) (cadr args) (caddr args) '()))))))

(define set-car!-prim (make-proc '<proc-67>))

(define set-cdr!-prim (make-proc '<proc-68>))

(define import-prim (make-proc '<proc-69>))

(define get-stack-trace-prim (make-proc '<proc-70>))

(define get-prim (make-proc '<proc-71>))

(define call/cc-prim (make-proc '<proc-73>))

(define abort-prim (make-proc '<proc-74>))

(define require-prim (make-proc '<proc-75>))

(define cut-prim (make-proc '<proc-76>))

(define reverse-prim (make-proc '<proc-77>))

(define append-prim (make-proc '<proc-78>))

(define string->number-prim (make-proc '<proc-79>))

(define string=?-prim (make-proc '<proc-80>))

(define list-to-vector-prim (make-proc '<proc-81>))

(define list->string-prim (make-proc '<proc-82>))

(define dir-prim (make-proc '<proc-83>))

(define current-time-prim (make-proc '<proc-84>))

(define map-prim (make-proc '<proc-85>))

(define for-each-prim (make-proc '<proc-86>))

(define current-environment-prim (make-proc '<proc-87>))

(define using-primitive (make-proc '<proc-88>))

(define not-prim (make-proc '<proc-89>))

(define printf-primitive (make-proc '<proc-90>))

(define vector-prim (make-proc '<proc-91>))

(define-native
  vector_native
  (lambda args (apply vector args)))

(define vector-set!-prim (make-proc '<proc-92>))

(define vector-ref-prim (make-proc '<proc-93>))

(define make-vector-prim (make-proc '<proc-94>))

(define error-prim (make-proc '<proc-95>))

(define list-ref-prim (make-proc '<proc-96>))

(define current-directory-prim (make-proc '<proc-97>))

(define toplevel-env (make-toplevel-env))


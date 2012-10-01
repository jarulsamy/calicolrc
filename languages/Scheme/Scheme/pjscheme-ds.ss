(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

(load "petite-init.ss")
(load "define-datatype.ss")

(define-datatype aexpression aexpression?
 (lit-aexp (datum anything?) (info source-info?))
 (var-aexp (id symbol?) (info source-info?))
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
 (try-catch-finally-aexp (body aexpression?)
   (catch-var symbol?) (catch-exps (list-of aexpression?))
   (finally-exps (list-of aexpression?)) (info source-info?))
 (raise-aexp (exp aexpression?) (info source-info?))
 (dict-aexp
   (entries (list-of (list-of aexpression?)))
   (info source-info?))
 (help-aexp
   (var symbol?)
   (var-info source-info?)
   (info source-info?))
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
      (<cont-2> (k) (apply-cont k (list->vector value)))
      (<cont-3> (v1 k) (apply-cont k (cons v1 value)))
      (<cont-4> (x k)
       (unannotate-cps (cdr x) (make-cont '<cont-3> value k)))
      (<cont-5> (k)
       (apply-cont k (retag (list->vector value) 'none)))
      (<cont-6> (k) (apply-cont k (retag value 'none)))
      (<cont-7> (x k)
       (reannotate-seq-cps (cdr x) (make-cont '<cont-3> value k)))
      (<cont-8> () (halt* value))
      (<cont-9> (bindings k)
       (apply-cont k `(let (,(car bindings)) ,value)))
      (<cont-10> (clauses var k)
       (let ((clause (car clauses)))
         (cond
           ((eq?^ (car^ clause) 'else)
            (apply-cont k (cons clause value)))
           ((symbol?^ (car^ clause))
            (apply-cont
              k
              (cons
                `((eq? ,var ',(get-sexp (car^ clause))) ,@(cdr^ clause))
                value)))
           (else
            (apply-cont
              k
              (cons
                `((memq ,var ',(get-sexp (car^ clause))) ,@(cdr^ clause))
                value))))))
      (<cont-11> (transformer-name)
       (error transformer-name "bad concrete syntax: ~a" value))
      (<cont-12> (bodies info fail k)
       (if (list? value)
           (apply-cont2 k (lambda-aexp value bodies info) fail)
           (apply-cont2
             k
             (mu-lambda-aexp (head value) (last value) bodies info)
             fail)))
      (<cont-13> (aclauses name info fail k)
       (apply-cont2
         k
         (define-syntax-aexp name value aclauses info)
         fail))
      (<cont-14> (info handler fail k)
       (aparse (replace-info value info) handler fail k))
      (<cont-15> (info handler fail k)
       (reannotate-cps
         value
         (make-cont '<cont-14> info handler fail k)))
      (<cont-16> (adatum handler fail k)
       (let ((info (get-source-info adatum)))
         (if (original-source-info? adatum)
             (aparse
               (replace-info value (snoc 'quasiquote info))
               handler
               fail
               k)
             (aparse (replace-info value info) handler fail k))))
      (<cont-17> (adatum handler fail k)
       (reannotate-cps
         value
         (make-cont '<cont-16> adatum handler fail k)))
      (<cont-18> (info fail k)
       (apply-cont2 k (lit-aexp value info) fail))
      (<cont-19> (msg info handler fail)
       (apply-handler2
         handler
         (format
           "parse error: ~a ~s ~a"
           msg
           value
           (where-at
             (get-start-line info)
             (get-start-char info)
             (get-srcfile info)))
         fail))
      (<cont-20> (adatum fail k)
       (if (has-source-info? value)
           (apply-cont2 k value fail)
           (let ((info (get-source-info adatum)))
             (if (original-source-info? adatum)
                 (let ((macro-keyword (get-sexp (car^ adatum))))
                   (apply-cont2
                     k
                     (replace-info value (snoc macro-keyword info))
                     fail))
                 (apply-cont2 k (replace-info value info) fail)))))
      (<cont-21> (adatum fail k)
       (reannotate-cps value (make-cont '<cont-20> adatum fail k)))
      (<cont-22> (aclauses
                  adatum
                  aright-pattern
                  clauses
                  right-pattern
                  handler
                  fail
                  k)
       (if value
           (instantiate^
             right-pattern
             value
             aright-pattern
             (make-cont2 '<cont2-39> fail k))
           (process-macro-clauses^ (cdr clauses) (cdr aclauses) adatum
             handler fail k)))
      (<cont-23> (aclauses
                  adatum
                  aleft-pattern
                  aright-pattern
                  clauses
                  left-pattern
                  right-pattern
                  handler
                  fail
                  k)
       (unify-patterns^ left-pattern value aleft-pattern adatum
         (make-cont '<cont-22> aclauses adatum aright-pattern clauses
           right-pattern handler fail k)))
      (<cont-24> (v1 k) (apply-cont k `(append ,v1 ,value)))
      (<cont-25> (ax depth k)
       (qq-expand-cps
         (^cdr^ ax)
         depth
         (make-cont '<cont-24> value k)))
      (<cont-26> (k) (apply-cont k `(list->vector ,value)))
      (<cont-27> (ax k) (apply-cont k `(cons ',(car^ ax) ,value)))
      (<cont-28> (k) (apply-cont k `(cons 'quasiquote ,value)))
      (<cont-29> (v1 k)
       (apply-cont k `(list (append ,v1 ,value))))
      (<cont-30> (ax depth k)
       (qq-expand-cps
         (^cdr^ ax)
         depth
         (make-cont '<cont-29> value k)))
      (<cont-31> (k) (apply-cont k `(list ,value)))
      (<cont-32> (ax k)
       (apply-cont k `(list (cons ',(car^ ax) ,value))))
      (<cont-33> (k)
       (apply-cont k `(list (cons 'quasiquote ,value))))
      (<cont-34> (handler fail k2)
       (aparse
         value
         handler
         fail
         (make-cont2 '<cont2-69> handler k2)))
      (<cont-35> (handler fail k2) (aparse value handler fail k2))
      (<cont-36> (fail k2) (apply-cont2 k2 value fail))
      (<cont-37> (x y k)
       (if value
           (equal-objects? (cdr x) (cdr y) k)
           (apply-cont k #f)))
      (<cont-38> (i v1 v2 k)
       (if value
           (equal-vectors? v1 v2 (- i 1) k)
           (apply-cont k #f)))
      (<cont-39> (ls x y info handler fail k)
       (if value
           (apply-cont2 k y fail)
           (member-loop x (cdr y) ls info handler fail k)))
      (<cont-40> (ls1 k) (apply-cont k (cons (car ls1) value)))
      (<cont-41> (lists k) (append2 (car lists) value k))
      (<cont-42> (pattern var k)
       (if value (apply-cont k #t) (occurs? var (cdr pattern) k)))
      (<cont-43> (ap2 p1 p2 k)
       (if value
           (apply-cont k #f)
           (apply-cont k (make-sub 'unit p1 p2 ap2))))
      (<cont-44> (s-car k)
       (if (not value)
           (apply-cont k #f)
           (apply-cont k (make-sub 'composite s-car value))))
      (<cont-45> (apair1 apair2 pair1 pair2 k)
       (if (not value)
           (apply-cont k #f)
           (instantiate^
             (cdr pair1)
             value
             (^cdr^ apair1)
             (make-cont2 '<cont2-87> apair2 pair2 value k))))
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
      (<cont2-4> (var-info variable env handler k)
       (if (dlr-env-contains variable)
           (apply-cont2 k (dlr-env-lookup variable) value2)
           (if value1
               (lookup-variable-components value1 "" env handler value2 k)
               (runtime-error
                 (format "unbound variable ~a" variable)
                 var-info
                 handler
                 value2))))
      (<cont2-5> (components path var handler k)
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
      (<cont2-6> (bodies k)
       (apply-cont k `(let ,value1 ,@value2 ,@bodies)))
      (<cont2-7> (procs vars k2)
       (apply-cont2
         k2
         (cons `(,(car vars) 'undefined) value1)
         (cons `(set! ,(car vars) ,(car procs)) value2)))
      (<cont2-8> (exp k)
       (apply-cont k `(let ((r ,exp) ,@value1) (cond ,@value2))))
      (<cont2-9> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq?^ (car^ clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr^ clause))) value1)
               (cons '(else (else-code)) value2))
             (if (symbol?^ (car^ clause))
                 (let ((name (get-sexp (car^ clause))))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr^ clause))) value1)
                     (cons `((eq? ,var ',(car^ clause)) (,name)) value2)))
                 (let ((name (get-sexp (car^ (car^ clause)))))
                   (apply-cont2
                     k2
                     (cons `(,name (lambda () ,@(cdr^ clause))) value1)
                     (cons
                       `((memq ,var ',(car^ clause)) (,name))
                       value2)))))))
      (<cont2-10> (clauses var k2)
       (let ((clause (car clauses)))
         (if (eq?^ (car^ clause) 'else)
             (apply-cont2
               k2
               (cons `(else-code (lambda () ,@(cdr^ clause))) value1)
               (cons `(else (else-code)) value2))
             (if (symbol?^ (car^ clause))
                 (let ((name (get-sexp (car^ clause))))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr^ clause) ,@(cddr^ clause)))
                       value1)
                     (cons
                       `((eq? (car ,var) ',(car^ clause))
                          (apply ,name (cdr ,var)))
                       value2)))
                 (let ((name (get-sexp (car^ (car^ clause)))))
                   (apply-cont2
                     k2
                     (cons
                       `(,name (lambda ,(cadr^ clause) ,@(cddr^ clause)))
                       value1)
                     (cons
                       `((memq (car ,var) ',(car^ clause))
                          (apply ,name (cdr ,var)))
                       value2)))))))
      (<cont2-11> (exp type-name type-tester-name k)
       (apply-cont
         k
         `(let ((r ,exp) ,@value1)
            (if (not (,type-tester-name r))
                (error 'cases "~a is not a valid ~a" r ',type-name)
                (cond ,@value2)))))
      (<cont2-12> (v1 info k)
       (apply-cont2 k (app-aexp v1 value1 info) value2))
      (<cont2-13> (adatum info handler k)
       (aparse-all
         (cdr^ adatum)
         handler
         value2
         (make-cont2 '<cont2-12> value1 info k)))
      (<cont2-14> (info k)
       (apply-cont2 k (choose-aexp value1 info) value2))
      (<cont2-15> (info k)
       (apply-cont2 k (dict-aexp value1 info) value2))
      (<cont2-16> (info k)
       (apply-cont2 k (raise-aexp value1 info) value2))
      (<cont2-17> (adatum cexps body info k)
       (let ((cvar (get-sexp (catch-var^ (caddr^ adatum)))))
         (apply-cont2
           k
           (try-catch-finally-aexp body cvar cexps value1 info)
           value2)))
      (<cont2-18> (adatum body info handler k)
       (aparse-all
         (finally-exps^ (cadddr^ adatum))
         handler
         value2
         (make-cont2 '<cont2-17> adatum value1 body info k)))
      (<cont2-19> (adatum info handler k)
       (aparse-all
         (catch-exps^ (caddr^ adatum))
         handler
         value2
         (make-cont2 '<cont2-18> adatum value1 info handler k)))
      (<cont2-20> (body info k)
       (apply-cont2 k (try-finally-aexp body value1 info) value2))
      (<cont2-21> (adatum info handler k)
       (aparse-all
         (finally-exps^ (caddr^ adatum))
         handler
         value2
         (make-cont2 '<cont2-20> value1 info k)))
      (<cont2-22> (adatum body info k)
       (let ((cvar (get-sexp (catch-var^ (caddr^ adatum)))))
         (apply-cont2
           k
           (try-catch-aexp body cvar value1 info)
           value2)))
      (<cont2-23> (adatum info handler k)
       (aparse-all
         (catch-exps^ (caddr^ adatum))
         handler
         value2
         (make-cont2 '<cont2-22> adatum value1 info k)))
      (<cont2-24> (adatum info k)
       (unannotate-cps
         (cadr^ adatum)
         (make-cont '<cont-12> value1 info value2 k)))
      (<cont2-25> (adatum info handler k)
       (cond
         ((null? value1)
          (aparse-error "bad concrete syntax:" adatum handler value2))
         ((null? (cdr value1)) (apply-cont2 k (car value1) value2))
         (else (apply-cont2 k (begin-aexp value1 info) value2))))
      (<cont2-26> (adatum info k)
       (apply-cont2
         k
         (define!-aexp
           (get-sexp (cadr^ adatum))
           (get-sexp (caddr^ adatum))
           value1
           info)
         value2))
      (<cont2-27> (adatum info k)
       (apply-cont2
         k
         (define!-aexp (get-sexp (cadr^ adatum)) "" value1 info)
         value2))
      (<cont2-28> (adatum info k)
       (apply-cont2
         k
         (define-aexp
           (get-sexp (cadr^ adatum))
           (get-sexp (caddr^ adatum))
           value1
           info)
         value2))
      (<cont2-29> (adatum info k)
       (apply-cont2
         k
         (define-aexp (get-sexp (cadr^ adatum)) "" value1 info)
         value2))
      (<cont2-30> (info k)
       (apply-cont2 k (func-aexp value1 info) value2))
      (<cont2-31> (adatum info k)
       (let ((var-info (get-source-info (cadr^ adatum))))
         (apply-cont2
           k
           (assign-aexp (get-sexp (cadr^ adatum)) value1 var-info info)
           value2)))
      (<cont2-32> (v1 v2 info k)
       (apply-cont2 k (if-aexp v1 v2 value1 info) value2))
      (<cont2-33> (adatum v1 info handler k)
       (aparse
         (cadddr^ adatum)
         handler
         value2
         (make-cont2 '<cont2-32> v1 value1 info k)))
      (<cont2-34> (adatum info handler k)
       (aparse
         (caddr^ adatum)
         handler
         value2
         (make-cont2 '<cont2-33> adatum value1 info handler k)))
      (<cont2-35> (v1 info k)
       (apply-cont2
         k
         (if-aexp v1 value1 (lit-aexp #f 'none) info)
         value2))
      (<cont2-36> (adatum info handler k)
       (aparse
         (caddr^ adatum)
         handler
         value2
         (make-cont2 '<cont2-35> value1 info k)))
      (<cont2-37> (handler k) (aparse value1 handler value2 k))
      (<cont2-38> (adatum handler k)
       (if (pattern-macro? value1)
           (process-macro-clauses^ (macro-clauses^ value1)
             (macro-aclauses^ value1) adatum handler value2 k)
           (apply-macro
             value1
             adatum
             (make-cont '<cont-21> adatum value2 k))))
      (<cont2-39> (fail k) (apply-cont2 k value2 fail))
      (<cont2-40> (a k) (apply-cont2 k (cons a value1) value2))
      (<cont2-41> (entries handler k)
       (aparse-entries
         (cdr entries)
         handler
         value2
         (make-cont2 '<cont2-40> value1 k)))
      (<cont2-42> (adatum-list handler k)
       (aparse-all
         (cdr adatum-list)
         handler
         value2
         (make-cont2 '<cont2-40> value1 k)))
      (<cont2-43> (exp k)
       (apply-cont2 k (cons exp value1) value2))
      (<cont2-44> (src tokens-left handler k)
       (aparse-sexps tokens-left src handler value2
         (make-cont2 '<cont2-43> value1 k)))
      (<cont2-45> () (set! *last-fail* value2) (halt* value1))
      (<cont2-46> ()
       (m value1 toplevel-env REP-handler value2 REP-k))
      (<cont2-47> () (halt* #t))
      (<cont2-48> ()
       (aparse-sexps value1 'stdin try-parse-handler value2
         (make-cont2 '<cont2-47>)))
      (<cont2-49> (src tokens-left env handler k)
       (if (token-type? (first tokens-left) 'end-marker)
           (apply-cont2 k value1 value2)
           (read-and-eval-asexps tokens-left src env handler value2
             k)))
      (<cont2-50> (src tokens-left env handler k)
       (m value1 env handler value2
          (make-cont2 '<cont2-49> src tokens-left env handler k)))
      (<cont2-51> (args env info handler k)
       (cond
         ((dlr-exp? value1)
          (apply-cont2 k (dlr-apply value1 args) value2))
         ((procedure-object? value1)
          (apply-proc value1 args env info handler value2 k))
         (else
          (runtime-error
            (format "attempt to apply non-procedure ~a" value1)
            info
            handler
            value2))))
      (<cont2-52> (operator env info handler k)
       (m operator env handler value2
          (make-cont2 '<cont2-51> value1 env info handler k)))
      (<cont2-53> (k)
       (apply-cont2 k (binding-docstring value1) value2))
      (<cont2-54> (handler)
       (apply-handler2 handler value1 value2))
      (<cont2-55> (v k) (apply-cont2 k v value2))
      (<cont2-56> (fexps env handler k)
       (eval-sequence fexps env handler value2
         (make-cont2 '<cont2-55> value1 k)))
      (<cont2-57> (aclauses clauses k)
       (set-binding-value!
         value1
         (make-pattern-macro^ clauses aclauses))
       (apply-cont2 k void-value value2))
      (<cont2-58> (docstring var k)
       (set-global-value! var value1)
       (set-global-docstring! var docstring)
       (apply-cont2 k void-value value2))
      (<cont2-59> (docstring rhs-value k)
       (set-binding-value! value1 rhs-value)
       (set-binding-docstring! value1 docstring)
       (apply-cont2 k void-value value2))
      (<cont2-60> (docstring var env handler k)
       (lookup-binding-in-first-frame var env handler value2
         (make-cont2 '<cont2-59> docstring value1 k)))
      (<cont2-61> (rhs-value k)
       (let ((old-value (binding-value value1)))
         (set-binding-value! value1 rhs-value)
         (let ((new-fail (make-fail
                           '<fail-2>
                           value1
                           old-value
                           value2)))
           (apply-cont2 k void-value new-fail))))
      (<cont2-62> (var var-info env handler k)
       (lookup-binding var env var-info handler value2
         (make-cont2 '<cont2-61> value1 k)))
      (<cont2-63> (else-exp then-exp env handler k)
       (if value1
           (m then-exp env handler value2 k)
           (m else-exp env handler value2 k)))
      (<cont2-64> (k) (apply-cont2 k (dlr-func value1) value2))
      (<cont2-65> (e handler) (apply-handler2 handler e value2))
      (<cont2-66> (v1 k) (apply-cont2 k (cons v1 value1) value2))
      (<cont2-67> (exps env handler k)
       (m* (cdr exps) env handler value2
           (make-cont2 '<cont2-66> value1 k)))
      (<cont2-68> (exps env handler k)
       (if (null? (cdr exps))
           (apply-cont2 k value1 value2)
           (eval-sequence (cdr exps) env handler value2 k)))
      (<cont2-69> (handler k2)
       (m value1 toplevel-env handler value2 k2))
      (<cont2-70> (handler k2)
       (read-asexp value1 'stdin handler value2
         (make-cont4 '<cont4-11> handler k2)))
      (<cont2-71> (handler k2)
       (read-asexp value1 'stdin handler value2
         (make-cont4 '<cont4-12> handler k2)))
      (<cont2-72> (k)
       (if (null? load-stack)
           (printf "WARNING: empty load-stack encountered!\n")
           (set! load-stack (cdr load-stack)))
       (apply-cont2 k void-value value2))
      (<cont2-73> (filename env handler k)
       (read-and-eval-asexps value1 filename env handler value2
         (make-cont2 '<cont2-72> k)))
      (<cont2-74> (filenames env info handler k)
       (load-files (cdr filenames) env info handler value2 k))
      (<cont2-75> (filename env2 handler k2)
       (let ((module (extend env2 '() '())))
         (set-binding-value! value1 module)
         (load-file filename module 'none handler value2 k2)))
      (<cont2-76> (args sym info handler k)
       (cond
         ((null? (cdr args)) (apply-cont2 k value1 value2))
         ((not (environment? value1))
          (runtime-error
            (format "invalid module ~a" sym)
            info
            handler
            value2))
         (else
          (get-primitive (cdr args) value1 info handler value2 k))))
      (<cont2-77> (iterator proc env handler k)
       (iterate-continue proc iterator env handler value2 k))
      (<cont2-78> (iterator proc env handler k)
       (iterate-collect-continue proc iterator env handler value2
         (make-cont2 '<cont2-66> value1 k)))
      (<cont2-79> (list1 proc env handler k)
       (map1 proc (cdr list1) env handler value2
         (make-cont2 '<cont2-66> value1 k)))
      (<cont2-80> (list1 proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (list (car list1))) value1)
         value2))
      (<cont2-81> (list1 list2 proc env handler k)
       (map2 proc (cdr list1) (cdr list2) env handler value2
         (make-cont2 '<cont2-66> value1 k)))
      (<cont2-82> (list1 list2 proc k)
       (apply-cont2
         k
         (cons
           (dlr-apply proc (list (car list1) (car list2)))
           value1)
         value2))
      (<cont2-83> (lists proc env handler k)
       (mapN proc (map cdr lists) env handler value2
         (make-cont2 '<cont2-66> value1 k)))
      (<cont2-84> (lists proc k)
       (apply-cont2
         k
         (cons (dlr-apply proc (map car lists)) value1)
         value2))
      (<cont2-85> (arg-list proc env handler k)
       (for-each-primitive proc (map cdr arg-list) env handler
         value2 k))
      (<cont2-86> (new-acdr1 new-cdr1 s-car k)
       (unify-patterns^ new-cdr1 value1 new-acdr1 value2
         (make-cont '<cont-44> s-car k)))
      (<cont2-87> (apair2 pair2 s-car k)
       (instantiate^
         (cdr pair2)
         s-car
         (^cdr^ apair2)
         (make-cont2 '<cont2-86> value2 value1 s-car k)))
      (<cont2-88> (a aa ap k2)
       (apply-cont2
         k2
         (cons a value1)
         (cons^ aa value2 (get-source-info ap))))
      (<cont2-89> (ap pattern s k2)
       (instantiate^
         (cdr pattern)
         s
         (^cdr^ ap)
         (make-cont2 '<cont2-88> value1 value2 ap k2)))
      (<cont2-90> (s2 k2) (instantiate^ value1 s2 value2 k2))
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
      (else (error 'apply-cont3 "bad continuation3: ~a" k)))))

;;----------------------------------------------------------------------
;; continuation4 datatype

(define make-cont4 (lambda args (cons 'continuation4 args)))

(define*
  apply-cont4
  (lambda (k value1 value2 value3 value4)
    (record-case (cdr k)
      (<cont4-1> (src start k)
       (apply-cont4 k
         (make-asexp src start value2 (list->vector value1)) value2
         value3 value4))
      (<cont4-2> (src start k)
       (apply-cont4 k (make-asexp src start value2 value1) value2
         value3 value4))
      (<cont4-3> (keyword keyword-end src start k)
       (apply-cont4 k
         (make-asexp
           src
           start
           value2
           (list (make-asexp src start keyword-end keyword) value1))
         value2 value3 value4))
      (<cont4-4> (asexp1 k)
       (apply-cont4 k (cons asexp1 value1) value2 value3 value4))
      (<cont4-5> (src handler k)
       (read-avector-sequence value3 src handler value4
         (make-cont4 '<cont4-4> value1 k)))
      (<cont4-6> (asexp1 expected-terminator src handler k)
       (if (or (null?^ value1) (pair?^ value1))
           (close-asexp-sequence (cons asexp1 (get-sexp value1)) value3
             expected-terminator src handler value4 k)
           (close-asexp-sequence (cons asexp1 value1) value3
             expected-terminator src handler value4 k)))
      (<cont4-7> (expected-terminator src handler k)
       (if (token-type? (first value3) 'dot)
           (read-asexp (rest-of value3) src handler value4
             (make-cont4 '<cont4-6> value1 expected-terminator src
               handler k))
           (read-asexp-sequence value3 expected-terminator src handler
             value4 (make-cont4 '<cont4-4> value1 k))))
      (<cont4-8> (src handler k)
       (aparse
         value1
         handler
         value4
         (make-cont2 '<cont2-44> src value3 handler k)))
      (<cont4-9> ()
       (set! *tokens-left* value3)
       (aparse value1 REP-handler value4 (make-cont2 '<cont2-46>)))
      (<cont4-10> (src env handler k)
       (aparse
         value1
         handler
         value4
         (make-cont2 '<cont2-50> src value3 env handler k)))
      (<cont4-11> (handler k2)
       (if (token-type? (first value3) 'end-marker)
           (aparse value1 handler value4 k2)
           (read-error "tokens left over" value3 'stdin handler
             value4)))
      (<cont4-12> (handler k2)
       (if (token-type? (first value3) 'end-marker)
           (apply-cont2 k2 value1 value4)
           (read-error "tokens left over" value3 'stdin handler
             value4)))
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
      (<fail-3> (exps env handler fail k)
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
         (make-cont2 '<cont2-65> exception handler)))
      (<handler2-6> (cexps cvar fexps env handler k)
       (let ((new-env (extend env (list cvar) (list exception))))
         (let ((catch-handler (try-finally-handler
                                fexps
                                env
                                handler)))
           (eval-sequence cexps new-env catch-handler fail
             (make-cont2 '<cont2-56> fexps env handler k)))))
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
      (<proc-3> () (apply-cont2 k2 void-value fail))
      (<proc-4> () (halt* end-of-session))
      (<proc-5> ()
       (reannotate-cps
         (car args)
         (make-cont '<cont-34> handler fail k2)))
      (<proc-6> ()
       (reannotate-cps
         (car args)
         (make-cont '<cont-35> handler fail k2)))
      (<proc-7> ()
       (scan-input (car args) 'stdin handler fail
         (make-cont2 '<cont2-70> handler k2)))
      (<proc-8> ()
       (scan-input (car args) 'stdin handler fail
         (make-cont2 '<cont2-71> handler k2)))
      (<proc-9> ()
       (let ((proc (car args)) (proc-args (cadr args)))
         (apply-proc proc proc-args env2 info handler fail k2)))
      (<proc-10> ()
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
      (<proc-11> ()
       (for-each safe-print args)
       (apply-cont2 k2 void-value fail))
      (<proc-12> ()
       (let ((s (format "~a" (car args))))
         (set! *need-newline* (true? (not (ends-with-newline? s))))
         (display s)
         (apply-cont2 k2 void-value fail)))
      (<proc-13> ()
       (set! *need-newline* #f)
       (newline)
       (apply-cont2 k2 void-value fail))
      (<proc-14> ()
       (if (not (length-one? args))
           (runtime-error
             "incorrect number of arguments to load"
             info
             handler
             fail)
           (load-file (car args) toplevel-env info handler fail k2)))
      (<proc-15> ()
       (if (not (length-one? args))
           (runtime-error
             "incorrect number of arguments to length"
             info
             handler
             fail)
           (length-loop (car args) 0 (car args) info handler fail k2)))
      (<proc-16> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to symbol?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply symbol? args) fail))))
      (<proc-17> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to number?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply number? args) fail))))
      (<proc-18> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to boolean?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply boolean? args) fail))))
      (<proc-19> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to string?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply string? args) fail))))
      (<proc-20> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to null?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply null? args) fail))))
      (<proc-21> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to pair?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply pair? args) fail))))
      (<proc-22> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to cons"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply cons args) fail))))
      (<proc-23> ()
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
      (<proc-24> ()
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
      (<proc-25> ()
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
      (<proc-26> ()
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
      (<proc-27> () (apply-cont2 k2 args fail))
      (<proc-28> ()
       (if (not (all-numeric? args))
           (runtime-error
             "+ called on non-numeric argument(s)"
             info
             handler
             fail)
           (apply-cont2 k2 (apply + args) fail)))
      (<proc-29> ()
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
      (<proc-30> ()
       (if (not (all-numeric? args))
           (runtime-error
             "* called on non-numeric argument(s)"
             info
             handler
             fail)
           (apply-cont2 k2 (apply * args) fail)))
      (<proc-31> ()
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
      (<proc-32> ()
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
      (<proc-33> ()
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
      (<proc-34> ()
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
      (<proc-35> ()
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
      (<proc-36> ()
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
      (<proc-37> ()
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
      (<proc-38> ()
       (if (not (length-two? args))
           (runtime-error
             "incorrect number of arguments to equal?"
             info
             handler
             fail)
           (equal-objects?
             (car args)
             (cadr args)
             (make-cont '<cont-36> fail k2))))
      (<proc-39> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to eq?"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply eq? args) fail))))
      (<proc-40> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to memq"
            info
            handler
            fail))
         (else (apply-cont2 k2 (apply memq args) fail))))
      (<proc-41> ()
       (if (not (length-two? args))
           (runtime-error
             "incorrect number of arguments to member"
             info
             handler
             fail)
           (member-loop (car args) (cadr args) (cadr args) info handler
             fail k2)))
      (<proc-42> ()
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
      (<proc-43> ()
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
      (<proc-44> ()
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
      (<proc-45> ()
       (let ((filename (car args)))
         (if (null? (cdr args))
             (load-file filename env2 'none handler fail k2)
             (let ((module-name (cadr args)))
               (lookup-binding-in-first-frame module-name env2 handler fail
                 (make-cont2 '<cont2-75> filename env2 handler k2))))))
      (<proc-46> ()
       (get-primitive args env2 info handler fail k2))
      (<proc-47> (k) (apply-cont2 k (car args) fail))
      (<proc-48> ()
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
                 (let ((fake-k (make-proc '<proc-47> k2)))
                   (if (dlr-exp? proc)
                       (apply-cont2 k2 (dlr-apply proc (list fake-k)) fail)
                       (apply-proc proc (list fake-k) env2 info handler
                         fail k2)))))))
      (<proc-49> ()
       (if (null? args)
           (apply-cont2 REP-k void-value fail)
           (apply-cont2 REP-k (car args) fail)))
      (<proc-50> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to require"
            info
            handler
            fail))
         ((true? (car args)) (apply-cont2 k2 'ok fail))
         (else (apply-fail fail))))
      (<proc-51> ()
       (if (not (null? args))
           (runtime-error
             "incorrect number of arguments to cut"
             info
             handler
             fail)
           (apply-cont2 k2 'ok REP-fail)))
      (<proc-52> ()
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
      (<proc-53> ()
       (cond
         ((not (length-two? args))
          (runtime-error
            "incorrect number of arguments to append"
            info
            handler
            fail))
         ((not (list? (car args)))
          (runtime-error
            (format
              "append called on incorrect list structure ~s"
              (car args))
            info
            handler
            fail))
         (else (append-all args (make-cont '<cont-36> fail k2)))))
      (<proc-54> ()
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
      (<proc-55> () (apply-cont2 k2 (dir args env2) fail))
      (<proc-56> () (apply-cont2 k2 (get-current-time) fail))
      (<proc-57> ()
       (map-primitive (car args) (cdr args) env2 handler fail k2))
      (<proc-58> ()
       (for-each-primitive (car args) (cdr args) env2 handler fail
         k2))
      (<proc-59> () (apply-cont2 k2 env2 fail))
      (<proc-60> () (apply-cont2 k2 (using-prim args env2) fail))
      (<proc-61> ()
       (cond
         ((not (length-one? args))
          (runtime-error
            "incorrect number of arguments to not"
            info
            handler
            fail))
         (else (apply-cont2 k2 (not (car args)) fail))))
      (<proc-62> ()
       (apply printf-prim args)
       (apply-cont2 k2 void-value fail))
      (<proc-63> () (apply-cont2 k2 (list->vector args) fail))
      (<proc-64> ()
       (apply-cont2
         k2
         (vector-set! (car args) (cadr args) (caddr args))
         fail))
      (<proc-65> () (apply-cont2 k2 (apply vector-ref args) fail))
      (<proc-66> ()
       (apply-cont2 k2 (apply make-vector args) fail))
      (<proc-67> ()
       (let* ((location (format "Error in ~a: " (car args)))
              (message (string-append
                         location
                         (apply format (cdr args)))))
         (runtime-error message info handler fail)))
      (<proc-68> (external-function-object)
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
       (if (symbol?^ (cadr^ datum))
           (let* ((name (cadr^ datum))
                  (bindings (caddr^ datum))
                  (vars (map^ car^ bindings))
                  (exps (map^ cadr^ bindings))
                  (bodies (cdddr^ datum)))
             (apply-cont
               k
               `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
           (let* ((bindings (cadr^ datum))
                  (vars (map^ car^ bindings))
                  (exps (map^ cadr^ bindings))
                  (bodies (cddr^ datum)))
             (apply-cont k `((lambda ,vars ,@bodies) ,@exps)))))
      (<macro-2> ()
       (let* ((decls (cadr^ datum))
              (vars (map^ car^ decls))
              (procs (map^ cadr^ decls))
              (bodies (cddr^ datum)))
         (create-letrec-assignments^
           vars
           procs
           (make-cont2 '<cont2-6> bodies k))))
      (<macro-3> ()
       (let ((name (car^ (cadr^ datum)))
             (formals (cdr^ (cadr^ datum)))
             (bodies (cddr^ datum)))
         (apply-cont k `(define ,name (lambda ,formals ,@bodies)))))
      (<macro-4> ()
       (let ((exps (cdr^ datum)))
         (cond
           ((null? exps) (apply-cont k '#t))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont k `(if ,(car exps) (and ,@(cdr exps)) #f))))))
      (<macro-5> ()
       (let ((exps (cdr^ datum)))
         (cond
           ((null? exps) (apply-cont k '#f))
           ((null? (cdr exps)) (apply-cont k (car exps)))
           (else
            (apply-cont
              k
              `(let ((bool ,(car exps))
                     (else-code (lambda () (or ,@(cdr exps)))))
                 (if bool bool (else-code))))))))
      (<macro-6> ()
       (let ((clauses (cdr^ datum)))
         (if (null? clauses)
             (amacro-error 'cond-transformer^ datum)
             (let ((first-clause (car clauses))
                   (other-clauses (cdr clauses)))
               (if (or (null?^ first-clause) (not (list?^ first-clause)))
                   (amacro-error 'cond-transformer^ datum)
                   (let ((test-exp (car^ first-clause))
                         (then-exps (cdr^ first-clause)))
                     (cond
                       ((eq?^ test-exp 'else)
                        (cond
                          ((null? then-exps)
                           (amacro-error 'cond-transformer^ '(else)))
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
      (<macro-7> ()
       (let ((bindings (get-sexp (cadr^ datum)))
             (bodies (cddr^ datum)))
         (nest-let*-bindings^ bindings bodies k)))
      (<macro-8> ()
       (let ((exp (cadr^ datum)) (clauses (cddr^ datum)))
         (case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-8> exp k))))
      (<macro-9> ()
       (let ((exp (cadr^ datum)) (clauses (cddr^ datum)))
         (record-case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-8> exp k))))
      (<macro-10> ()
       (let* ((datatype-name (get-sexp (cadr^ datum)))
              (type-tester-name (string->symbol
                                  (string-append
                                    (symbol->string datatype-name)
                                    "?"))))
         (if (not (eq?^ (cadr (cdr^ datum)) type-tester-name))
             (amacro-error 'define-datatype-transformer^ datum)
             (let* ((variants (cddr (cdr^ datum)))
                    (variant-names (define-datatype-variant-names
                                     variants))
                    (tester-def `(define ,type-tester-name
                                   (lambda (x)
                                     (and (pair? x)
                                          (not (not (memq
                                                      (car x)
                                                      ',variant-names))))))))
               (apply-cont
                 k
                 `(begin
                    ,tester-def
                    ,@(make-define-datatype-defines variant-names)))))))
      (<macro-11> ()
       (let* ((type-name (get-sexp (cadr^ datum)))
              (type-tester-name (string->symbol
                                  (string-append
                                    (symbol->string type-name)
                                    "?")))
              (exp (caddr^ datum))
              (clauses (cdddr^ datum)))
         (record-case-clauses->cond-clauses^
           'r
           clauses
           (make-cont2 '<cont2-11> exp type-name type-tester-name k))))
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
      (format "scan error: ~a ~a" msg (where-at line char src))
      fail)))

(define*
  unexpected-char-error
  (lambda (chars src handler fail)
    (let ((c (next-avail chars)))
      (if (char=? c #\nul)
          (scan-error "unexpected end of input" scan-line scan-char
            src handler fail)
          (scan-error (format "unexpected character ~a encountered" c)
            scan-line scan-char src handler fail)))))

(define*
  convert-buffer-to-token
  (lambda (token-type buffer src handler fail k)
    (let ((buffer (reverse buffer)))
      (case token-type
        (end-marker (apply-cont k (make-token 'end-marker)))
        (integer
         (apply-cont
           k
           (make-info-token 'integer (list->string buffer))))
        (decimal
         (apply-cont
           k
           (make-info-token 'decimal (list->string buffer))))
        (rational
         (apply-cont
           k
           (make-info-token 'rational (list->string buffer))))
        (identifier
         (apply-cont
           k
           (make-info-token
             'identifier
             (string->symbol (list->string buffer)))))
        (boolean
         (apply-cont
           k
           (make-info-token
             'boolean
             (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character
         (apply-cont k (make-info-token 'character (car buffer))))
        (named-character
         (let ((name (list->string buffer)))
           (cond
             ((string=? name "nul")
              (apply-cont k (make-info-token 'character #\nul)))
             ((string=? name "space")
              (apply-cont k (make-info-token 'character #\space)))
             ((string=? name "tab")
              (apply-cont k (make-info-token 'character #\tab)))
             ((string=? name "newline")
              (apply-cont k (make-info-token 'character #\newline)))
             ((string=? name "linefeed")
              (apply-cont k (make-info-token 'character #\newline)))
             ((string=? name "backspace")
              (apply-cont k (make-info-token 'character #\backspace)))
             ((string=? name "return")
              (apply-cont k (make-info-token 'character #\return)))
             ((string=? name "page")
              (apply-cont k (make-info-token 'character #\page)))
             (else
              (scan-error (format "invalid character name #\\~a" name)
                token-start-line token-start-char src handler fail)))))
        (string
         (apply-cont
           k
           (make-info-token 'string (list->string buffer))))
        (else (apply-cont k (make-token token-type)))))))

(define make-token
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

(define make-info-token
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

(define snoc
  (lambda (x lyst)
    (cond
      ((null? lyst) (list x))
      (else (cons (car lyst) (snoc x (cdr lyst)))))))

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
          (read-error (format "unexpected ~a encountered" (car token))
            tokens src handler fail)))))

(define*
  read-error
  (lambda (msg tokens src handler fail)
    (let ((token (first tokens)))
      (apply-handler2
        handler
        (format
          "read error: ~a ~a"
          msg
          (where-at
            (get-token-start-line token)
            (get-token-start-char token)
            src))
        fail))))

(define where-at
  (lambda (line char src)
    (if (eq? src 'stdin)
        (format "at line ~a, char ~a" line char)
        (format "at line ~a, char ~a of ~a" line char src))))

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

(define make-asexp
  (lambda (src start end sexp)
    (list asexp-tag sexp (cons src (append start end)))))

(define retag
  (lambda (sexp info) (list asexp-tag sexp info)))

(define asexp?
  (lambda (x) (and (pair? x) (eq? (car x) asexp-tag))))

(define get-sexp (lambda (asexp) (cadr asexp)))

(define get-source-info (lambda (asexp) (caddr asexp)))

(define get-srcfile (lambda (info) (car info)))

(define get-start-line (lambda (info) (cadr info)))

(define get-start-char (lambda (info) (caddr info)))

(define get-start-pos (lambda (info) (cadddr info)))

(define get-end-line (lambda (info) (car (cddddr info))))

(define get-end-char (lambda (info) (cadr (cddddr info))))

(define get-end-pos (lambda (info) (caddr (cddddr info))))

(define has-source-info?
  (lambda (asexp) (not (eq? (get-source-info asexp) 'none))))

(define original-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
         (= (length (get-source-info asexp)) 7))))

(define source-info?
  (lambda (x) (or (eq? x 'none) (list? x))))

(define replace-info
  (lambda (asexp info) (retag (get-sexp asexp) info)))

(define car^ (lambda (asexp) (car (get-sexp asexp))))

(define cdr^ (lambda (asexp) (cdr (get-sexp asexp))))

(define cddr^ (lambda (asexp) (cddr (get-sexp asexp))))

(define cdddr^ (lambda (asexp) (cdddr (get-sexp asexp))))

(define cadr^ (lambda (asexp) (cadr (get-sexp asexp))))

(define caddr^ (lambda (asexp) (caddr (get-sexp asexp))))

(define cadddr^ (lambda (asexp) (cadddr (get-sexp asexp))))

(define map^ (lambda (f asexp) (map f (get-sexp asexp))))

(define symbol?^
  (lambda (asexp) (symbol? (get-sexp asexp))))

(define string?^
  (lambda (asexp) (string? (get-sexp asexp))))

(define length^ (lambda (asexp) (length (get-sexp asexp))))

(define eq?^ (lambda (asexp x) (eq? (get-sexp asexp) x)))

(define vector?^
  (lambda (asexp) (vector? (get-sexp asexp))))

(define vector->list^
  (lambda (asexp) (vector->list (get-sexp asexp))))

(define cons^
  (lambda (a b info)
    (cond
      ((null?^ b) (retag (list a) info))
      ((pair?^ b) (retag (cons a (get-sexp b)) info))
      (else (retag (cons a b) info)))))

(define ^cdr^
  (lambda (asexp)
    (if (asexp? (cdr^ asexp))
        (cdr^ asexp)
        (retag (cdr^ asexp) 'none))))

(define null?^
  (lambda (x) (and (asexp? x) (null? (get-sexp x)))))

(define pair?^
  (lambda (x) (and (asexp? x) (pair? (get-sexp x)))))

(define list?^
  (lambda (x) (and (asexp? x) (list-of-asexp? (get-sexp x)))))

(define list-of-asexp?
  (lambda (x)
    (or (null? x)
        (and (pair? x)
             (asexp? (car x))
             (or (list-of-asexp? (cdr x)) (list?^ (cdr x)))))))

(define*
  unannotate-cps
  (lambda (x k)
    (cond
      ((asexp? x) (unannotate-cps (get-sexp x) k))
      ((pair? x)
       (unannotate-cps (car x) (make-cont '<cont-4> x k)))
      ((vector? x)
       (unannotate-cps (vector->list x) (make-cont '<cont-2> k)))
      (else (apply-cont k x)))))

(define*
  reannotate-cps
  (lambda (x k)
    (cond
      ((asexp? x) (apply-cont k x))
      ((pair? x) (reannotate-seq-cps x (make-cont '<cont-6> k)))
      ((vector? x)
       (reannotate-seq-cps
         (vector->list x)
         (make-cont '<cont-5> k)))
      (else (apply-cont k (retag x 'none))))))

(define*
  reannotate-seq-cps
  (lambda (x k)
    (cond
      ((null? x) (apply-cont k '()))
      ((asexp? x) (apply-cont k x))
      ((not (pair? x)) (reannotate-cps x k))
      ((or (null?^ (cdr x)) (pair?^ (cdr x)))
       (reannotate-seq-cps (cons (car x) (get-sexp (cdr x))) k))
      (else (reannotate-cps (car x) (make-cont '<cont-7> x k))))))

(define*
  read-asexp
  (lambda (tokens src handler fail k)
    (let ((start (get-token-start (first tokens)))
          (end (get-token-end (first tokens))))
      (record-case (first tokens)
        (integer (str)
         (apply-cont4 k
           (make-asexp src start end (string->integer str)) end
           (rest-of tokens) fail))
        (decimal (str)
         (apply-cont4 k
           (make-asexp src start end (string->decimal str)) end
           (rest-of tokens) fail))
        (rational (str)
         (let ((num (string->rational str)))
           (if (true? num)
               (apply-cont4 k (make-asexp src start end num) end
                 (rest-of tokens) fail)
               (read-error (format "cannot represent ~a" str) tokens src
                 handler fail))))
        (boolean (bool)
         (apply-cont4 k (make-asexp src start end bool) end
           (rest-of tokens) fail))
        (character (char)
         (apply-cont4 k (make-asexp src start end char) end
           (rest-of tokens) fail))
        (string (str)
         (apply-cont4 k (make-asexp src start end str) end
           (rest-of tokens) fail))
        (identifier (id)
         (apply-cont4 k (make-asexp src start end id) end
           (rest-of tokens) fail))
        (apostrophe ()
         (read-annotated-abbreviation tokens 'quote src handler fail
           k))
        (backquote ()
         (read-annotated-abbreviation tokens 'quasiquote src handler
           fail k))
        (comma ()
         (read-annotated-abbreviation tokens 'unquote src handler
           fail k))
        (comma-at ()
         (read-annotated-abbreviation tokens 'unquote-splicing src
           handler fail k))
        (lparen ()
         (let ((tokens (rest-of tokens)))
           (read-asexp-sequence tokens 'rparen src handler fail
             (make-cont4 '<cont4-2> src start k))))
        (lbracket ()
         (let ((tokens (rest-of tokens)))
           (read-asexp-sequence tokens 'rbracket src handler fail
             (make-cont4 '<cont4-2> src start k))))
        (lvector ()
         (read-avector-sequence (rest-of tokens) src handler fail
           (make-cont4 '<cont4-1> src start k)))
        (else (unexpected-token-error tokens src handler fail))))))

(define*
  read-annotated-abbreviation
  (lambda (tokens keyword src handler fail k)
    (let ((start (get-token-start (first tokens)))
          (keyword-end (get-token-end (first tokens))))
      (read-asexp (rest-of tokens) src handler fail
        (make-cont4 '<cont4-3> keyword keyword-end src start k)))))

(define*
  read-avector-sequence
  (lambda (tokens src handler fail k)
    (record-case (first tokens)
      (rparen ()
       (close-asexp-sequence '() tokens 'rparen src handler fail
         k))
      (dot ()
       (read-error "unexpected dot (.)" tokens src handler fail))
      (else (read-asexp
             tokens
             src
             handler
             fail
             (make-cont4 '<cont4-5> src handler k))))))

(define*
  read-asexp-sequence
  (lambda (tokens expected-terminator src handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-asexp-sequence '() tokens expected-terminator src
         handler fail k))
      (dot ()
       (read-error "unexpected dot (.)" tokens src handler fail))
      (else (read-asexp
             tokens
             src
             handler
             fail
             (make-cont4 '<cont4-7> expected-terminator src handler
               k))))))

(define*
  close-asexp-sequence
  (lambda (asexps tokens expected-terminator src handler fail
           k)
    (let ((end (get-token-end (first tokens))))
      (record-case (first tokens)
        ((rparen rbracket) ()
         (cond
           ((token-type? (first tokens) expected-terminator)
            (apply-cont4 k asexps end (rest-of tokens) fail))
           ((eq? expected-terminator 'rparen)
            (read-error "parenthesized list terminated by bracket"
              tokens src handler fail))
           ((eq? expected-terminator 'rbracket)
            (read-error "bracketed list terminated by parenthesis"
              tokens src handler fail))))
        (else (unexpected-token-error tokens src handler fail))))))

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
  (lambda (variable env var-info handler fail k)
    (lookup-binding variable env var-info handler fail
      (make-cont2 '<cont2-3> k))))

(define*
  lookup-binding
  (lambda (variable env var-info handler fail k)
    (let ((binding (search-env env variable)))
      (if binding
          (apply-cont2 k binding fail)
          (split-variable
            variable
            fail
            (make-cont2 '<cont2-4> var-info variable env handler k))))))

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
        (make-cont2 '<cont2-5> components path var handler k)))))

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

(define pattern-macro?
  (lambda (x) (and (pair? x) (eq? (car x) 'pattern-macro))))

(define*
  macro-error
  (lambda (transformer-name datum)
    (error transformer-name "bad concrete syntax: ~a" datum)))

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

(define literal?
  (lambda (datum)
    (or (number? datum)
        (boolean? datum)
        (char? datum)
        (string? datum))))

(define anything? (lambda (datum) #t))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define begin
      cond and or let let* letrec case record-case try catch
      finally raise dict help choose)))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x) (memq x (get-reserved-keywords)))))

(define*
  create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null? vars)
        (apply-cont2 k2 '() '())
        (create-letrec-assignments^
          (cdr vars)
          (cdr procs)
          (make-cont2 '<cont2-7> procs vars k2)))))

(define*
  nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null? bindings) (null? (cdr bindings)))
        (apply-cont k `(let ,bindings ,@bodies))
        (nest-let*-bindings^
          (cdr bindings)
          bodies
          (make-cont '<cont-9> bindings k)))))

(define*
  case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null? clauses)
        (apply-cont k '())
        (case-clauses->simple-cond-clauses^
          var
          (cdr clauses)
          (make-cont '<cont-10> clauses var k)))))

(define*
  case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (case-clauses->cond-clauses^
          var
          (cdr clauses)
          (make-cont2 '<cont2-9> clauses var k2)))))

(define*
  record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null? clauses)
        (apply-cont2 k2 '() '())
        (record-case-clauses->cond-clauses^
          var
          (cdr clauses)
          (make-cont2 '<cont2-10> clauses var k2)))))

(define define-datatype-variant-names
  (lambda (variants)
    (cond
      ((null? variants) '())
      (else
       (cons
         (get-sexp (car^ (car variants)))
         (define-datatype-variant-names (cdr variants)))))))

(define make-define-datatype-defines
  (lambda (names)
    (cond
      ((null? names) '())
      (else
       (cons
         `(define ,(car names)
            (lambda args (cons ',(car names) args)))
         (make-define-datatype-defines (cdr names)))))))

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case
        'define-datatype 'cases)
      (list and-transformer^ or-transformer^ cond-transformer^
        let-transformer^ letrec-transformer^ let*-transformer^
        case-transformer^ record-case-transformer^
        define-datatype-transformer^ cases-transformer^))))

(define*
  amacro-error
  (lambda (transformer-name adatum)
    (unannotate-cps
      adatum
      (make-cont '<cont-11> transformer-name))))

(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define macro-clauses^ (lambda (macro) (cadr macro)))

(define macro-aclauses^ (lambda (macro) (caddr macro)))

(define application?^
  (lambda (asexp)
    (and (list?^ asexp)
         (not (null?^ asexp))
         (not (reserved-keyword? (get-sexp (car^ asexp)))))))

(define mit-style?^
  (lambda (asexp) (not (symbol?^ (cadr^ asexp)))))

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

(define try-body^ (lambda (x) (cadr^ x)))

(define catch-var^ (lambda (x) (cadr^ x)))

(define catch-exps^ (lambda (x) (cddr^ x)))

(define finally-exps^ (lambda (x) (cdr (get-sexp x))))

(define*
  aparse
  (lambda (adatum handler fail k)
    (let ((info (get-source-info adatum)))
      (cond
        ((null?^ adatum)
         (apply-cont2 k (lit-aexp (get-sexp adatum) info) fail))
        ((literal?^ adatum)
         (apply-cont2 k (lit-aexp (get-sexp adatum) info) fail))
        ((vector?^ adatum)
         (unannotate-cps adatum (make-cont '<cont-18> info fail k)))
        ((symbol?^ adatum)
         (apply-cont2 k (var-aexp (get-sexp adatum) info) fail))
        ((quote?^ adatum)
         (unannotate-cps
           (cadr^ adatum)
           (make-cont '<cont-18> info fail k)))
        ((quasiquote?^ adatum)
         (qq-expand-cps
           (cadr^ adatum)
           0
           (make-cont '<cont-17> adatum handler fail k)))
        ((unquote?^ adatum)
         (aparse-error "misplaced" adatum handler fail))
        ((unquote-splicing?^ adatum)
         (aparse-error "misplaced" adatum handler fail))
        ((syntactic-sugar?^ adatum)
         (expand-once^
           adatum
           handler
           fail
           (make-cont2 '<cont2-37> handler k)))
        ((if-then?^ adatum)
         (aparse
           (cadr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-36> adatum info handler k)))
        ((if-else?^ adatum)
         (aparse
           (cadr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-34> adatum info handler k)))
        ((assignment?^ adatum)
         (aparse
           (caddr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-31> adatum info k)))
        ((func?^ adatum)
         (aparse
           (cadr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-30> info k)))
        ((define?^ adatum)
         (cond
           ((mit-style?^ adatum)
            (apply-macro
              mit-define-transformer^
              adatum
              (make-cont '<cont-15> info handler fail k)))
           ((= (length^ adatum) 3)
            (aparse
              (caddr^ adatum)
              handler
              fail
              (make-cont2 '<cont2-29> adatum info k)))
           ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum)))
            (aparse
              (cadddr^ adatum)
              handler
              fail
              (make-cont2 '<cont2-28> adatum info k)))
           (else
            (aparse-error "bad concrete syntax:" adatum handler fail))))
        ((define!?^ adatum)
         (cond
           ((mit-style?^ adatum)
            (apply-macro
              mit-define-transformer^
              adatum
              (make-cont '<cont-15> info handler fail k)))
           ((= (length^ adatum) 3)
            (aparse
              (caddr^ adatum)
              handler
              fail
              (make-cont2 '<cont2-27> adatum info k)))
           ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum)))
            (aparse
              (cadddr^ adatum)
              handler
              fail
              (make-cont2 '<cont2-26> adatum info k)))
           (else
            (aparse-error "bad concrete syntax:" adatum handler fail))))
        ((define-syntax?^ adatum)
         (let ((name (get-sexp (cadr^ adatum)))
               (aclauses (cddr^ adatum)))
           (unannotate-cps
             aclauses
             (make-cont '<cont-13> aclauses name info fail k))))
        ((begin?^ adatum)
         (aparse-all
           (cdr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-25> adatum info handler k)))
        ((lambda?^ adatum)
         (aparse-all
           (cddr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-24> adatum info k)))
        ((try?^ adatum)
         (cond
           ((= (length^ adatum) 2)
            (aparse (try-body^ adatum) handler fail k))
           ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
            (aparse
              (try-body^ adatum)
              handler
              fail
              (make-cont2 '<cont2-23> adatum info handler k)))
           ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
            (aparse
              (try-body^ adatum)
              handler
              fail
              (make-cont2 '<cont2-21> adatum info handler k)))
           ((and (= (length^ adatum) 4)
                 (catch?^ (caddr^ adatum))
                 (finally?^ (cadddr^ adatum)))
            (aparse
              (try-body^ adatum)
              handler
              fail
              (make-cont2 '<cont2-19> adatum info handler k)))
           (else
            (aparse-error "bad try syntax:" adatum handler fail))))
        ((raise?^ adatum)
         (aparse
           (cadr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-16> info k)))
        ((dict?^ adatum)
         (aparse-entries
           (cdr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-15> info k)))
        ((help?^ adatum)
         (if (symbol?^ (cadr^ adatum))
             (let ((var (get-sexp (cadr^ adatum)))
                   (var-info (get-source-info (cadr^ adatum))))
               (apply-cont2 k (help-aexp var var-info info) fail))
             (aparse-error "bad concrete syntax:" adatum handler fail)))
        ((choose?^ adatum)
         (aparse-all
           (cdr^ adatum)
           handler
           fail
           (make-cont2 '<cont2-14> info k)))
        ((application?^ adatum)
         (aparse
           (car^ adatum)
           handler
           fail
           (make-cont2 '<cont2-13> adatum info handler k)))
        (else
         (aparse-error
           "bad concrete syntax:"
           adatum
           handler
           fail))))))

(define*
  aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps
        adatum
        (make-cont '<cont-19> msg info handler fail)))))

(define*
  expand-once^
  (lambda (adatum handler fail k)
    (lookup-value (get-sexp (car^ adatum)) macro-env 'none
      handler fail (make-cont2 '<cont2-38> adatum handler k))))

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
              (aleft-pattern (car^ (car aclauses)))
              (aright-pattern (cadr^ (car aclauses))))
          (unannotate-cps
            adatum
            (make-cont '<cont-23> aclauses adatum aleft-pattern
              aright-pattern clauses left-pattern right-pattern handler
              fail k))))))

(define*
  aparse-entries
  (lambda (entries handler fail k)
    (if (null? entries)
        (apply-cont2 k '() fail)
        (aparse-all
          (get-sexp (car entries))
          handler
          fail
          (make-cont2 '<cont2-41> entries handler k)))))

(define*
  aparse-all
  (lambda (adatum-list handler fail k)
    (if (null? adatum-list)
        (apply-cont2 k '() fail)
        (aparse
          (car adatum-list)
          handler
          fail
          (make-cont2 '<cont2-42> adatum-list handler k)))))

(define*
  aparse-sexps
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k '() fail)
        (read-asexp tokens src handler fail
          (make-cont4 '<cont4-8> src handler k)))))

(define*
  qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps
         (^cdr^ ax)
         (+ depth 1)
         (make-cont '<cont-28> k)))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
         ((> depth 0)
          (qq-expand-cps
            (^cdr^ ax)
            (- depth 1)
            (make-cont '<cont-27> ax k)))
         ((and (unquote?^ ax)
               (not (null? (cdr^ ax)))
               (null? (cddr^ ax)))
          (apply-cont k (cadr^ ax)))
         (else (apply-cont k `',ax))))
      ((vector?^ ax)
       (qq-expand-cps
         (retag (vector->list^ ax) 'none)
         depth
         (make-cont '<cont-26> k)))
      ((not (pair?^ ax)) (apply-cont k `',ax))
      ((null? (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-25> ax depth k))))))

(define*
  qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps
         (^cdr^ ax)
         (+ depth 1)
         (make-cont '<cont-33> k)))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
         ((> depth 0)
          (qq-expand-cps
            (^cdr^ ax)
            (- depth 1)
            (make-cont '<cont-32> ax k)))
         ((unquote?^ ax) (apply-cont k `(list . ,(^cdr^ ax))))
         ((null? (cddr^ ax)) (apply-cont k (cadr^ ax)))
         (else (apply-cont k `(append . ,(^cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth (make-cont '<cont-31> k)))
      ((not (pair?^ ax)) (apply-cont k `'(,ax)))
      ((null? (cdr^ ax))
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-31> k)))
      (else
       (qq-expand-list-cps
         (car^ ax)
         depth
         (make-cont '<cont-30> ax depth k))))))

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
           `(define ,id ,(aunparse rhs-exp))
           `(define ,id ,docstring ,(aunparse rhs-exp))))
     (define!-aexp
       (id docstring rhs-exp info)
       (if (string=? docstring "")
           `(define! ,id ,(aunparse rhs-exp))
           `(define! ,id ,docstring ,(aunparse rhs-exp))))
     (define-syntax-aexp
       (name clauses aclauses info)
       `(define-syntax ,name ,@clauses))
     (begin-aexp (exps info) `(begin ,@(map aunparse exps)))
     (lambda-aexp
       (formals bodies info)
       `(lambda ,formals ,@(map aunparse bodies)))
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
     (dict-aexp
       (entries info)
       `(dict
          ,@(map (lambda (b)
                   `(,(aunparse (car b)) ,(aunparse (cadr b))))
                 entries)))
     (help-aexp (var var-info info) `(help ,var))
     (choose-aexp (exps info) `(choose ,@(map aunparse exps)))
     (else (error 'aunparse "bad abstract syntax: ~s" aexp)))))

(define dlr-exp? (lambda (x) #f))

(define dlr-func (lambda (x) x))

(define dlr-env-contains (lambda (x) #f))

(define dlr-env-lookup (lambda (x) #f))

(define dlr-object? (lambda (x) #f))

(define dlr-lookup-components (lambda (x y) #f))

(define set-global-value! (lambda (var x) #f))

(define set-global-docstring! (lambda (var x) #f))

(define using-prim (lambda ignore #f))

(define iterator? (lambda ignore #f))

(define get_type (lambda (x) 'unknown))

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
    (read-asexp *tokens-left* src REP-handler *last-fail*
      (make-cont4 '<cont4-9>))))

(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
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

(define*
  read-and-eval-asexps
  (lambda (tokens src env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
        (apply-cont2 k void-value fail)
        (read-asexp tokens src handler fail
          (make-cont4 '<cont4-10> src env handler k)))))

(define*
  m
  (lambda (exp env handler fail k)
    (cases aexpression exp
     (lit-aexp (datum info) (apply-cont2 k datum fail))
     (var-aexp
       (id info)
       (lookup-value id env info handler fail k))
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
          (make-cont2 '<cont2-60> docstring var env handler k)))
     (define!-aexp
       (var docstring rhs-exp info)
       (m rhs-exp env handler fail
          (make-cont2 '<cont2-58> docstring var k)))
     (define-syntax-aexp
       (name clauses aclauses info)
       (lookup-binding-in-first-frame name macro-env handler fail
         (make-cont2 '<cont2-57> aclauses clauses k)))
     (begin-aexp
       (exps info)
       (eval-sequence exps env handler fail k))
     (lambda-aexp
       (formals bodies info)
       (apply-cont2 k (closure formals bodies env) fail))
     (mu-lambda-aexp
       (formals runt bodies info)
       (apply-cont2 k (mu-closure formals runt bodies env) fail))
     (try-catch-aexp
       (body cvar cexps info)
       (let ((new-handler (try-catch-handler cvar cexps env handler
                            k)))
         (m body env new-handler fail k)))
     (try-finally-aexp
       (body fexps info)
       (let ((new-handler (try-finally-handler fexps env handler)))
         (m body env new-handler fail
            (make-cont2 '<cont2-56> fexps env handler k))))
     (try-catch-finally-aexp
       (body cvar cexps fexps info)
       (let ((new-handler (try-catch-finally-handler cvar cexps
                            fexps env handler k)))
         (m body env new-handler fail
            (make-cont2 '<cont2-56> fexps env handler k))))
     (raise-aexp
       (exp info)
       (m exp env handler fail (make-cont2 '<cont2-54> handler)))
     (dict-aexp
       (pairs info)
       (apply-cont2 k (list 'dict pairs) fail))
     (help-aexp
       (var var-info info)
       (if (reserved-keyword? var)
           (apply-cont2 k (format "~a is a keyword" var) fail)
           (lookup-binding var env var-info handler fail
             (make-cont2 '<cont2-53> k))))
     (choose-aexp
       (exps info)
       (eval-choices exps env handler fail k))
     (app-aexp
       (operator operands info)
       (m* operands env handler fail
           (make-cont2 '<cont2-52> operator env info handler k)))
     (else (error 'm "bad abstract syntax: ~s" exp)))))

(define*
  runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
        (apply-handler2
          handler
          (format "runtime error: ~a" msg)
          fail)
        (let ((src (get-srcfile info))
              (line (get-start-line info))
              (char (get-start-char info)))
          (apply-handler2
            handler
            (format "runtime error: ~a ~a" msg (where-at line char src))
            fail)))))

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
        (let ((new-fail (make-fail '<fail-3> exps env handler fail
                          k)))
          (m (car exps) env handler new-fail k)))))

(define closure
  (lambda (formals bodies env)
    (make-proc '<proc-1> bodies formals env)))

(define mu-closure
  (lambda (formals runt bodies env)
    (make-proc '<proc-2> bodies formals runt env)))

(define*
  m*
  (lambda (exps env handler fail k)
    (if (null? exps)
        (apply-cont2 k '() fail)
        (m (car exps) env handler fail
           (make-cont2 '<cont2-67> exps env handler k)))))

(define*
  eval-sequence
  (lambda (exps env handler fail k)
    (m (car exps) env handler fail
       (make-cont2 '<cont2-68> exps env handler k))))

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
  (lambda (filename env info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (apply-cont2 k void-value fail))
      ((not (string? filename))
       (runtime-error
         (format "filename ~a is not a string" filename)
         info
         handler
         fail))
      ((not (file-exists? filename))
       (runtime-error
         (format "attempted to load nonexistent file ~a" filename)
         info
         handler
         fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
         (make-cont2 '<cont2-73> filename env handler k))))))

(define*
  load-files
  (lambda (filenames env info handler fail k)
    (if (null? filenames)
        (apply-cont2 k void-value fail)
        (load-file (car filenames) env info handler fail
          (make-cont2 '<cont2-74> filenames env info handler k)))))

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
         (make-cont '<cont-37> x y k)))
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
          (make-cont '<cont-38> i v1 v2 k)))))

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
         (make-cont '<cont-39> ls x y info handler fail k))))))

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

(define*
  get-primitive
  (lambda (args env info handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env 'none handler fail
        (make-cont2 '<cont2-76> args sym info handler k)))))

(define*
  append2
  (lambda (ls1 ls2 k)
    (if (null? ls1)
        (apply-cont k ls2)
        (append2 (cdr ls1) ls2 (make-cont '<cont-40> ls1 k)))))

(define*
  append-all
  (lambda (lists k)
    (cond
      ((null? lists) (apply-cont k '()))
      ((null? (cdr lists)) (apply-cont k (car lists)))
      (else
       (append-all (cdr lists) (make-cont '<cont-41> lists k))))))

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
            (make-cont2 '<cont2-77> iterator proc env handler k))))))

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
            (make-cont2 '<cont2-78> iterator proc env handler k))))))

(define*
  map1
  (lambda (proc list1 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (map1 proc (cdr list1) env handler fail
              (make-cont2 '<cont2-80> list1 proc k))
            (apply-proc proc (list (car list1)) env 'none handler fail
              (make-cont2 '<cont2-79> list1 proc env handler k))))))

(define*
  map2
  (lambda (proc list1 list2 env handler fail k)
    (if (null? list1)
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (map2 proc (cdr list1) (cdr list2) env handler fail
              (make-cont2 '<cont2-82> list1 list2 proc k))
            (apply-proc proc (list (car list1) (car list2)) env 'none
              handler fail
              (make-cont2 '<cont2-81> list1 list2 proc env handler k))))))

(define*
  mapN
  (lambda (proc lists env handler fail k)
    (if (null? (car lists))
        (apply-cont2 k '() fail)
        (if (dlr-exp? proc)
            (mapN proc (map cdr lists) env handler fail
              (make-cont2 '<cont2-84> lists proc k))
            (apply-proc proc (map car lists) env 'none handler fail
              (make-cont2 '<cont2-83> lists proc env handler k))))))

(define*
  for-each-primitive
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
        (iterate proc (car lists) env handler fail k)
        (let ((arg-list (listify lists)))
          (if (null? (car arg-list))
              (apply-cont2 k void-value fail)
              (if (dlr-exp? proc)
                  (begin
                    (dlr-apply proc (map car arg-list))
                    (for-each-primitive proc (map cdr arg-list) env handler
                      fail k))
                  (apply-proc proc (map car arg-list) env 'none handler
                    fail
                    (make-cont2 '<cont2-85> arg-list proc env handler
                      k))))))))

(define make-initial-env-extended (lambda (env) env))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
      (make-initial-environment
        (list 'void 'exit 'eval 'parse 'parse-string 'read-string
         'apply 'sqrt 'print 'display 'newline 'load 'length 'symbol?
         'number? 'boolean? 'string? 'null? 'pair? 'cons 'car 'cdr
         'cadr 'caddr 'list '+ '- '* '/ '< '> '= '=? 'abs 'equal?
         'eq? 'memq 'member 'range 'set-car! 'set-cdr! 'import 'get
         'call-with-current-continuation 'call/cc 'abort 'require
         'cut 'reverse 'append 'list->vector 'dir 'current-time 'map
         'for-each 'env 'using 'not 'printf 'vector 'vector-set!
         'vector-ref 'make-vector '<= '>= 'error)
        (list void-prim exit-prim eval-prim parse-prim
         parse-string-prim read-string-prim apply-prim sqrt-prim
         print-prim display-prim newline-prim load-prim length-prim
         symbol?-prim number?-prim boolean?-prim string?-prim
         null?-prim pair?-prim cons-prim car-prim cdr-prim cadr-prim
         caddr-prim list-prim plus-prim minus-prim times-prim
         divide-prim lt-prim gt-prim equal-sign-prim equal-sign-prim
         abs-prim equal?-prim eq?-prim memq-prim member-prim
         range-prim set-car!-prim set-cdr!-prim import-prim get-prim
         call/cc-prim call/cc-prim abort-prim require-prim cut-prim
         reverse-prim append-prim list-to-vector-prim dir-prim
         current-time-prim map-prim for-each-prim env-prim
         using-primitive not-prim printf-primitive vector-prim
         vector-set!-prim vector-ref-prim make-vector-prim
         lt-or-eq-prim gt-or-eq-prim error-prim)))))

(define make-external-proc
  (lambda (external-function-object)
    (make-proc '<proc-68> external-function-object)))

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
         (make-cont '<cont-42> pattern var k))))))

(define*
  unify-patterns^
  (lambda (p1 p2 ap1 ap2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
           (apply-cont k (make-sub 'unit p1 p2 ap2))
           (occurs? p1 p2 (make-cont '<cont-43> ap2 p1 p2 k))))
      ((pattern-variable? p2) (unify-patterns^ p2 p1 ap2 ap1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2))
       (apply-cont k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs^ p1 p2 ap1 ap2 k))
      (else (apply-cont k #f)))))

(define*
  unify-pairs^
  (lambda (pair1 pair2 apair1 apair2 k)
    (unify-patterns^ (car pair1) (car pair2) (car^ apair1)
      (car^ apair2)
      (make-cont '<cont-45> apair1 apair2 pair1 pair2 k))))

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
         (make-cont2 '<cont2-89> ap pattern s k2)))
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
       (apply-sub^ s1 var avar (make-cont2 '<cont2-90> s2 k2)))
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

(define asexp-tag (box 'tag))

(define init-cont (make-cont '<cont-8>))

(define init-cont2 (make-cont2 '<cont2-2>))

(define init-cont3 (make-cont3 '<cont3-2>))

(define init-handler (make-handler '<handler-1>))

(define init-handler2 (make-handler2 '<handler2-1>))

(define init-fail (make-fail '<fail-1>))

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

(define macro-env (make-macro-env^))

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

(define raise?^ (tagged-list^ 'raise = 2))

(define dict?^ (tagged-list^ 'dict >= 1))

(define help?^ (tagged-list^ 'help = 2))

(define choose?^ (tagged-list^ 'choose >= 1))

(define try?^ (tagged-list^ 'try >= 2))

(define catch?^ (tagged-list^ 'catch >= 3))

(define finally?^ (tagged-list^ 'finally >= 2))

(define dlr-apply apply)

(define printf-prim printf)

(define REP-k (make-cont2 '<cont2-45>))

(define REP-handler (make-handler2 '<handler2-2>))

(define REP-fail (make-fail '<fail-1>))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define try-parse-handler (make-handler2 '<handler2-3>))

(define void-prim (make-proc '<proc-3>))

(define void-value '<void>)

(define exit-prim (make-proc '<proc-4>))

(define end-of-session '(exiting the interpreter))

(define eval-prim (make-proc '<proc-5>))

(define parse-prim (make-proc '<proc-6>))

(define parse-string-prim (make-proc '<proc-7>))

(define read-string-prim (make-proc '<proc-8>))

(define apply-prim (make-proc '<proc-9>))

(define sqrt-prim (make-proc '<proc-10>))

(define print-prim (make-proc '<proc-11>))

(define display-prim (make-proc '<proc-12>))

(define newline-prim (make-proc '<proc-13>))

(define *need-newline* #f)

(define load-prim (make-proc '<proc-14>))

(define load-stack '())

(define length-prim (make-proc '<proc-15>))

(define symbol?-prim (make-proc '<proc-16>))

(define number?-prim (make-proc '<proc-17>))

(define boolean?-prim (make-proc '<proc-18>))

(define string?-prim (make-proc '<proc-19>))

(define null?-prim (make-proc '<proc-20>))

(define pair?-prim (make-proc '<proc-21>))

(define cons-prim (make-proc '<proc-22>))

(define car-prim (make-proc '<proc-23>))

(define cdr-prim (make-proc '<proc-24>))

(define cadr-prim (make-proc '<proc-25>))

(define caddr-prim (make-proc '<proc-26>))

(define list-prim (make-proc '<proc-27>))

(define plus-prim (make-proc '<proc-28>))

(define minus-prim (make-proc '<proc-29>))

(define times-prim (make-proc '<proc-30>))

(define divide-prim (make-proc '<proc-31>))

(define lt-prim (make-proc '<proc-32>))

(define gt-prim (make-proc '<proc-33>))

(define lt-or-eq-prim (make-proc '<proc-34>))

(define gt-or-eq-prim (make-proc '<proc-35>))

(define equal-sign-prim (make-proc '<proc-36>))

(define abs-prim (make-proc '<proc-37>))

(define equal?-prim (make-proc '<proc-38>))

(define eq?-prim (make-proc '<proc-39>))

(define memq-prim (make-proc '<proc-40>))

(define member-prim (make-proc '<proc-41>))

(define range-prim (make-proc '<proc-42>))

(define set-car!-prim (make-proc '<proc-43>))

(define set-cdr!-prim (make-proc '<proc-44>))

(define import-prim (make-proc '<proc-45>))

(define get-prim (make-proc '<proc-46>))

(define call/cc-prim (make-proc '<proc-48>))

(define abort-prim (make-proc '<proc-49>))

(define require-prim (make-proc '<proc-50>))

(define cut-prim (make-proc '<proc-51>))

(define reverse-prim (make-proc '<proc-52>))

(define append-prim (make-proc '<proc-53>))

(define list-to-vector-prim (make-proc '<proc-54>))

(define dir-prim (make-proc '<proc-55>))

(define current-time-prim (make-proc '<proc-56>))

(define map-prim (make-proc '<proc-57>))

(define for-each-prim (make-proc '<proc-58>))

(define env-prim (make-proc '<proc-59>))

(define using-primitive (make-proc '<proc-60>))

(define not-prim (make-proc '<proc-61>))

(define printf-primitive (make-proc '<proc-62>))

(define vector-prim (make-proc '<proc-63>))

(define vector-set!-prim (make-proc '<proc-64>))

(define vector-ref-prim (make-proc '<proc-65>))

(define make-vector-prim (make-proc '<proc-66>))

(define error-prim (make-proc '<proc-67>))

(define toplevel-env (make-toplevel-env))


;;------------------------------------------------------------------------------------
;; define-datatype/cases expander
;;
;; This version doesn't do much error checking. It does not check
;; whether the values used to construct a new datatype record are of
;; the correct types, and does not check whether the field names in a
;; cases variant appear in the same order as specified by the datatype
;; definition, or whether the cases clauses cover all of the possible
;; variants. It also requires that the datatype tester function for
;; datatype X be named X?.

(define expand-define-datatype
  (lambda (def)
    (let* ((datatype-name (cadr def))
           (type-tester-name
            (string->symbol (string-append (symbol->string datatype-name) "?"))))
      (if (not (eq? type-tester-name (caddr def)))
          (error 'define-datatype "type tester function must be named ~a" type-tester-name)
          (let* ((variants (cdddr def))
                 (tester-def `(define ,type-tester-name
                                (lambda (x)
                                  (and (pair? x)
                                       (eq? (car x) ',datatype-name))))))
            `(begin
               ,tester-def
               ,@(map (lambda (variant)
                        (let ((variant-name (car variant)))
                          `(define ,variant-name (lambda args (cons ',variant-name args)))))
                      variants)))))))

(define expand-cases
  (lambda (cases-exp)
    (let* ((type-name (cadr cases-exp))
           (type-tester-name
            (string->symbol (string-append (symbol->string type-name) "?")))
           (exp (caddr cases-exp))
           (clauses (cdddr cases-exp))
           (clause-names (map car clauses))
           (clause-lambdas
            (map (lambda (clause)
                   (let ((variant-name (car clause)))
                     (if (eq? variant-name 'else)
                         `(lambda () ,@(cdr clause))
                         `(lambda ,(cadr clause) ,@(cddr clause)))))
                 clauses))
           (clause-bindings (map list clause-names clause-lambdas))
           (exp-binding `(result ,exp))
           (cond-clauses
            (map (lambda (cname)
                   (if (eq? cname 'else)
                       `(else (apply else (cdr result)))
                       `((eq? (car result) ',cname) (apply ,cname (cdr result)))))
                 clause-names)))
      `(let ((result ,exp)
             ,@clause-bindings)
         (if (not (,type-tester-name result))
             (error 'cases "~a is not a valid ~a" result ',type-name)
             (cond ,@cond-clauses))))))

;;------------------------------------------------------------------------------------
;; test 1

(define dd1
  '(define-datatype thing thing?
     (thing1)
     (thing2
      (f1 field1?)
      (f2 field2?)
      (f3 field3?))
     (thing3
      (f1 field1?)
      (f2 field2?))))

(define cases1
  '(cases thing (cons x y)
          (thing1 () b1 b2 b3)
          (thing2 (f1 f2 . f3) b1 b2 b3)
          (thing3 args b1 b2 b3)
          (else d1 d2 d3)))

;; (expand-define-datatype dd1)
;; (expand-cases cases1)

;;------------------------------------------------------------------------------------
;; test 2

(define dd2
  '(define-datatype expression expression?
     (var-exp
      (id symbol?))
     (if-exp
      (test-exp expression?)
      (then-exp expression?)
      (else-exp expression?))
     (lambda-exp
      (formals (list-of symbol?))
      (bodies (list-of expression?)))
     (app-exp
      (operator expression?)
      (operands (list-of expression?)))))

(define cases2
  '(cases expression exp
          (var-exp (id info)
                   (lookup-value id env info handler fail k))
          (if-exp (test-exp then-exp else-exp info)
                  (m test-exp env handler fail
                     (lambda (bool fail)
                       (if bool
                           (m then-exp env handler fail k)
                           (m else-exp env handler fail k)))))
          (lambda-exp (formals bodies info)
                      (k (closure formals bodies env) fail))
          (app-exp (operator operands info)
                   (m* operands env handler fail
                       (lambda (args fail)
                         (m operator env handler fail
                            (lambda (proc fail)
                              (proc args env info handler fail k))))))
          (else (error 'm "bad abstract syntax: ~s" exp))))

;; (expand-define-datatype dd2)
;; (expand-cases cases2)

(load "transformer-macros.ss")

;;----------------------------------------------------------------------
;; EOPL support

;; aexpression datatype
(define lit-aexp
  (lambda args (return* (cons 'lit-aexp args))))
(define var-aexp
  (lambda args (return* (cons 'var-aexp args))))
(define if-aexp
  (lambda args (return* (cons 'if-aexp args))))
(define assign-aexp
  (lambda args (return* (cons 'assign-aexp args))))
(define func-aexp
  (lambda args (return* (cons 'func-aexp args))))
(define define-aexp
  (lambda args (return* (cons 'define-aexp args))))
(define define!-aexp
  (lambda args (return* (cons 'define!-aexp args))))
(define define-syntax-aexp
  (lambda args (return* (cons 'define-syntax-aexp args))))
(define begin-aexp
  (lambda args (return* (cons 'begin-aexp args))))
(define lambda-aexp
  (lambda args (return* (cons 'lambda-aexp args))))
(define mu-lambda-aexp
  (lambda args (return* (cons 'mu-lambda-aexp args))))
(define trace-lambda-aexp
  (lambda args (return* (cons 'trace-lambda-aexp args))))
(define mu-trace-lambda-aexp
  (lambda args (return* (cons 'mu-trace-lambda-aexp args))))
(define app-aexp
  (lambda args (return* (cons 'app-aexp args))))
(define try-catch-aexp
  (lambda args (return* (cons 'try-catch-aexp args))))
(define try-finally-aexp
  (lambda args (return* (cons 'try-finally-aexp args))))
(define try-catch-finally-aexp
  (lambda args (return* (cons 'try-catch-finally-aexp args))))
(define raise-aexp
  (lambda args (return* (cons 'raise-aexp args))))
(define dict-aexp
  (lambda args (return* (cons 'dict-aexp args))))
(define help-aexp
  (lambda args (return* (cons 'help-aexp args))))
(define choose-aexp
  (lambda args (return* (cons 'choose-aexp args))))

;;----------------------------------------------------------------------

;; global registers
(define pc 'undefined)
(define aclauses_reg 'undefined)
(define action_reg 'undefined)
(define adatum-list_reg 'undefined)
(define adatum_reg 'undefined)
(define ap1_reg 'undefined)
(define ap2_reg 'undefined)
(define ap_reg 'undefined)
(define apair1_reg 'undefined)
(define apair2_reg 'undefined)
(define args_reg 'undefined)
(define asexps_reg 'undefined)
(define avar_reg 'undefined)
(define ax_reg 'undefined)
(define bindings_reg 'undefined)
(define bodies_reg 'undefined)
(define buffer_reg 'undefined)
(define char_reg 'undefined)
(define chars_reg 'undefined)
(define clauses_reg 'undefined)
(define components_reg 'undefined)
(define datum_reg 'undefined)
(define depth_reg 'undefined)
(define entries_reg 'undefined)
(define env2_reg 'undefined)
(define env_reg 'undefined)
(define exception_reg 'undefined)
(define exp_reg 'undefined)
(define expected-terminator_reg 'undefined)
(define exps_reg 'undefined)
(define fail_reg 'undefined)
(define filename_reg 'undefined)
(define filenames_reg 'undefined)
(define final_reg 'undefined)
(define generator_reg 'undefined)
(define handler_reg 'undefined)
(define i_reg 'undefined)
(define info_reg 'undefined)
(define input_reg 'undefined)
(define iterator_reg 'undefined)
(define k2_reg 'undefined)
(define k_reg 'undefined)
(define keyword_reg 'undefined)
(define line_reg 'undefined)
(define list1_reg 'undefined)
(define list2_reg 'undefined)
(define lists_reg 'undefined)
(define ls1_reg 'undefined)
(define ls2_reg 'undefined)
(define ls_reg 'undefined)
(define macro_reg 'undefined)
(define msg_reg 'undefined)
(define p1_reg 'undefined)
(define p2_reg 'undefined)
(define pair1_reg 'undefined)
(define pair2_reg 'undefined)
(define path_reg 'undefined)
(define pattern_reg 'undefined)
(define proc_reg 'undefined)
(define procs_reg 'undefined)
(define s_reg 'undefined)
(define src_reg 'undefined)
(define sum_reg 'undefined)
(define token-type_reg 'undefined)
(define tokens_reg 'undefined)
(define transformer-name_reg 'undefined)
(define v1_reg 'undefined)
(define v2_reg 'undefined)
(define value1_reg 'undefined)
(define value2_reg 'undefined)
(define value3_reg 'undefined)
(define value4_reg 'undefined)
(define value_reg 'undefined)
(define var-info_reg 'undefined)
(define var_reg 'undefined)
(define variable_reg 'undefined)
(define vars_reg 'undefined)
(define x_reg 'undefined)
(define y_reg 'undefined)

;; temporary registers
(define temp_2 'undefined)
(define temp_3 'undefined)
(define temp_4 'undefined)
(define temp_1 'undefined)

(define make-cont
  (lambda args (return* (cons 'continuation args))))

(define*
  apply-cont
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont-1>)
          (let ((chars 'undefined) (fail 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 3))
            (set! fail (list-ref temp_1 2))
            (set! chars (list-ref temp_1 1))
            (set! value3_reg fail)
            (set! value2_reg chars)
            (set! value1_reg value_reg)
            (set! k_reg k)
            (set! pc apply-cont3))
          (if (eq? (car temp_1) '<cont-2>)
              (let ((k 'undefined))
                (set! k (list-ref temp_1 1))
                (set! value_reg (list->vector value_reg))
                (set! k_reg k)
                (set! pc apply-cont))
              (if (eq? (car temp_1) '<cont-3>)
                  (let ((v1 'undefined) (k 'undefined))
                    (set! k (list-ref temp_1 2))
                    (set! v1 (list-ref temp_1 1))
                    (set! value_reg (cons v1 value_reg))
                    (set! k_reg k)
                    (set! pc apply-cont))
                  (if (eq? (car temp_1) '<cont-4>)
                      (let ((x 'undefined) (k 'undefined))
                        (set! k (list-ref temp_1 2))
                        (set! x (list-ref temp_1 1))
                        (set! k_reg (make-cont '<cont-3> value_reg k))
                        (set! x_reg (cdr x))
                        (set! pc unannotate-cps))
                      (if (eq? (car temp_1) '<cont-5>)
                          (let ((k 'undefined))
                            (set! k (list-ref temp_1 1))
                            (set! value_reg (retag (list->vector value_reg) 'none))
                            (set! k_reg k)
                            (set! pc apply-cont))
                          (if (eq? (car temp_1) '<cont-6>)
                              (let ((k 'undefined))
                                (set! k (list-ref temp_1 1))
                                (set! value_reg (retag value_reg 'none))
                                (set! k_reg k)
                                (set! pc apply-cont))
                              (if (eq? (car temp_1) '<cont-7>)
                                  (let ((x 'undefined) (k 'undefined))
                                    (set! k (list-ref temp_1 2))
                                    (set! x (list-ref temp_1 1))
                                    (set! k_reg (make-cont '<cont-3> value_reg k))
                                    (set! x_reg (cdr x))
                                    (set! pc reannotate-seq-cps))
                                  (if (eq? (car temp_1) '<cont-8>)
                                      (begin (set! final_reg value_reg) (set! pc #f))
                                      (if (eq? (car temp_1) '<cont-9>)
                                          (let ((bindings 'undefined) (k 'undefined))
                                            (set! k (list-ref temp_1 2))
                                            (set! bindings (list-ref temp_1 1))
                                            (set! value_reg
                                              (append
                                                (list 'let)
                                                (append (list (list (car bindings))) (list value_reg))))
                                            (set! k_reg k)
                                            (set! pc apply-cont))
                                          (if (eq? (car temp_1) '<cont-10>)
                                              (let ((clauses 'undefined) (var 'undefined) (k 'undefined))
                                                (set! k (list-ref temp_1 3))
                                                (set! var (list-ref temp_1 2))
                                                (set! clauses (list-ref temp_1 1))
                                                (let ((clause 'undefined))
                                                  (set! clause (car clauses))
                                                  (if (eq?^ (car^ clause) 'else)
                                                      (begin
                                                        (set! value_reg (cons clause value_reg))
                                                        (set! k_reg k)
                                                        (set! pc apply-cont))
                                                      (if (symbol?^ (car^ clause))
                                                          (begin
                                                            (set! value_reg
                                                              (cons
                                                                (append
                                                                  (list
                                                                    (append
                                                                      (list 'eq?)
                                                                      (append
                                                                        (list var)
                                                                        (list
                                                                          (append (list 'quote) (list (get-sexp (car^ clause))))))))
                                                                  (cdr^ clause))
                                                                value_reg))
                                                            (set! k_reg k)
                                                            (set! pc apply-cont))
                                                          (begin
                                                            (set! value_reg
                                                              (cons
                                                                (append
                                                                  (list
                                                                    (append
                                                                      (list 'memq)
                                                                      (append
                                                                        (list var)
                                                                        (list
                                                                          (append (list 'quote) (list (get-sexp (car^ clause))))))))
                                                                  (cdr^ clause))
                                                                value_reg))
                                                            (set! k_reg k)
                                                            (set! pc apply-cont))))))
                                              (if (eq? (car temp_1) '<cont-11>)
                                                  (let ((transformer-name 'undefined))
                                                    (set! transformer-name (list-ref temp_1 1))
                                                    (error transformer-name
                                                      "bad concrete syntax: ~a"
                                                      value_reg))
                                                  (if (eq? (car temp_1) '<cont-12>)
                                                      (let ((adatum 'undefined)
                                                            (bodies 'undefined)
                                                            (info 'undefined)
                                                            (fail 'undefined)
                                                            (k 'undefined))
                                                        (set! k (list-ref temp_1 5))
                                                        (set! fail (list-ref temp_1 4))
                                                        (set! info (list-ref temp_1 3))
                                                        (set! bodies (list-ref temp_1 2))
                                                        (set! adatum (list-ref temp_1 1))
                                                        (if (list? value_reg)
                                                            (begin
                                                              (set! value2_reg fail)
                                                              (set! value1_reg
                                                                (trace-lambda-aexp
                                                                  (get-sexp (cadr^ adatum))
                                                                  value_reg
                                                                  bodies
                                                                  info))
                                                              (set! k_reg k)
                                                              (set! pc apply-cont2))
                                                            (begin
                                                              (set! value2_reg fail)
                                                              (set! value1_reg
                                                                (mu-trace-lambda-aexp (get-sexp (cadr^ adatum))
                                                                  (head value_reg) (last value_reg) bodies info))
                                                              (set! k_reg k)
                                                              (set! pc apply-cont2))))
                                                      (if (eq? (car temp_1) '<cont-13>)
                                                          (let ((bodies 'undefined)
                                                                (info 'undefined)
                                                                (fail 'undefined)
                                                                (k 'undefined))
                                                            (set! k (list-ref temp_1 4))
                                                            (set! fail (list-ref temp_1 3))
                                                            (set! info (list-ref temp_1 2))
                                                            (set! bodies (list-ref temp_1 1))
                                                            (if (list? value_reg)
                                                                (begin
                                                                  (set! value2_reg fail)
                                                                  (set! value1_reg (lambda-aexp value_reg bodies info))
                                                                  (set! k_reg k)
                                                                  (set! pc apply-cont2))
                                                                (begin
                                                                  (set! value2_reg fail)
                                                                  (set! value1_reg
                                                                    (mu-lambda-aexp
                                                                      (head value_reg)
                                                                      (last value_reg)
                                                                      bodies
                                                                      info))
                                                                  (set! k_reg k)
                                                                  (set! pc apply-cont2))))
                                                          (if (eq? (car temp_1) '<cont-14>)
                                                              (let ((aclauses 'undefined)
                                                                    (name 'undefined)
                                                                    (info 'undefined)
                                                                    (fail 'undefined)
                                                                    (k 'undefined))
                                                                (set! k (list-ref temp_1 5))
                                                                (set! fail (list-ref temp_1 4))
                                                                (set! info (list-ref temp_1 3))
                                                                (set! name (list-ref temp_1 2))
                                                                (set! aclauses (list-ref temp_1 1))
                                                                (set! value2_reg fail)
                                                                (set! value1_reg
                                                                  (define-syntax-aexp name value_reg aclauses info))
                                                                (set! k_reg k)
                                                                (set! pc apply-cont2))
                                                              (if (eq? (car temp_1) '<cont-15>)
                                                                  (let ((info 'undefined)
                                                                        (handler 'undefined)
                                                                        (fail 'undefined)
                                                                        (k 'undefined))
                                                                    (set! k (list-ref temp_1 4))
                                                                    (set! fail (list-ref temp_1 3))
                                                                    (set! handler (list-ref temp_1 2))
                                                                    (set! info (list-ref temp_1 1))
                                                                    (set! k_reg k)
                                                                    (set! fail_reg fail)
                                                                    (set! handler_reg handler)
                                                                    (set! adatum_reg (replace-info value_reg info))
                                                                    (set! pc aparse))
                                                                  (if (eq? (car temp_1) '<cont-16>)
                                                                      (let ((info 'undefined)
                                                                            (handler 'undefined)
                                                                            (fail 'undefined)
                                                                            (k 'undefined))
                                                                        (set! k (list-ref temp_1 4))
                                                                        (set! fail (list-ref temp_1 3))
                                                                        (set! handler (list-ref temp_1 2))
                                                                        (set! info (list-ref temp_1 1))
                                                                        (set! k_reg (make-cont '<cont-15> info handler fail k))
                                                                        (set! x_reg value_reg)
                                                                        (set! pc reannotate-cps))
                                                                      (if (eq? (car temp_1) '<cont-17>)
                                                                          (let ((adatum 'undefined)
                                                                                (handler 'undefined)
                                                                                (fail 'undefined)
                                                                                (k 'undefined))
                                                                            (set! k (list-ref temp_1 4))
                                                                            (set! fail (list-ref temp_1 3))
                                                                            (set! handler (list-ref temp_1 2))
                                                                            (set! adatum (list-ref temp_1 1))
                                                                            (let ((info 'undefined))
                                                                              (set! info (get-source-info adatum))
                                                                              (if (original-source-info? adatum)
                                                                                  (begin
                                                                                    (set! k_reg k)
                                                                                    (set! fail_reg fail)
                                                                                    (set! handler_reg handler)
                                                                                    (set! adatum_reg
                                                                                      (replace-info value_reg (snoc 'quasiquote info)))
                                                                                    (set! pc aparse))
                                                                                  (begin
                                                                                    (set! k_reg k)
                                                                                    (set! fail_reg fail)
                                                                                    (set! handler_reg handler)
                                                                                    (set! adatum_reg (replace-info value_reg info))
                                                                                    (set! pc aparse)))))
                                                                          (if (eq? (car temp_1) '<cont-18>)
                                                                              (let ((adatum 'undefined)
                                                                                    (handler 'undefined)
                                                                                    (fail 'undefined)
                                                                                    (k 'undefined))
                                                                                (set! k (list-ref temp_1 4))
                                                                                (set! fail (list-ref temp_1 3))
                                                                                (set! handler (list-ref temp_1 2))
                                                                                (set! adatum (list-ref temp_1 1))
                                                                                (set! k_reg (make-cont '<cont-17> adatum handler fail k))
                                                                                (set! x_reg value_reg)
                                                                                (set! pc reannotate-cps))
                                                                              (if (eq? (car temp_1) '<cont-19>)
                                                                                  (let ((info 'undefined) (fail 'undefined) (k 'undefined))
                                                                                    (set! k (list-ref temp_1 3))
                                                                                    (set! fail (list-ref temp_1 2))
                                                                                    (set! info (list-ref temp_1 1))
                                                                                    (set! value2_reg fail)
                                                                                    (set! value1_reg (lit-aexp value_reg info))
                                                                                    (set! k_reg k)
                                                                                    (set! pc apply-cont2))
                                                                                  (if (eq? (car temp_1) '<cont-20>)
                                                                                      (let ((msg 'undefined)
                                                                                            (info 'undefined)
                                                                                            (handler 'undefined)
                                                                                            (fail 'undefined))
                                                                                        (set! fail (list-ref temp_1 4))
                                                                                        (set! handler (list-ref temp_1 3))
                                                                                        (set! info (list-ref temp_1 2))
                                                                                        (set! msg (list-ref temp_1 1))
                                                                                        (set! fail_reg fail)
                                                                                        (set! exception_reg
                                                                                          (format
                                                                                            "parse error: ~a ~s ~a"
                                                                                            msg
                                                                                            value_reg
                                                                                            (where-at
                                                                                              (get-start-line info)
                                                                                              (get-start-char info)
                                                                                              (get-srcfile info))))
                                                                                        (set! handler_reg handler)
                                                                                        (set! pc apply-handler2))
                                                                                      (if (eq? (car temp_1) '<cont-21>)
                                                                                          (let ((adatum 'undefined) (fail 'undefined) (k 'undefined))
                                                                                            (set! k (list-ref temp_1 3))
                                                                                            (set! fail (list-ref temp_1 2))
                                                                                            (set! adatum (list-ref temp_1 1))
                                                                                            (if (has-source-info? value_reg)
                                                                                                (begin
                                                                                                  (set! value2_reg fail)
                                                                                                  (set! value1_reg value_reg)
                                                                                                  (set! k_reg k)
                                                                                                  (set! pc apply-cont2))
                                                                                                (let ((info 'undefined))
                                                                                                  (set! info (get-source-info adatum))
                                                                                                  (if (original-source-info? adatum)
                                                                                                      (let ((macro-keyword 'undefined))
                                                                                                        (set! macro-keyword (get-sexp (car^ adatum)))
                                                                                                        (set! value2_reg fail)
                                                                                                        (set! value1_reg
                                                                                                          (replace-info value_reg (snoc macro-keyword info)))
                                                                                                        (set! k_reg k)
                                                                                                        (set! pc apply-cont2))
                                                                                                      (begin
                                                                                                        (set! value2_reg fail)
                                                                                                        (set! value1_reg (replace-info value_reg info))
                                                                                                        (set! k_reg k)
                                                                                                        (set! pc apply-cont2))))))
                                                                                          (if (eq? (car temp_1) '<cont-22>)
                                                                                              (let ((adatum 'undefined) (fail 'undefined) (k 'undefined))
                                                                                                (set! k (list-ref temp_1 3))
                                                                                                (set! fail (list-ref temp_1 2))
                                                                                                (set! adatum (list-ref temp_1 1))
                                                                                                (set! k_reg (make-cont '<cont-21> adatum fail k))
                                                                                                (set! x_reg value_reg)
                                                                                                (set! pc reannotate-cps))
                                                                                              (if (eq? (car temp_1) '<cont-23>)
                                                                                                  (let ((aclauses 'undefined)
                                                                                                        (adatum 'undefined)
                                                                                                        (aright-pattern 'undefined)
                                                                                                        (clauses 'undefined)
                                                                                                        (right-pattern 'undefined)
                                                                                                        (handler 'undefined)
                                                                                                        (fail 'undefined)
                                                                                                        (k 'undefined))
                                                                                                    (set! k (list-ref temp_1 8))
                                                                                                    (set! fail (list-ref temp_1 7))
                                                                                                    (set! handler (list-ref temp_1 6))
                                                                                                    (set! right-pattern (list-ref temp_1 5))
                                                                                                    (set! clauses (list-ref temp_1 4))
                                                                                                    (set! aright-pattern (list-ref temp_1 3))
                                                                                                    (set! adatum (list-ref temp_1 2))
                                                                                                    (set! aclauses (list-ref temp_1 1))
                                                                                                    (if value_reg
                                                                                                        (begin
                                                                                                          (set! k2_reg (make-cont2 '<cont2-40> fail k))
                                                                                                          (set! ap_reg aright-pattern)
                                                                                                          (set! s_reg value_reg)
                                                                                                          (set! pattern_reg right-pattern)
                                                                                                          (set! pc instantiate^))
                                                                                                        (begin
                                                                                                          (set! k_reg k)
                                                                                                          (set! fail_reg fail)
                                                                                                          (set! handler_reg handler)
                                                                                                          (set! adatum_reg adatum)
                                                                                                          (set! aclauses_reg (cdr aclauses))
                                                                                                          (set! clauses_reg (cdr clauses))
                                                                                                          (set! pc process-macro-clauses^))))
                                                                                                  (if (eq? (car temp_1) '<cont-24>)
                                                                                                      (let ((aclauses 'undefined)
                                                                                                            (adatum 'undefined)
                                                                                                            (aleft-pattern 'undefined)
                                                                                                            (aright-pattern 'undefined)
                                                                                                            (clauses 'undefined)
                                                                                                            (left-pattern 'undefined)
                                                                                                            (right-pattern 'undefined)
                                                                                                            (handler 'undefined)
                                                                                                            (fail 'undefined)
                                                                                                            (k 'undefined))
                                                                                                        (set! k (list-ref temp_1 10))
                                                                                                        (set! fail (list-ref temp_1 9))
                                                                                                        (set! handler (list-ref temp_1 8))
                                                                                                        (set! right-pattern (list-ref temp_1 7))
                                                                                                        (set! left-pattern (list-ref temp_1 6))
                                                                                                        (set! clauses (list-ref temp_1 5))
                                                                                                        (set! aright-pattern (list-ref temp_1 4))
                                                                                                        (set! aleft-pattern (list-ref temp_1 3))
                                                                                                        (set! adatum (list-ref temp_1 2))
                                                                                                        (set! aclauses (list-ref temp_1 1))
                                                                                                        (set! k_reg
                                                                                                          (make-cont '<cont-23> aclauses adatum aright-pattern clauses
                                                                                                            right-pattern handler fail k))
                                                                                                        (set! ap2_reg adatum)
                                                                                                        (set! ap1_reg aleft-pattern)
                                                                                                        (set! p2_reg value_reg)
                                                                                                        (set! p1_reg left-pattern)
                                                                                                        (set! pc unify-patterns^))
                                                                                                      (if (eq? (car temp_1) '<cont-25>)
                                                                                                          (let ((v1 'undefined) (k 'undefined))
                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                            (set! v1 (list-ref temp_1 1))
                                                                                                            (set! value_reg
                                                                                                              (append (list 'append) (append (list v1) (list value_reg))))
                                                                                                            (set! k_reg k)
                                                                                                            (set! pc apply-cont))
                                                                                                          (if (eq? (car temp_1) '<cont-26>)
                                                                                                              (let ((ax 'undefined) (depth 'undefined) (k 'undefined))
                                                                                                                (set! k (list-ref temp_1 3))
                                                                                                                (set! depth (list-ref temp_1 2))
                                                                                                                (set! ax (list-ref temp_1 1))
                                                                                                                (set! k_reg (make-cont '<cont-25> value_reg k))
                                                                                                                (set! depth_reg depth)
                                                                                                                (set! ax_reg (^cdr^ ax))
                                                                                                                (set! pc qq-expand-cps))
                                                                                                              (if (eq? (car temp_1) '<cont-27>)
                                                                                                                  (let ((k 'undefined))
                                                                                                                    (set! k (list-ref temp_1 1))
                                                                                                                    (set! value_reg
                                                                                                                      (append (list 'list->vector) (list value_reg)))
                                                                                                                    (set! k_reg k)
                                                                                                                    (set! pc apply-cont))
                                                                                                                  (if (eq? (car temp_1) '<cont-28>)
                                                                                                                      (let ((ax 'undefined) (k 'undefined))
                                                                                                                        (set! k (list-ref temp_1 2))
                                                                                                                        (set! ax (list-ref temp_1 1))
                                                                                                                        (set! value_reg
                                                                                                                          (append
                                                                                                                            (list 'cons)
                                                                                                                            (append
                                                                                                                              (list (append (list 'quote) (list (car^ ax))))
                                                                                                                              (list value_reg))))
                                                                                                                        (set! k_reg k)
                                                                                                                        (set! pc apply-cont))
                                                                                                                      (if (eq? (car temp_1) '<cont-29>)
                                                                                                                          (let ((k 'undefined))
                                                                                                                            (set! k (list-ref temp_1 1))
                                                                                                                            (set! value_reg
                                                                                                                              (append
                                                                                                                                (list 'cons)
                                                                                                                                (append
                                                                                                                                  (list (append (list 'quote) (list 'quasiquote)))
                                                                                                                                  (list value_reg))))
                                                                                                                            (set! k_reg k)
                                                                                                                            (set! pc apply-cont))
                                                                                                                          (if (eq? (car temp_1) '<cont-30>)
                                                                                                                              (let ((v1 'undefined) (k 'undefined))
                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                (set! v1 (list-ref temp_1 1))
                                                                                                                                (set! value_reg
                                                                                                                                  (append
                                                                                                                                    (list 'list)
                                                                                                                                    (list
                                                                                                                                      (append
                                                                                                                                        (list 'append)
                                                                                                                                        (append (list v1) (list value_reg))))))
                                                                                                                                (set! k_reg k)
                                                                                                                                (set! pc apply-cont))
                                                                                                                              (if (eq? (car temp_1) '<cont-31>)
                                                                                                                                  (let ((ax 'undefined) (depth 'undefined) (k 'undefined))
                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                    (set! depth (list-ref temp_1 2))
                                                                                                                                    (set! ax (list-ref temp_1 1))
                                                                                                                                    (set! k_reg (make-cont '<cont-30> value_reg k))
                                                                                                                                    (set! depth_reg depth)
                                                                                                                                    (set! ax_reg (^cdr^ ax))
                                                                                                                                    (set! pc qq-expand-cps))
                                                                                                                                  (if (eq? (car temp_1) '<cont-32>)
                                                                                                                                      (let ((k 'undefined))
                                                                                                                                        (set! k (list-ref temp_1 1))
                                                                                                                                        (set! value_reg (append (list 'list) (list value_reg)))
                                                                                                                                        (set! k_reg k)
                                                                                                                                        (set! pc apply-cont))
                                                                                                                                      (if (eq? (car temp_1) '<cont-33>)
                                                                                                                                          (let ((ax 'undefined) (k 'undefined))
                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                            (set! ax (list-ref temp_1 1))
                                                                                                                                            (set! value_reg
                                                                                                                                              (append
                                                                                                                                                (list 'list)
                                                                                                                                                (list
                                                                                                                                                  (append
                                                                                                                                                    (list 'cons)
                                                                                                                                                    (append
                                                                                                                                                      (list (append (list 'quote) (list (car^ ax))))
                                                                                                                                                      (list value_reg))))))
                                                                                                                                            (set! k_reg k)
                                                                                                                                            (set! pc apply-cont))
                                                                                                                                          (if (eq? (car temp_1) '<cont-34>)
                                                                                                                                              (let ((k 'undefined))
                                                                                                                                                (set! k (list-ref temp_1 1))
                                                                                                                                                (set! value_reg
                                                                                                                                                  (append
                                                                                                                                                    (list 'list)
                                                                                                                                                    (list
                                                                                                                                                      (append
                                                                                                                                                        (list 'cons)
                                                                                                                                                        (append
                                                                                                                                                          (list (append (list 'quote) (list 'quasiquote)))
                                                                                                                                                          (list value_reg))))))
                                                                                                                                                (set! k_reg k)
                                                                                                                                                (set! pc apply-cont))
                                                                                                                                              (if (eq? (car temp_1) '<cont-35>)
                                                                                                                                                  (let ((v1 'undefined) (fail 'undefined) (k 'undefined))
                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                    (set! fail (list-ref temp_1 2))
                                                                                                                                                    (set! v1 (list-ref temp_1 1))
                                                                                                                                                    (set! value2_reg fail)
                                                                                                                                                    (set! value1_reg (cons v1 value_reg))
                                                                                                                                                    (set! k_reg k)
                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                  (if (eq? (car temp_1) '<cont-36>)
                                                                                                                                                      (let ((handler 'undefined)
                                                                                                                                                            (fail 'undefined)
                                                                                                                                                            (k2 'undefined))
                                                                                                                                                        (set! k2 (list-ref temp_1 3))
                                                                                                                                                        (set! fail (list-ref temp_1 2))
                                                                                                                                                        (set! handler (list-ref temp_1 1))
                                                                                                                                                        (set! k_reg (make-cont2 '<cont2-72> handler k2))
                                                                                                                                                        (set! fail_reg fail)
                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                        (set! adatum_reg value_reg)
                                                                                                                                                        (set! pc aparse))
                                                                                                                                                      (if (eq? (car temp_1) '<cont-37>)
                                                                                                                                                          (let ((handler 'undefined)
                                                                                                                                                                (fail 'undefined)
                                                                                                                                                                (k2 'undefined))
                                                                                                                                                            (set! k2 (list-ref temp_1 3))
                                                                                                                                                            (set! fail (list-ref temp_1 2))
                                                                                                                                                            (set! handler (list-ref temp_1 1))
                                                                                                                                                            (set! k_reg k2)
                                                                                                                                                            (set! fail_reg fail)
                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                            (set! adatum_reg value_reg)
                                                                                                                                                            (set! pc aparse))
                                                                                                                                                          (if (eq? (car temp_1) '<cont-38>)
                                                                                                                                                              (let ((fail 'undefined) (k2 'undefined))
                                                                                                                                                                (set! k2 (list-ref temp_1 2))
                                                                                                                                                                (set! fail (list-ref temp_1 1))
                                                                                                                                                                (set! value2_reg fail)
                                                                                                                                                                (set! value1_reg value_reg)
                                                                                                                                                                (set! k_reg k2)
                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                              (if (eq? (car temp_1) '<cont-39>)
                                                                                                                                                                  (let ((x 'undefined) (y 'undefined) (k 'undefined))
                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                    (set! y (list-ref temp_1 2))
                                                                                                                                                                    (set! x (list-ref temp_1 1))
                                                                                                                                                                    (if value_reg
                                                                                                                                                                        (begin
                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                          (set! y_reg (cdr y))
                                                                                                                                                                          (set! x_reg (cdr x))
                                                                                                                                                                          (set! pc equal-objects?))
                                                                                                                                                                        (begin
                                                                                                                                                                          (set! value_reg #f)
                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                          (set! pc apply-cont))))
                                                                                                                                                                  (if (eq? (car temp_1) '<cont-40>)
                                                                                                                                                                      (let ((i 'undefined)
                                                                                                                                                                            (v1 'undefined)
                                                                                                                                                                            (v2 'undefined)
                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                        (set! k (list-ref temp_1 4))
                                                                                                                                                                        (set! v2 (list-ref temp_1 3))
                                                                                                                                                                        (set! v1 (list-ref temp_1 2))
                                                                                                                                                                        (set! i (list-ref temp_1 1))
                                                                                                                                                                        (if value_reg
                                                                                                                                                                            (begin
                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                              (set! i_reg (- i 1))
                                                                                                                                                                              (set! v2_reg v2)
                                                                                                                                                                              (set! v1_reg v1)
                                                                                                                                                                              (set! pc equal-vectors?))
                                                                                                                                                                            (begin
                                                                                                                                                                              (set! value_reg #f)
                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                              (set! pc apply-cont))))
                                                                                                                                                                      (if (eq? (car temp_1) '<cont-41>)
                                                                                                                                                                          (let ((ls 'undefined)
                                                                                                                                                                                (x 'undefined)
                                                                                                                                                                                (y 'undefined)
                                                                                                                                                                                (info 'undefined)
                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                (fail 'undefined)
                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                            (set! k (list-ref temp_1 7))
                                                                                                                                                                            (set! fail (list-ref temp_1 6))
                                                                                                                                                                            (set! handler (list-ref temp_1 5))
                                                                                                                                                                            (set! info (list-ref temp_1 4))
                                                                                                                                                                            (set! y (list-ref temp_1 3))
                                                                                                                                                                            (set! x (list-ref temp_1 2))
                                                                                                                                                                            (set! ls (list-ref temp_1 1))
                                                                                                                                                                            (if value_reg
                                                                                                                                                                                (begin
                                                                                                                                                                                  (set! value2_reg fail)
                                                                                                                                                                                  (set! value1_reg y)
                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                  (set! pc apply-cont2))
                                                                                                                                                                                (begin
                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                  (set! fail_reg fail)
                                                                                                                                                                                  (set! handler_reg handler)
                                                                                                                                                                                  (set! info_reg info)
                                                                                                                                                                                  (set! ls_reg ls)
                                                                                                                                                                                  (set! y_reg (cdr y))
                                                                                                                                                                                  (set! x_reg x)
                                                                                                                                                                                  (set! pc member-loop))))
                                                                                                                                                                          (if (eq? (car temp_1) '<cont-42>)
                                                                                                                                                                              (let ((ls1 'undefined) (k 'undefined))
                                                                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                                                                (set! ls1 (list-ref temp_1 1))
                                                                                                                                                                                (set! value_reg (cons (car ls1) value_reg))
                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                (set! pc apply-cont))
                                                                                                                                                                              (if (eq? (car temp_1) '<cont-43>)
                                                                                                                                                                                  (let ((lists 'undefined) (k 'undefined))
                                                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                                                    (set! lists (list-ref temp_1 1))
                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                    (set! ls2_reg value_reg)
                                                                                                                                                                                    (set! ls1_reg (car lists))
                                                                                                                                                                                    (set! pc append2))
                                                                                                                                                                                  (if (eq? (car temp_1) '<cont-44>)
                                                                                                                                                                                      (let ((pattern 'undefined) (var 'undefined) (k 'undefined))
                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                        (set! var (list-ref temp_1 2))
                                                                                                                                                                                        (set! pattern (list-ref temp_1 1))
                                                                                                                                                                                        (if value_reg
                                                                                                                                                                                            (begin
                                                                                                                                                                                              (set! value_reg #t)
                                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                                              (set! pc apply-cont))
                                                                                                                                                                                            (begin
                                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                                              (set! pattern_reg (cdr pattern))
                                                                                                                                                                                              (set! var_reg var)
                                                                                                                                                                                              (set! pc occurs?))))
                                                                                                                                                                                      (if (eq? (car temp_1) '<cont-45>)
                                                                                                                                                                                          (let ((ap2 'undefined)
                                                                                                                                                                                                (p1 'undefined)
                                                                                                                                                                                                (p2 'undefined)
                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                            (set! p2 (list-ref temp_1 3))
                                                                                                                                                                                            (set! p1 (list-ref temp_1 2))
                                                                                                                                                                                            (set! ap2 (list-ref temp_1 1))
                                                                                                                                                                                            (if value_reg
                                                                                                                                                                                                (begin
                                                                                                                                                                                                  (set! value_reg #f)
                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                  (set! pc apply-cont))
                                                                                                                                                                                                (begin
                                                                                                                                                                                                  (set! value_reg (make-sub 'unit p1 p2 ap2))
                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                  (set! pc apply-cont))))
                                                                                                                                                                                          (if (eq? (car temp_1) '<cont-46>)
                                                                                                                                                                                              (let ((s-car 'undefined) (k 'undefined))
                                                                                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                                                                                (set! s-car (list-ref temp_1 1))
                                                                                                                                                                                                (if (not value_reg)
                                                                                                                                                                                                    (begin
                                                                                                                                                                                                      (set! value_reg #f)
                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                      (set! pc apply-cont))
                                                                                                                                                                                                    (begin
                                                                                                                                                                                                      (set! value_reg (make-sub 'composite s-car value_reg))
                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                      (set! pc apply-cont))))
                                                                                                                                                                                              (if (eq? (car temp_1) '<cont-47>)
                                                                                                                                                                                                  (let ((apair1 'undefined)
                                                                                                                                                                                                        (apair2 'undefined)
                                                                                                                                                                                                        (pair1 'undefined)
                                                                                                                                                                                                        (pair2 'undefined)
                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                    (set! k (list-ref temp_1 5))
                                                                                                                                                                                                    (set! pair2 (list-ref temp_1 4))
                                                                                                                                                                                                    (set! pair1 (list-ref temp_1 3))
                                                                                                                                                                                                    (set! apair2 (list-ref temp_1 2))
                                                                                                                                                                                                    (set! apair1 (list-ref temp_1 1))
                                                                                                                                                                                                    (if (not value_reg)
                                                                                                                                                                                                        (begin
                                                                                                                                                                                                          (set! value_reg #f)
                                                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                                                          (set! pc apply-cont))
                                                                                                                                                                                                        (begin
                                                                                                                                                                                                          (set! k2_reg
                                                                                                                                                                                                            (make-cont2 '<cont2-90> apair2 pair2 value_reg k))
                                                                                                                                                                                                          (set! ap_reg (^cdr^ apair1))
                                                                                                                                                                                                          (set! s_reg value_reg)
                                                                                                                                                                                                          (set! pattern_reg (cdr pair1))
                                                                                                                                                                                                          (set! pc instantiate^))))
                                                                                                                                                                                                  (error 'apply-cont
                                                                                                                                                                                                    "bad continuation: ~a"
                                                                                                                                                                                                    k_reg)))))))))))))))))))))))))))))))))))))))))))))))))))

(define make-cont2
  (lambda args (return* (cons 'continuation2 args))))

(define*
  apply-cont2
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont2-1>)
          (let ((token 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 2))
            (set! token (list-ref temp_1 1))
            (set! value1_reg (cons token value1_reg))
            (set! k_reg k)
            (set! pc apply-cont2))
          (if (eq? (car temp_1) '<cont2-2>)
              (begin (set! final_reg value1_reg) (set! pc #f))
              (if (eq? (car temp_1) '<cont2-3>)
                  (let ((k 'undefined))
                    (set! k (list-ref temp_1 1))
                    (set! value1_reg (binding-value value1_reg))
                    (set! k_reg k)
                    (set! pc apply-cont2))
                  (if (eq? (car temp_1) '<cont2-4>)
                      (let ((var-info 'undefined)
                            (variable 'undefined)
                            (env 'undefined)
                            (handler 'undefined)
                            (k 'undefined))
                        (set! k (list-ref temp_1 5))
                        (set! handler (list-ref temp_1 4))
                        (set! env (list-ref temp_1 3))
                        (set! variable (list-ref temp_1 2))
                        (set! var-info (list-ref temp_1 1))
                        (if (dlr-env-contains variable)
                            (begin
                              (set! value1_reg (dlr-env-lookup variable))
                              (set! k_reg k)
                              (set! pc apply-cont2))
                            (if value1_reg
                                (begin
                                  (set! k_reg k)
                                  (set! fail_reg value2_reg)
                                  (set! handler_reg handler)
                                  (set! env_reg env)
                                  (set! path_reg "")
                                  (set! components_reg value1_reg)
                                  (set! pc lookup-variable-components))
                                (begin
                                  (set! fail_reg value2_reg)
                                  (set! handler_reg handler)
                                  (set! info_reg var-info)
                                  (set! msg_reg (format "unbound variable ~a" variable))
                                  (set! pc runtime-error)))))
                      (if (eq? (car temp_1) '<cont2-5>)
                          (let ((components 'undefined)
                                (path 'undefined)
                                (var 'undefined)
                                (handler 'undefined)
                                (k 'undefined))
                            (set! k (list-ref temp_1 5))
                            (set! handler (list-ref temp_1 4))
                            (set! var (list-ref temp_1 3))
                            (set! path (list-ref temp_1 2))
                            (set! components (list-ref temp_1 1))
                            (if (null? (cdr components))
                                (begin (set! k_reg k) (set! pc apply-cont2))
                                (let ((result 'undefined) (new-path 'undefined))
                                  (set! new-path
                                    (if (string=? path "")
                                        (format "~a" var)
                                        (format "~a.~a" path var)))
                                  (set! result (binding-value value1_reg))
                                  (if (not (environment? result))
                                      (if (dlr-object? result)
                                          (begin
                                            (set! value1_reg
                                              (dlr-lookup-components result (cdr components)))
                                            (set! k_reg k)
                                            (set! pc apply-cont2))
                                          (begin
                                            (set! fail_reg value2_reg)
                                            (set! exception_reg (format "~a is not a module" new-path))
                                            (set! handler_reg handler)
                                            (set! pc apply-handler2)))
                                      (begin
                                        (set! k_reg k)
                                        (set! fail_reg value2_reg)
                                        (set! handler_reg handler)
                                        (set! env_reg result)
                                        (set! path_reg new-path)
                                        (set! components_reg (cdr components))
                                        (set! pc lookup-variable-components))))))
                          (if (eq? (car temp_1) '<cont2-6>)
                              (let ((bodies 'undefined) (k 'undefined))
                                (set! k (list-ref temp_1 2))
                                (set! bodies (list-ref temp_1 1))
                                (set! value_reg
                                  (append
                                    (list 'let)
                                    (append (list value1_reg) (append value2_reg bodies))))
                                (set! k_reg k)
                                (set! pc apply-cont))
                              (if (eq? (car temp_1) '<cont2-7>)
                                  (let ((procs 'undefined) (vars 'undefined) (k2 'undefined))
                                    (set! k2 (list-ref temp_1 3))
                                    (set! vars (list-ref temp_1 2))
                                    (set! procs (list-ref temp_1 1))
                                    (set! value2_reg
                                      (cons
                                        (append
                                          (list 'set!)
                                          (append (list (car vars)) (list (car procs))))
                                        value2_reg))
                                    (set! value1_reg
                                      (cons
                                        (append
                                          (list (car vars))
                                          (list (append (list 'quote) (list 'undefined))))
                                        value1_reg))
                                    (set! k_reg k2)
                                    (set! pc apply-cont2))
                                  (if (eq? (car temp_1) '<cont2-8>)
                                      (let ((exp 'undefined) (k 'undefined))
                                        (set! k (list-ref temp_1 2))
                                        (set! exp (list-ref temp_1 1))
                                        (set! value_reg
                                          (append
                                            (list 'let)
                                            (append
                                              (list
                                                (append (list (append (list 'r) (list exp))) value1_reg))
                                              (list (append (list 'cond) value2_reg)))))
                                        (set! k_reg k)
                                        (set! pc apply-cont))
                                      (if (eq? (car temp_1) '<cont2-9>)
                                          (let ((clauses 'undefined) (var 'undefined) (k2 'undefined))
                                            (set! k2 (list-ref temp_1 3))
                                            (set! var (list-ref temp_1 2))
                                            (set! clauses (list-ref temp_1 1))
                                            (let ((clause 'undefined))
                                              (set! clause (car clauses))
                                              (if (eq?^ (car^ clause) 'else)
                                                  (begin
                                                    (set! value2_reg
                                                      (cons (list 'else (list 'else-code)) value2_reg))
                                                    (set! value1_reg
                                                      (cons
                                                        (append
                                                          (list 'else-code)
                                                          (list
                                                            (append (list 'lambda) (append (list '()) (cdr^ clause)))))
                                                        value1_reg))
                                                    (set! k_reg k2)
                                                    (set! pc apply-cont2))
                                                  (if (symbol?^ (car^ clause))
                                                      (let ((name 'undefined))
                                                        (set! name (get-sexp (car^ clause)))
                                                        (set! value2_reg
                                                          (cons
                                                            (append
                                                              (list
                                                                (append
                                                                  (list 'eq?)
                                                                  (append
                                                                    (list var)
                                                                    (list (append (list 'quote) (list (car^ clause)))))))
                                                              (list (list name)))
                                                            value2_reg))
                                                        (set! value1_reg
                                                          (cons
                                                            (append
                                                              (list name)
                                                              (list
                                                                (append (list 'lambda) (append (list '()) (cdr^ clause)))))
                                                            value1_reg))
                                                        (set! k_reg k2)
                                                        (set! pc apply-cont2))
                                                      (let ((name 'undefined))
                                                        (set! name (get-sexp (car^ (car^ clause))))
                                                        (set! value2_reg
                                                          (cons
                                                            (append
                                                              (list
                                                                (append
                                                                  (list 'memq)
                                                                  (append
                                                                    (list var)
                                                                    (list (append (list 'quote) (list (car^ clause)))))))
                                                              (list (list name)))
                                                            value2_reg))
                                                        (set! value1_reg
                                                          (cons
                                                            (append
                                                              (list name)
                                                              (list
                                                                (append (list 'lambda) (append (list '()) (cdr^ clause)))))
                                                            value1_reg))
                                                        (set! k_reg k2)
                                                        (set! pc apply-cont2))))))
                                          (if (eq? (car temp_1) '<cont2-10>)
                                              (let ((clauses 'undefined) (var 'undefined) (k2 'undefined))
                                                (set! k2 (list-ref temp_1 3))
                                                (set! var (list-ref temp_1 2))
                                                (set! clauses (list-ref temp_1 1))
                                                (let ((clause 'undefined))
                                                  (set! clause (car clauses))
                                                  (if (eq?^ (car^ clause) 'else)
                                                      (begin
                                                        (set! value2_reg
                                                          (cons
                                                            (append (list 'else) (list (list 'else-code)))
                                                            value2_reg))
                                                        (set! value1_reg
                                                          (cons
                                                            (append
                                                              (list 'else-code)
                                                              (list
                                                                (append (list 'lambda) (append (list '()) (cdr^ clause)))))
                                                            value1_reg))
                                                        (set! k_reg k2)
                                                        (set! pc apply-cont2))
                                                      (if (symbol?^ (car^ clause))
                                                          (let ((name 'undefined))
                                                            (set! name (get-sexp (car^ clause)))
                                                            (set! value2_reg
                                                              (cons
                                                                (append
                                                                  (list
                                                                    (append
                                                                      (list 'eq?)
                                                                      (append
                                                                        (list (append (list 'car) (list var)))
                                                                        (list (append (list 'quote) (list (car^ clause)))))))
                                                                  (list
                                                                    (append
                                                                      (list 'apply)
                                                                      (append
                                                                        (list name)
                                                                        (list (append (list 'cdr) (list var)))))))
                                                                value2_reg))
                                                            (set! value1_reg
                                                              (cons
                                                                (append
                                                                  (list name)
                                                                  (list
                                                                    (append
                                                                      (list 'lambda)
                                                                      (append (list (cadr^ clause)) (cddr^ clause)))))
                                                                value1_reg))
                                                            (set! k_reg k2)
                                                            (set! pc apply-cont2))
                                                          (let ((name 'undefined))
                                                            (set! name (get-sexp (car^ (car^ clause))))
                                                            (set! value2_reg
                                                              (cons
                                                                (append
                                                                  (list
                                                                    (append
                                                                      (list 'memq)
                                                                      (append
                                                                        (list (append (list 'car) (list var)))
                                                                        (list (append (list 'quote) (list (car^ clause)))))))
                                                                  (list
                                                                    (append
                                                                      (list 'apply)
                                                                      (append
                                                                        (list name)
                                                                        (list (append (list 'cdr) (list var)))))))
                                                                value2_reg))
                                                            (set! value1_reg
                                                              (cons
                                                                (append
                                                                  (list name)
                                                                  (list
                                                                    (append
                                                                      (list 'lambda)
                                                                      (append (list (cadr^ clause)) (cddr^ clause)))))
                                                                value1_reg))
                                                            (set! k_reg k2)
                                                            (set! pc apply-cont2))))))
                                              (if (eq? (car temp_1) '<cont2-11>)
                                                  (let ((exp 'undefined)
                                                        (type-name 'undefined)
                                                        (type-tester-name 'undefined)
                                                        (k 'undefined))
                                                    (set! k (list-ref temp_1 4))
                                                    (set! type-tester-name (list-ref temp_1 3))
                                                    (set! type-name (list-ref temp_1 2))
                                                    (set! exp (list-ref temp_1 1))
                                                    (set! value_reg
                                                      (append
                                                        (list 'let)
                                                        (append
                                                          (list
                                                            (append (list (append (list 'r) (list exp))) value1_reg))
                                                          (list
                                                            (append
                                                              (list 'if)
                                                              (append
                                                                (list
                                                                  (append
                                                                    (list 'not)
                                                                    (list (append (list type-tester-name) (list 'r)))))
                                                                (append
                                                                  (list
                                                                    (append
                                                                      (list 'error)
                                                                      (append
                                                                        (list (append (list 'quote) (list 'cases)))
                                                                        (append
                                                                          (list "~a is not a valid ~a")
                                                                          (append
                                                                            (list 'r)
                                                                            (list (append (list 'quote) (list type-name))))))))
                                                                  (list (append (list 'cond) value2_reg)))))))))
                                                    (set! k_reg k)
                                                    (set! pc apply-cont))
                                                  (if (eq? (car temp_1) '<cont2-12>)
                                                      (let ((v1 'undefined) (info 'undefined) (k 'undefined))
                                                        (set! k (list-ref temp_1 3))
                                                        (set! info (list-ref temp_1 2))
                                                        (set! v1 (list-ref temp_1 1))
                                                        (set! value1_reg (app-aexp v1 value1_reg info))
                                                        (set! k_reg k)
                                                        (set! pc apply-cont2))
                                                      (if (eq? (car temp_1) '<cont2-13>)
                                                          (let ((adatum 'undefined)
                                                                (info 'undefined)
                                                                (handler 'undefined)
                                                                (k 'undefined))
                                                            (set! k (list-ref temp_1 4))
                                                            (set! handler (list-ref temp_1 3))
                                                            (set! info (list-ref temp_1 2))
                                                            (set! adatum (list-ref temp_1 1))
                                                            (set! k_reg (make-cont2 '<cont2-12> value1_reg info k))
                                                            (set! fail_reg value2_reg)
                                                            (set! handler_reg handler)
                                                            (set! adatum-list_reg (cdr^ adatum))
                                                            (set! pc aparse-all))
                                                          (if (eq? (car temp_1) '<cont2-14>)
                                                              (let ((info 'undefined) (k 'undefined))
                                                                (set! k (list-ref temp_1 2))
                                                                (set! info (list-ref temp_1 1))
                                                                (set! value1_reg (choose-aexp value1_reg info))
                                                                (set! k_reg k)
                                                                (set! pc apply-cont2))
                                                              (if (eq? (car temp_1) '<cont2-15>)
                                                                  (let ((info 'undefined) (k 'undefined))
                                                                    (set! k (list-ref temp_1 2))
                                                                    (set! info (list-ref temp_1 1))
                                                                    (set! value1_reg (dict-aexp value1_reg info))
                                                                    (set! k_reg k)
                                                                    (set! pc apply-cont2))
                                                                  (if (eq? (car temp_1) '<cont2-16>)
                                                                      (let ((info 'undefined) (k 'undefined))
                                                                        (set! k (list-ref temp_1 2))
                                                                        (set! info (list-ref temp_1 1))
                                                                        (set! value1_reg (raise-aexp value1_reg info))
                                                                        (set! k_reg k)
                                                                        (set! pc apply-cont2))
                                                                      (if (eq? (car temp_1) '<cont2-17>)
                                                                          (let ((adatum 'undefined)
                                                                                (cexps 'undefined)
                                                                                (body 'undefined)
                                                                                (info 'undefined)
                                                                                (k 'undefined))
                                                                            (set! k (list-ref temp_1 5))
                                                                            (set! info (list-ref temp_1 4))
                                                                            (set! body (list-ref temp_1 3))
                                                                            (set! cexps (list-ref temp_1 2))
                                                                            (set! adatum (list-ref temp_1 1))
                                                                            (let ((cvar 'undefined))
                                                                              (set! cvar (get-sexp (catch-var^ (caddr^ adatum))))
                                                                              (set! value1_reg
                                                                                (try-catch-finally-aexp body cvar cexps value1_reg info))
                                                                              (set! k_reg k)
                                                                              (set! pc apply-cont2)))
                                                                          (if (eq? (car temp_1) '<cont2-18>)
                                                                              (let ((adatum 'undefined)
                                                                                    (body 'undefined)
                                                                                    (info 'undefined)
                                                                                    (handler 'undefined)
                                                                                    (k 'undefined))
                                                                                (set! k (list-ref temp_1 5))
                                                                                (set! handler (list-ref temp_1 4))
                                                                                (set! info (list-ref temp_1 3))
                                                                                (set! body (list-ref temp_1 2))
                                                                                (set! adatum (list-ref temp_1 1))
                                                                                (set! k_reg
                                                                                  (make-cont2 '<cont2-17> adatum value1_reg body info k))
                                                                                (set! fail_reg value2_reg)
                                                                                (set! handler_reg handler)
                                                                                (set! adatum-list_reg (finally-exps^ (cadddr^ adatum)))
                                                                                (set! pc aparse-all))
                                                                              (if (eq? (car temp_1) '<cont2-19>)
                                                                                  (let ((adatum 'undefined)
                                                                                        (info 'undefined)
                                                                                        (handler 'undefined)
                                                                                        (k 'undefined))
                                                                                    (set! k (list-ref temp_1 4))
                                                                                    (set! handler (list-ref temp_1 3))
                                                                                    (set! info (list-ref temp_1 2))
                                                                                    (set! adatum (list-ref temp_1 1))
                                                                                    (set! k_reg
                                                                                      (make-cont2 '<cont2-18> adatum value1_reg info handler k))
                                                                                    (set! fail_reg value2_reg)
                                                                                    (set! handler_reg handler)
                                                                                    (set! adatum-list_reg (catch-exps^ (caddr^ adatum)))
                                                                                    (set! pc aparse-all))
                                                                                  (if (eq? (car temp_1) '<cont2-20>)
                                                                                      (let ((body 'undefined) (info 'undefined) (k 'undefined))
                                                                                        (set! k (list-ref temp_1 3))
                                                                                        (set! info (list-ref temp_1 2))
                                                                                        (set! body (list-ref temp_1 1))
                                                                                        (set! value1_reg (try-finally-aexp body value1_reg info))
                                                                                        (set! k_reg k)
                                                                                        (set! pc apply-cont2))
                                                                                      (if (eq? (car temp_1) '<cont2-21>)
                                                                                          (let ((adatum 'undefined)
                                                                                                (info 'undefined)
                                                                                                (handler 'undefined)
                                                                                                (k 'undefined))
                                                                                            (set! k (list-ref temp_1 4))
                                                                                            (set! handler (list-ref temp_1 3))
                                                                                            (set! info (list-ref temp_1 2))
                                                                                            (set! adatum (list-ref temp_1 1))
                                                                                            (set! k_reg (make-cont2 '<cont2-20> value1_reg info k))
                                                                                            (set! fail_reg value2_reg)
                                                                                            (set! handler_reg handler)
                                                                                            (set! adatum-list_reg (finally-exps^ (caddr^ adatum)))
                                                                                            (set! pc aparse-all))
                                                                                          (if (eq? (car temp_1) '<cont2-22>)
                                                                                              (let ((adatum 'undefined)
                                                                                                    (body 'undefined)
                                                                                                    (info 'undefined)
                                                                                                    (k 'undefined))
                                                                                                (set! k (list-ref temp_1 4))
                                                                                                (set! info (list-ref temp_1 3))
                                                                                                (set! body (list-ref temp_1 2))
                                                                                                (set! adatum (list-ref temp_1 1))
                                                                                                (let ((cvar 'undefined))
                                                                                                  (set! cvar (get-sexp (catch-var^ (caddr^ adatum))))
                                                                                                  (set! value1_reg (try-catch-aexp body cvar value1_reg info))
                                                                                                  (set! k_reg k)
                                                                                                  (set! pc apply-cont2)))
                                                                                              (if (eq? (car temp_1) '<cont2-23>)
                                                                                                  (let ((adatum 'undefined)
                                                                                                        (info 'undefined)
                                                                                                        (handler 'undefined)
                                                                                                        (k 'undefined))
                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                    (set! info (list-ref temp_1 2))
                                                                                                    (set! adatum (list-ref temp_1 1))
                                                                                                    (set! k_reg
                                                                                                      (make-cont2 '<cont2-22> adatum value1_reg info k))
                                                                                                    (set! fail_reg value2_reg)
                                                                                                    (set! handler_reg handler)
                                                                                                    (set! adatum-list_reg (catch-exps^ (caddr^ adatum)))
                                                                                                    (set! pc aparse-all))
                                                                                                  (if (eq? (car temp_1) '<cont2-24>)
                                                                                                      (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                        (set! info (list-ref temp_1 2))
                                                                                                        (set! adatum (list-ref temp_1 1))
                                                                                                        (set! k_reg
                                                                                                          (make-cont '<cont-12> adatum value1_reg info value2_reg k))
                                                                                                        (set! x_reg (caddr^ adatum))
                                                                                                        (set! pc unannotate-cps))
                                                                                                      (if (eq? (car temp_1) '<cont2-25>)
                                                                                                          (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                            (set! info (list-ref temp_1 2))
                                                                                                            (set! adatum (list-ref temp_1 1))
                                                                                                            (set! k_reg
                                                                                                              (make-cont '<cont-13> value1_reg info value2_reg k))
                                                                                                            (set! x_reg (cadr^ adatum))
                                                                                                            (set! pc unannotate-cps))
                                                                                                          (if (eq? (car temp_1) '<cont2-26>)
                                                                                                              (let ((adatum 'undefined)
                                                                                                                    (info 'undefined)
                                                                                                                    (handler 'undefined)
                                                                                                                    (k 'undefined))
                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                (set! info (list-ref temp_1 2))
                                                                                                                (set! adatum (list-ref temp_1 1))
                                                                                                                (if (null? value1_reg)
                                                                                                                    (begin
                                                                                                                      (set! fail_reg value2_reg)
                                                                                                                      (set! handler_reg handler)
                                                                                                                      (set! adatum_reg adatum)
                                                                                                                      (set! msg_reg "bad concrete syntax:")
                                                                                                                      (set! pc aparse-error))
                                                                                                                    (if (null? (cdr value1_reg))
                                                                                                                        (begin
                                                                                                                          (set! value1_reg (car value1_reg))
                                                                                                                          (set! k_reg k)
                                                                                                                          (set! pc apply-cont2))
                                                                                                                        (begin
                                                                                                                          (set! value1_reg (begin-aexp value1_reg info))
                                                                                                                          (set! k_reg k)
                                                                                                                          (set! pc apply-cont2)))))
                                                                                                              (if (eq? (car temp_1) '<cont2-27>)
                                                                                                                  (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                    (set! info (list-ref temp_1 2))
                                                                                                                    (set! adatum (list-ref temp_1 1))
                                                                                                                    (set! value1_reg
                                                                                                                      (define!-aexp
                                                                                                                        (get-sexp (cadr^ adatum))
                                                                                                                        (get-sexp (caddr^ adatum))
                                                                                                                        value1_reg
                                                                                                                        info))
                                                                                                                    (set! k_reg k)
                                                                                                                    (set! pc apply-cont2))
                                                                                                                  (if (eq? (car temp_1) '<cont2-28>)
                                                                                                                      (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                        (set! info (list-ref temp_1 2))
                                                                                                                        (set! adatum (list-ref temp_1 1))
                                                                                                                        (set! value1_reg
                                                                                                                          (define!-aexp (get-sexp (cadr^ adatum)) "" value1_reg info))
                                                                                                                        (set! k_reg k)
                                                                                                                        (set! pc apply-cont2))
                                                                                                                      (if (eq? (car temp_1) '<cont2-29>)
                                                                                                                          (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                                            (set! info (list-ref temp_1 2))
                                                                                                                            (set! adatum (list-ref temp_1 1))
                                                                                                                            (set! value1_reg
                                                                                                                              (define-aexp
                                                                                                                                (get-sexp (cadr^ adatum))
                                                                                                                                (get-sexp (caddr^ adatum))
                                                                                                                                value1_reg
                                                                                                                                info))
                                                                                                                            (set! k_reg k)
                                                                                                                            (set! pc apply-cont2))
                                                                                                                          (if (eq? (car temp_1) '<cont2-30>)
                                                                                                                              (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                                (set! k (list-ref temp_1 3))
                                                                                                                                (set! info (list-ref temp_1 2))
                                                                                                                                (set! adatum (list-ref temp_1 1))
                                                                                                                                (set! value1_reg
                                                                                                                                  (define-aexp (get-sexp (cadr^ adatum)) "" value1_reg info))
                                                                                                                                (set! k_reg k)
                                                                                                                                (set! pc apply-cont2))
                                                                                                                              (if (eq? (car temp_1) '<cont2-31>)
                                                                                                                                  (let ((info 'undefined) (k 'undefined))
                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                    (set! info (list-ref temp_1 1))
                                                                                                                                    (set! value1_reg (func-aexp value1_reg info))
                                                                                                                                    (set! k_reg k)
                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                  (if (eq? (car temp_1) '<cont2-32>)
                                                                                                                                      (let ((adatum 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                        (set! info (list-ref temp_1 2))
                                                                                                                                        (set! adatum (list-ref temp_1 1))
                                                                                                                                        (let ((var-info 'undefined))
                                                                                                                                          (set! var-info (get-source-info (cadr^ adatum)))
                                                                                                                                          (set! value1_reg
                                                                                                                                            (assign-aexp
                                                                                                                                              (get-sexp (cadr^ adatum))
                                                                                                                                              value1_reg
                                                                                                                                              var-info
                                                                                                                                              info))
                                                                                                                                          (set! k_reg k)
                                                                                                                                          (set! pc apply-cont2)))
                                                                                                                                      (if (eq? (car temp_1) '<cont2-33>)
                                                                                                                                          (let ((v1 'undefined)
                                                                                                                                                (v2 'undefined)
                                                                                                                                                (info 'undefined)
                                                                                                                                                (k 'undefined))
                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                            (set! info (list-ref temp_1 3))
                                                                                                                                            (set! v2 (list-ref temp_1 2))
                                                                                                                                            (set! v1 (list-ref temp_1 1))
                                                                                                                                            (set! value1_reg (if-aexp v1 v2 value1_reg info))
                                                                                                                                            (set! k_reg k)
                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                          (if (eq? (car temp_1) '<cont2-34>)
                                                                                                                                              (let ((adatum 'undefined)
                                                                                                                                                    (v1 'undefined)
                                                                                                                                                    (info 'undefined)
                                                                                                                                                    (handler 'undefined)
                                                                                                                                                    (k 'undefined))
                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                (set! info (list-ref temp_1 3))
                                                                                                                                                (set! v1 (list-ref temp_1 2))
                                                                                                                                                (set! adatum (list-ref temp_1 1))
                                                                                                                                                (set! k_reg (make-cont2 '<cont2-33> v1 value1_reg info k))
                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                (set! adatum_reg (cadddr^ adatum))
                                                                                                                                                (set! pc aparse))
                                                                                                                                              (if (eq? (car temp_1) '<cont2-35>)
                                                                                                                                                  (let ((adatum 'undefined)
                                                                                                                                                        (info 'undefined)
                                                                                                                                                        (handler 'undefined)
                                                                                                                                                        (k 'undefined))
                                                                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                                                                    (set! info (list-ref temp_1 2))
                                                                                                                                                    (set! adatum (list-ref temp_1 1))
                                                                                                                                                    (set! k_reg
                                                                                                                                                      (make-cont2 '<cont2-34> adatum value1_reg info handler k))
                                                                                                                                                    (set! fail_reg value2_reg)
                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                    (set! adatum_reg (caddr^ adatum))
                                                                                                                                                    (set! pc aparse))
                                                                                                                                                  (if (eq? (car temp_1) '<cont2-36>)
                                                                                                                                                      (let ((v1 'undefined) (info 'undefined) (k 'undefined))
                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                        (set! info (list-ref temp_1 2))
                                                                                                                                                        (set! v1 (list-ref temp_1 1))
                                                                                                                                                        (set! value1_reg
                                                                                                                                                          (if-aexp v1 value1_reg (lit-aexp #f 'none) info))
                                                                                                                                                        (set! k_reg k)
                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                      (if (eq? (car temp_1) '<cont2-37>)
                                                                                                                                                          (let ((adatum 'undefined)
                                                                                                                                                                (info 'undefined)
                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                (k 'undefined))
                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                            (set! handler (list-ref temp_1 3))
                                                                                                                                                            (set! info (list-ref temp_1 2))
                                                                                                                                                            (set! adatum (list-ref temp_1 1))
                                                                                                                                                            (set! k_reg (make-cont2 '<cont2-36> value1_reg info k))
                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                            (set! adatum_reg (caddr^ adatum))
                                                                                                                                                            (set! pc aparse))
                                                                                                                                                          (if (eq? (car temp_1) '<cont2-38>)
                                                                                                                                                              (let ((handler 'undefined) (k 'undefined))
                                                                                                                                                                (set! k (list-ref temp_1 2))
                                                                                                                                                                (set! handler (list-ref temp_1 1))
                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                (set! adatum_reg value1_reg)
                                                                                                                                                                (set! pc aparse))
                                                                                                                                                              (if (eq? (car temp_1) '<cont2-39>)
                                                                                                                                                                  (let ((adatum 'undefined)
                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                    (set! adatum (list-ref temp_1 1))
                                                                                                                                                                    (if (pattern-macro? value1_reg)
                                                                                                                                                                        (begin
                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                          (set! fail_reg value2_reg)
                                                                                                                                                                          (set! handler_reg handler)
                                                                                                                                                                          (set! adatum_reg adatum)
                                                                                                                                                                          (set! aclauses_reg (macro-aclauses^ value1_reg))
                                                                                                                                                                          (set! clauses_reg (macro-clauses^ value1_reg))
                                                                                                                                                                          (set! pc process-macro-clauses^))
                                                                                                                                                                        (begin
                                                                                                                                                                          (set! k_reg (make-cont '<cont-22> adatum value2_reg k))
                                                                                                                                                                          (set! datum_reg adatum)
                                                                                                                                                                          (set! macro_reg value1_reg)
                                                                                                                                                                          (set! pc apply-macro))))
                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-40>)
                                                                                                                                                                      (let ((fail 'undefined) (k 'undefined))
                                                                                                                                                                        (set! k (list-ref temp_1 2))
                                                                                                                                                                        (set! fail (list-ref temp_1 1))
                                                                                                                                                                        (set! value1_reg value2_reg)
                                                                                                                                                                        (set! value2_reg fail)
                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-41>)
                                                                                                                                                                          (let ((a 'undefined) (k 'undefined))
                                                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                                                            (set! a (list-ref temp_1 1))
                                                                                                                                                                            (set! value1_reg (cons a value1_reg))
                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-42>)
                                                                                                                                                                              (let ((entries 'undefined)
                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                (set! k (list-ref temp_1 3))
                                                                                                                                                                                (set! handler (list-ref temp_1 2))
                                                                                                                                                                                (set! entries (list-ref temp_1 1))
                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-41> value1_reg k))
                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                (set! entries_reg (cdr entries))
                                                                                                                                                                                (set! pc aparse-entries))
                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-43>)
                                                                                                                                                                                  (let ((adatum-list 'undefined)
                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                                    (set! adatum-list (list-ref temp_1 1))
                                                                                                                                                                                    (set! k_reg (make-cont2 '<cont2-41> value1_reg k))
                                                                                                                                                                                    (set! fail_reg value2_reg)
                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                    (set! adatum-list_reg (cdr adatum-list))
                                                                                                                                                                                    (set! pc aparse-all))
                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-44>)
                                                                                                                                                                                      (let ((exp 'undefined) (k 'undefined))
                                                                                                                                                                                        (set! k (list-ref temp_1 2))
                                                                                                                                                                                        (set! exp (list-ref temp_1 1))
                                                                                                                                                                                        (set! value1_reg (cons exp value1_reg))
                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-45>)
                                                                                                                                                                                          (let ((src 'undefined)
                                                                                                                                                                                                (tokens-left 'undefined)
                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                            (set! handler (list-ref temp_1 3))
                                                                                                                                                                                            (set! tokens-left (list-ref temp_1 2))
                                                                                                                                                                                            (set! src (list-ref temp_1 1))
                                                                                                                                                                                            (set! k_reg (make-cont2 '<cont2-44> value1_reg k))
                                                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                            (set! src_reg src)
                                                                                                                                                                                            (set! tokens_reg tokens-left)
                                                                                                                                                                                            (set! pc aparse-sexps))
                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-46>)
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! *last-fail* value2_reg)
                                                                                                                                                                                                (set! final_reg value1_reg)
                                                                                                                                                                                                (set! pc #f))
                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-47>)
                                                                                                                                                                                                  (begin
                                                                                                                                                                                                    (set! k_reg REP-k)
                                                                                                                                                                                                    (set! fail_reg value2_reg)
                                                                                                                                                                                                    (set! handler_reg REP-handler)
                                                                                                                                                                                                    (set! env_reg toplevel-env)
                                                                                                                                                                                                    (set! exp_reg value1_reg)
                                                                                                                                                                                                    (set! pc m))
                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-48>)
                                                                                                                                                                                                      (begin (set! final_reg #t) (set! pc #f))
                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-49>)
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! k_reg (make-cont2 '<cont2-48>))
                                                                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                                                                            (set! handler_reg try-parse-handler)
                                                                                                                                                                                                            (set! src_reg 'stdin)
                                                                                                                                                                                                            (set! tokens_reg value1_reg)
                                                                                                                                                                                                            (set! pc aparse-sexps))
                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-50>)
                                                                                                                                                                                                              (let ((src 'undefined)
                                                                                                                                                                                                                    (tokens-left 'undefined)
                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                (set! tokens-left (list-ref temp_1 2))
                                                                                                                                                                                                                (set! src (list-ref temp_1 1))
                                                                                                                                                                                                                (if (token-type? (first tokens-left) 'end-marker)
                                                                                                                                                                                                                    (begin (set! k_reg k) (set! pc apply-cont2))
                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                      (set! k_reg k)
                                                                                                                                                                                                                      (set! fail_reg value2_reg)
                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                      (set! env_reg env)
                                                                                                                                                                                                                      (set! src_reg src)
                                                                                                                                                                                                                      (set! tokens_reg tokens-left)
                                                                                                                                                                                                                      (set! pc read-and-eval-asexps))))
                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-51>)
                                                                                                                                                                                                                  (let ((src 'undefined)
                                                                                                                                                                                                                        (tokens-left 'undefined)
                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                    (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                    (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                    (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                    (set! tokens-left (list-ref temp_1 2))
                                                                                                                                                                                                                    (set! src (list-ref temp_1 1))
                                                                                                                                                                                                                    (set! k_reg
                                                                                                                                                                                                                      (make-cont2 '<cont2-50> src tokens-left env handler k))
                                                                                                                                                                                                                    (set! fail_reg value2_reg)
                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                    (set! env_reg env)
                                                                                                                                                                                                                    (set! exp_reg value1_reg)
                                                                                                                                                                                                                    (set! pc m))
                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-52>)
                                                                                                                                                                                                                      (let ((exp 'undefined) (k 'undefined))
                                                                                                                                                                                                                        (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                        (set! exp (list-ref temp_1 1))
                                                                                                                                                                                                                        (handle-debug-info exp value1_reg)
                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-53>)
                                                                                                                                                                                                                          (let ((args 'undefined)
                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                (info 'undefined)
                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                            (set! info (list-ref temp_1 3))
                                                                                                                                                                                                                            (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                            (set! args (list-ref temp_1 1))
                                                                                                                                                                                                                            (if (dlr-exp? value1_reg)
                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                  (set! value1_reg (dlr-apply value1_reg args))
                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                  (set! pc apply-cont2))
                                                                                                                                                                                                                                (if (procedure-object? value1_reg)
                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                      (set! k2_reg k)
                                                                                                                                                                                                                                      (set! fail_reg value2_reg)
                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                      (set! info_reg info)
                                                                                                                                                                                                                                      (set! env2_reg env)
                                                                                                                                                                                                                                      (set! args_reg args)
                                                                                                                                                                                                                                      (set! proc_reg value1_reg)
                                                                                                                                                                                                                                      (set! pc apply-proc))
                                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                                      (set! fail_reg value2_reg)
                                                                                                                                                                                                                                      (set! handler_reg handler)
                                                                                                                                                                                                                                      (set! info_reg info)
                                                                                                                                                                                                                                      (set! msg_reg
                                                                                                                                                                                                                                        (format "attempt to apply non-procedure ~a" value1_reg))
                                                                                                                                                                                                                                      (set! pc runtime-error)))))
                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-54>)
                                                                                                                                                                                                                              (let ((operator 'undefined)
                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                    (info 'undefined)
                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                (set! info (list-ref temp_1 3))
                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                (set! operator (list-ref temp_1 1))
                                                                                                                                                                                                                                (set! k_reg
                                                                                                                                                                                                                                  (make-cont2 '<cont2-53> value1_reg env info handler k))
                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                (set! exp_reg operator)
                                                                                                                                                                                                                                (set! pc m))
                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-55>)
                                                                                                                                                                                                                                  (let ((k 'undefined))
                                                                                                                                                                                                                                    (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                    (set! value1_reg (binding-docstring value1_reg))
                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-56>)
                                                                                                                                                                                                                                      (let ((handler 'undefined))
                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                        (set! exception_reg value1_reg)
                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                        (set! pc apply-handler2))
                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-57>)
                                                                                                                                                                                                                                          (let ((v 'undefined) (k 'undefined))
                                                                                                                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                            (set! v (list-ref temp_1 1))
                                                                                                                                                                                                                                            (set! value1_reg v)
                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-58>)
                                                                                                                                                                                                                                              (let ((fexps 'undefined)
                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                (set! fexps (list-ref temp_1 1))
                                                                                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-57> value1_reg k))
                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                (set! exps_reg fexps)
                                                                                                                                                                                                                                                (set! pc eval-sequence))
                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-59>)
                                                                                                                                                                                                                                                  (let ((aclauses 'undefined)
                                                                                                                                                                                                                                                        (clauses 'undefined)
                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                    (set! clauses (list-ref temp_1 2))
                                                                                                                                                                                                                                                    (set! aclauses (list-ref temp_1 1))
                                                                                                                                                                                                                                                    (set-binding-value!
                                                                                                                                                                                                                                                      value1_reg
                                                                                                                                                                                                                                                      (make-pattern-macro^ clauses aclauses))
                                                                                                                                                                                                                                                    (set! value1_reg void-value)
                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-60>)
                                                                                                                                                                                                                                                      (let ((docstring 'undefined)
                                                                                                                                                                                                                                                            (var 'undefined)
                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                        (set! var (list-ref temp_1 2))
                                                                                                                                                                                                                                                        (set! docstring (list-ref temp_1 1))
                                                                                                                                                                                                                                                        (set-global-value! var value1_reg)
                                                                                                                                                                                                                                                        (set-global-docstring! var docstring)
                                                                                                                                                                                                                                                        (set! value1_reg void-value)
                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-61>)
                                                                                                                                                                                                                                                          (let ((docstring 'undefined)
                                                                                                                                                                                                                                                                (rhs-value 'undefined)
                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                            (set! rhs-value (list-ref temp_1 2))
                                                                                                                                                                                                                                                            (set! docstring (list-ref temp_1 1))
                                                                                                                                                                                                                                                            (set-binding-value! value1_reg rhs-value)
                                                                                                                                                                                                                                                            (set-binding-docstring! value1_reg docstring)
                                                                                                                                                                                                                                                            (set! value1_reg void-value)
                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-62>)
                                                                                                                                                                                                                                                              (let ((docstring 'undefined)
                                                                                                                                                                                                                                                                    (var 'undefined)
                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                (set! var (list-ref temp_1 2))
                                                                                                                                                                                                                                                                (set! docstring (list-ref temp_1 1))
                                                                                                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-61> docstring value1_reg k))
                                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                (set! var_reg var)
                                                                                                                                                                                                                                                                (set! pc lookup-binding-in-first-frame))
                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-63>)
                                                                                                                                                                                                                                                                  (let ((rhs-value 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                                                    (set! rhs-value (list-ref temp_1 1))
                                                                                                                                                                                                                                                                    (let ((old-value 'undefined))
                                                                                                                                                                                                                                                                      (set! old-value (binding-value value1_reg))
                                                                                                                                                                                                                                                                      (set-binding-value! value1_reg rhs-value)
                                                                                                                                                                                                                                                                      (let ((new-fail 'undefined))
                                                                                                                                                                                                                                                                        (set! new-fail
                                                                                                                                                                                                                                                                          (make-fail '<fail-2> value1_reg old-value value2_reg))
                                                                                                                                                                                                                                                                        (set! value2_reg new-fail)
                                                                                                                                                                                                                                                                        (set! value1_reg void-value)
                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                        (set! pc apply-cont2))))
                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-64>)
                                                                                                                                                                                                                                                                      (let ((var 'undefined)
                                                                                                                                                                                                                                                                            (var-info 'undefined)
                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                        (set! var-info (list-ref temp_1 2))
                                                                                                                                                                                                                                                                        (set! var (list-ref temp_1 1))
                                                                                                                                                                                                                                                                        (set! k_reg (make-cont2 '<cont2-63> value1_reg k))
                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                        (set! var-info_reg var-info)
                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                        (set! variable_reg var)
                                                                                                                                                                                                                                                                        (set! pc lookup-binding))
                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-65>)
                                                                                                                                                                                                                                                                          (let ((else-exp 'undefined)
                                                                                                                                                                                                                                                                                (then-exp 'undefined)
                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                            (set! then-exp (list-ref temp_1 2))
                                                                                                                                                                                                                                                                            (set! else-exp (list-ref temp_1 1))
                                                                                                                                                                                                                                                                            (if value1_reg
                                                                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                                                                  (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                  (set! handler_reg handler)
                                                                                                                                                                                                                                                                                  (set! env_reg env)
                                                                                                                                                                                                                                                                                  (set! exp_reg then-exp)
                                                                                                                                                                                                                                                                                  (set! pc m))
                                                                                                                                                                                                                                                                                (begin
                                                                                                                                                                                                                                                                                  (set! k_reg k)
                                                                                                                                                                                                                                                                                  (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                  (set! handler_reg handler)
                                                                                                                                                                                                                                                                                  (set! env_reg env)
                                                                                                                                                                                                                                                                                  (set! exp_reg else-exp)
                                                                                                                                                                                                                                                                                  (set! pc m))))
                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-66>)
                                                                                                                                                                                                                                                                              (let ((k 'undefined))
                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                (set! value1_reg (dlr-func value1_reg))
                                                                                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-67>)
                                                                                                                                                                                                                                                                                  (let ((e 'undefined) (handler 'undefined))
                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                    (set! e (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                    (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                    (set! exception_reg e)
                                                                                                                                                                                                                                                                                    (set! handler_reg handler)
                                                                                                                                                                                                                                                                                    (set! pc apply-handler2))
                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-68>)
                                                                                                                                                                                                                                                                                      (let ((k2 'undefined))
                                                                                                                                                                                                                                                                                        (set! k2 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                        (decrement-closure-depth)
                                                                                                                                                                                                                                                                                        (printf
                                                                                                                                                                                                                                                                                          "~sreturn: ~s~%"
                                                                                                                                                                                                                                                                                          (apply string-append (repeat " |" (get-closure-depth)))
                                                                                                                                                                                                                                                                                          value1_reg)
                                                                                                                                                                                                                                                                                        (set! k_reg k2)
                                                                                                                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-69>)
                                                                                                                                                                                                                                                                                          (let ((v1 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                            (set! v1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                            (set! value1_reg (cons v1 value1_reg))
                                                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-70>)
                                                                                                                                                                                                                                                                                              (let ((exps 'undefined)
                                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                (set! exps (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-69> value1_reg k))
                                                                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                                                (set! exps_reg (cdr exps))
                                                                                                                                                                                                                                                                                                (set! pc m*))
                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-71>)
                                                                                                                                                                                                                                                                                                  (let ((exps 'undefined)
                                                                                                                                                                                                                                                                                                        (env 'undefined)
                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                    (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                    (set! exps (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                    (if (null? (cdr exps))
                                                                                                                                                                                                                                                                                                        (begin (set! k_reg k) (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                                          (set! k_reg k)
                                                                                                                                                                                                                                                                                                          (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                          (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                          (set! env_reg env)
                                                                                                                                                                                                                                                                                                          (set! exps_reg (cdr exps))
                                                                                                                                                                                                                                                                                                          (set! pc eval-sequence))))
                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-72>)
                                                                                                                                                                                                                                                                                                      (let ((handler 'undefined) (k2 'undefined))
                                                                                                                                                                                                                                                                                                        (set! k2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                        (set! k_reg k2)
                                                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                        (set! env_reg toplevel-env)
                                                                                                                                                                                                                                                                                                        (set! exp_reg value1_reg)
                                                                                                                                                                                                                                                                                                        (set! pc m))
                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-73>)
                                                                                                                                                                                                                                                                                                          (let ((handler 'undefined) (k2 'undefined))
                                                                                                                                                                                                                                                                                                            (set! k2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                            (set! k_reg (make-cont4 '<cont4-11> handler k2))
                                                                                                                                                                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                            (set! src_reg 'stdin)
                                                                                                                                                                                                                                                                                                            (set! tokens_reg value1_reg)
                                                                                                                                                                                                                                                                                                            (set! pc read-asexp))
                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-74>)
                                                                                                                                                                                                                                                                                                              (let ((handler 'undefined) (k2 'undefined))
                                                                                                                                                                                                                                                                                                                (set! k2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                (set! k_reg (make-cont4 '<cont4-12> handler k2))
                                                                                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                (set! src_reg 'stdin)
                                                                                                                                                                                                                                                                                                                (set! tokens_reg value1_reg)
                                                                                                                                                                                                                                                                                                                (set! pc read-asexp))
                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-75>)
                                                                                                                                                                                                                                                                                                                  (let ((k 'undefined))
                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                    (if (null? load-stack)
                                                                                                                                                                                                                                                                                                                        (printf "WARNING: empty load-stack encountered!\n")
                                                                                                                                                                                                                                                                                                                        (set! load-stack (cdr load-stack)))
                                                                                                                                                                                                                                                                                                                    (set! value1_reg void-value)
                                                                                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-76>)
                                                                                                                                                                                                                                                                                                                      (let ((filename 'undefined)
                                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                        (set! filename (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                        (set! k_reg (make-cont2 '<cont2-75> k))
                                                                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                                        (set! src_reg filename)
                                                                                                                                                                                                                                                                                                                        (set! tokens_reg value1_reg)
                                                                                                                                                                                                                                                                                                                        (set! pc read-and-eval-asexps))
                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-77>)
                                                                                                                                                                                                                                                                                                                          (let ((filenames 'undefined)
                                                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                                                (info 'undefined)
                                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                            (set! info (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                            (set! filenames (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                            (set! info_reg info)
                                                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                                                            (set! filenames_reg (cdr filenames))
                                                                                                                                                                                                                                                                                                                            (set! pc load-files))
                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-78>)
                                                                                                                                                                                                                                                                                                                              (let ((filename 'undefined)
                                                                                                                                                                                                                                                                                                                                    (env2 'undefined)
                                                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                    (k2 'undefined))
                                                                                                                                                                                                                                                                                                                                (set! k2 (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                (set! env2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                (set! filename (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                (let ((module 'undefined))
                                                                                                                                                                                                                                                                                                                                  (set! module (extend env2 '() '()))
                                                                                                                                                                                                                                                                                                                                  (set-binding-value! value1_reg module)
                                                                                                                                                                                                                                                                                                                                  (set! k_reg k2)
                                                                                                                                                                                                                                                                                                                                  (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                  (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                  (set! info_reg 'none)
                                                                                                                                                                                                                                                                                                                                  (set! env_reg module)
                                                                                                                                                                                                                                                                                                                                  (set! filename_reg filename)
                                                                                                                                                                                                                                                                                                                                  (set! pc load-file)))
                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-79>)
                                                                                                                                                                                                                                                                                                                                  (let ((args 'undefined)
                                                                                                                                                                                                                                                                                                                                        (sym 'undefined)
                                                                                                                                                                                                                                                                                                                                        (info 'undefined)
                                                                                                                                                                                                                                                                                                                                        (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                    (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                    (set! info (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                    (set! sym (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                    (set! args (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                    (if (null? (cdr args))
                                                                                                                                                                                                                                                                                                                                        (begin (set! k_reg k) (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                        (if (not (environment? value1_reg))
                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                              (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                              (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                              (set! info_reg info)
                                                                                                                                                                                                                                                                                                                                              (set! msg_reg (format "invalid module ~a" sym))
                                                                                                                                                                                                                                                                                                                                              (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                                                                                              (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                              (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                              (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                              (set! info_reg info)
                                                                                                                                                                                                                                                                                                                                              (set! env_reg value1_reg)
                                                                                                                                                                                                                                                                                                                                              (set! args_reg (cdr args))
                                                                                                                                                                                                                                                                                                                                              (set! pc get-primitive)))))
                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-80>)
                                                                                                                                                                                                                                                                                                                                      (let ((iterator 'undefined)
                                                                                                                                                                                                                                                                                                                                            (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                        (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                        (set! iterator (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                        (set! iterator_reg iterator)
                                                                                                                                                                                                                                                                                                                                        (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                        (set! pc iterate-continue))
                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-81>)
                                                                                                                                                                                                                                                                                                                                          (let ((iterator 'undefined)
                                                                                                                                                                                                                                                                                                                                                (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                            (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                            (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                            (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                            (set! iterator (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                            (set! k_reg (make-cont2 '<cont2-69> value1_reg k))
                                                                                                                                                                                                                                                                                                                                            (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                            (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                            (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                            (set! iterator_reg iterator)
                                                                                                                                                                                                                                                                                                                                            (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                            (set! pc iterate-collect-continue))
                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-82>)
                                                                                                                                                                                                                                                                                                                                              (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                (set! k_reg (make-cont2 '<cont2-69> value1_reg k))
                                                                                                                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                (set! list1_reg (cdr list1))
                                                                                                                                                                                                                                                                                                                                                (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                                (set! pc map1))
                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-83>)
                                                                                                                                                                                                                                                                                                                                                  (let ((list1 'undefined) (proc 'undefined) (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                    (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                    (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                      (cons (dlr-apply proc (list (car list1))) value1_reg))
                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-84>)
                                                                                                                                                                                                                                                                                                                                                      (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                                                            (list2 'undefined)
                                                                                                                                                                                                                                                                                                                                                            (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 6))
                                                                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                        (set! proc (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                        (set! list2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                        (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                        (set! k_reg (make-cont2 '<cont2-69> value1_reg k))
                                                                                                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                        (set! list2_reg (cdr list2))
                                                                                                                                                                                                                                                                                                                                                        (set! list1_reg (cdr list1))
                                                                                                                                                                                                                                                                                                                                                        (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                                        (set! pc map2))
                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-85>)
                                                                                                                                                                                                                                                                                                                                                          (let ((list1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                (list2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                            (set! proc (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                            (set! list2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                            (set! list1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                            (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                              (cons
                                                                                                                                                                                                                                                                                                                                                                (dlr-apply proc (list (car list1) (car list2)))
                                                                                                                                                                                                                                                                                                                                                                value1_reg))
                                                                                                                                                                                                                                                                                                                                                            (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-86>)
                                                                                                                                                                                                                                                                                                                                                              (let ((lists 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                                                (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                (set! lists (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                  (make-cont2 '<cont2-69> value1_reg k))
                                                                                                                                                                                                                                                                                                                                                                (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                                                (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                                (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                                (set! lists_reg (map cdr lists))
                                                                                                                                                                                                                                                                                                                                                                (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                                                (set! pc mapN))
                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-87>)
                                                                                                                                                                                                                                                                                                                                                                  (let ((lists 'undefined)
                                                                                                                                                                                                                                                                                                                                                                        (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                                        (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                    (set! k (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                    (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                    (set! lists (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                                      (cons
                                                                                                                                                                                                                                                                                                                                                                        (dlr-apply proc (map car lists))
                                                                                                                                                                                                                                                                                                                                                                        value1_reg))
                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<cont2-88>)
                                                                                                                                                                                                                                                                                                                                                                      (let ((arg-list 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (proc 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (env 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (handler 'undefined)
                                                                                                                                                                                                                                                                                                                                                                            (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                        (set! k (list-ref temp_1 5))
                                                                                                                                                                                                                                                                                                                                                                        (set! handler (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                        (set! env (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                        (set! proc (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                        (set! arg-list (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                        (set! k_reg k)
                                                                                                                                                                                                                                                                                                                                                                        (set! fail_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                                                        (set! handler_reg handler)
                                                                                                                                                                                                                                                                                                                                                                        (set! env_reg env)
                                                                                                                                                                                                                                                                                                                                                                        (set! lists_reg (map cdr arg-list))
                                                                                                                                                                                                                                                                                                                                                                        (set! proc_reg proc)
                                                                                                                                                                                                                                                                                                                                                                        (set! pc for-each-primitive))
                                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<cont2-89>)
                                                                                                                                                                                                                                                                                                                                                                          (let ((new-acdr1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                (new-cdr1 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                (s-car 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                            (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                            (set! s-car (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                            (set! new-cdr1 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                            (set! new-acdr1 (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                            (set! k_reg
                                                                                                                                                                                                                                                                                                                                                                              (make-cont '<cont-46> s-car k))
                                                                                                                                                                                                                                                                                                                                                                            (set! ap2_reg value2_reg)
                                                                                                                                                                                                                                                                                                                                                                            (set! ap1_reg new-acdr1)
                                                                                                                                                                                                                                                                                                                                                                            (set! p2_reg value1_reg)
                                                                                                                                                                                                                                                                                                                                                                            (set! p1_reg new-cdr1)
                                                                                                                                                                                                                                                                                                                                                                            (set! pc unify-patterns^))
                                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<cont2-90>)
                                                                                                                                                                                                                                                                                                                                                                              (let ((apair2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                    (pair2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                    (s-car 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                    (k 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                                (set! s-car (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                                (set! pair2 (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                                (set! apair2
                                                                                                                                                                                                                                                                                                                                                                                  (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                                (set! k2_reg
                                                                                                                                                                                                                                                                                                                                                                                  (make-cont2 '<cont2-89>
                                                                                                                                                                                                                                                                                                                                                                                    value2_reg value1_reg s-car
                                                                                                                                                                                                                                                                                                                                                                                    k))
                                                                                                                                                                                                                                                                                                                                                                                (set! ap_reg (^cdr^ apair2))
                                                                                                                                                                                                                                                                                                                                                                                (set! s_reg s-car)
                                                                                                                                                                                                                                                                                                                                                                                (set! pattern_reg (cdr pair2))
                                                                                                                                                                                                                                                                                                                                                                                (set! pc instantiate^))
                                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<cont2-91>)
                                                                                                                                                                                                                                                                                                                                                                                  (let ((a 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                        (aa 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                        (ap 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                        (k2 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                    (set! k2
                                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                                    (set! ap
                                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                                    (set! aa
                                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                                    (set! a (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                                    (set! value2_reg
                                                                                                                                                                                                                                                                                                                                                                                      (cons^
                                                                                                                                                                                                                                                                                                                                                                                        aa
                                                                                                                                                                                                                                                                                                                                                                                        value2_reg
                                                                                                                                                                                                                                                                                                                                                                                        (get-source-info ap)))
                                                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                                                      (cons a value1_reg))
                                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k2)
                                                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                           '<cont2-92>)
                                                                                                                                                                                                                                                                                                                                                                                      (let ((ap 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                            (pattern 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                            (s 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                            (k2 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                        (set! k2
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 4))
                                                                                                                                                                                                                                                                                                                                                                                        (set! s
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 3))
                                                                                                                                                                                                                                                                                                                                                                                        (set! pattern
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 2))
                                                                                                                                                                                                                                                                                                                                                                                        (set! ap
                                                                                                                                                                                                                                                                                                                                                                                          (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                                        (set! k2_reg
                                                                                                                                                                                                                                                                                                                                                                                          (make-cont2 '<cont2-91>
                                                                                                                                                                                                                                                                                                                                                                                            value1_reg
                                                                                                                                                                                                                                                                                                                                                                                            value2_reg ap k2))
                                                                                                                                                                                                                                                                                                                                                                                        (set! ap_reg (^cdr^ ap))
                                                                                                                                                                                                                                                                                                                                                                                        (set! s_reg s)
                                                                                                                                                                                                                                                                                                                                                                                        (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                          (cdr pattern))
                                                                                                                                                                                                                                                                                                                                                                                        (set! pc instantiate^))
                                                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1)
                                                                                                                                                                                                                                                                                                                                                                                               '<cont2-93>)
                                                                                                                                                                                                                                                                                                                                                                                          (let ((s2 'undefined)
                                                                                                                                                                                                                                                                                                                                                                                                (k2 'undefined))
                                                                                                                                                                                                                                                                                                                                                                                            (set! k2
                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                2))
                                                                                                                                                                                                                                                                                                                                                                                            (set! s2
                                                                                                                                                                                                                                                                                                                                                                                              (list-ref
                                                                                                                                                                                                                                                                                                                                                                                                temp_1
                                                                                                                                                                                                                                                                                                                                                                                                1))
                                                                                                                                                                                                                                                                                                                                                                                            (set! k2_reg k2)
                                                                                                                                                                                                                                                                                                                                                                                            (set! ap_reg
                                                                                                                                                                                                                                                                                                                                                                                              value2_reg)
                                                                                                                                                                                                                                                                                                                                                                                            (set! s_reg s2)
                                                                                                                                                                                                                                                                                                                                                                                            (set! pattern_reg
                                                                                                                                                                                                                                                                                                                                                                                              value1_reg)
                                                                                                                                                                                                                                                                                                                                                                                            (set! pc
                                                                                                                                                                                                                                                                                                                                                                                              instantiate^))
                                                                                                                                                                                                                                                                                                                                                                                          (error 'apply-cont2
                                                                                                                                                                                                                                                                                                                                                                                            "bad continuation2: ~a"
                                                                                                                                                                                                                                                                                                                                                                                            k_reg)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define make-cont3
  (lambda args (return* (cons 'continuation3 args))))

(define*
  apply-cont3
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont3-1>)
          (let ((src 'undefined) (handler 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 3))
            (set! handler (list-ref temp_1 2))
            (set! src (list-ref temp_1 1))
            (if (token-type? value1_reg 'end-marker)
                (begin
                  (set! value2_reg value3_reg)
                  (set! value1_reg (list value1_reg))
                  (set! k_reg k)
                  (set! pc apply-cont2))
                (begin
                  (set! k_reg (make-cont2 '<cont2-1> value1_reg k))
                  (set! fail_reg value3_reg)
                  (set! handler_reg handler)
                  (set! src_reg src)
                  (set! chars_reg value2_reg)
                  (set! pc scan-input-loop))))
          (if (eq? (car temp_1) '<cont3-2>)
              (begin (set! final_reg value1_reg) (set! pc #f))
              (error 'apply-cont3 "bad continuation3: ~a" k_reg))))))

(define make-cont4
  (lambda args (return* (cons 'continuation4 args))))

(define*
  apply-cont4
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr k_reg))
      (if (eq? (car temp_1) '<cont4-1>)
          (let ((src 'undefined) (start 'undefined) (k 'undefined))
            (set! k (list-ref temp_1 3))
            (set! start (list-ref temp_1 2))
            (set! src (list-ref temp_1 1))
            (set! value1_reg
              (make-asexp src start value2_reg (list->vector value1_reg)))
            (set! k_reg k)
            (set! pc apply-cont4))
          (if (eq? (car temp_1) '<cont4-2>)
              (let ((src 'undefined) (start 'undefined) (k 'undefined))
                (set! k (list-ref temp_1 3))
                (set! start (list-ref temp_1 2))
                (set! src (list-ref temp_1 1))
                (set! value1_reg
                  (make-asexp src start value2_reg value1_reg))
                (set! k_reg k)
                (set! pc apply-cont4))
              (if (eq? (car temp_1) '<cont4-3>)
                  (let ((keyword 'undefined)
                        (keyword-end 'undefined)
                        (src 'undefined)
                        (start 'undefined)
                        (k 'undefined))
                    (set! k (list-ref temp_1 5))
                    (set! start (list-ref temp_1 4))
                    (set! src (list-ref temp_1 3))
                    (set! keyword-end (list-ref temp_1 2))
                    (set! keyword (list-ref temp_1 1))
                    (set! value1_reg
                      (make-asexp
                        src
                        start
                        value2_reg
                        (list
                          (make-asexp src start keyword-end keyword)
                          value1_reg)))
                    (set! k_reg k)
                    (set! pc apply-cont4))
                  (if (eq? (car temp_1) '<cont4-4>)
                      (let ((asexp1 'undefined) (k 'undefined))
                        (set! k (list-ref temp_1 2))
                        (set! asexp1 (list-ref temp_1 1))
                        (set! value1_reg (cons asexp1 value1_reg))
                        (set! k_reg k)
                        (set! pc apply-cont4))
                      (if (eq? (car temp_1) '<cont4-5>)
                          (let ((src 'undefined) (handler 'undefined) (k 'undefined))
                            (set! k (list-ref temp_1 3))
                            (set! handler (list-ref temp_1 2))
                            (set! src (list-ref temp_1 1))
                            (set! k_reg (make-cont4 '<cont4-4> value1_reg k))
                            (set! fail_reg value4_reg)
                            (set! handler_reg handler)
                            (set! src_reg src)
                            (set! tokens_reg value3_reg)
                            (set! pc read-avector-sequence))
                          (if (eq? (car temp_1) '<cont4-6>)
                              (let ((asexp1 'undefined)
                                    (expected-terminator 'undefined)
                                    (src 'undefined)
                                    (handler 'undefined)
                                    (k 'undefined))
                                (set! k (list-ref temp_1 5))
                                (set! handler (list-ref temp_1 4))
                                (set! src (list-ref temp_1 3))
                                (set! expected-terminator (list-ref temp_1 2))
                                (set! asexp1 (list-ref temp_1 1))
                                (if (or (null?^ value1_reg) (pair?^ value1_reg))
                                    (begin
                                      (set! k_reg k)
                                      (set! fail_reg value4_reg)
                                      (set! handler_reg handler)
                                      (set! src_reg src)
                                      (set! expected-terminator_reg expected-terminator)
                                      (set! tokens_reg value3_reg)
                                      (set! asexps_reg (cons asexp1 (get-sexp value1_reg)))
                                      (set! pc close-asexp-sequence))
                                    (begin
                                      (set! k_reg k)
                                      (set! fail_reg value4_reg)
                                      (set! handler_reg handler)
                                      (set! src_reg src)
                                      (set! expected-terminator_reg expected-terminator)
                                      (set! tokens_reg value3_reg)
                                      (set! asexps_reg (cons asexp1 value1_reg))
                                      (set! pc close-asexp-sequence))))
                              (if (eq? (car temp_1) '<cont4-7>)
                                  (let ((expected-terminator 'undefined)
                                        (src 'undefined)
                                        (handler 'undefined)
                                        (k 'undefined))
                                    (set! k (list-ref temp_1 4))
                                    (set! handler (list-ref temp_1 3))
                                    (set! src (list-ref temp_1 2))
                                    (set! expected-terminator (list-ref temp_1 1))
                                    (if (token-type? (first value3_reg) 'dot)
                                        (begin
                                          (set! k_reg
                                            (make-cont4 '<cont4-6> value1_reg expected-terminator src
                                              handler k))
                                          (set! fail_reg value4_reg)
                                          (set! handler_reg handler)
                                          (set! src_reg src)
                                          (set! tokens_reg (rest-of value3_reg))
                                          (set! pc read-asexp))
                                        (begin
                                          (set! k_reg (make-cont4 '<cont4-4> value1_reg k))
                                          (set! fail_reg value4_reg)
                                          (set! handler_reg handler)
                                          (set! src_reg src)
                                          (set! expected-terminator_reg expected-terminator)
                                          (set! tokens_reg value3_reg)
                                          (set! pc read-asexp-sequence))))
                                  (if (eq? (car temp_1) '<cont4-8>)
                                      (let ((src 'undefined) (handler 'undefined) (k 'undefined))
                                        (set! k (list-ref temp_1 3))
                                        (set! handler (list-ref temp_1 2))
                                        (set! src (list-ref temp_1 1))
                                        (set! k_reg
                                          (make-cont2 '<cont2-45> src value3_reg handler k))
                                        (set! fail_reg value4_reg)
                                        (set! handler_reg handler)
                                        (set! adatum_reg value1_reg)
                                        (set! pc aparse))
                                      (if (eq? (car temp_1) '<cont4-9>)
                                          (begin
                                            (set! *tokens-left* value3_reg)
                                            (set! k_reg (make-cont2 '<cont2-47>))
                                            (set! fail_reg value4_reg)
                                            (set! handler_reg REP-handler)
                                            (set! adatum_reg value1_reg)
                                            (set! pc aparse))
                                          (if (eq? (car temp_1) '<cont4-10>)
                                              (let ((src 'undefined)
                                                    (env 'undefined)
                                                    (handler 'undefined)
                                                    (k 'undefined))
                                                (set! k (list-ref temp_1 4))
                                                (set! handler (list-ref temp_1 3))
                                                (set! env (list-ref temp_1 2))
                                                (set! src (list-ref temp_1 1))
                                                (set! k_reg
                                                  (make-cont2 '<cont2-51> src value3_reg env handler k))
                                                (set! fail_reg value4_reg)
                                                (set! handler_reg handler)
                                                (set! adatum_reg value1_reg)
                                                (set! pc aparse))
                                              (if (eq? (car temp_1) '<cont4-11>)
                                                  (let ((handler 'undefined) (k2 'undefined))
                                                    (set! k2 (list-ref temp_1 2))
                                                    (set! handler (list-ref temp_1 1))
                                                    (if (token-type? (first value3_reg) 'end-marker)
                                                        (begin
                                                          (set! k_reg k2)
                                                          (set! fail_reg value4_reg)
                                                          (set! handler_reg handler)
                                                          (set! adatum_reg value1_reg)
                                                          (set! pc aparse))
                                                        (begin
                                                          (set! fail_reg value4_reg)
                                                          (set! handler_reg handler)
                                                          (set! src_reg 'stdin)
                                                          (set! tokens_reg value3_reg)
                                                          (set! msg_reg "tokens left over")
                                                          (set! pc read-error))))
                                                  (if (eq? (car temp_1) '<cont4-12>)
                                                      (let ((handler 'undefined) (k2 'undefined))
                                                        (set! k2 (list-ref temp_1 2))
                                                        (set! handler (list-ref temp_1 1))
                                                        (if (token-type? (first value3_reg) 'end-marker)
                                                            (begin
                                                              (set! value2_reg value4_reg)
                                                              (set! k_reg k2)
                                                              (set! pc apply-cont2))
                                                            (begin
                                                              (set! fail_reg value4_reg)
                                                              (set! handler_reg handler)
                                                              (set! src_reg 'stdin)
                                                              (set! tokens_reg value3_reg)
                                                              (set! msg_reg "tokens left over")
                                                              (set! pc read-error))))
                                                      (error 'apply-cont4
                                                        "bad continuation4: ~a"
                                                        k_reg))))))))))))))))

(define make-fail
  (lambda args (return* (cons 'fail-continuation args))))

(define*
  apply-fail
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr fail_reg))
      (if (eq? (car temp_1) '<fail-1>)
          (begin (set! final_reg "no more choices") (set! pc #f))
          (if (eq? (car temp_1) '<fail-2>)
              (let ((binding 'undefined)
                    (old-value 'undefined)
                    (fail 'undefined))
                (set! fail (list-ref temp_1 3))
                (set! old-value (list-ref temp_1 2))
                (set! binding (list-ref temp_1 1))
                (set-binding-value! binding old-value)
                (set! fail_reg fail)
                (set! pc apply-fail))
              (if (eq? (car temp_1) '<fail-3>)
                  (let ((exps 'undefined)
                        (env 'undefined)
                        (handler 'undefined)
                        (fail 'undefined)
                        (k 'undefined))
                    (set! k (list-ref temp_1 5))
                    (set! fail (list-ref temp_1 4))
                    (set! handler (list-ref temp_1 3))
                    (set! env (list-ref temp_1 2))
                    (set! exps (list-ref temp_1 1))
                    (set! k_reg k)
                    (set! fail_reg fail)
                    (set! handler_reg handler)
                    (set! env_reg env)
                    (set! exps_reg (cdr exps))
                    (set! pc eval-choices))
                  (error 'apply-fail
                    "bad fail-continuation: ~a"
                    fail_reg)))))))

(define make-handler
  (lambda args (return* (cons 'handler args))))

(define*
  apply-handler
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr handler_reg))
      (if (eq? (car temp_1) '<handler-1>)
          (begin
            (set! final_reg (list 'exception exception_reg))
            (set! pc #f))
          (error 'apply-handler "bad handler: ~a" handler_reg)))))

(define make-handler2
  (lambda args (return* (cons 'handler2 args))))

(define*
  apply-handler2
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr handler_reg))
      (if (eq? (car temp_1) '<handler2-1>)
          (begin
            (set! final_reg (list 'exception exception_reg))
            (set! pc #f))
          (if (eq? (car temp_1) '<handler2-2>)
              (begin
                (set! *last-fail* fail_reg)
                (set! final_reg (list 'exception exception_reg))
                (set! pc #f))
              (if (eq? (car temp_1) '<handler2-3>)
                  (begin (set! final_reg #f) (set! pc #f))
                  (if (eq? (car temp_1) '<handler2-4>)
                      (let ((cexps 'undefined)
                            (cvar 'undefined)
                            (env 'undefined)
                            (handler 'undefined)
                            (k 'undefined))
                        (set! k (list-ref temp_1 5))
                        (set! handler (list-ref temp_1 4))
                        (set! env (list-ref temp_1 3))
                        (set! cvar (list-ref temp_1 2))
                        (set! cexps (list-ref temp_1 1))
                        (let ((new-env 'undefined))
                          (set! new-env (extend env (list cvar) (list exception_reg)))
                          (set! k_reg k)
                          (set! handler_reg handler)
                          (set! env_reg new-env)
                          (set! exps_reg cexps)
                          (set! pc eval-sequence)))
                      (if (eq? (car temp_1) '<handler2-5>)
                          (let ((fexps 'undefined)
                                (env 'undefined)
                                (handler 'undefined))
                            (set! handler (list-ref temp_1 3))
                            (set! env (list-ref temp_1 2))
                            (set! fexps (list-ref temp_1 1))
                            (set! k_reg (make-cont2 '<cont2-67> exception_reg handler))
                            (set! handler_reg handler)
                            (set! env_reg env)
                            (set! exps_reg fexps)
                            (set! pc eval-sequence))
                          (if (eq? (car temp_1) '<handler2-6>)
                              (let ((cexps 'undefined)
                                    (cvar 'undefined)
                                    (fexps 'undefined)
                                    (env 'undefined)
                                    (handler 'undefined)
                                    (k 'undefined))
                                (set! k (list-ref temp_1 6))
                                (set! handler (list-ref temp_1 5))
                                (set! env (list-ref temp_1 4))
                                (set! fexps (list-ref temp_1 3))
                                (set! cvar (list-ref temp_1 2))
                                (set! cexps (list-ref temp_1 1))
                                (let ((new-env 'undefined))
                                  (set! new-env (extend env (list cvar) (list exception_reg)))
                                  (let ((catch-handler 'undefined))
                                    (set! catch-handler (try-finally-handler fexps env handler))
                                    (set! k_reg (make-cont2 '<cont2-58> fexps env handler k))
                                    (set! handler_reg catch-handler)
                                    (set! env_reg new-env)
                                    (set! exps_reg cexps)
                                    (set! pc eval-sequence))))
                              (error 'apply-handler2
                                "bad handler2: ~a"
                                handler_reg))))))))))

(define make-proc
  (lambda args (return* (cons 'procedure args))))

(define*
  apply-proc
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr proc_reg))
      (if (eq? (car temp_1) '<proc-1>)
          (let ((bodies 'undefined)
                (name 'undefined)
                (formals 'undefined)
                (env 'undefined))
            (set! env (list-ref temp_1 4))
            (set! formals (list-ref temp_1 3))
            (set! name (list-ref temp_1 2))
            (set! bodies (list-ref temp_1 1))
            (if (= (length args_reg) (length formals))
                (begin
                  (printf
                    "~scall: ~s~%"
                    (apply string-append (repeat " |" (get-closure-depth)))
                    (cons name args_reg))
                  (increment-closure-depth)
                  (set! k_reg (make-cont2 '<cont2-68> k2_reg))
                  (set! env_reg (extend env formals args_reg))
                  (set! exps_reg bodies)
                  (set! pc eval-sequence))
                (begin
                  (set! msg_reg
                    "incorrect number of arguments in application")
                  (set! pc runtime-error))))
          (if (eq? (car temp_1) '<proc-2>)
              (let ((bodies 'undefined)
                    (name 'undefined)
                    (formals 'undefined)
                    (runt 'undefined)
                    (env 'undefined))
                (set! env (list-ref temp_1 5))
                (set! runt (list-ref temp_1 4))
                (set! formals (list-ref temp_1 3))
                (set! name (list-ref temp_1 2))
                (set! bodies (list-ref temp_1 1))
                (if (>= (length args_reg) (length formals))
                    (let ((new-env 'undefined))
                      (set! new-env
                        (extend
                          env
                          (cons runt formals)
                          (cons
                            (list-tail args_reg (length formals))
                            (list-head args_reg (length formals)))))
                      (printf
                        "~scall: ~s~%"
                        (apply string-append (repeat " |" (get-closure-depth)))
                        (cons name args_reg))
                      (increment-closure-depth)
                      (set! k_reg (make-cont2 '<cont2-68> k2_reg))
                      (set! env_reg new-env)
                      (set! exps_reg bodies)
                      (set! pc eval-sequence))
                    (begin
                      (set! msg_reg "not enough arguments in application")
                      (set! pc runtime-error))))
              (if (eq? (car temp_1) '<proc-3>)
                  (let ((bodies 'undefined)
                        (formals 'undefined)
                        (env 'undefined))
                    (set! env (list-ref temp_1 3))
                    (set! formals (list-ref temp_1 2))
                    (set! bodies (list-ref temp_1 1))
                    (if (= (length args_reg) (length formals))
                        (begin
                          (set! k_reg k2_reg)
                          (set! env_reg (extend env formals args_reg))
                          (set! exps_reg bodies)
                          (set! pc eval-sequence))
                        (begin
                          (set! msg_reg
                            "incorrect number of arguments in application")
                          (set! pc runtime-error))))
                  (if (eq? (car temp_1) '<proc-4>)
                      (let ((bodies 'undefined)
                            (formals 'undefined)
                            (runt 'undefined)
                            (env 'undefined))
                        (set! env (list-ref temp_1 4))
                        (set! runt (list-ref temp_1 3))
                        (set! formals (list-ref temp_1 2))
                        (set! bodies (list-ref temp_1 1))
                        (if (>= (length args_reg) (length formals))
                            (let ((new-env 'undefined))
                              (set! new-env
                                (extend
                                  env
                                  (cons runt formals)
                                  (cons
                                    (list-tail args_reg (length formals))
                                    (list-head args_reg (length formals)))))
                              (set! k_reg k2_reg)
                              (set! env_reg new-env)
                              (set! exps_reg bodies)
                              (set! pc eval-sequence))
                            (begin
                              (set! msg_reg "not enough arguments in application")
                              (set! pc runtime-error))))
                      (if (eq? (car temp_1) '<proc-5>)
                          (begin
                            (set! value2_reg fail_reg)
                            (set! value1_reg void-value)
                            (set! k_reg k2_reg)
                            (set! pc apply-cont2))
                          (if (eq? (car temp_1) '<proc-6>)
                              (begin (set! final_reg end-of-session) (set! pc #f))
                              (if (eq? (car temp_1) '<proc-7>)
                                  (begin
                                    (set! k_reg
                                      (make-cont '<cont-36> handler_reg fail_reg k2_reg))
                                    (set! x_reg (car args_reg))
                                    (set! pc reannotate-cps))
                                  (if (eq? (car temp_1) '<proc-8>)
                                      (begin
                                        (set! k_reg
                                          (make-cont '<cont-37> handler_reg fail_reg k2_reg))
                                        (set! x_reg (car args_reg))
                                        (set! pc reannotate-cps))
                                      (if (eq? (car temp_1) '<proc-9>)
                                          (if (not (length-one? args_reg))
                                              (begin
                                                (set! msg_reg
                                                  "incorrect number of arguments to string-length")
                                                (set! pc runtime-error))
                                              (if (not (string? (car args_reg)))
                                                  (begin
                                                    (set! msg_reg "string-length called on non-string argument")
                                                    (set! pc runtime-error))
                                                  (begin
                                                    (set! value2_reg fail_reg)
                                                    (set! value1_reg (apply string-length args_reg))
                                                    (set! k_reg k2_reg)
                                                    (set! pc apply-cont2))))
                                          (if (eq? (car temp_1) '<proc-10>)
                                              (if (not (length-two? args_reg))
                                                  (begin
                                                    (set! msg_reg "incorrect number of arguments to string-ref")
                                                    (set! pc runtime-error))
                                                  (if (not (string? (car args_reg)))
                                                      (begin
                                                        (set! msg_reg
                                                          "string-ref called with non-string first argument")
                                                        (set! pc runtime-error))
                                                      (if (not (number? (cadr args_reg)))
                                                          (begin
                                                            (set! msg_reg
                                                              "string-ref called with non-numberic second argument")
                                                            (set! pc runtime-error))
                                                          (begin
                                                            (set! value2_reg fail_reg)
                                                            (set! value1_reg (apply string-ref args_reg))
                                                            (set! k_reg k2_reg)
                                                            (set! pc apply-cont2)))))
                                              (if (eq? (car temp_1) '<proc-11>)
                                                  (begin
                                                    (set! value2_reg fail_reg)
                                                    (set! value1_reg (aunparse (car args_reg)))
                                                    (set! k_reg k2_reg)
                                                    (set! pc apply-cont2))
                                                  (if (eq? (car temp_1) '<proc-12>)
                                                      (begin
                                                        (set! k_reg (make-cont2 '<cont2-73> handler_reg k2_reg))
                                                        (set! src_reg 'stdin)
                                                        (set! input_reg (car args_reg))
                                                        (set! pc scan-input))
                                                      (if (eq? (car temp_1) '<proc-13>)
                                                          (begin
                                                            (set! k_reg (make-cont2 '<cont2-74> handler_reg k2_reg))
                                                            (set! src_reg 'stdin)
                                                            (set! input_reg (car args_reg))
                                                            (set! pc scan-input))
                                                          (if (eq? (car temp_1) '<proc-14>)
                                                              (let ((proc 'undefined) (proc-args 'undefined))
                                                                (set! proc-args (cadr args_reg))
                                                                (set! proc (car args_reg))
                                                                (set! args_reg proc-args)
                                                                (set! proc_reg proc)
                                                                (set! pc apply-proc))
                                                              (if (eq? (car temp_1) '<proc-15>)
                                                                  (if (not (length-one? args_reg))
                                                                      (begin
                                                                        (set! msg_reg "incorrect number of arguments to sqrt")
                                                                        (set! pc runtime-error))
                                                                      (if (not (all-numeric? args_reg))
                                                                          (begin
                                                                            (set! msg_reg "sqrt called on non-numeric argument(s)")
                                                                            (set! pc runtime-error))
                                                                          (begin
                                                                            (set! value2_reg fail_reg)
                                                                            (set! value1_reg (apply sqrt args_reg))
                                                                            (set! k_reg k2_reg)
                                                                            (set! pc apply-cont2))))
                                                                  (if (eq? (car temp_1) '<proc-16>)
                                                                      (begin
                                                                        (for-each safe-print args_reg)
                                                                        (set! value2_reg fail_reg)
                                                                        (set! value1_reg void-value)
                                                                        (set! k_reg k2_reg)
                                                                        (set! pc apply-cont2))
                                                                      (if (eq? (car temp_1) '<proc-17>)
                                                                          (begin
                                                                            (set! value2_reg fail_reg)
                                                                            (set! value1_reg (apply char->string args_reg))
                                                                            (set! k_reg k2_reg)
                                                                            (set! pc apply-cont2))
                                                                          (if (eq? (car temp_1) '<proc-18>)
                                                                              (begin
                                                                                (set! value2_reg fail_reg)
                                                                                (set! value1_reg
                                                                                  (substring (car args_reg) (cadr args_reg) (caddr args_reg)))
                                                                                (set! k_reg k2_reg)
                                                                                (set! pc apply-cont2))
                                                                              (if (eq? (car temp_1) '<proc-19>)
                                                                                  (begin
                                                                                    (set! value2_reg fail_reg)
                                                                                    (set! value1_reg (number->string (car args_reg)))
                                                                                    (set! k_reg k2_reg)
                                                                                    (set! pc apply-cont2))
                                                                                  (if (eq? (car temp_1) '<proc-20>)
                                                                                      (begin
                                                                                        (set! value2_reg fail_reg)
                                                                                        (set! value1_reg (assv (car args_reg) (cadr args_reg)))
                                                                                        (set! k_reg k2_reg)
                                                                                        (set! pc apply-cont2))
                                                                                      (if (eq? (car temp_1) '<proc-21>)
                                                                                          (begin
                                                                                            (set! value2_reg fail_reg)
                                                                                            (set! value1_reg (memv (car args_reg) (cadr args_reg)))
                                                                                            (set! k_reg k2_reg)
                                                                                            (set! pc apply-cont2))
                                                                                          (if (eq? (car temp_1) '<proc-22>)
                                                                                              (let ((s 'undefined))
                                                                                                (set! s (format "~a" (car args_reg)))
                                                                                                (set! *need-newline* (true? (not (ends-with-newline? s))))
                                                                                                (display s)
                                                                                                (set! value2_reg fail_reg)
                                                                                                (set! value1_reg void-value)
                                                                                                (set! k_reg k2_reg)
                                                                                                (set! pc apply-cont2))
                                                                                              (if (eq? (car temp_1) '<proc-23>)
                                                                                                  (begin
                                                                                                    (set! *need-newline* #f)
                                                                                                    (newline)
                                                                                                    (set! value2_reg fail_reg)
                                                                                                    (set! value1_reg void-value)
                                                                                                    (set! k_reg k2_reg)
                                                                                                    (set! pc apply-cont2))
                                                                                                  (if (eq? (car temp_1) '<proc-24>)
                                                                                                      (if (not (length-one? args_reg))
                                                                                                          (begin
                                                                                                            (set! msg_reg "incorrect number of arguments to load")
                                                                                                            (set! pc runtime-error))
                                                                                                          (begin
                                                                                                            (set! k_reg k2_reg)
                                                                                                            (set! env_reg toplevel-env)
                                                                                                            (set! filename_reg (car args_reg))
                                                                                                            (set! pc load-file)))
                                                                                                      (if (eq? (car temp_1) '<proc-25>)
                                                                                                          (if (not (length-one? args_reg))
                                                                                                              (begin
                                                                                                                (set! msg_reg "incorrect number of arguments to length")
                                                                                                                (set! pc runtime-error))
                                                                                                              (begin
                                                                                                                (set! ls_reg (car args_reg))
                                                                                                                (set! sum_reg 0)
                                                                                                                (set! x_reg (car args_reg))
                                                                                                                (set! pc length-loop)))
                                                                                                          (if (eq? (car temp_1) '<proc-26>)
                                                                                                              (if (not (length-one? args_reg))
                                                                                                                  (begin
                                                                                                                    (set! msg_reg
                                                                                                                      (format
                                                                                                                        "incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument"
                                                                                                                        args_reg))
                                                                                                                    (set! pc runtime-error))
                                                                                                                  (begin
                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                    (set! value1_reg (apply symbol? args_reg))
                                                                                                                    (set! k_reg k2_reg)
                                                                                                                    (set! pc apply-cont2)))
                                                                                                              (if (eq? (car temp_1) '<proc-27>)
                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                      (begin
                                                                                                                        (set! msg_reg "incorrect number of arguments to number?")
                                                                                                                        (set! pc runtime-error))
                                                                                                                      (begin
                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                        (set! value1_reg (apply number? args_reg))
                                                                                                                        (set! k_reg k2_reg)
                                                                                                                        (set! pc apply-cont2)))
                                                                                                                  (if (eq? (car temp_1) '<proc-28>)
                                                                                                                      (if (not (length-one? args_reg))
                                                                                                                          (begin
                                                                                                                            (set! msg_reg "incorrect number of arguments to boolean?")
                                                                                                                            (set! pc runtime-error))
                                                                                                                          (begin
                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                            (set! value1_reg (apply boolean? args_reg))
                                                                                                                            (set! k_reg k2_reg)
                                                                                                                            (set! pc apply-cont2)))
                                                                                                                      (if (eq? (car temp_1) '<proc-29>)
                                                                                                                          (if (not (length-one? args_reg))
                                                                                                                              (begin
                                                                                                                                (set! msg_reg "incorrect number of arguments to string?")
                                                                                                                                (set! pc runtime-error))
                                                                                                                              (begin
                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                (set! value1_reg (apply string? args_reg))
                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                (set! pc apply-cont2)))
                                                                                                                          (if (eq? (car temp_1) '<proc-30>)
                                                                                                                              (if (not (length-one? args_reg))
                                                                                                                                  (begin
                                                                                                                                    (set! msg_reg "incorrect number of arguments to char?")
                                                                                                                                    (set! pc runtime-error))
                                                                                                                                  (begin
                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                    (set! value1_reg (apply char? args_reg))
                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                              (if (eq? (car temp_1) '<proc-31>)
                                                                                                                                  (if (not (length-two? args_reg))
                                                                                                                                      (begin
                                                                                                                                        (set! msg_reg "incorrect number of arguments to char=?")
                                                                                                                                        (set! pc runtime-error))
                                                                                                                                      (begin
                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                        (set! value1_reg (apply char=? args_reg))
                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                  (if (eq? (car temp_1) '<proc-32>)
                                                                                                                                      (if (not (length-one? args_reg))
                                                                                                                                          (begin
                                                                                                                                            (set! msg_reg
                                                                                                                                              "incorrect number of arguments to char-whitespace?")
                                                                                                                                            (set! pc runtime-error))
                                                                                                                                          (begin
                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                            (set! value1_reg (apply char-whitespace? args_reg))
                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                            (set! pc apply-cont2)))
                                                                                                                                      (if (eq? (car temp_1) '<proc-33>)
                                                                                                                                          (if (not (length-one? args_reg))
                                                                                                                                              (begin
                                                                                                                                                (set! msg_reg
                                                                                                                                                  "incorrect number of arguments to char-alphabetic?")
                                                                                                                                                (set! pc runtime-error))
                                                                                                                                              (begin
                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                (set! value1_reg (apply char-alphabetic? args_reg))
                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                (set! pc apply-cont2)))
                                                                                                                                          (if (eq? (car temp_1) '<proc-34>)
                                                                                                                                              (if (not (length-one? args_reg))
                                                                                                                                                  (begin
                                                                                                                                                    (set! msg_reg
                                                                                                                                                      "incorrect number of arguments to char-numeric?")
                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                  (begin
                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                    (set! value1_reg (apply char-numeric? args_reg))
                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                              (if (eq? (car temp_1) '<proc-35>)
                                                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                                                      (begin
                                                                                                                                                        (set! msg_reg "incorrect number of arguments to null?")
                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                      (begin
                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                        (set! value1_reg (apply null? args_reg))
                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                                  (if (eq? (car temp_1) '<proc-36>)
                                                                                                                                                      (if (not (length-one? args_reg))
                                                                                                                                                          (begin
                                                                                                                                                            (set! msg_reg "incorrect number of arguments to pair?")
                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                          (begin
                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                            (set! value1_reg (apply pair? args_reg))
                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                            (set! pc apply-cont2)))
                                                                                                                                                      (if (eq? (car temp_1) '<proc-37>)
                                                                                                                                                          (if (not (length-two? args_reg))
                                                                                                                                                              (begin
                                                                                                                                                                (set! msg_reg "incorrect number of arguments to cons")
                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                              (begin
                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                (set! value1_reg (apply cons args_reg))
                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                (set! pc apply-cont2)))
                                                                                                                                                          (if (eq? (car temp_1) '<proc-38>)
                                                                                                                                                              (if (not (pair? (car args_reg)))
                                                                                                                                                                  (begin
                                                                                                                                                                    (set! msg_reg
                                                                                                                                                                      (format "car called on non-pair ~s" (car args_reg)))
                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                  (begin
                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                    (set! value1_reg (apply car args_reg))
                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                                              (if (eq? (car temp_1) '<proc-39>)
                                                                                                                                                                  (if (not (pair? (car args_reg)))
                                                                                                                                                                      (begin
                                                                                                                                                                        (set! msg_reg
                                                                                                                                                                          (format "cdr called on non-pair ~s" (car args_reg)))
                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                      (begin
                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                        (set! value1_reg (apply cdr args_reg))
                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                                                  (if (eq? (car temp_1) '<proc-40>)
                                                                                                                                                                      (if (not (length-at-least? 2 (car args_reg)))
                                                                                                                                                                          (begin
                                                                                                                                                                            (set! msg_reg
                                                                                                                                                                              (format
                                                                                                                                                                                "cadr called on incorrect list structure ~s"
                                                                                                                                                                                (car args_reg)))
                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                          (begin
                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                            (set! value1_reg (apply cadr args_reg))
                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                            (set! pc apply-cont2)))
                                                                                                                                                                      (if (eq? (car temp_1) '<proc-41>)
                                                                                                                                                                          (if (not (length-at-least? 3 (car args_reg)))
                                                                                                                                                                              (begin
                                                                                                                                                                                (set! msg_reg
                                                                                                                                                                                  (format
                                                                                                                                                                                    "caddr called on incorrect list structure ~s"
                                                                                                                                                                                    (car args_reg)))
                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                              (begin
                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                (set! value1_reg (apply caddr args_reg))
                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                (set! pc apply-cont2)))
                                                                                                                                                                          (if (eq? (car temp_1) '<proc-42>)
                                                                                                                                                                              (begin
                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                (set! value1_reg args_reg)
                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                              (if (eq? (car temp_1) '<proc-43>)
                                                                                                                                                                                  (if (not (all-numeric? args_reg))
                                                                                                                                                                                      (begin
                                                                                                                                                                                        (set! msg_reg "+ called on non-numeric argument(s)")
                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                      (begin
                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                        (set! value1_reg (apply + args_reg))
                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-44>)
                                                                                                                                                                                      (if (null? args_reg)
                                                                                                                                                                                          (begin
                                                                                                                                                                                            (set! msg_reg "incorrect number of arguments to -")
                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                          (if (not (all-numeric? args_reg))
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! msg_reg "- called on non-numeric argument(s)")
                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                (set! value1_reg (apply - args_reg))
                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                (set! pc apply-cont2))))
                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-45>)
                                                                                                                                                                                          (if (not (all-numeric? args_reg))
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! msg_reg "* called on non-numeric argument(s)")
                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                (set! value1_reg (apply * args_reg))
                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                (set! pc apply-cont2)))
                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-46>)
                                                                                                                                                                                              (if (null? args_reg)
                                                                                                                                                                                                  (begin
                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to /")
                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                  (if (not (all-numeric? args_reg))
                                                                                                                                                                                                      (begin
                                                                                                                                                                                                        (set! msg_reg "/ called on non-numeric argument(s)")
                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                      (if (member 0 (cdr args_reg))
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! msg_reg "division by zero")
                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                            (set! value1_reg (apply / args_reg))
                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                            (set! pc apply-cont2)))))
                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-47>)
                                                                                                                                                                                                  (if (not (length-at-least? 2 args_reg))
                                                                                                                                                                                                      (begin
                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to <")
                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                      (if (not (all-numeric? args_reg))
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! msg_reg "< called on non-numeric argument(s)")
                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                            (set! value1_reg (apply < args_reg))
                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                            (set! pc apply-cont2))))
                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-48>)
                                                                                                                                                                                                      (if (not (length-at-least? 2 args_reg))
                                                                                                                                                                                                          (begin
                                                                                                                                                                                                            (set! msg_reg "incorrect number of arguments to >")
                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                          (if (not (all-numeric? args_reg))
                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                (set! msg_reg "> called on non-numeric argument(s)")
                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                (set! value1_reg (apply > args_reg))
                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                (set! pc apply-cont2))))
                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-49>)
                                                                                                                                                                                                          (if (not (length-at-least? 2 args_reg))
                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                (set! msg_reg "incorrect number of arguments to <=")
                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                              (if (not (all-numeric? args_reg))
                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                    (set! msg_reg "<= called on non-numeric argument(s)")
                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                    (set! value1_reg (apply <= args_reg))
                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                    (set! pc apply-cont2))))
                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-50>)
                                                                                                                                                                                                              (if (not (length-at-least? 2 args_reg))
                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to >=")
                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                  (if (not (all-numeric? args_reg))
                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                        (set! msg_reg ">= called on non-numeric argument(s)")
                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                        (set! value1_reg (apply >= args_reg))
                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                        (set! pc apply-cont2))))
                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-51>)
                                                                                                                                                                                                                  (if (not (length-at-least? 2 args_reg))
                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to =")
                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                      (if (not (all-numeric? args_reg))
                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                            (set! msg_reg "= called on non-numeric argument(s)")
                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                            (set! value1_reg (apply = args_reg))
                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                            (set! pc apply-cont2))))
                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-52>)
                                                                                                                                                                                                                      (if (not (length-one? args_reg))
                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                            (set! msg_reg "incorrect number of arguments to abs")
                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                          (if (not (all-numeric? args_reg))
                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                (set! msg_reg "abs called on non-numeric argument(s)")
                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                (set! value1_reg (apply abs args_reg))
                                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                                (set! pc apply-cont2))))
                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-53>)
                                                                                                                                                                                                                          (if (not (length-two? args_reg))
                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                (set! msg_reg "incorrect number of arguments to equal?")
                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                (set! k_reg (make-cont '<cont-38> fail_reg k2_reg))
                                                                                                                                                                                                                                (set! y_reg (cadr args_reg))
                                                                                                                                                                                                                                (set! x_reg (car args_reg))
                                                                                                                                                                                                                                (set! pc equal-objects?)))
                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-54>)
                                                                                                                                                                                                                              (if (not (length-two? args_reg))
                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to eq?")
                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                    (set! value1_reg (apply eq? args_reg))
                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-55>)
                                                                                                                                                                                                                                  (if (not (length-two? args_reg))
                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to memq")
                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                                        (set! value1_reg (apply memq args_reg))
                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-56>)
                                                                                                                                                                                                                                      (if (not (length-two? args_reg))
                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                            (set! msg_reg "incorrect number of arguments to member")
                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                            (set! ls_reg (cadr args_reg))
                                                                                                                                                                                                                                            (set! y_reg (cadr args_reg))
                                                                                                                                                                                                                                            (set! x_reg (car args_reg))
                                                                                                                                                                                                                                            (set! pc member-loop)))
                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-57>)
                                                                                                                                                                                                                                          (if (or (null? args_reg) (length-at-least? 4 args_reg))
                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                (set! msg_reg "incorrect number of arguments to range")
                                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                                              (if (not (all-numeric? args_reg))
                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                    (set! msg_reg "range called on non-numeric argument(s)")
                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                    (set! value1_reg (apply range args_reg))
                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                    (set! pc apply-cont2))))
                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-58>)
                                                                                                                                                                                                                                              (if (not (length-two? args_reg))
                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to set-car!")
                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                  (if (not (pair? (car args_reg)))
                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                        (set! msg_reg
                                                                                                                                                                                                                                                          (format "set-car! called on non-pair ~s" (car args_reg)))
                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                        (set! value1_reg (apply set-car! args_reg))
                                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                                        (set! pc apply-cont2))))
                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-59>)
                                                                                                                                                                                                                                                  (if (not (length-two? args_reg))
                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to set-cdr!")
                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                      (if (not (pair? (car args_reg)))
                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                            (set! msg_reg
                                                                                                                                                                                                                                                              (format "set-cdr! called on non-pair ~s" (car args_reg)))
                                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                            (set! value1_reg (apply set-cdr! args_reg))
                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                            (set! pc apply-cont2))))
                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-60>)
                                                                                                                                                                                                                                                      (let ((filename 'undefined))
                                                                                                                                                                                                                                                        (set! filename (car args_reg))
                                                                                                                                                                                                                                                        (if (null? (cdr args_reg))
                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                              (set! k_reg k2_reg)
                                                                                                                                                                                                                                                              (set! info_reg 'none)
                                                                                                                                                                                                                                                              (set! env_reg env2_reg)
                                                                                                                                                                                                                                                              (set! filename_reg filename)
                                                                                                                                                                                                                                                              (set! pc load-file))
                                                                                                                                                                                                                                                            (let ((module-name 'undefined))
                                                                                                                                                                                                                                                              (set! module-name (cadr args_reg))
                                                                                                                                                                                                                                                              (set! k_reg
                                                                                                                                                                                                                                                                (make-cont2 '<cont2-78> filename env2_reg handler_reg
                                                                                                                                                                                                                                                                  k2_reg))
                                                                                                                                                                                                                                                              (set! env_reg env2_reg)
                                                                                                                                                                                                                                                              (set! var_reg module-name)
                                                                                                                                                                                                                                                              (set! pc lookup-binding-in-first-frame))))
                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-61>)
                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                            (set! env_reg env2_reg)
                                                                                                                                                                                                                                                            (set! pc get-primitive))
                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-62>)
                                                                                                                                                                                                                                                              (let ((k 'undefined))
                                                                                                                                                                                                                                                                (set! k (list-ref temp_1 1))
                                                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                (set! value1_reg (car args_reg))
                                                                                                                                                                                                                                                                (set! k_reg k)
                                                                                                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-63>)
                                                                                                                                                                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to call/cc")
                                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                                      (let ((proc 'undefined))
                                                                                                                                                                                                                                                                        (set! proc (car args_reg))
                                                                                                                                                                                                                                                                        (if (not (procedure-object? proc))
                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                              (set! msg_reg "call/cc called with non-procedure")
                                                                                                                                                                                                                                                                              (set! pc runtime-error))
                                                                                                                                                                                                                                                                            (let ((fake-k 'undefined))
                                                                                                                                                                                                                                                                              (set! fake-k (make-proc '<proc-62> k2_reg))
                                                                                                                                                                                                                                                                              (if (dlr-exp? proc)
                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                    (set! value1_reg (dlr-apply proc (list fake-k)))
                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                    (set! args_reg (list fake-k))
                                                                                                                                                                                                                                                                                    (set! proc_reg proc)
                                                                                                                                                                                                                                                                                    (set! pc apply-proc)))))))
                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-64>)
                                                                                                                                                                                                                                                                      (if (null? args_reg)
                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                            (set! value1_reg void-value)
                                                                                                                                                                                                                                                                            (set! k_reg REP-k)
                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                            (set! value1_reg (car args_reg))
                                                                                                                                                                                                                                                                            (set! k_reg REP-k)
                                                                                                                                                                                                                                                                            (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-65>)
                                                                                                                                                                                                                                                                          (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                (set! msg_reg "incorrect number of arguments to require")
                                                                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                                                                              (if (true? (car args_reg))
                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                    (set! value1_reg 'ok)
                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                  (set! pc apply-fail)))
                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-66>)
                                                                                                                                                                                                                                                                              (if (not (null? args_reg))
                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to cut")
                                                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                    (set! value2_reg REP-fail)
                                                                                                                                                                                                                                                                                    (set! value1_reg 'ok)
                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-67>)
                                                                                                                                                                                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to reverse")
                                                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                                                      (if (not (list? args_reg))
                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                            (set! msg_reg
                                                                                                                                                                                                                                                                                              (format
                                                                                                                                                                                                                                                                                                "reverse called on incorrect list structure ~s"
                                                                                                                                                                                                                                                                                                (car args_reg)))
                                                                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                            (set! value1_reg (apply reverse args_reg))
                                                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))))
                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-68>)
                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                        (set! k_reg (make-cont '<cont-38> fail_reg k2_reg))
                                                                                                                                                                                                                                                                                        (set! lists_reg args_reg)
                                                                                                                                                                                                                                                                                        (set! pc append-all))
                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-69>)
                                                                                                                                                                                                                                                                                          (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                (set! msg_reg
                                                                                                                                                                                                                                                                                                  "incorrect number of arguments to string->number")
                                                                                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                (set! value1_reg (apply string->number args_reg))
                                                                                                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-70>)
                                                                                                                                                                                                                                                                                              (if (not (length-two? args_reg))
                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                    (set! msg_reg "incorrect number of arguments to string=?")
                                                                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                    (set! value1_reg (apply string=? args_reg))
                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-71>)
                                                                                                                                                                                                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                        (set! msg_reg
                                                                                                                                                                                                                                                                                                          "incorrect number of arguments to list->vector")
                                                                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                      (if (not (list? (car args_reg)))
                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                            (set! msg_reg
                                                                                                                                                                                                                                                                                                              (format
                                                                                                                                                                                                                                                                                                                "list->vector called on incorrect list structure ~s"
                                                                                                                                                                                                                                                                                                                (car args_reg)))
                                                                                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                            (set! value1_reg (apply list->vector args_reg))
                                                                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))))
                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-72>)
                                                                                                                                                                                                                                                                                                      (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                            (set! msg_reg
                                                                                                                                                                                                                                                                                                              "incorrect number of arguments to list->string")
                                                                                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                          (if (not (list? (car args_reg)))
                                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                                (set! msg_reg
                                                                                                                                                                                                                                                                                                                  (format
                                                                                                                                                                                                                                                                                                                    "list->string called on incorrect list structure ~s"
                                                                                                                                                                                                                                                                                                                    (car args_reg)))
                                                                                                                                                                                                                                                                                                                (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                              (if (not (all-char? (car args_reg)))
                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                    (set! msg_reg
                                                                                                                                                                                                                                                                                                                      (format
                                                                                                                                                                                                                                                                                                                        "list->string called on non-char list ~s"
                                                                                                                                                                                                                                                                                                                        (car args_reg)))
                                                                                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                    (set! value1_reg (apply list->string args_reg))
                                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2)))))
                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-73>)
                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                            (set! value1_reg (dir args_reg env2_reg))
                                                                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-74>)
                                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                (set! value1_reg (get-current-time))
                                                                                                                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-75>)
                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                    (set! env_reg env2_reg)
                                                                                                                                                                                                                                                                                                                    (set! proc_reg (car args_reg))
                                                                                                                                                                                                                                                                                                                    (set! args_reg (cdr args_reg))
                                                                                                                                                                                                                                                                                                                    (set! pc map-primitive))
                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-76>)
                                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                        (set! env_reg env2_reg)
                                                                                                                                                                                                                                                                                                                        (set! lists_reg (cdr args_reg))
                                                                                                                                                                                                                                                                                                                        (set! proc_reg (car args_reg))
                                                                                                                                                                                                                                                                                                                        (set! pc for-each-primitive))
                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-77>)
                                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                            (set! value1_reg env2_reg)
                                                                                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-78>)
                                                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                (set! value1_reg (using-prim args_reg env2_reg))
                                                                                                                                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-79>)
                                                                                                                                                                                                                                                                                                                                  (if (not (length-one? args_reg))
                                                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                                                        (set! msg_reg "incorrect number of arguments to not")
                                                                                                                                                                                                                                                                                                                                        (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                        (set! value1_reg (not (car args_reg)))
                                                                                                                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-80>)
                                                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                                                        (apply printf-prim args_reg)
                                                                                                                                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                        (set! value1_reg void-value)
                                                                                                                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-81>)
                                                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                                                            (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                            (set! value1_reg (list->vector args_reg))
                                                                                                                                                                                                                                                                                                                                            (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                            (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-82>)
                                                                                                                                                                                                                                                                                                                                              (begin
                                                                                                                                                                                                                                                                                                                                                (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                                (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                  (vector-set!
                                                                                                                                                                                                                                                                                                                                                    (car args_reg)
                                                                                                                                                                                                                                                                                                                                                    (cadr args_reg)
                                                                                                                                                                                                                                                                                                                                                    (caddr args_reg)))
                                                                                                                                                                                                                                                                                                                                                (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                                (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-83>)
                                                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg (apply vector-ref args_reg))
                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                  (if (eq? (car temp_1) '<proc-84>)
                                                                                                                                                                                                                                                                                                                                                      (begin
                                                                                                                                                                                                                                                                                                                                                        (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                                        (set! value1_reg (apply make-vector args_reg))
                                                                                                                                                                                                                                                                                                                                                        (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                                        (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                      (if (eq? (car temp_1) '<proc-85>)
                                                                                                                                                                                                                                                                                                                                                          (let ((location 'undefined) (message 'undefined))
                                                                                                                                                                                                                                                                                                                                                            (set! location
                                                                                                                                                                                                                                                                                                                                                              (format "Error in ~a: " (car args_reg)))
                                                                                                                                                                                                                                                                                                                                                            (set! message
                                                                                                                                                                                                                                                                                                                                                              (string-append
                                                                                                                                                                                                                                                                                                                                                                location
                                                                                                                                                                                                                                                                                                                                                                (apply format (cdr args_reg))))
                                                                                                                                                                                                                                                                                                                                                            (set! msg_reg message)
                                                                                                                                                                                                                                                                                                                                                            (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                                                                          (if (eq? (car temp_1) '<proc-86>)
                                                                                                                                                                                                                                                                                                                                                              (if (not (length-two? args_reg))
                                                                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                                                                    (set! msg_reg
                                                                                                                                                                                                                                                                                                                                                                      "incorrect number of arguments to list-ref")
                                                                                                                                                                                                                                                                                                                                                                    (set! pc runtime-error))
                                                                                                                                                                                                                                                                                                                                                                  (begin
                                                                                                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg (apply list-ref args_reg))
                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2)))
                                                                                                                                                                                                                                                                                                                                                              (if (eq? (car temp_1) '<proc-87>)
                                                                                                                                                                                                                                                                                                                                                                  (let ((external-function-object 'undefined))
                                                                                                                                                                                                                                                                                                                                                                    (set! external-function-object
                                                                                                                                                                                                                                                                                                                                                                      (list-ref temp_1 1))
                                                                                                                                                                                                                                                                                                                                                                    (set! value2_reg fail_reg)
                                                                                                                                                                                                                                                                                                                                                                    (set! value1_reg
                                                                                                                                                                                                                                                                                                                                                                      (apply*
                                                                                                                                                                                                                                                                                                                                                                        external-function-object
                                                                                                                                                                                                                                                                                                                                                                        args_reg))
                                                                                                                                                                                                                                                                                                                                                                    (set! k_reg k2_reg)
                                                                                                                                                                                                                                                                                                                                                                    (set! pc apply-cont2))
                                                                                                                                                                                                                                                                                                                                                                  (error 'apply-proc
                                                                                                                                                                                                                                                                                                                                                                    "bad procedure: ~a"
                                                                                                                                                                                                                                                                                                                                                                    proc_reg)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define make-macro
  (lambda args (return* (cons 'macro-transformer args))))

(define*
  apply-macro
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr macro_reg))
      (if (eq? (car temp_1) '<macro-1>)
          (if (symbol?^ (cadr^ datum_reg))
              (let ((name 'undefined)
                    (bindings 'undefined)
                    (vars 'undefined)
                    (exps 'undefined)
                    (bodies 'undefined))
                (set! name (cadr^ datum_reg))
                (set! bindings (caddr^ datum_reg))
                (set! vars (map^ car^ bindings))
                (set! exps (map^ cadr^ bindings))
                (set! bodies (cdddr^ datum_reg))
                (set! value_reg
                  (append
                    (list 'letrec)
                    (append
                      (list
                        (list
                          (append
                            (list name)
                            (list
                              (append (list 'lambda) (append (list vars) bodies))))))
                      (list (append (list name) exps)))))
                (set! pc apply-cont))
              (let ((bindings 'undefined)
                    (vars 'undefined)
                    (exps 'undefined)
                    (bodies 'undefined))
                (set! bindings (cadr^ datum_reg))
                (set! vars (map^ car^ bindings))
                (set! exps (map^ cadr^ bindings))
                (set! bodies (cddr^ datum_reg))
                (set! value_reg
                  (append
                    (list (append (list 'lambda) (append (list vars) bodies)))
                    exps))
                (set! pc apply-cont)))
          (if (eq? (car temp_1) '<macro-2>)
              (let ((decls 'undefined)
                    (vars 'undefined)
                    (procs 'undefined)
                    (bodies 'undefined))
                (set! decls (cadr^ datum_reg))
                (set! vars (map^ car^ decls))
                (set! procs (map^ cadr^ decls))
                (set! bodies (cddr^ datum_reg))
                (set! k2_reg (make-cont2 '<cont2-6> bodies k_reg))
                (set! procs_reg procs)
                (set! vars_reg vars)
                (set! pc create-letrec-assignments^))
              (if (eq? (car temp_1) '<macro-3>)
                  (let ((name 'undefined)
                        (formals 'undefined)
                        (bodies 'undefined))
                    (set! bodies (cddr^ datum_reg))
                    (set! formals (cdr^ (cadr^ datum_reg)))
                    (set! name (car^ (cadr^ datum_reg)))
                    (set! value_reg
                      (append
                        (list 'define)
                        (append
                          (list name)
                          (list
                            (append (list 'lambda) (append (list formals) bodies))))))
                    (set! pc apply-cont))
                  (if (eq? (car temp_1) '<macro-4>)
                      (let ((exps 'undefined))
                        (set! exps (cdr^ datum_reg))
                        (if (null? exps)
                            (begin (set! value_reg #t) (set! pc apply-cont))
                            (if (null? (cdr exps))
                                (begin (set! value_reg (car exps)) (set! pc apply-cont))
                                (begin
                                  (set! value_reg
                                    (append
                                      (list 'if)
                                      (append
                                        (list (car exps))
                                        (append (list (append (list 'and) (cdr exps))) (list #f)))))
                                  (set! pc apply-cont)))))
                      (if (eq? (car temp_1) '<macro-5>)
                          (let ((exps 'undefined))
                            (set! exps (cdr^ datum_reg))
                            (if (null? exps)
                                (begin (set! value_reg #f) (set! pc apply-cont))
                                (if (null? (cdr exps))
                                    (begin (set! value_reg (car exps)) (set! pc apply-cont))
                                    (begin
                                      (set! value_reg
                                        (append
                                          (list 'let)
                                          (append
                                            (list
                                              (append
                                                (list (append (list 'bool) (list (car exps))))
                                                (list
                                                  (append
                                                    (list 'else-code)
                                                    (list
                                                      (append
                                                        (list 'lambda)
                                                        (append
                                                          (list '())
                                                          (list (append (list 'or) (cdr exps))))))))))
                                            (list
                                              (append
                                                (list 'if)
                                                (append
                                                  (list 'bool)
                                                  (append (list 'bool) (list (list 'else-code)))))))))
                                      (set! pc apply-cont)))))
                          (if (eq? (car temp_1) '<macro-6>)
                              (let ((clauses 'undefined))
                                (set! clauses (cdr^ datum_reg))
                                (if (null? clauses)
                                    (begin
                                      (set! adatum_reg datum_reg)
                                      (set! transformer-name_reg 'cond-transformer^)
                                      (set! pc amacro-error))
                                    (let ((first-clause 'undefined) (other-clauses 'undefined))
                                      (set! other-clauses (cdr clauses))
                                      (set! first-clause (car clauses))
                                      (if (or (null?^ first-clause) (not (list?^ first-clause)))
                                          (begin
                                            (set! adatum_reg datum_reg)
                                            (set! transformer-name_reg 'cond-transformer^)
                                            (set! pc amacro-error))
                                          (let ((test-exp 'undefined) (then-exps 'undefined))
                                            (set! then-exps (cdr^ first-clause))
                                            (set! test-exp (car^ first-clause))
                                            (if (eq?^ test-exp 'else)
                                                (if (null? then-exps)
                                                    (begin
                                                      (set! adatum_reg (list 'else))
                                                      (set! transformer-name_reg 'cond-transformer^)
                                                      (set! pc amacro-error))
                                                    (if (null? (cdr then-exps))
                                                        (begin
                                                          (set! value_reg (car then-exps))
                                                          (set! pc apply-cont))
                                                        (begin
                                                          (set! value_reg (append (list 'begin) then-exps))
                                                          (set! pc apply-cont))))
                                                (if (null? then-exps)
                                                    (if (null? other-clauses)
                                                        (begin
                                                          (set! value_reg
                                                            (append
                                                              (list 'let)
                                                              (append
                                                                (list (list (append (list 'bool) (list test-exp))))
                                                                (list
                                                                  (append (list 'if) (append (list 'bool) (list 'bool)))))))
                                                          (set! pc apply-cont))
                                                        (begin
                                                          (set! value_reg
                                                            (append
                                                              (list 'let)
                                                              (append
                                                                (list
                                                                  (append
                                                                    (list (append (list 'bool) (list test-exp)))
                                                                    (list
                                                                      (append
                                                                        (list 'else-code)
                                                                        (list
                                                                          (append
                                                                            (list 'lambda)
                                                                            (append
                                                                              (list '())
                                                                              (list (append (list 'cond) other-clauses)))))))))
                                                                (list
                                                                  (append
                                                                    (list 'if)
                                                                    (append
                                                                      (list 'bool)
                                                                      (append (list 'bool) (list (list 'else-code)))))))))
                                                          (set! pc apply-cont)))
                                                    (if (eq?^ (car then-exps) '=>)
                                                        (if (null? (cdr then-exps))
                                                            (begin
                                                              (set! adatum_reg first-clause)
                                                              (set! transformer-name_reg 'cond-transformer^)
                                                              (set! pc amacro-error))
                                                            (if (null? other-clauses)
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'let)
                                                                      (append
                                                                        (list
                                                                          (append
                                                                            (list (append (list 'bool) (list test-exp)))
                                                                            (list
                                                                              (append
                                                                                (list 'th)
                                                                                (list
                                                                                  (append
                                                                                    (list 'lambda)
                                                                                    (append (list '()) (list (cadr then-exps)))))))))
                                                                        (list
                                                                          (append
                                                                            (list 'if)
                                                                            (append
                                                                              (list 'bool)
                                                                              (list (append (list (list 'th)) (list 'bool)))))))))
                                                                  (set! pc apply-cont))
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'let)
                                                                      (append
                                                                        (list
                                                                          (append
                                                                            (list (append (list 'bool) (list test-exp)))
                                                                            (append
                                                                              (list
                                                                                (append
                                                                                  (list 'th)
                                                                                  (list
                                                                                    (append
                                                                                      (list 'lambda)
                                                                                      (append (list '()) (list (cadr then-exps)))))))
                                                                              (list
                                                                                (append
                                                                                  (list 'else-code)
                                                                                  (list
                                                                                    (append
                                                                                      (list 'lambda)
                                                                                      (append
                                                                                        (list '())
                                                                                        (list (append (list 'cond) other-clauses))))))))))
                                                                        (list
                                                                          (append
                                                                            (list 'if)
                                                                            (append
                                                                              (list 'bool)
                                                                              (append
                                                                                (list (append (list (list 'th)) (list 'bool)))
                                                                                (list (list 'else-code)))))))))
                                                                  (set! pc apply-cont))))
                                                        (if (null? other-clauses)
                                                            (if (null? (cdr then-exps))
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'if)
                                                                      (append (list test-exp) (list (car then-exps)))))
                                                                  (set! pc apply-cont))
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'if)
                                                                      (append
                                                                        (list test-exp)
                                                                        (list (append (list 'begin) then-exps)))))
                                                                  (set! pc apply-cont)))
                                                            (if (null? (cdr then-exps))
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'if)
                                                                      (append
                                                                        (list test-exp)
                                                                        (append
                                                                          (list (car then-exps))
                                                                          (list (append (list 'cond) other-clauses))))))
                                                                  (set! pc apply-cont))
                                                                (begin
                                                                  (set! value_reg
                                                                    (append
                                                                      (list 'if)
                                                                      (append
                                                                        (list test-exp)
                                                                        (append
                                                                          (list (append (list 'begin) then-exps))
                                                                          (list (append (list 'cond) other-clauses))))))
                                                                  (set! pc apply-cont))))))))))))
                              (if (eq? (car temp_1) '<macro-7>)
                                  (let ((bindings 'undefined) (bodies 'undefined))
                                    (set! bodies (cddr^ datum_reg))
                                    (set! bindings (get-sexp (cadr^ datum_reg)))
                                    (set! bodies_reg bodies)
                                    (set! bindings_reg bindings)
                                    (set! pc nest-let*-bindings^))
                                  (if (eq? (car temp_1) '<macro-8>)
                                      (let ((exp 'undefined) (clauses 'undefined))
                                        (set! clauses (cddr^ datum_reg))
                                        (set! exp (cadr^ datum_reg))
                                        (set! k2_reg (make-cont2 '<cont2-8> exp k_reg))
                                        (set! clauses_reg clauses)
                                        (set! var_reg 'r)
                                        (set! pc case-clauses->cond-clauses^))
                                      (if (eq? (car temp_1) '<macro-9>)
                                          (let ((exp 'undefined) (clauses 'undefined))
                                            (set! clauses (cddr^ datum_reg))
                                            (set! exp (cadr^ datum_reg))
                                            (set! k2_reg (make-cont2 '<cont2-8> exp k_reg))
                                            (set! clauses_reg clauses)
                                            (set! var_reg 'r)
                                            (set! pc record-case-clauses->cond-clauses^))
                                          (if (eq? (car temp_1) '<macro-10>)
                                              (let ((datatype-name 'undefined)
                                                    (type-tester-name 'undefined))
                                                (set! datatype-name (get-sexp (cadr^ datum_reg)))
                                                (set! type-tester-name
                                                  (string->symbol
                                                    (string-append (symbol->string datatype-name) "?")))
                                                (if (not (eq?^ (cadr (cdr^ datum_reg)) type-tester-name))
                                                    (begin
                                                      (set! adatum_reg datum_reg)
                                                      (set! transformer-name_reg 'define-datatype-transformer^)
                                                      (set! pc amacro-error))
                                                    (let ((variants 'undefined)
                                                          (variant-names 'undefined)
                                                          (variant-tests 'undefined)
                                                          (tester-def 'undefined))
                                                      (set! variants (cddr (cdr^ datum_reg)))
                                                      (set! variant-names
                                                        (define-datatype-variant-names variants))
                                                      (set! variant-tests
                                                        (define-datatype-variant-tests variants))
                                                      (set! tester-def
                                                        (append
                                                          (list 'define)
                                                          (append
                                                            (list type-tester-name)
                                                            (list
                                                              (append
                                                                (list 'lambda)
                                                                (append
                                                                  (list (list 'x))
                                                                  (list
                                                                    (append
                                                                      (list 'and)
                                                                      (append
                                                                        (list (append (list 'pair?) (list 'x)))
                                                                        (list
                                                                          (append
                                                                            (list 'not)
                                                                            (list
                                                                              (append
                                                                                (list 'not)
                                                                                (list
                                                                                  (append
                                                                                    (list 'memq)
                                                                                    (append
                                                                                      (list (append (list 'car) (list 'x)))
                                                                                      (list
                                                                                        (append (list 'quote) (list variant-names)))))))))))))))))))
                                                      (set! value_reg
                                                        (append
                                                          (list 'begin)
                                                          (append
                                                            (list tester-def)
                                                            (make-define-datatype-defines
                                                              variant-names
                                                              variant-tests))))
                                                      (set! pc apply-cont))))
                                              (if (eq? (car temp_1) '<macro-11>)
                                                  (let ((type-name 'undefined)
                                                        (type-tester-name 'undefined)
                                                        (exp 'undefined)
                                                        (clauses 'undefined))
                                                    (set! type-name (get-sexp (cadr^ datum_reg)))
                                                    (set! type-tester-name
                                                      (string->symbol
                                                        (string-append (symbol->string type-name) "?")))
                                                    (set! exp (caddr^ datum_reg))
                                                    (set! clauses (cdddr^ datum_reg))
                                                    (set! k2_reg
                                                      (make-cont2 '<cont2-11> exp type-name type-tester-name
                                                        k_reg))
                                                    (set! clauses_reg clauses)
                                                    (set! var_reg 'r)
                                                    (set! pc record-case-clauses->cond-clauses^))
                                                  (error 'apply-macro
                                                    "bad macro-transformer: ~a"
                                                    macro_reg)))))))))))))))

(define next-avail
  (lambda (n) (return* (string-ref chars-to-scan n))))

(define remaining (lambda (n) (return* (+ 1 n))))

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
    (if (char=? (next-avail chars) #\newline)
        (begin (set! scan-line (+ 1 scan-line)) (set! scan-char 1))
        (set! scan-char (+ 1 scan-char)))
    (set! scan-position (+ 1 scan-position))))

(define mark-token-start
  (lambda ()
    (set! token-start-line scan-line)
    (set! token-start-char scan-char)
    (set! token-start-position scan-position)))

(define*
  scan-input
  (lambda ()
    (initialize-scan-counters)
    (set! chars-to-scan
      (string-append input_reg (string #\nul)))
    (set! chars_reg 0)
    (set! pc scan-input-loop)))

(define*
  scan-input-loop
  (lambda ()
    (set! k_reg
      (make-cont3 '<cont3-1> src_reg handler_reg k_reg))
    (set! buffer_reg '())
    (set! action_reg (list 'goto 'start-state))
    (set! pc apply-action)))

(define*
  apply-action
  (lambda ()
    (if (eq? (car action_reg) 'shift)
        (let ((next 'undefined))
          (set! next (list-ref action_reg 1))
          (increment-scan-counters chars_reg)
          (set! buffer_reg (cons (next-avail chars_reg) buffer_reg))
          (set! chars_reg (remaining chars_reg))
          (set! action_reg next)
          (set! pc apply-action))
        (if (eq? (car action_reg) 'replace)
            (let ((new-char 'undefined) (next 'undefined))
              (set! next (list-ref action_reg 2))
              (set! new-char (list-ref action_reg 1))
              (increment-scan-counters chars_reg)
              (set! chars_reg (remaining chars_reg))
              (set! buffer_reg (cons new-char buffer_reg))
              (set! action_reg next)
              (set! pc apply-action))
            (if (eq? (car action_reg) 'drop)
                (let ((next 'undefined))
                  (set! next (list-ref action_reg 1))
                  (increment-scan-counters chars_reg)
                  (set! chars_reg (remaining chars_reg))
                  (set! action_reg next)
                  (set! pc apply-action))
                (if (eq? (car action_reg) 'goto)
                    (let ((state 'undefined))
                      (set! state (list-ref action_reg 1))
                      (if (eq? state 'token-start-state) (mark-token-start))
                      (let ((action 'undefined))
                        (set! action (apply-state state (next-avail chars_reg)))
                        (if (eq? action 'error)
                            (set! pc unexpected-char-error)
                            (begin (set! action_reg action) (set! pc apply-action)))))
                    (if (eq? (car action_reg) 'emit)
                        (let ((token-type 'undefined))
                          (set! token-type (list-ref action_reg 1))
                          (set! k_reg (make-cont '<cont-1> chars_reg fail_reg k_reg))
                          (set! token-type_reg token-type)
                          (set! pc convert-buffer-to-token))
                        (error 'apply-action "invalid action: ~a" action_reg))))))))

(define*
  scan-error
  (lambda ()
    (set! exception_reg
      (format
        "scan error: ~a ~a"
        msg_reg
        (where-at line_reg char_reg src_reg)))
    (set! pc apply-handler2)))

(define*
  unexpected-char-error
  (lambda ()
    (let ((c 'undefined))
      (set! c (next-avail chars_reg))
      (if (char=? c #\nul)
          (begin
            (set! char_reg scan-char)
            (set! line_reg scan-line)
            (set! msg_reg "unexpected end of input")
            (set! pc scan-error))
          (begin
            (set! char_reg scan-char)
            (set! line_reg scan-line)
            (set! msg_reg
              (format "unexpected character ~a encountered" c))
            (set! pc scan-error))))))

(define*
  convert-buffer-to-token
  (lambda ()
    (let ((buffer 'undefined))
      (set! buffer (reverse buffer_reg))
      (if (eq? token-type_reg 'end-marker)
          (begin
            (set! value_reg (make-token 'end-marker))
            (set! pc apply-cont))
          (if (eq? token-type_reg 'integer)
              (begin
                (set! value_reg
                  (make-info-token 'integer (list->string buffer)))
                (set! pc apply-cont))
              (if (eq? token-type_reg 'decimal)
                  (begin
                    (set! value_reg
                      (make-info-token 'decimal (list->string buffer)))
                    (set! pc apply-cont))
                  (if (eq? token-type_reg 'rational)
                      (begin
                        (set! value_reg
                          (make-info-token 'rational (list->string buffer)))
                        (set! pc apply-cont))
                      (if (eq? token-type_reg 'identifier)
                          (begin
                            (set! value_reg
                              (make-info-token
                                'identifier
                                (string->symbol (list->string buffer))))
                            (set! pc apply-cont))
                          (if (eq? token-type_reg 'boolean)
                              (begin
                                (set! value_reg
                                  (make-info-token
                                    'boolean
                                    (or (char=? (car buffer) #\t) (char=? (car buffer) #\T))))
                                (set! pc apply-cont))
                              (if (eq? token-type_reg 'character)
                                  (begin
                                    (set! value_reg (make-info-token 'character (car buffer)))
                                    (set! pc apply-cont))
                                  (if (eq? token-type_reg 'named-character)
                                      (let ((name 'undefined))
                                        (set! name (list->string buffer))
                                        (if (string=? name "nul")
                                            (begin
                                              (set! value_reg (make-info-token 'character #\nul))
                                              (set! pc apply-cont))
                                            (if (string=? name "space")
                                                (begin
                                                  (set! value_reg (make-info-token 'character #\space))
                                                  (set! pc apply-cont))
                                                (if (string=? name "tab")
                                                    (begin
                                                      (set! value_reg (make-info-token 'character #\tab))
                                                      (set! pc apply-cont))
                                                    (if (string=? name "newline")
                                                        (begin
                                                          (set! value_reg (make-info-token 'character #\newline))
                                                          (set! pc apply-cont))
                                                        (if (string=? name "linefeed")
                                                            (begin
                                                              (set! value_reg (make-info-token 'character #\newline))
                                                              (set! pc apply-cont))
                                                            (if (string=? name "backspace")
                                                                (begin
                                                                  (set! value_reg (make-info-token 'character #\backspace))
                                                                  (set! pc apply-cont))
                                                                (if (string=? name "return")
                                                                    (begin
                                                                      (set! value_reg (make-info-token 'character #\return))
                                                                      (set! pc apply-cont))
                                                                    (if (string=? name "page")
                                                                        (begin
                                                                          (set! value_reg (make-info-token 'character #\page))
                                                                          (set! pc apply-cont))
                                                                        (begin
                                                                          (set! char_reg token-start-char)
                                                                          (set! line_reg token-start-line)
                                                                          (set! msg_reg (format "invalid character name #\\~a" name))
                                                                          (set! pc scan-error)))))))))))
                                      (if (eq? token-type_reg 'string)
                                          (begin
                                            (set! value_reg
                                              (make-info-token 'string (list->string buffer)))
                                            (set! pc apply-cont))
                                          (begin
                                            (set! value_reg (make-token token-type_reg))
                                            (set! pc apply-cont))))))))))))))

(define make-token
  (lambda (token-type)
    (let ((start 'undefined) (end 'undefined))
      (set! end
        (list last-scan-line last-scan-char last-scan-position))
      (set! start
        (list
          token-start-line
          token-start-char
          token-start-position))
      (if (eq? token-type 'end-marker)
          (return* (list token-type end end))
          (return* (list token-type start end))))))

(define make-info-token
  (lambda (token-type token-info)
    (return*
      (list
        token-type
        token-info
        (list
          token-start-line
          token-start-char
          token-start-position)
        (list last-scan-line last-scan-char last-scan-position)))))

(define token-type?
  (lambda (token class) (return* (eq? (car token) class))))

(define get-token-start
  (lambda (token) (return* (rac (rdc token)))))

(define get-token-end
  (lambda (token) (return* (rac token))))

(define get-token-start-line
  (lambda (token) (return* (car (get-token-start token)))))

(define get-token-start-char
  (lambda (token) (return* (cadr (get-token-start token)))))

(define get-token-start-pos
  (lambda (token) (return* (caddr (get-token-start token)))))

(define snoc
  (lambda (x lyst)
    (if (null? lyst)
        (return* (list x))
        (return* (cons (car lyst) (snoc x (cdr lyst)))))))

(define rac
  (lambda (lyst)
    (if (null? (cdr lyst))
        (return* (car lyst))
        (return* (rac (cdr lyst))))))

(define rdc
  (lambda (lyst)
    (if (null? (cdr lyst))
        (return* '())
        (return* (cons (car lyst) (rdc (cdr lyst)))))))

(define char-delimiter?
  (lambda (c)
    (return*
      (or (char-whitespace? c)
          (char=? c #\')
          (char=? c #\()
          (char=? c #\[)
          (char=? c #\))
          (char=? c #\])
          (char=? c #\")
          (char=? c #\;)
          (char=? c #\#)
          (char=? c #\nul)))))

(define char-initial?
  (lambda (c)
    (return*
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
          (char=? c #\~)))))

(define char-special-subsequent?
  (lambda (c)
    (return*
      (or (char=? c #\+)
          (char=? c #\-)
          (char=? c #\@)
          (char=? c #\.)))))

(define char-subsequent?
  (lambda (c)
    (return*
      (or (char-initial? c)
          (char-numeric? c)
          (char-special-subsequent? c)))))

(define char-sign?
  (lambda (c) (return* (or (char=? c #\+) (char=? c #\-)))))

(define char-boolean?
  (lambda (c)
    (return*
      (or (char=? c #\t)
          (char=? c #\T)
          (char=? c #\f)
          (char=? c #\F)))))

(define apply-state
  (lambda (state c)
    (if (eq? state 'start-state)
        (if (char-whitespace? c)
            (return* (list 'drop (list 'goto 'start-state)))
            (if (char=? c #\;)
                (return* (list 'drop (list 'goto 'comment-state)))
                (if (char=? c #\nul)
                    (return* (list 'drop (list 'emit 'end-marker)))
                    (return* (list 'goto 'token-start-state)))))
        (if (eq? state 'token-start-state)
            (if (char=? c #\()
                (return* (list 'drop (list 'emit 'lparen)))
                (if (char=? c #\[)
                    (return* (list 'drop (list 'emit 'lbracket)))
                    (if (char=? c #\))
                        (return* (list 'drop (list 'emit 'rparen)))
                        (if (char=? c #\])
                            (return* (list 'drop (list 'emit 'rbracket)))
                            (if (char=? c #\')
                                (return* (list 'drop (list 'emit 'apostrophe)))
                                (if (char=? c #\`)
                                    (return* (list 'drop (list 'emit 'backquote)))
                                    (if (char=? c #\,)
                                        (return* (list 'drop (list 'goto 'comma-state)))
                                        (if (char=? c #\#)
                                            (return* (list 'drop (list 'goto 'hash-prefix-state)))
                                            (if (char=? c #\")
                                                (return* (list 'drop (list 'goto 'string-state)))
                                                (if (char-initial? c)
                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                    (if (char-sign? c)
                                                        (return* (list 'shift (list 'goto 'signed-state)))
                                                        (if (char=? c #\.)
                                                            (return* (list 'shift (list 'goto 'decimal-point-state)))
                                                            (if (char-numeric? c)
                                                                (return* (list 'shift (list 'goto 'whole-number-state)))
                                                                (return* 'error))))))))))))))
            (if (eq? state 'comment-state)
                (if (char=? c #\newline)
                    (return* (list 'drop (list 'goto 'start-state)))
                    (if (char=? c #\nul)
                        (return* (list 'drop (list 'emit 'end-marker)))
                        (return* (list 'drop (list 'goto 'comment-state)))))
                (if (eq? state 'comma-state)
                    (if (char=? c #\@)
                        (return* (list 'drop (list 'emit 'comma-at)))
                        (return* (list 'emit 'comma)))
                    (if (eq? state 'hash-prefix-state)
                        (if (char-boolean? c)
                            (return* (list 'shift (list 'emit 'boolean)))
                            (if (char=? c #\\)
                                (return* (list 'drop (list 'goto 'character-state)))
                                (if (char=? c #\()
                                    (return* (list 'drop (list 'emit 'lvector)))
                                    (return* 'error))))
                        (if (eq? state 'character-state)
                            (if (char-alphabetic? c)
                                (return*
                                  (list 'shift (list 'goto 'alphabetic-character-state)))
                                (if (not (char=? c #\nul))
                                    (return* (list 'shift (list 'emit 'character)))
                                    (return* 'error)))
                            (if (eq? state 'alphabetic-character-state)
                                (if (char-alphabetic? c)
                                    (return* (list 'shift (list 'goto 'named-character-state)))
                                    (return* (list 'emit 'character)))
                                (if (eq? state 'named-character-state)
                                    (if (char-delimiter? c)
                                        (return* (list 'emit 'named-character))
                                        (return* (list 'shift (list 'goto 'named-character-state))))
                                    (if (eq? state 'string-state)
                                        (if (char=? c #\")
                                            (return* (list 'drop (list 'emit 'string)))
                                            (if (char=? c #\\)
                                                (return* (list 'drop (list 'goto 'string-escape-state)))
                                                (if (not (char=? c #\nul))
                                                    (return* (list 'shift (list 'goto 'string-state)))
                                                    (return* 'error))))
                                        (if (eq? state 'string-escape-state)
                                            (if (char=? c #\")
                                                (return* (list 'shift (list 'goto 'string-state)))
                                                (if (char=? c #\\)
                                                    (return* (list 'shift (list 'goto 'string-state)))
                                                    (if (char=? c #\b)
                                                        (return*
                                                          (list 'replace #\backspace (list 'goto 'string-state)))
                                                        (if (char=? c #\f)
                                                            (return* (list 'replace #\page (list 'goto 'string-state)))
                                                            (if (char=? c #\n)
                                                                (return*
                                                                  (list 'replace #\newline (list 'goto 'string-state)))
                                                                (if (char=? c #\t)
                                                                    (return* (list 'replace #\tab (list 'goto 'string-state)))
                                                                    (if (char=? c #\r)
                                                                        (return*
                                                                          (list 'replace #\return (list 'goto 'string-state)))
                                                                        (return* 'error))))))))
                                            (if (eq? state 'identifier-state)
                                                (if (char-subsequent? c)
                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                    (if (char-delimiter? c)
                                                        (return* (list 'emit 'identifier))
                                                        (return* 'error)))
                                                (if (eq? state 'signed-state)
                                                    (if (char-numeric? c)
                                                        (return* (list 'shift (list 'goto 'whole-number-state)))
                                                        (if (char=? c #\.)
                                                            (return*
                                                              (list 'shift (list 'goto 'signed-decimal-point-state)))
                                                            (if (char-delimiter? c)
                                                                (return* (list 'emit 'identifier))
                                                                (if (char-subsequent? c)
                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                    (return* 'error)))))
                                                    (if (eq? state 'decimal-point-state)
                                                        (if (char-numeric? c)
                                                            (return*
                                                              (list 'shift (list 'goto 'fractional-number-state)))
                                                            (if (char-delimiter? c)
                                                                (return* (list 'emit 'dot))
                                                                (if (char-subsequent? c)
                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                    (return* 'error))))
                                                        (if (eq? state 'signed-decimal-point-state)
                                                            (if (char-numeric? c)
                                                                (return*
                                                                  (list 'shift (list 'goto 'fractional-number-state)))
                                                                (if (char-delimiter? c)
                                                                    (return* (list 'emit 'identifier))
                                                                    (if (char-subsequent? c)
                                                                        (return* (list 'shift (list 'goto 'identifier-state)))
                                                                        (return* 'error))))
                                                            (if (eq? state 'whole-number-state)
                                                                (if (char-numeric? c)
                                                                    (return* (list 'shift (list 'goto 'whole-number-state)))
                                                                    (if (char=? c #\.)
                                                                        (return*
                                                                          (list 'shift (list 'goto 'fractional-number-state)))
                                                                        (if (char=? c #\/)
                                                                            (return* (list 'shift (list 'goto 'rational-number-state)))
                                                                            (if (or (char=? c #\e) (char=? c #\E))
                                                                                (return* (list 'shift (list 'goto 'suffix-state)))
                                                                                (if (char-delimiter? c)
                                                                                    (return* (list 'emit 'integer))
                                                                                    (if (char-subsequent? c)
                                                                                        (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                        (return* 'error)))))))
                                                                (if (eq? state 'fractional-number-state)
                                                                    (if (char-numeric? c)
                                                                        (return*
                                                                          (list 'shift (list 'goto 'fractional-number-state)))
                                                                        (if (or (char=? c #\e) (char=? c #\E))
                                                                            (return* (list 'shift (list 'goto 'suffix-state)))
                                                                            (if (char-delimiter? c)
                                                                                (return* (list 'emit 'decimal))
                                                                                (if (char-subsequent? c)
                                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                    (return* 'error)))))
                                                                    (if (eq? state 'rational-number-state)
                                                                        (if (char-numeric? c)
                                                                            (return* (list 'shift (list 'goto 'rational-number-state*)))
                                                                            (if (char-delimiter? c)
                                                                                (return* (list 'emit 'identifier))
                                                                                (if (char-subsequent? c)
                                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                    (return* 'error))))
                                                                        (if (eq? state 'rational-number-state*)
                                                                            (if (char-numeric? c)
                                                                                (return* (list 'shift (list 'goto 'rational-number-state*)))
                                                                                (if (char-delimiter? c)
                                                                                    (return* (list 'emit 'rational))
                                                                                    (if (char-subsequent? c)
                                                                                        (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                        (return* 'error))))
                                                                            (if (eq? state 'suffix-state)
                                                                                (if (char-sign? c)
                                                                                    (return* (list 'shift (list 'goto 'signed-exponent-state)))
                                                                                    (if (char-numeric? c)
                                                                                        (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                        (if (char-delimiter? c)
                                                                                            (return* (list 'emit 'identifier))
                                                                                            (if (char-subsequent? c)
                                                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                                (return* 'error)))))
                                                                                (if (eq? state 'signed-exponent-state)
                                                                                    (if (char-numeric? c)
                                                                                        (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                        (if (char-delimiter? c)
                                                                                            (return* (list 'emit 'identifier))
                                                                                            (if (char-subsequent? c)
                                                                                                (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                                (return* 'error))))
                                                                                    (if (eq? state 'exponent-state)
                                                                                        (if (char-numeric? c)
                                                                                            (return* (list 'shift (list 'goto 'exponent-state)))
                                                                                            (if (char-delimiter? c)
                                                                                                (return* (list 'emit 'decimal))
                                                                                                (if (char-subsequent? c)
                                                                                                    (return* (list 'shift (list 'goto 'identifier-state)))
                                                                                                    (return* 'error))))
                                                                                        (error 'apply-state
                                                                                          "invalid state: ~a"
                                                                                          state))))))))))))))))))))))))

(define first (lambda (x) (return* (car x))))

(define rest-of (lambda (x) (return* (cdr x))))

(define string->integer
  (lambda (str) (return* (string->number str))))

(define string->decimal
  (lambda (str) (return* (string->number str))))

(define string->rational
  (lambda (str) (return* (string->number str))))

(define true? (lambda (v) (if v (return* #t) (return* #f))))

(define*
  unexpected-token-error
  (lambda ()
    (let ((token 'undefined))
      (set! token (first tokens_reg))
      (if (token-type? token 'end-marker)
          (begin
            (set! msg_reg "unexpected end of input")
            (set! pc read-error))
          (begin
            (set! msg_reg
              (format "unexpected ~a encountered" (car token)))
            (set! pc read-error))))))

(define*
  read-error
  (lambda ()
    (let ((token 'undefined))
      (set! token (first tokens_reg))
      (set! exception_reg
        (format
          "read error: ~a ~a"
          msg_reg
          (where-at
            (get-token-start-line token)
            (get-token-start-char token)
            src_reg)))
      (set! pc apply-handler2))))

(define where-at
  (lambda (line char src)
    (if (eq? src 'stdin)
        (return* (format "at line ~a, char ~a" line char))
        (return*
          (format "at line ~a, char ~a of ~a" line char src)))))

(define read-content
  (lambda (filename)
    (return*
      (apply
        string
        (call-with-input-file
          filename
          (lambda (port)
            (let ((loop 'undefined))
              (set! loop
                (lambda (char)
                  (if (eof-object? char)
                      '()
                      (cons char (loop (read-char port))))))
              (loop (read-char port)))))))))

(define make-asexp
  (lambda (src start end sexp)
    (return*
      (list asexp-tag sexp (cons src (append start end))))))

(define retag
  (lambda (sexp info) (return* (list asexp-tag sexp info))))

(define asexp?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) asexp-tag)))))

(define get-sexp (lambda (asexp) (return* (cadr asexp))))

(define get-source-info
  (lambda (asexp) (return* (caddr asexp))))

(define get-srcfile (lambda (info) (return* (car info))))

(define get-start-line
  (lambda (info) (return* (cadr info))))

(define get-start-char
  (lambda (info) (return* (caddr info))))

(define get-start-pos
  (lambda (info) (return* (cadddr info))))

(define get-end-line
  (lambda (info) (return* (car (cddddr info)))))

(define get-end-char
  (lambda (info) (return* (cadr (cddddr info)))))

(define get-end-pos
  (lambda (info) (return* (caddr (cddddr info)))))

(define has-source-info?
  (lambda (asexp)
    (return* (not (eq? (get-source-info asexp) 'none)))))

(define original-source-info?
  (lambda (asexp)
    (return*
      (and (has-source-info? asexp)
           (= (length (get-source-info asexp)) 7)))))

(define source-info?
  (lambda (x) (return* (or (eq? x 'none) (list? x)))))

(define replace-info
  (lambda (asexp info)
    (return* (retag (get-sexp asexp) info))))

(define car^
  (lambda (asexp) (return* (car (get-sexp asexp)))))

(define cdr^
  (lambda (asexp) (return* (cdr (get-sexp asexp)))))

(define cddr^
  (lambda (asexp) (return* (cddr (get-sexp asexp)))))

(define cdddr^
  (lambda (asexp) (return* (cdddr (get-sexp asexp)))))

(define cadr^
  (lambda (asexp) (return* (cadr (get-sexp asexp)))))

(define caddr^
  (lambda (asexp) (return* (caddr (get-sexp asexp)))))

(define cadddr^
  (lambda (asexp) (return* (cadddr (get-sexp asexp)))))

(define map^
  (lambda (f asexp) (return* (map f (get-sexp asexp)))))

(define symbol?^
  (lambda (asexp) (return* (symbol? (get-sexp asexp)))))

(define string?^
  (lambda (asexp) (return* (string? (get-sexp asexp)))))

(define length^
  (lambda (asexp) (return* (length (get-sexp asexp)))))

(define eq?^
  (lambda (asexp x) (return* (eq? (get-sexp asexp) x))))

(define vector?^
  (lambda (asexp) (return* (vector? (get-sexp asexp)))))

(define vector->list^
  (lambda (asexp) (return* (vector->list (get-sexp asexp)))))

(define cons^
  (lambda (a b info)
    (if (null?^ b)
        (return* (retag (list a) info))
        (if (pair?^ b)
            (return* (retag (cons a (get-sexp b)) info))
            (return* (retag (cons a b) info))))))

(define ^cdr^
  (lambda (asexp)
    (if (asexp? (cdr^ asexp))
        (return* (cdr^ asexp))
        (return* (retag (cdr^ asexp) 'none)))))

(define null?^
  (lambda (x)
    (return* (and (asexp? x) (null? (get-sexp x))))))

(define pair?^
  (lambda (x)
    (return* (and (asexp? x) (pair? (get-sexp x))))))

(define list?^
  (lambda (x)
    (return* (and (asexp? x) (list-of-asexp? (get-sexp x))))))

(define list-of-asexp?
  (lambda (x)
    (return*
      (or (null? x)
          (and (pair? x)
               (asexp? (car x))
               (or (list-of-asexp? (cdr x)) (list?^ (cdr x))))))))

(define*
  unannotate-cps
  (lambda ()
    (if (asexp? x_reg)
        (begin
          (set! x_reg (get-sexp x_reg))
          (set! pc unannotate-cps))
        (if (pair? x_reg)
            (begin
              (set! k_reg (make-cont '<cont-4> x_reg k_reg))
              (set! x_reg (car x_reg))
              (set! pc unannotate-cps))
            (if (vector? x_reg)
                (begin
                  (set! k_reg (make-cont '<cont-2> k_reg))
                  (set! x_reg (vector->list x_reg))
                  (set! pc unannotate-cps))
                (begin (set! value_reg x_reg) (set! pc apply-cont)))))))

(define*
  reannotate-cps
  (lambda ()
    (if (asexp? x_reg)
        (begin (set! value_reg x_reg) (set! pc apply-cont))
        (if (pair? x_reg)
            (begin
              (set! k_reg (make-cont '<cont-6> k_reg))
              (set! pc reannotate-seq-cps))
            (if (vector? x_reg)
                (begin
                  (set! k_reg (make-cont '<cont-5> k_reg))
                  (set! x_reg (vector->list x_reg))
                  (set! pc reannotate-seq-cps))
                (begin
                  (set! value_reg (retag x_reg 'none))
                  (set! pc apply-cont)))))))

(define*
  reannotate-seq-cps
  (lambda ()
    (if (null? x_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (if (asexp? x_reg)
            (begin (set! value_reg x_reg) (set! pc apply-cont))
            (if (not (pair? x_reg))
                (set! pc reannotate-cps)
                (if (or (null?^ (cdr x_reg)) (pair?^ (cdr x_reg)))
                    (begin
                      (set! x_reg (cons (car x_reg) (get-sexp (cdr x_reg))))
                      (set! pc reannotate-seq-cps))
                    (begin
                      (set! k_reg (make-cont '<cont-7> x_reg k_reg))
                      (set! x_reg (car x_reg))
                      (set! pc reannotate-cps))))))))

(define*
  read-asexp
  (lambda ()
    (let ((start 'undefined) (end 'undefined))
      (set! end (get-token-end (first tokens_reg)))
      (set! start (get-token-start (first tokens_reg)))
      (let ((temp_1 'undefined))
        (set! temp_1 (first tokens_reg))
        (if (eq? (car temp_1) 'integer)
            (let ((str 'undefined))
              (set! str (list-ref temp_1 1))
              (set! value4_reg fail_reg)
              (set! value3_reg (rest-of tokens_reg))
              (set! value2_reg end)
              (set! value1_reg
                (make-asexp src_reg start end (string->integer str)))
              (set! pc apply-cont4))
            (if (eq? (car temp_1) 'decimal)
                (let ((str 'undefined))
                  (set! str (list-ref temp_1 1))
                  (set! value4_reg fail_reg)
                  (set! value3_reg (rest-of tokens_reg))
                  (set! value2_reg end)
                  (set! value1_reg
                    (make-asexp src_reg start end (string->decimal str)))
                  (set! pc apply-cont4))
                (if (eq? (car temp_1) 'rational)
                    (let ((str 'undefined))
                      (set! str (list-ref temp_1 1))
                      (let ((num 'undefined))
                        (set! num (string->rational str))
                        (if (true? num)
                            (begin
                              (set! value4_reg fail_reg)
                              (set! value3_reg (rest-of tokens_reg))
                              (set! value2_reg end)
                              (set! value1_reg (make-asexp src_reg start end num))
                              (set! pc apply-cont4))
                            (begin
                              (set! msg_reg (format "cannot represent ~a" str))
                              (set! pc read-error)))))
                    (if (eq? (car temp_1) 'boolean)
                        (let ((bool 'undefined))
                          (set! bool (list-ref temp_1 1))
                          (set! value4_reg fail_reg)
                          (set! value3_reg (rest-of tokens_reg))
                          (set! value2_reg end)
                          (set! value1_reg (make-asexp src_reg start end bool))
                          (set! pc apply-cont4))
                        (if (eq? (car temp_1) 'character)
                            (let ((char 'undefined))
                              (set! char (list-ref temp_1 1))
                              (set! value4_reg fail_reg)
                              (set! value3_reg (rest-of tokens_reg))
                              (set! value2_reg end)
                              (set! value1_reg (make-asexp src_reg start end char))
                              (set! pc apply-cont4))
                            (if (eq? (car temp_1) 'string)
                                (let ((str 'undefined))
                                  (set! str (list-ref temp_1 1))
                                  (set! value4_reg fail_reg)
                                  (set! value3_reg (rest-of tokens_reg))
                                  (set! value2_reg end)
                                  (set! value1_reg (make-asexp src_reg start end str))
                                  (set! pc apply-cont4))
                                (if (eq? (car temp_1) 'identifier)
                                    (let ((id 'undefined))
                                      (set! id (list-ref temp_1 1))
                                      (set! value4_reg fail_reg)
                                      (set! value3_reg (rest-of tokens_reg))
                                      (set! value2_reg end)
                                      (set! value1_reg (make-asexp src_reg start end id))
                                      (set! pc apply-cont4))
                                    (if (eq? (car temp_1) 'apostrophe)
                                        (begin
                                          (set! keyword_reg 'quote)
                                          (set! pc read-annotated-abbreviation))
                                        (if (eq? (car temp_1) 'backquote)
                                            (begin
                                              (set! keyword_reg 'quasiquote)
                                              (set! pc read-annotated-abbreviation))
                                            (if (eq? (car temp_1) 'comma)
                                                (begin
                                                  (set! keyword_reg 'unquote)
                                                  (set! pc read-annotated-abbreviation))
                                                (if (eq? (car temp_1) 'comma-at)
                                                    (begin
                                                      (set! keyword_reg 'unquote-splicing)
                                                      (set! pc read-annotated-abbreviation))
                                                    (if (eq? (car temp_1) 'lparen)
                                                        (let ((tokens 'undefined))
                                                          (set! tokens (rest-of tokens_reg))
                                                          (set! k_reg (make-cont4 '<cont4-2> src_reg start k_reg))
                                                          (set! expected-terminator_reg 'rparen)
                                                          (set! tokens_reg tokens)
                                                          (set! pc read-asexp-sequence))
                                                        (if (eq? (car temp_1) 'lbracket)
                                                            (let ((tokens 'undefined))
                                                              (set! tokens (rest-of tokens_reg))
                                                              (set! k_reg (make-cont4 '<cont4-2> src_reg start k_reg))
                                                              (set! expected-terminator_reg 'rbracket)
                                                              (set! tokens_reg tokens)
                                                              (set! pc read-asexp-sequence))
                                                            (if (eq? (car temp_1) 'lvector)
                                                                (begin
                                                                  (set! k_reg (make-cont4 '<cont4-1> src_reg start k_reg))
                                                                  (set! tokens_reg (rest-of tokens_reg))
                                                                  (set! pc read-avector-sequence))
                                                                (set! pc unexpected-token-error)))))))))))))))))))

(define*
  read-annotated-abbreviation
  (lambda ()
    (let ((start 'undefined) (keyword-end 'undefined))
      (set! keyword-end (get-token-end (first tokens_reg)))
      (set! start (get-token-start (first tokens_reg)))
      (set! k_reg
        (make-cont4 '<cont4-3> keyword_reg keyword-end src_reg start
          k_reg))
      (set! tokens_reg (rest-of tokens_reg))
      (set! pc read-asexp))))

(define*
  read-avector-sequence
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (eq? (car temp_1) 'rparen)
          (begin
            (set! expected-terminator_reg 'rparen)
            (set! asexps_reg '())
            (set! pc close-asexp-sequence))
          (if (eq? (car temp_1) 'dot)
              (begin
                (set! msg_reg "unexpected dot (.)")
                (set! pc read-error))
              (begin
                (set! k_reg
                  (make-cont4 '<cont4-5> src_reg handler_reg k_reg))
                (set! pc read-asexp)))))))

(define*
  read-asexp-sequence
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (first tokens_reg))
      (if (memq (car temp_1) (list 'rparen 'rbracket))
          (begin (set! asexps_reg '()) (set! pc close-asexp-sequence))
          (if (eq? (car temp_1) 'dot)
              (begin
                (set! msg_reg "unexpected dot (.)")
                (set! pc read-error))
              (begin
                (set! k_reg
                  (make-cont4 '<cont4-7> expected-terminator_reg src_reg
                    handler_reg k_reg))
                (set! pc read-asexp)))))))

(define*
  close-asexp-sequence
  (lambda ()
    (let ((end 'undefined))
      (set! end (get-token-end (first tokens_reg)))
      (let ((temp_1 'undefined))
        (set! temp_1 (first tokens_reg))
        (if (memq (car temp_1) (list 'rparen 'rbracket))
            (if (token-type? (first tokens_reg) expected-terminator_reg)
                (begin
                  (set! value4_reg fail_reg)
                  (set! value3_reg (rest-of tokens_reg))
                  (set! value2_reg end)
                  (set! value1_reg asexps_reg)
                  (set! pc apply-cont4))
                (if (eq? expected-terminator_reg 'rparen)
                    (begin
                      (set! msg_reg "parenthesized list terminated by bracket")
                      (set! pc read-error))
                    (if (eq? expected-terminator_reg 'rbracket)
                        (begin
                          (set! msg_reg "bracketed list terminated by parenthesis")
                          (set! pc read-error)))))
            (set! pc unexpected-token-error))))))

(define make-binding
  (lambda (variable value)
    (return* (list variable "" value))))

(define binding-variable
  (lambda (binding) (return* (car binding))))

(define binding-docstring
  (lambda (binding) (return* (cadr binding))))

(define binding-value
  (lambda (binding) (return* (caddr binding))))

(define set-binding-docstring!
  (lambda (binding docstring)
    (return* (set-car! (cdr binding) docstring))))

(define set-binding-value!
  (lambda (binding value)
    (return* (set-car! (cddr binding) value))))

(define make-frame
  (lambda (variables values)
    (return* (map make-binding variables values))))

(define first-binding
  (lambda (frame) (return* (car frame))))

(define rest-of-bindings
  (lambda (frame) (return* (cdr frame))))

(define empty-frame?
  (lambda (frame) (return* (null? frame))))

(define search-frame
  (lambda (frame variable)
    (if (empty-frame? frame)
        (return* #f)
        (if (eq? (binding-variable (first-binding frame)) variable)
            (return* (first-binding frame))
            (return*
              (search-frame (rest-of-bindings frame) variable))))))

(define environment?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'environment)))))

(define make-empty-environment
  (lambda () (return* (cons 'environment (list '())))))

(define make-initial-environment
  (lambda (vars vals)
    (return*
      (cons 'environment (list (make-frame vars vals))))))

(define first-frame (lambda (env) (return* (cadr env))))

(define frames (lambda (env) (return* (cdr env))))

(define set-first-frame!
  (lambda (env new-frame)
    (return* (set-car! (cdr env) new-frame))))

(define extend
  (lambda (env variables values)
    (return*
      (cons
        'environment
        (cons (make-frame variables values) (cdr env))))))

(define search-env
  (lambda (env variable)
    (return* (search-frames (cdr env) variable))))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
        (return* #f)
        (let ((binding 'undefined))
          (set! binding (search-frame (car frames) variable))
          (if binding
              (return* binding)
              (return* (search-frames (cdr frames) variable)))))))

(define*
  lookup-value
  (lambda ()
    (set! k_reg (make-cont2 '<cont2-3> k_reg))
    (set! pc lookup-binding)))

(define*
  lookup-binding
  (lambda ()
    (let ((binding 'undefined))
      (set! binding (search-env env_reg variable_reg))
      (if binding
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg binding)
            (set! pc apply-cont2))
          (begin
            (set! k_reg
              (make-cont2 '<cont2-4> var-info_reg variable_reg env_reg
                handler_reg k_reg))
            (set! pc split-variable))))))

(define*
  lookup-binding-in-first-frame
  (lambda ()
    (let ((frame 'undefined))
      (set! frame (first-frame env_reg))
      (let ((binding 'undefined))
        (set! binding (search-frame frame var_reg))
        (if binding
            (begin
              (set! value2_reg fail_reg)
              (set! value1_reg binding)
              (set! pc apply-cont2))
            (let ((new-binding 'undefined))
              (set! new-binding (make-binding var_reg 'undefined))
              (let ((new-frame 'undefined))
                (set! new-frame (cons new-binding frame))
                (set-first-frame! env_reg new-frame)
                (set! value2_reg fail_reg)
                (set! value1_reg new-binding)
                (set! pc apply-cont2))))))))

(define*
  lookup-variable-components
  (lambda ()
    (let ((var 'undefined))
      (set! var (car components_reg))
      (set! k_reg
        (make-cont2 '<cont2-5> components_reg path_reg var
          handler_reg k_reg))
      (set! var_reg var)
      (set! pc lookup-module-binding))))

(define*
  lookup-module-binding
  (lambda ()
    (let ((binding 'undefined))
      (set! binding (search-env env_reg var_reg))
      (if binding
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg binding)
            (set! pc apply-cont2))
          (if (dlr-env-contains var_reg)
              (begin
                (set! value2_reg fail_reg)
                (set! value1_reg (dlr-env-lookup var_reg))
                (set! pc apply-cont2))
              (if (string=? path_reg "")
                  (begin
                    (set! exception_reg (format "unbound module '~a'" var_reg))
                    (set! pc apply-handler2))
                  (begin
                    (set! exception_reg
                      (format
                        "unbound variable '~a' in module '~a'"
                        var_reg
                        path_reg))
                    (set! pc apply-handler2))))))))

(define*
  split-variable
  (lambda ()
    (let ((strings 'undefined))
      (set! strings
        (group (string->list (symbol->string variable_reg)) #\.))
      (if (or (member "" strings) (= (length strings) 1))
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg #f)
            (set! pc apply-cont2))
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg (map string->symbol strings))
            (set! pc apply-cont2))))))

(define group
  (lambda (chars delimiter)
    (let ((position 'undefined) (group 'undefined))
      (set! position
        (lambda (chars)
          (if (char=? (car chars) delimiter)
              0
              (+ 1 (position (cdr chars))))))
      (set! group
        (lambda (chars)
          (if (null? chars)
              '()
              (if (not (member delimiter chars))
                  (list (apply string chars))
                  (let ((n 'undefined))
                    (set! n (position chars))
                    (cons
                      (apply string (list-head chars n))
                      (group (cdr (list-tail chars n)))))))))
      (return* (group chars)))))

(define pattern-macro?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'pattern-macro)))))

(define*
  macro-error
  (lambda ()
    (error transformer-name_reg
      "bad concrete syntax: ~a"
      datum_reg)))

(define head
  (lambda (formals)
    (if (symbol? formals)
        (return* '())
        (if (pair? (cdr formals))
            (return* (cons (car formals) (head (cdr formals))))
            (return* (list (car formals)))))))

(define last
  (lambda (formals)
    (if (symbol? formals)
        (return* formals)
        (if (pair? (cdr formals))
            (return* (last (cdr formals)))
            (return* (cdr formals))))))

(define literal?
  (lambda (datum)
    (return*
      (or (number? datum)
          (boolean? datum)
          (char? datum)
          (string? datum)))))

(define anything? (lambda (datum) (return* #t)))

(define get-reserved-keywords
  (lambda ()
    (return*
      (list 'quote 'func 'define! 'quasiquote 'lambda 'if 'set!
       'define 'begin 'cond 'and 'or 'let 'let* 'letrec 'case
       'record-case 'try 'catch 'finally 'raise 'dict 'help
       'choose))))

(define reserved-keyword?
  (lambda (x)
    (return*
      (and (symbol? x)
           (not (eq? (memq x (get-reserved-keywords)) #f))))))

(define*
  create-letrec-assignments^
  (lambda ()
    (if (null? vars_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-7> procs_reg vars_reg k2_reg))
          (set! procs_reg (cdr procs_reg))
          (set! vars_reg (cdr vars_reg))
          (set! pc create-letrec-assignments^)))))

(define*
  nest-let*-bindings^
  (lambda ()
    (if (or (null? bindings_reg) (null? (cdr bindings_reg)))
        (begin
          (set! value_reg
            (append
              (list 'let)
              (append (list bindings_reg) bodies_reg)))
          (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont '<cont-9> bindings_reg k_reg))
          (set! bindings_reg (cdr bindings_reg))
          (set! pc nest-let*-bindings^)))))

(define*
  case-clauses->simple-cond-clauses^
  (lambda ()
    (if (null? clauses_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-10> clauses_reg var_reg k_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc case-clauses->simple-cond-clauses^)))))

(define*
  case-clauses->cond-clauses^
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-9> clauses_reg var_reg k2_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc case-clauses->cond-clauses^)))))

(define*
  record-case-clauses->cond-clauses^
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! value2_reg '())
          (set! value1_reg '())
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (begin
          (set! k2_reg
            (make-cont2 '<cont2-10> clauses_reg var_reg k2_reg))
          (set! clauses_reg (cdr clauses_reg))
          (set! pc record-case-clauses->cond-clauses^)))))

(define define-datatype-variant-names
  (lambda (variants)
    (if (null? variants)
        (return* '())
        (return*
          (cons
            (get-sexp (car^ (car variants)))
            (define-datatype-variant-names (cdr variants)))))))

(define define-datatype-variant-tests
  (lambda (variants)
    (if (null? variants)
        (return* '())
        (if (null? (cdr^ (car variants)))
            (return*
              (cons '() (define-datatype-variant-tests (cdr variants))))
            (return*
              (cons
                (get-sexp (cadr^ (cadr^ (car variants))))
                (define-datatype-variant-tests (cdr variants))))))))

(define make-define-datatype-defines
  (lambda (names tests)
    (if (null? names)
        (return* '())
        (if (null? (car tests))
            (return*
              (cons
                (append
                  (list 'define)
                  (append
                    (list (car names))
                    (list
                      (append
                        (list 'lambda)
                        (append
                          (list 'args)
                          (list
                            (append
                              (list 'cons)
                              (append
                                (list (append (list 'quote) (list (car names))))
                                (list 'args)))))))))
                (make-define-datatype-defines (cdr names) (cdr tests))))
            (return*
              (cons
                (append
                  (list 'define)
                  (append
                    (list (car names))
                    (list
                      (append
                        (list 'lambda)
                        (append
                          (list 'args)
                          (list
                            (append
                              (list 'if)
                              (append
                                (list
                                  (append
                                    (list 'apply)
                                    (append
                                      (list (car tests))
                                      (list
                                        (append
                                          (list 'list)
                                          (list (append (list 'car) (list 'args))))))))
                                (append
                                  (list
                                    (append
                                      (list 'cons)
                                      (append
                                        (list (append (list 'quote) (list (car names))))
                                        (list 'args))))
                                  (list
                                    (append
                                      (list 'error)
                                      (append
                                        (list (append (list 'quote) (list (car names))))
                                        (append
                                          (list "'~s' not a valid value")
                                          (list (append (list 'car) (list 'args))))))))))))))))
                (make-define-datatype-defines (cdr names) (cdr tests))))))))

(define make-macro-env^
  (lambda ()
    (return*
      (make-initial-environment
        (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case
          'define-datatype 'cases)
        (list and-transformer^ or-transformer^ cond-transformer^
          let-transformer^ letrec-transformer^ let*-transformer^
          case-transformer^ record-case-transformer^
          define-datatype-transformer^ cases-transformer^)))))

(define*
  amacro-error
  (lambda ()
    (set! k_reg (make-cont '<cont-11> transformer-name_reg))
    (set! x_reg adatum_reg)
    (set! pc unannotate-cps)))

(define make-pattern-macro^
  (lambda (clauses aclauses)
    (return* (list 'pattern-macro clauses aclauses))))

(define macro-clauses^
  (lambda (macro) (return* (cadr macro))))

(define macro-aclauses^
  (lambda (macro) (return* (caddr macro))))

(define application?^
  (lambda (asexp)
    (return*
      (and (list?^ asexp)
           (not (null?^ asexp))
           (not (reserved-keyword? (get-sexp (car^ asexp))))))))

(define mit-style?^
  (lambda (asexp) (return* (not (symbol?^ (cadr^ asexp))))))

(define literal?^
  (lambda (asexp)
    (let ((s 'undefined))
      (set! s (get-sexp asexp))
      (return*
        (or (number? s) (boolean? s) (char? s) (string? s))))))

(define syntactic-sugar?^
  (lambda (asexp)
    (return*
      (and (pair?^ asexp)
           (symbol?^ (car^ asexp))
           (true? (search-env macro-env (get-sexp (car^ asexp))))))))

(define tagged-list^
  (lambda (tag op len)
    (lambda (asexp)
      (return*
        (and (list?^ asexp)
             (op (length^ asexp) len)
             (symbol?^ (car^ asexp))
             (eq? (get-sexp (car^ asexp)) tag))))))

(define try-body^ (lambda (x) (return* (cadr^ x))))

(define catch-var^ (lambda (x) (return* (cadr^ x))))

(define catch-exps^ (lambda (x) (return* (cddr^ x))))

(define finally-exps^
  (lambda (x) (return* (cdr (get-sexp x)))))

(define*
  aparse
  (lambda ()
    (let ((info 'undefined))
      (set! info (get-source-info adatum_reg))
      (if (null?^ adatum_reg)
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg (lit-aexp (get-sexp adatum_reg) info))
            (set! pc apply-cont2))
          (if (literal?^ adatum_reg)
              (begin
                (set! value2_reg fail_reg)
                (set! value1_reg (lit-aexp (get-sexp adatum_reg) info))
                (set! pc apply-cont2))
              (if (vector?^ adatum_reg)
                  (begin
                    (set! k_reg (make-cont '<cont-19> info fail_reg k_reg))
                    (set! x_reg adatum_reg)
                    (set! pc unannotate-cps))
                  (if (symbol?^ adatum_reg)
                      (begin
                        (set! value2_reg fail_reg)
                        (set! value1_reg (var-aexp (get-sexp adatum_reg) info))
                        (set! pc apply-cont2))
                      (if (quote?^ adatum_reg)
                          (begin
                            (set! k_reg (make-cont '<cont-19> info fail_reg k_reg))
                            (set! x_reg (cadr^ adatum_reg))
                            (set! pc unannotate-cps))
                          (if (quasiquote?^ adatum_reg)
                              (begin
                                (set! k_reg
                                  (make-cont '<cont-18> adatum_reg handler_reg fail_reg
                                    k_reg))
                                (set! depth_reg 0)
                                (set! ax_reg (cadr^ adatum_reg))
                                (set! pc qq-expand-cps))
                              (if (unquote?^ adatum_reg)
                                  (begin (set! msg_reg "misplaced") (set! pc aparse-error))
                                  (if (unquote-splicing?^ adatum_reg)
                                      (begin (set! msg_reg "misplaced") (set! pc aparse-error))
                                      (if (syntactic-sugar?^ adatum_reg)
                                          (begin
                                            (set! k_reg (make-cont2 '<cont2-38> handler_reg k_reg))
                                            (set! pc expand-once^))
                                          (if (if-then?^ adatum_reg)
                                              (begin
                                                (set! k_reg
                                                  (make-cont2 '<cont2-37> adatum_reg info handler_reg k_reg))
                                                (set! adatum_reg (cadr^ adatum_reg))
                                                (set! pc aparse))
                                              (if (if-else?^ adatum_reg)
                                                  (begin
                                                    (set! k_reg
                                                      (make-cont2 '<cont2-35> adatum_reg info handler_reg k_reg))
                                                    (set! adatum_reg (cadr^ adatum_reg))
                                                    (set! pc aparse))
                                                  (if (assignment?^ adatum_reg)
                                                      (begin
                                                        (set! k_reg (make-cont2 '<cont2-32> adatum_reg info k_reg))
                                                        (set! adatum_reg (caddr^ adatum_reg))
                                                        (set! pc aparse))
                                                      (if (func?^ adatum_reg)
                                                          (begin
                                                            (set! k_reg (make-cont2 '<cont2-31> info k_reg))
                                                            (set! adatum_reg (cadr^ adatum_reg))
                                                            (set! pc aparse))
                                                          (if (define?^ adatum_reg)
                                                              (if (mit-style?^ adatum_reg)
                                                                  (begin
                                                                    (set! k_reg
                                                                      (make-cont '<cont-16> info handler_reg fail_reg k_reg))
                                                                    (set! datum_reg adatum_reg)
                                                                    (set! macro_reg mit-define-transformer^)
                                                                    (set! pc apply-macro))
                                                                  (if (= (length^ adatum_reg) 3)
                                                                      (begin
                                                                        (set! k_reg (make-cont2 '<cont2-30> adatum_reg info k_reg))
                                                                        (set! adatum_reg (caddr^ adatum_reg))
                                                                        (set! pc aparse))
                                                                      (if (and (= (length^ adatum_reg) 4)
                                                                               (string?^ (caddr^ adatum_reg)))
                                                                          (begin
                                                                            (set! k_reg (make-cont2 '<cont2-29> adatum_reg info k_reg))
                                                                            (set! adatum_reg (cadddr^ adatum_reg))
                                                                            (set! pc aparse))
                                                                          (begin
                                                                            (set! msg_reg "bad concrete syntax:")
                                                                            (set! pc aparse-error)))))
                                                              (if (define!?^ adatum_reg)
                                                                  (if (mit-style?^ adatum_reg)
                                                                      (begin
                                                                        (set! k_reg
                                                                          (make-cont '<cont-16> info handler_reg fail_reg k_reg))
                                                                        (set! datum_reg adatum_reg)
                                                                        (set! macro_reg mit-define-transformer^)
                                                                        (set! pc apply-macro))
                                                                      (if (= (length^ adatum_reg) 3)
                                                                          (begin
                                                                            (set! k_reg (make-cont2 '<cont2-28> adatum_reg info k_reg))
                                                                            (set! adatum_reg (caddr^ adatum_reg))
                                                                            (set! pc aparse))
                                                                          (if (and (= (length^ adatum_reg) 4)
                                                                                   (string?^ (caddr^ adatum_reg)))
                                                                              (begin
                                                                                (set! k_reg (make-cont2 '<cont2-27> adatum_reg info k_reg))
                                                                                (set! adatum_reg (cadddr^ adatum_reg))
                                                                                (set! pc aparse))
                                                                              (begin
                                                                                (set! msg_reg "bad concrete syntax:")
                                                                                (set! pc aparse-error)))))
                                                                  (if (define-syntax?^ adatum_reg)
                                                                      (let ((name 'undefined) (aclauses 'undefined))
                                                                        (set! aclauses (cddr^ adatum_reg))
                                                                        (set! name (get-sexp (cadr^ adatum_reg)))
                                                                        (set! k_reg
                                                                          (make-cont '<cont-14> aclauses name info fail_reg k_reg))
                                                                        (set! x_reg aclauses)
                                                                        (set! pc unannotate-cps))
                                                                      (if (begin?^ adatum_reg)
                                                                          (begin
                                                                            (set! k_reg
                                                                              (make-cont2 '<cont2-26> adatum_reg info handler_reg k_reg))
                                                                            (set! adatum-list_reg (cdr^ adatum_reg))
                                                                            (set! pc aparse-all))
                                                                          (if (lambda?^ adatum_reg)
                                                                              (begin
                                                                                (set! k_reg (make-cont2 '<cont2-25> adatum_reg info k_reg))
                                                                                (set! adatum-list_reg (cddr^ adatum_reg))
                                                                                (set! pc aparse-all))
                                                                              (if (trace-lambda?^ adatum_reg)
                                                                                  (begin
                                                                                    (set! k_reg (make-cont2 '<cont2-24> adatum_reg info k_reg))
                                                                                    (set! adatum-list_reg (cdddr^ adatum_reg))
                                                                                    (set! pc aparse-all))
                                                                                  (if (try?^ adatum_reg)
                                                                                      (if (= (length^ adatum_reg) 2)
                                                                                          (begin
                                                                                            (set! adatum_reg (try-body^ adatum_reg))
                                                                                            (set! pc aparse))
                                                                                          (if (and (= (length^ adatum_reg) 3)
                                                                                                   (catch?^ (caddr^ adatum_reg)))
                                                                                              (begin
                                                                                                (set! k_reg
                                                                                                  (make-cont2 '<cont2-23> adatum_reg info handler_reg k_reg))
                                                                                                (set! adatum_reg (try-body^ adatum_reg))
                                                                                                (set! pc aparse))
                                                                                              (if (and (= (length^ adatum_reg) 3)
                                                                                                       (finally?^ (caddr^ adatum_reg)))
                                                                                                  (begin
                                                                                                    (set! k_reg
                                                                                                      (make-cont2 '<cont2-21> adatum_reg info handler_reg k_reg))
                                                                                                    (set! adatum_reg (try-body^ adatum_reg))
                                                                                                    (set! pc aparse))
                                                                                                  (if (and (= (length^ adatum_reg) 4)
                                                                                                           (catch?^ (caddr^ adatum_reg))
                                                                                                           (finally?^ (cadddr^ adatum_reg)))
                                                                                                      (begin
                                                                                                        (set! k_reg
                                                                                                          (make-cont2 '<cont2-19> adatum_reg info handler_reg k_reg))
                                                                                                        (set! adatum_reg (try-body^ adatum_reg))
                                                                                                        (set! pc aparse))
                                                                                                      (begin
                                                                                                        (set! msg_reg "bad try syntax:")
                                                                                                        (set! pc aparse-error))))))
                                                                                      (if (raise?^ adatum_reg)
                                                                                          (begin
                                                                                            (set! k_reg (make-cont2 '<cont2-16> info k_reg))
                                                                                            (set! adatum_reg (cadr^ adatum_reg))
                                                                                            (set! pc aparse))
                                                                                          (if (dict?^ adatum_reg)
                                                                                              (begin
                                                                                                (set! k_reg (make-cont2 '<cont2-15> info k_reg))
                                                                                                (set! entries_reg (cdr^ adatum_reg))
                                                                                                (set! pc aparse-entries))
                                                                                              (if (help?^ adatum_reg)
                                                                                                  (if (symbol?^ (cadr^ adatum_reg))
                                                                                                      (let ((var 'undefined) (var-info 'undefined))
                                                                                                        (set! var-info (get-source-info (cadr^ adatum_reg)))
                                                                                                        (set! var (get-sexp (cadr^ adatum_reg)))
                                                                                                        (set! value2_reg fail_reg)
                                                                                                        (set! value1_reg (help-aexp var var-info info))
                                                                                                        (set! pc apply-cont2))
                                                                                                      (begin
                                                                                                        (set! msg_reg "bad concrete syntax:")
                                                                                                        (set! pc aparse-error)))
                                                                                                  (if (choose?^ adatum_reg)
                                                                                                      (begin
                                                                                                        (set! k_reg (make-cont2 '<cont2-14> info k_reg))
                                                                                                        (set! adatum-list_reg (cdr^ adatum_reg))
                                                                                                        (set! pc aparse-all))
                                                                                                      (if (application?^ adatum_reg)
                                                                                                          (begin
                                                                                                            (set! k_reg
                                                                                                              (make-cont2 '<cont2-13> adatum_reg info handler_reg k_reg))
                                                                                                            (set! adatum_reg (car^ adatum_reg))
                                                                                                            (set! pc aparse))
                                                                                                          (begin
                                                                                                            (set! msg_reg "bad concrete syntax:")
                                                                                                            (set! pc aparse-error))))))))))))))))))))))))))))))

(define*
  aparse-error
  (lambda ()
    (let ((info 'undefined))
      (set! info (get-source-info adatum_reg))
      (set! k_reg
        (make-cont '<cont-20> msg_reg info handler_reg fail_reg))
      (set! x_reg adatum_reg)
      (set! pc unannotate-cps))))

(define*
  expand-once^
  (lambda ()
    (set! k_reg
      (make-cont2 '<cont2-39> adatum_reg handler_reg k_reg))
    (set! var-info_reg 'none)
    (set! env_reg macro-env)
    (set! variable_reg (get-sexp (car^ adatum_reg)))
    (set! pc lookup-value)))

(define*
  process-macro-clauses^
  (lambda ()
    (if (null? clauses_reg)
        (begin
          (set! msg_reg "no matching clause found for")
          (set! pc aparse-error))
        (let ((left-pattern 'undefined)
              (right-pattern 'undefined)
              (aleft-pattern 'undefined)
              (aright-pattern 'undefined))
          (set! aright-pattern (cadr^ (car aclauses_reg)))
          (set! aleft-pattern (car^ (car aclauses_reg)))
          (set! right-pattern (cadar clauses_reg))
          (set! left-pattern (caar clauses_reg))
          (set! k_reg
            (make-cont '<cont-24> aclauses_reg adatum_reg aleft-pattern
              aright-pattern clauses_reg left-pattern right-pattern
              handler_reg fail_reg k_reg))
          (set! x_reg adatum_reg)
          (set! pc unannotate-cps)))))

(define*
  aparse-entries
  (lambda ()
    (if (null? entries_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont2 '<cont2-42> entries_reg handler_reg k_reg))
          (set! adatum-list_reg (get-sexp (car entries_reg)))
          (set! pc aparse-all)))))

(define*
  aparse-all
  (lambda ()
    (if (null? adatum-list_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont2 '<cont2-43> adatum-list_reg handler_reg k_reg))
          (set! adatum_reg (car adatum-list_reg))
          (set! pc aparse)))))

(define*
  aparse-sexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont4 '<cont4-8> src_reg handler_reg k_reg))
          (set! pc read-asexp)))))

(define*
  qq-expand-cps
  (lambda ()
    (if (quasiquote?^ ax_reg)
        (begin
          (set! k_reg (make-cont '<cont-29> k_reg))
          (set! depth_reg (+ depth_reg 1))
          (set! ax_reg (^cdr^ ax_reg))
          (set! pc qq-expand-cps))
        (if (or (unquote?^ ax_reg) (unquote-splicing?^ ax_reg))
            (if (> depth_reg 0)
                (begin
                  (set! k_reg (make-cont '<cont-28> ax_reg k_reg))
                  (set! depth_reg (- depth_reg 1))
                  (set! ax_reg (^cdr^ ax_reg))
                  (set! pc qq-expand-cps))
                (if (and (unquote?^ ax_reg)
                         (not (null? (cdr^ ax_reg)))
                         (null? (cddr^ ax_reg)))
                    (begin (set! value_reg (cadr^ ax_reg)) (set! pc apply-cont))
                    (begin
                      (set! value_reg (append (list 'quote) (list ax_reg)))
                      (set! pc apply-cont))))
            (if (vector?^ ax_reg)
                (begin
                  (set! k_reg (make-cont '<cont-27> k_reg))
                  (set! ax_reg (retag (vector->list^ ax_reg) 'none))
                  (set! pc qq-expand-cps))
                (if (not (pair?^ ax_reg))
                    (begin
                      (set! value_reg (append (list 'quote) (list ax_reg)))
                      (set! pc apply-cont))
                    (if (null? (cdr^ ax_reg))
                        (begin
                          (set! ax_reg (car^ ax_reg))
                          (set! pc qq-expand-list-cps))
                        (begin
                          (set! k_reg (make-cont '<cont-26> ax_reg depth_reg k_reg))
                          (set! ax_reg (car^ ax_reg))
                          (set! pc qq-expand-list-cps)))))))))

(define*
  qq-expand-list-cps
  (lambda ()
    (if (quasiquote?^ ax_reg)
        (begin
          (set! k_reg (make-cont '<cont-34> k_reg))
          (set! depth_reg (+ depth_reg 1))
          (set! ax_reg (^cdr^ ax_reg))
          (set! pc qq-expand-cps))
        (if (or (unquote?^ ax_reg) (unquote-splicing?^ ax_reg))
            (if (> depth_reg 0)
                (begin
                  (set! k_reg (make-cont '<cont-33> ax_reg k_reg))
                  (set! depth_reg (- depth_reg 1))
                  (set! ax_reg (^cdr^ ax_reg))
                  (set! pc qq-expand-cps))
                (if (unquote?^ ax_reg)
                    (begin
                      (set! value_reg (append (list 'list) (^cdr^ ax_reg)))
                      (set! pc apply-cont))
                    (if (null? (cddr^ ax_reg))
                        (begin (set! value_reg (cadr^ ax_reg)) (set! pc apply-cont))
                        (begin
                          (set! value_reg (append (list 'append) (^cdr^ ax_reg)))
                          (set! pc apply-cont)))))
            (if (vector?^ ax_reg)
                (begin
                  (set! k_reg (make-cont '<cont-32> k_reg))
                  (set! pc qq-expand-cps))
                (if (not (pair?^ ax_reg))
                    (begin
                      (set! value_reg (append (list 'quote) (list (list ax_reg))))
                      (set! pc apply-cont))
                    (if (null? (cdr^ ax_reg))
                        (begin
                          (set! k_reg (make-cont '<cont-32> k_reg))
                          (set! ax_reg (car^ ax_reg))
                          (set! pc qq-expand-list-cps))
                        (begin
                          (set! k_reg (make-cont '<cont-31> ax_reg depth_reg k_reg))
                          (set! ax_reg (car^ ax_reg))
                          (set! pc qq-expand-list-cps)))))))))

(define*
  unparse-exps
  (lambda ()
    (if (null? exps_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (return*
          (unparse
            (car exps_reg)
            handle
            fail_reg
            (lambda (v1)
              (set! k_reg (make-cont '<cont-35> v1 fail_reg k_reg))
              (set! exps_reg (cdr exps_reg))
              (set! pc unparse-exps)))))))

(define aunparse
  (lambda (aexp)
    (let ((ae 'undefined))
      (set! ae (car aexp))
      (if (eq? ae 'lit-aexp)
          (let ((datum 'undefined))
            (set! datum (cadr aexp))
            (if (literal? datum)
                (return* datum)
                (if (vector? datum)
                    (return* datum)
                    (return* (append (list 'quote) (list datum))))))
          (if (eq? ae 'var-aexp)
              (let ((id 'undefined)) (set! id (cadr aexp)) (return* id))
              (if (eq? ae 'if-aexp)
                  (let ((test-aexp 'undefined)
                        (then-aexp 'undefined)
                        (else-aexp 'undefined))
                    (set! else-aexp (cadddr aexp))
                    (set! then-aexp (caddr aexp))
                    (set! test-aexp (cadr aexp))
                    (return*
                      (append
                        (list 'if)
                        (append
                          (list (aunparse test-aexp))
                          (append
                            (list (aunparse then-aexp))
                            (list (aunparse else-aexp)))))))
                  (if (eq? ae 'assign-aexp)
                      (let ((var 'undefined) (rhs-exp 'undefined))
                        (set! rhs-exp (caddr aexp))
                        (set! var (cadr aexp))
                        (return*
                          (append
                            (list 'set!)
                            (append (list var) (list (aunparse rhs-exp))))))
                      (if (eq? ae 'func-aexp)
                          (let ((exp 'undefined))
                            (set! exp (cadr aexp))
                            (return* (append (list 'func) (list (aunparse exp)))))
                          (if (eq? ae 'define-aexp)
                              (let ((id 'undefined)
                                    (docstring 'undefined)
                                    (rhs-exp 'undefined))
                                (set! rhs-exp (cadddr aexp))
                                (set! docstring (caddr aexp))
                                (set! id (cadr aexp))
                                (if (string=? docstring "")
                                    (return*
                                      (append
                                        (list 'define)
                                        (append (list id) (list (aunparse rhs-exp)))))
                                    (return*
                                      (append
                                        (list 'define)
                                        (append
                                          (list id)
                                          (append (list docstring) (list (aunparse rhs-exp))))))))
                              (if (eq? ae 'define!-aexp)
                                  (let ((id 'undefined)
                                        (docstring 'undefined)
                                        (rhs-exp 'undefined))
                                    (set! rhs-exp (cadddr aexp))
                                    (set! docstring (caddr aexp))
                                    (set! id (cadr aexp))
                                    (if (string=? docstring "")
                                        (return*
                                          (append
                                            (list 'define!)
                                            (append (list id) (list (aunparse rhs-exp)))))
                                        (return*
                                          (append
                                            (list 'define!)
                                            (append
                                              (list id)
                                              (append (list docstring) (list (aunparse rhs-exp))))))))
                                  (if (eq? ae 'define-syntax-aexp)
                                      (let ((name 'undefined) (clauses 'undefined))
                                        (set! clauses (caddr aexp))
                                        (set! name (cadr aexp))
                                        (return*
                                          (append
                                            (list 'define-syntax)
                                            (append (list name) clauses))))
                                      (if (eq? ae 'begin-aexp)
                                          (let ((exps 'undefined))
                                            (set! exps (cadr aexp))
                                            (return* (append (list 'begin) (map aunparse exps))))
                                          (if (eq? ae 'lambda-aexp)
                                              (let ((formals 'undefined) (bodies 'undefined))
                                                (set! bodies (caddr aexp))
                                                (set! formals (cadr aexp))
                                                (return*
                                                  (append
                                                    (list 'lambda)
                                                    (append (list formals) (map aunparse bodies)))))
                                              (if (eq? ae 'mu-lambda-aexp)
                                                  (let ((formals 'undefined)
                                                        (runt 'undefined)
                                                        (bodies 'undefined))
                                                    (set! bodies (cadddr aexp))
                                                    (set! runt (caddr aexp))
                                                    (set! formals (cadr aexp))
                                                    (return*
                                                      (append
                                                        (list 'lambda)
                                                        (append
                                                          (list (append formals runt))
                                                          (map aunparse bodies)))))
                                                  (if (eq? ae 'trace-lambda-aexp)
                                                      (let ((name 'undefined)
                                                            (formals 'undefined)
                                                            (bodies 'undefined))
                                                        (set! bodies (cadddr aexp))
                                                        (set! formals (caddr aexp))
                                                        (set! name (cadr aexp))
                                                        (return*
                                                          (append
                                                            (list 'trace-lambda)
                                                            (append
                                                              (list name)
                                                              (append (list formals) (map aunparse bodies))))))
                                                      (if (eq? ae 'mu-trace-lambda-aexp)
                                                          (let ((name 'undefined)
                                                                (formals 'undefined)
                                                                (runt 'undefined)
                                                                (bodies 'undefined))
                                                            (set! bodies (car (cddddr aexp)))
                                                            (set! runt (cadddr aexp))
                                                            (set! formals (caddr aexp))
                                                            (set! name (cadr aexp))
                                                            (return*
                                                              (append
                                                                (list 'trace-lambda)
                                                                (append
                                                                  (list name)
                                                                  (append
                                                                    (list (append formals runt))
                                                                    (map aunparse bodies))))))
                                                          (if (eq? ae 'app-aexp)
                                                              (let ((operator 'undefined) (operands 'undefined))
                                                                (set! operands (caddr aexp))
                                                                (set! operator (cadr aexp))
                                                                (return*
                                                                  (append
                                                                    (list (aunparse operator))
                                                                    (map aunparse operands))))
                                                              (if (eq? ae 'try-catch-aexp)
                                                                  (let ((body 'undefined)
                                                                        (catch-var 'undefined)
                                                                        (catch-exps 'undefined))
                                                                    (set! catch-exps (cadddr aexp))
                                                                    (set! catch-var (caddr aexp))
                                                                    (set! body (cadr aexp))
                                                                    (return*
                                                                      (append
                                                                        (list 'try)
                                                                        (append
                                                                          (list (aunparse body))
                                                                          (list
                                                                            (append
                                                                              (list 'catch)
                                                                              (append (list catch-var) (map aunparse catch-exps))))))))
                                                                  (if (eq? ae 'try-finally-aexp)
                                                                      (let ((body 'undefined) (finally-exps 'undefined))
                                                                        (set! finally-exps (caddr aexp))
                                                                        (set! body (cadr aexp))
                                                                        (return*
                                                                          (append
                                                                            (list 'try)
                                                                            (append
                                                                              (list (aunparse body))
                                                                              (list
                                                                                (append (list 'finally) (map aunparse finally-exps)))))))
                                                                      (if (eq? ae 'try-catch-finally-aexp)
                                                                          (let ((body 'undefined)
                                                                                (catch-var 'undefined)
                                                                                (catch-exps 'undefined)
                                                                                (finally-exps 'undefined))
                                                                            (set! finally-exps (car (cddddr aexp)))
                                                                            (set! catch-exps (cadddr aexp))
                                                                            (set! catch-var (caddr aexp))
                                                                            (set! body (cadr aexp))
                                                                            (return*
                                                                              (append
                                                                                (list 'try)
                                                                                (append
                                                                                  (list (aunparse body))
                                                                                  (append
                                                                                    (list
                                                                                      (append
                                                                                        (list 'catch)
                                                                                        (append (list catch-var) (map aunparse catch-exps))))
                                                                                    (list
                                                                                      (append (list 'finally) (map aunparse finally-exps))))))))
                                                                          (if (eq? ae 'raise-aexp)
                                                                              (let ((exp 'undefined))
                                                                                (set! exp (cadr aexp))
                                                                                (return* (append (list 'raise) (list (aunparse exp)))))
                                                                              (if (eq? ae 'help-aexp)
                                                                                  (let ((var 'undefined))
                                                                                    (set! var (cadr aexp))
                                                                                    (return* (append (list 'help) (list var))))
                                                                                  (if (eq? ae 'choose-aexp)
                                                                                      (let ((exps 'undefined))
                                                                                        (set! exps (cadr aexp))
                                                                                        (return* (append (list 'choose) (map aunparse exps))))
                                                                                      (error 'unparse
                                                                                        "bad concrete syntax: ~s"
                                                                                        aexp))))))))))))))))))))))))

(define dlr-exp? (lambda (x) (return* #f)))

(define dlr-func (lambda (x) (return* x)))

(define dlr-env-contains (lambda (x) (return* #f)))

(define dlr-env-lookup (lambda (x) (return* #f)))

(define dlr-object? (lambda (x) (return* #f)))

(define dlr-lookup-components (lambda (x y) (return* #f)))

(define set-global-value! (lambda (var x) (return* #f)))

(define set-global-docstring! (lambda (var x) (return* #f)))

(define using-prim (lambda ignore (return* #f)))

(define iterator? (lambda ignore (return* #f)))

(define get_type (lambda (x) (return* 'unknown)))

(define read-line
  (lambda (prompt)
    (printf prompt)
    (let ((input 'undefined))
      (set! input (read))
      (return* (format "~s" input)))))

(define raw-read-line
  (lambda (prompt)
    (printf prompt)
    (let ((loop 'undefined))
      (set! loop
        (lambda (input)
          (if (string? input)
              input
              (begin
                (printf
                  "Error: input must be enclosed in quotation marks.\n==> ")
                (loop (read))))))
      (return* (loop (read))))))

(define start
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! pc read-eval-print-loop)))

(define restart
  (lambda ()
    (printf "Restarting...\n")
    (set! pc read-eval-print-loop)))

(define*
  read-eval-print-loop
  (lambda ()
    (let ((input 'undefined))
      (set! input (raw-read-line "==> "))
      (let ((result 'undefined))
        (set! result (execute input 'stdin))
        (if (not (void? result)) (safe-print result))
        (if *need-newline* (newline))
        (if (end-of-session? result)
            (begin (set! final_reg 'goodbye) (set! pc #f))
            (set! pc read-eval-print-loop))))))

(define exception?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'exception)))))

(define execute-string
  (lambda (input) (return* (execute input 'stdin))))

(define execute-file
  (lambda (filename)
    (return* (execute (read-content filename) filename))))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (let ((result 'undefined))
      (set! result
        (begin
          (set! k_reg REP-k)
          (set! fail_reg *last-fail*)
          (set! handler_reg REP-handler)
          (set! src_reg src)
          (set! input_reg input)
          (set! pc scan-input)))
      (if (exception? result)
          (return* result)
          (begin
            (set! *tokens-left* result)
            (if (token-type? (first *tokens-left*) 'end-marker)
                (return* void-value)
                (return* (execute-loop src))))))))

(define execute-loop
  (lambda (src)
    (let ((result 'undefined))
      (set! result (execute-next-expression src))
      (if (or (exception? result)
              (end-of-session? result)
              (token-type? (first *tokens-left*) 'end-marker))
          (return* result)
          (return* (execute-loop src))))))

(define execute-next-expression
  (lambda (src)
    (set! k_reg (make-cont4 '<cont4-9>))
    (set! fail_reg *last-fail*)
    (set! handler_reg REP-handler)
    (set! src_reg src)
    (set! tokens_reg *tokens-left*)
    (set! pc read-asexp)))

(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (set! *last-fail* REP-fail)))

(define execute-string-rm
  (lambda (input) (return* (execute-rm input 'stdin))))

(define execute-file-rm
  (lambda (filename)
    (return* (execute-rm (read-content filename) filename))))

(define execute-rm
  (lambda (input src)
    (set! load-stack '())
    (set! k_reg REP-k)
    (set! fail_reg *last-fail*)
    (set! handler_reg REP-handler)
    (set! src_reg src)
    (set! input_reg input)
    (set! pc scan-input)
    (let ((result 'undefined))
      (set! result (trampoline))
      (if (exception? result)
          (return* result)
          (begin
            (set! *tokens-left* result)
            (if (token-type? (first *tokens-left*) 'end-marker)
                (return* void-value)
                (return* (execute-loop-rm src))))))))

(define execute-loop-rm
  (lambda (src)
    (execute-next-expression src)
    (let ((result 'undefined))
      (set! result (trampoline))
      (if (or (exception? result)
              (end-of-session? result)
              (token-type? (first *tokens-left*) 'end-marker))
          (return* result)
          (return* (execute-loop-rm src))))))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (set! k_reg (make-cont2 '<cont2-49>))
    (set! fail_reg *last-fail*)
    (set! handler_reg try-parse-handler)
    (set! src_reg 'stdin)
    (set! input_reg input)
    (set! pc scan-input)
    (return* (trampoline))))

(define*
  read-and-eval-asexps
  (lambda ()
    (if (token-type? (first tokens_reg) 'end-marker)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg void-value)
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont4 '<cont4-10> src_reg env_reg handler_reg k_reg))
          (set! pc read-asexp)))))

(define make-debugging-k
  (lambda (exp k)
    (if (not *tracing-on?*)
        (return* k)
        (return* (make-cont2 '<cont2-52> exp k)))))

(define highlight-expression
  (lambda (exp)
    (printf "call: ~s~%" (aunparse exp))
    (let ((info 'undefined))
      (set! info (rac exp))
      (if (not (eq? info 'none))
          (printf
            "['~a' at line ~a column ~a]~%"
            (get-srcfile info)
            (get-start-line info)
            (get-start-char info))))))

(define handle-debug-info
  (lambda (exp result)
    (printf "~s evaluates to ~a~%" (aunparse exp) result)))

(define*
  m
  (lambda ()
    (if *tracing-on?* (highlight-expression exp_reg))
    (let ((k 'undefined))
      (set! k (make-debugging-k exp_reg k_reg))
      (if (eq? (car exp_reg) 'lit-aexp)
          (let ((datum 'undefined))
            (set! datum (list-ref exp_reg 1))
            (set! value2_reg fail_reg)
            (set! value1_reg datum)
            (set! k_reg k)
            (set! pc apply-cont2))
          (if (eq? (car exp_reg) 'var-aexp)
              (let ((id 'undefined) (info 'undefined))
                (set! info (list-ref exp_reg 2))
                (set! id (list-ref exp_reg 1))
                (set! k_reg k)
                (set! var-info_reg info)
                (set! variable_reg id)
                (set! pc lookup-value))
              (if (eq? (car exp_reg) 'func-aexp)
                  (let ((exp 'undefined))
                    (set! exp (list-ref exp_reg 1))
                    (set! k_reg (make-cont2 '<cont2-66> k))
                    (set! exp_reg exp)
                    (set! pc m))
                  (if (eq? (car exp_reg) 'if-aexp)
                      (let ((test-exp 'undefined)
                            (then-exp 'undefined)
                            (else-exp 'undefined))
                        (set! else-exp (list-ref exp_reg 3))
                        (set! then-exp (list-ref exp_reg 2))
                        (set! test-exp (list-ref exp_reg 1))
                        (set! k_reg
                          (make-cont2 '<cont2-65> else-exp then-exp env_reg
                            handler_reg k))
                        (set! exp_reg test-exp)
                        (set! pc m))
                      (if (eq? (car exp_reg) 'assign-aexp)
                          (let ((var 'undefined)
                                (rhs-exp 'undefined)
                                (var-info 'undefined))
                            (set! var-info (list-ref exp_reg 3))
                            (set! rhs-exp (list-ref exp_reg 2))
                            (set! var (list-ref exp_reg 1))
                            (set! k_reg
                              (make-cont2 '<cont2-64> var var-info env_reg handler_reg k))
                            (set! exp_reg rhs-exp)
                            (set! pc m))
                          (if (eq? (car exp_reg) 'define-aexp)
                              (let ((var 'undefined)
                                    (docstring 'undefined)
                                    (rhs-exp 'undefined))
                                (set! rhs-exp (list-ref exp_reg 3))
                                (set! docstring (list-ref exp_reg 2))
                                (set! var (list-ref exp_reg 1))
                                (set! k_reg
                                  (make-cont2 '<cont2-62> docstring var env_reg handler_reg
                                    k))
                                (set! exp_reg rhs-exp)
                                (set! pc m))
                              (if (eq? (car exp_reg) 'define!-aexp)
                                  (let ((var 'undefined)
                                        (docstring 'undefined)
                                        (rhs-exp 'undefined))
                                    (set! rhs-exp (list-ref exp_reg 3))
                                    (set! docstring (list-ref exp_reg 2))
                                    (set! var (list-ref exp_reg 1))
                                    (set! k_reg (make-cont2 '<cont2-60> docstring var k))
                                    (set! exp_reg rhs-exp)
                                    (set! pc m))
                                  (if (eq? (car exp_reg) 'define-syntax-aexp)
                                      (let ((name 'undefined)
                                            (clauses 'undefined)
                                            (aclauses 'undefined))
                                        (set! aclauses (list-ref exp_reg 3))
                                        (set! clauses (list-ref exp_reg 2))
                                        (set! name (list-ref exp_reg 1))
                                        (set! k_reg (make-cont2 '<cont2-59> aclauses clauses k))
                                        (set! env_reg macro-env)
                                        (set! var_reg name)
                                        (set! pc lookup-binding-in-first-frame))
                                      (if (eq? (car exp_reg) 'begin-aexp)
                                          (let ((exps 'undefined))
                                            (set! exps (list-ref exp_reg 1))
                                            (set! k_reg k)
                                            (set! exps_reg exps)
                                            (set! pc eval-sequence))
                                          (if (eq? (car exp_reg) 'lambda-aexp)
                                              (let ((formals 'undefined) (bodies 'undefined))
                                                (set! bodies (list-ref exp_reg 2))
                                                (set! formals (list-ref exp_reg 1))
                                                (set! value2_reg fail_reg)
                                                (set! value1_reg (closure formals bodies env_reg))
                                                (set! k_reg k)
                                                (set! pc apply-cont2))
                                              (if (eq? (car exp_reg) 'mu-lambda-aexp)
                                                  (let ((formals 'undefined)
                                                        (runt 'undefined)
                                                        (bodies 'undefined))
                                                    (set! bodies (list-ref exp_reg 3))
                                                    (set! runt (list-ref exp_reg 2))
                                                    (set! formals (list-ref exp_reg 1))
                                                    (set! value2_reg fail_reg)
                                                    (set! value1_reg (mu-closure formals runt bodies env_reg))
                                                    (set! k_reg k)
                                                    (set! pc apply-cont2))
                                                  (if (eq? (car exp_reg) 'trace-lambda-aexp)
                                                      (let ((name 'undefined)
                                                            (formals 'undefined)
                                                            (bodies 'undefined))
                                                        (set! bodies (list-ref exp_reg 3))
                                                        (set! formals (list-ref exp_reg 2))
                                                        (set! name (list-ref exp_reg 1))
                                                        (set! value2_reg fail_reg)
                                                        (set! value1_reg
                                                          (trace-closure name formals bodies env_reg))
                                                        (set! k_reg k)
                                                        (set! pc apply-cont2))
                                                      (if (eq? (car exp_reg) 'mu-trace-lambda-aexp)
                                                          (let ((name 'undefined)
                                                                (formals 'undefined)
                                                                (runt 'undefined)
                                                                (bodies 'undefined))
                                                            (set! bodies (list-ref exp_reg 4))
                                                            (set! runt (list-ref exp_reg 3))
                                                            (set! formals (list-ref exp_reg 2))
                                                            (set! name (list-ref exp_reg 1))
                                                            (set! value2_reg fail_reg)
                                                            (set! value1_reg
                                                              (mu-trace-closure name formals runt bodies env_reg))
                                                            (set! k_reg k)
                                                            (set! pc apply-cont2))
                                                          (if (eq? (car exp_reg) 'try-catch-aexp)
                                                              (let ((body 'undefined)
                                                                    (cvar 'undefined)
                                                                    (cexps 'undefined))
                                                                (set! cexps (list-ref exp_reg 3))
                                                                (set! cvar (list-ref exp_reg 2))
                                                                (set! body (list-ref exp_reg 1))
                                                                (let ((new-handler 'undefined))
                                                                  (set! new-handler
                                                                    (try-catch-handler cvar cexps env_reg handler_reg k))
                                                                  (set! k_reg k)
                                                                  (set! handler_reg new-handler)
                                                                  (set! exp_reg body)
                                                                  (set! pc m)))
                                                              (if (eq? (car exp_reg) 'try-finally-aexp)
                                                                  (let ((body 'undefined) (fexps 'undefined))
                                                                    (set! fexps (list-ref exp_reg 2))
                                                                    (set! body (list-ref exp_reg 1))
                                                                    (let ((new-handler 'undefined))
                                                                      (set! new-handler
                                                                        (try-finally-handler fexps env_reg handler_reg))
                                                                      (set! k_reg
                                                                        (make-cont2 '<cont2-58> fexps env_reg handler_reg k))
                                                                      (set! handler_reg new-handler)
                                                                      (set! exp_reg body)
                                                                      (set! pc m)))
                                                                  (if (eq? (car exp_reg) 'try-catch-finally-aexp)
                                                                      (let ((body 'undefined)
                                                                            (cvar 'undefined)
                                                                            (cexps 'undefined)
                                                                            (fexps 'undefined))
                                                                        (set! fexps (list-ref exp_reg 4))
                                                                        (set! cexps (list-ref exp_reg 3))
                                                                        (set! cvar (list-ref exp_reg 2))
                                                                        (set! body (list-ref exp_reg 1))
                                                                        (let ((new-handler 'undefined))
                                                                          (set! new-handler
                                                                            (try-catch-finally-handler cvar cexps fexps env_reg
                                                                              handler_reg k))
                                                                          (set! k_reg
                                                                            (make-cont2 '<cont2-58> fexps env_reg handler_reg k))
                                                                          (set! handler_reg new-handler)
                                                                          (set! exp_reg body)
                                                                          (set! pc m)))
                                                                      (if (eq? (car exp_reg) 'raise-aexp)
                                                                          (let ((exp 'undefined))
                                                                            (set! exp (list-ref exp_reg 1))
                                                                            (set! k_reg (make-cont2 '<cont2-56> handler_reg))
                                                                            (set! exp_reg exp)
                                                                            (set! pc m))
                                                                          (if (eq? (car exp_reg) 'dict-aexp)
                                                                              (let ((pairs 'undefined))
                                                                                (set! pairs (list-ref exp_reg 1))
                                                                                (set! value2_reg fail_reg)
                                                                                (set! value1_reg (list 'dict pairs))
                                                                                (set! k_reg k)
                                                                                (set! pc apply-cont2))
                                                                              (if (eq? (car exp_reg) 'help-aexp)
                                                                                  (let ((var 'undefined) (var-info 'undefined))
                                                                                    (set! var-info (list-ref exp_reg 2))
                                                                                    (set! var (list-ref exp_reg 1))
                                                                                    (if (reserved-keyword? var)
                                                                                        (begin
                                                                                          (set! value2_reg fail_reg)
                                                                                          (set! value1_reg (format "~a is a keyword" var))
                                                                                          (set! k_reg k)
                                                                                          (set! pc apply-cont2))
                                                                                        (begin
                                                                                          (set! k_reg (make-cont2 '<cont2-55> k))
                                                                                          (set! var-info_reg var-info)
                                                                                          (set! variable_reg var)
                                                                                          (set! pc lookup-binding))))
                                                                                  (if (eq? (car exp_reg) 'choose-aexp)
                                                                                      (let ((exps 'undefined))
                                                                                        (set! exps (list-ref exp_reg 1))
                                                                                        (set! k_reg k)
                                                                                        (set! exps_reg exps)
                                                                                        (set! pc eval-choices))
                                                                                      (if (eq? (car exp_reg) 'app-aexp)
                                                                                          (let ((operator 'undefined)
                                                                                                (operands 'undefined)
                                                                                                (info 'undefined))
                                                                                            (set! info (list-ref exp_reg 3))
                                                                                            (set! operands (list-ref exp_reg 2))
                                                                                            (set! operator (list-ref exp_reg 1))
                                                                                            (set! k_reg
                                                                                              (make-cont2 '<cont2-54> operator env_reg info handler_reg
                                                                                                k))
                                                                                            (set! exps_reg operands)
                                                                                            (set! pc m*))
                                                                                          (error 'm
                                                                                            "bad abstract syntax: ~s"
                                                                                            exp_reg)))))))))))))))))))))))))

(define*
  runtime-error
  (lambda ()
    (if (eq? info_reg 'none)
        (begin
          (set! exception_reg (format "runtime error: ~a" msg_reg))
          (set! pc apply-handler2))
        (let ((src 'undefined) (line 'undefined) (char 'undefined))
          (set! char (get-start-char info_reg))
          (set! line (get-start-line info_reg))
          (set! src (get-srcfile info_reg))
          (set! exception_reg
            (format
              "runtime error: ~a ~a"
              msg_reg
              (where-at line char src)))
          (set! pc apply-handler2)))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (return*
      (make-handler2 '<handler2-4> cexps cvar env handler k))))

(define try-finally-handler
  (lambda (fexps env handler)
    (return* (make-handler2 '<handler2-5> fexps env handler))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (return*
      (make-handler2 '<handler2-6> cexps cvar fexps env handler
        k))))

(define*
  eval-choices
  (lambda ()
    (if (null? exps_reg)
        (set! pc apply-fail)
        (let ((new-fail 'undefined))
          (set! new-fail
            (make-fail '<fail-3> exps_reg env_reg handler_reg fail_reg
              k_reg))
          (set! fail_reg new-fail)
          (set! exp_reg (car exps_reg))
          (set! pc m)))))

(define get-closure-depth
  (lambda () (return* _closure-depth)))

(define increment-closure-depth
  (lambda () (set! _closure-depth (+ _closure-depth 1))))

(define decrement-closure-depth
  (lambda () (set! _closure-depth (- _closure-depth 1))))

(define repeat
  (lambda (item times)
    (if (= times 0)
        (return* '())
        (return* (cons item (repeat item (- times 1)))))))

(define trace-closure
  (lambda (name formals bodies env)
    (return* (make-proc '<proc-1> bodies name formals env))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (return*
      (make-proc '<proc-2> bodies name formals runt env))))

(define closure
  (lambda (formals bodies env)
    (return* (make-proc '<proc-3> bodies formals env))))

(define mu-closure
  (lambda (formals runt bodies env)
    (return* (make-proc '<proc-4> bodies formals runt env))))

(define*
  m*
  (lambda ()
    (if (null? exps_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont2 '<cont2-70> exps_reg env_reg handler_reg k_reg))
          (set! exp_reg (car exps_reg))
          (set! pc m)))))

(define*
  eval-sequence
  (lambda ()
    (set! k_reg
      (make-cont2 '<cont2-71> exps_reg env_reg handler_reg k_reg))
    (set! exp_reg (car exps_reg))
    (set! pc m)))

(define length-one?
  (lambda (ls)
    (return* (and (not (null? ls)) (null? (cdr ls))))))

(define length-two?
  (lambda (ls)
    (return*
      (and (not (null? ls))
           (not (null? (cdr ls)))
           (null? (cddr ls))))))

(define length-at-least?
  (lambda (n ls)
    (if (< n 1)
        (return* #t)
        (if (or (null? ls) (not (pair? ls)))
            (return* #f)
            (return* (length-at-least? (- n 1) (cdr ls)))))))

(define all-numeric?
  (lambda (ls)
    (return*
      (or (null? ls)
          (and (number? (car ls)) (all-numeric? (cdr ls)))))))

(define all-char?
  (lambda (ls)
    (return*
      (or (null? ls)
          (and (char? (car ls)) (all-char? (cdr ls)))))))

(define void? (lambda (x) (return* (eq? x void-value))))

(define end-of-session?
  (lambda (x) (return* (eq? x end-of-session))))

(define safe-print
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print (make-safe arg))))

(define make-safe
  (lambda (x)
    (if (procedure-object? x)
        (return* '<procedure>)
        (if (environment-object? x)
            (return* '<environment>)
            (if (pair? x)
                (return* (cons (make-safe (car x)) (make-safe (cdr x))))
                (if (vector? x)
                    (return* (list->vector (make-safe (vector->list x))))
                    (return* x)))))))

(define procedure-object?
  (lambda (x)
    (return*
      (or (procedure? x)
          (and (pair? x) (eq? (car x) 'procedure))))))

(define environment-object?
  (lambda (x)
    (return* (and (pair? x) (eq? (car x) 'environment)))))

(define ends-with-newline?
  (lambda (s)
    (let ((len 'undefined))
      (set! len (string-length s))
      (return* (equal? (substring s (- len 1) len) "\n")))))

(define*
  load-file
  (lambda ()
    (if (member filename_reg load-stack)
        (begin
          (printf "skipping recursive load of ~a~%" filename_reg)
          (set! value2_reg fail_reg)
          (set! value1_reg void-value)
          (set! pc apply-cont2))
        (if (not (string? filename_reg))
            (begin
              (set! msg_reg
                (format "filename ~a is not a string" filename_reg))
              (set! pc runtime-error))
            (if (not (file-exists? filename_reg))
                (begin
                  (set! msg_reg
                    (format
                      "attempted to load nonexistent file ~a"
                      filename_reg))
                  (set! pc runtime-error))
                (begin
                  (set! load-stack (cons filename_reg load-stack))
                  (set! k_reg
                    (make-cont2 '<cont2-76> filename_reg env_reg handler_reg
                      k_reg))
                  (set! src_reg filename_reg)
                  (set! input_reg (read-content filename_reg))
                  (set! pc scan-input)))))))

(define*
  load-files
  (lambda ()
    (if (null? filenames_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg void-value)
          (set! pc apply-cont2))
        (begin
          (set! k_reg
            (make-cont2 '<cont2-77> filenames_reg env_reg info_reg
              handler_reg k_reg))
          (set! filename_reg (car filenames_reg))
          (set! pc load-file)))))

(define*
  length-loop
  (lambda ()
    (if (null? x_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg sum_reg)
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (if (not (pair? x_reg))
            (begin
              (set! msg_reg
                (format "length called on improper list ~s" ls_reg))
              (set! pc runtime-error))
            (begin
              (set! sum_reg (+ sum_reg 1))
              (set! x_reg (cdr x_reg))
              (set! pc length-loop))))))

(define*
  equal-objects?
  (lambda ()
    (if (or (and (null? x_reg) (null? y_reg))
            (and (boolean? x_reg)
                 (boolean? y_reg)
                 (or (and x_reg y_reg) (and (not x_reg) (not y_reg))))
            (and (symbol? x_reg) (symbol? y_reg) (eq? x_reg y_reg))
            (and (number? x_reg) (number? y_reg) (= x_reg y_reg))
            (and (char? x_reg) (char? y_reg) (char=? x_reg y_reg))
            (and (string? x_reg)
                 (string? y_reg)
                 (string=? x_reg y_reg)))
        (begin (set! value_reg #t) (set! pc apply-cont))
        (if (and (pair? x_reg) (pair? y_reg))
            (begin
              (set! k_reg (make-cont '<cont-39> x_reg y_reg k_reg))
              (set! y_reg (car y_reg))
              (set! x_reg (car x_reg))
              (set! pc equal-objects?))
            (if (and (vector? x_reg)
                     (vector? y_reg)
                     (= (vector-length x_reg) (vector-length y_reg)))
                (begin
                  (set! i_reg (- (vector-length x_reg) 1))
                  (set! v2_reg y_reg)
                  (set! v1_reg x_reg)
                  (set! pc equal-vectors?))
                (begin (set! value_reg #f) (set! pc apply-cont)))))))

(define*
  equal-vectors?
  (lambda ()
    (if (< i_reg 0)
        (begin (set! value_reg #t) (set! pc apply-cont))
        (begin
          (set! k_reg
            (make-cont '<cont-40> i_reg v1_reg v2_reg k_reg))
          (set! y_reg (vector-ref v2_reg i_reg))
          (set! x_reg (vector-ref v1_reg i_reg))
          (set! pc equal-objects?)))))

(define*
  member-loop
  (lambda ()
    (if (null? y_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg #f)
          (set! pc apply-cont2))
        (if (not (pair? y_reg))
            (begin
              (set! msg_reg
                (format "member called on improper list ~s" ls_reg))
              (set! pc runtime-error))
            (begin
              (set! k_reg
                (make-cont '<cont-41> ls_reg x_reg y_reg info_reg
                  handler_reg fail_reg k_reg))
              (set! y_reg (car y_reg))
              (set! pc equal-objects?))))))

(define range
  (lambda args
    (let ((range 'undefined))
      (set! range
        (lambda (n end step acc)
          (if (>= n end)
              (reverse acc)
              (range (+ n step) end step (cons n acc)))))
      (if (null? (cdr args))
          (return* (range 0 (car args) 1 '()))
          (if (null? (cddr args))
              (return* (range (car args) (cadr args) 1 '()))
              (return*
                (range (car args) (cadr args) (caddr args) '())))))))

(define*
  get-primitive
  (lambda ()
    (let ((sym 'undefined))
      (set! sym (car args_reg))
      (set! k_reg
        (make-cont2 '<cont2-79> args_reg sym info_reg handler_reg
          k_reg))
      (set! var-info_reg 'none)
      (set! variable_reg sym)
      (set! pc lookup-value))))

(define*
  append2
  (lambda ()
    (if (null? ls1_reg)
        (begin (set! value_reg ls2_reg) (set! pc apply-cont))
        (begin
          (set! k_reg (make-cont '<cont-42> ls1_reg k_reg))
          (set! ls1_reg (cdr ls1_reg))
          (set! pc append2)))))

(define*
  append-all
  (lambda ()
    (if (null? lists_reg)
        (begin (set! value_reg '()) (set! pc apply-cont))
        (if (null? (cdr lists_reg))
            (begin
              (set! value_reg (car lists_reg))
              (set! pc apply-cont))
            (begin
              (set! k_reg (make-cont '<cont-43> lists_reg k_reg))
              (set! lists_reg (cdr lists_reg))
              (set! pc append-all))))))

(define dir
  (lambda (args env)
    (return*
      (sort
        symbol<?
        (if (null? args)
            (flatten
              (append
                (get-reserved-keywords)
                (map get-variables-from-frame (frames macro-env))
                (map get-variables-from-frame (frames env))))
            (get-variables-from-frame (car (frames (car args)))))))))

(define get-variables-from-frame
  (lambda (frame) (return* (map binding-variable frame))))

(define symbol<?
  (lambda (a b)
    (let ((a_string 'undefined) (b_string 'undefined))
      (set! b_string (symbol->string b))
      (set! a_string (symbol->string a))
      (return* (string<? a_string b_string)))))

(define flatten
  (lambda (lists)
    (if (null? lists)
        (return* '())
        (if (list? (car lists))
            (return*
              (append (flatten (car lists)) (flatten (cdr lists))))
            (return* (cons (car lists) (flatten (cdr lists))))))))

(define get-current-time
  (lambda ()
    (let ((now 'undefined))
      (set! now (current-time))
      (return*
        (+ (time-second now)
           (inexact (/ (time-nanosecond now) 1000000000)))))))

(define*
  map-primitive
  (lambda ()
    (if (iterator? (car args_reg))
        (begin
          (set! generator_reg (car args_reg))
          (set! pc iterate-collect))
        (let ((len 'undefined) (list-args 'undefined))
          (set! list-args (listify args_reg))
          (set! len (length args_reg))
          (if (= len 1)
              (begin (set! list1_reg (car list-args)) (set! pc map1))
              (if (= len 2)
                  (begin
                    (set! list2_reg (cadr list-args))
                    (set! list1_reg (car list-args))
                    (set! pc map2))
                  (begin (set! lists_reg list-args) (set! pc mapN))))))))

(define listify
  (lambda (arg-list)
    (if (null? arg-list)
        (return* '())
        (if (list? (car arg-list))
            (return* (cons (car arg-list) (listify (cdr arg-list))))
            (if (vector? (car arg-list))
                (return*
                  (cons
                    (vector->list (car arg-list))
                    (listify (cdr arg-list))))
                (if (string? (car arg-list))
                    (return*
                      (cons
                        (string->list (car arg-list))
                        (listify (cdr arg-list))))
                    (error 'map
                      "cannot use object type '~a' in map"
                      (get_type (car arg-list)))))))))

(define*
  iterate
  (lambda ()
    (let ((iterator 'undefined))
      (set! iterator (get-iterator generator_reg))
      (set! iterator_reg iterator)
      (set! pc iterate-continue))))

(define*
  iterate-continue
  (lambda ()
    (let ((item 'undefined))
      (set! item (next-item iterator_reg))
      (if (null? item)
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg '())
            (set! pc apply-cont2))
          (begin
            (set! k2_reg
              (make-cont2 '<cont2-80> iterator_reg proc_reg env_reg
                handler_reg k_reg))
            (set! info_reg 'none)
            (set! env2_reg env_reg)
            (set! args_reg (list item))
            (set! pc apply-proc))))))

(define*
  iterate-collect
  (lambda ()
    (let ((iterator 'undefined))
      (set! iterator (get-iterator generator_reg))
      (set! iterator_reg iterator)
      (set! pc iterate-collect-continue))))

(define*
  iterate-collect-continue
  (lambda ()
    (let ((item 'undefined))
      (set! item (next-item iterator_reg))
      (if (null? item)
          (begin
            (set! value2_reg fail_reg)
            (set! value1_reg '())
            (set! pc apply-cont2))
          (begin
            (set! k2_reg
              (make-cont2 '<cont2-81> iterator_reg proc_reg env_reg
                handler_reg k_reg))
            (set! info_reg 'none)
            (set! env2_reg env_reg)
            (set! args_reg (list item))
            (set! pc apply-proc))))))

(define*
  map1
  (lambda ()
    (if (null? list1_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg
                (make-cont2 '<cont2-83> list1_reg proc_reg k_reg))
              (set! list1_reg (cdr list1_reg))
              (set! pc map1))
            (begin
              (set! k2_reg
                (make-cont2 '<cont2-82> list1_reg proc_reg env_reg
                  handler_reg k_reg))
              (set! info_reg 'none)
              (set! env2_reg env_reg)
              (set! args_reg (list (car list1_reg)))
              (set! pc apply-proc))))))

(define*
  map2
  (lambda ()
    (if (null? list1_reg)
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg
                (make-cont2 '<cont2-85> list1_reg list2_reg proc_reg k_reg))
              (set! list2_reg (cdr list2_reg))
              (set! list1_reg (cdr list1_reg))
              (set! pc map2))
            (begin
              (set! k2_reg
                (make-cont2 '<cont2-84> list1_reg list2_reg proc_reg env_reg
                  handler_reg k_reg))
              (set! info_reg 'none)
              (set! env2_reg env_reg)
              (set! args_reg (list (car list1_reg) (car list2_reg)))
              (set! pc apply-proc))))))

(define*
  mapN
  (lambda ()
    (if (null? (car lists_reg))
        (begin
          (set! value2_reg fail_reg)
          (set! value1_reg '())
          (set! pc apply-cont2))
        (if (dlr-exp? proc_reg)
            (begin
              (set! k_reg
                (make-cont2 '<cont2-87> lists_reg proc_reg k_reg))
              (set! lists_reg (map cdr lists_reg))
              (set! pc mapN))
            (begin
              (set! k2_reg
                (make-cont2 '<cont2-86> lists_reg proc_reg env_reg
                  handler_reg k_reg))
              (set! info_reg 'none)
              (set! env2_reg env_reg)
              (set! args_reg (map car lists_reg))
              (set! pc apply-proc))))))

(define*
  for-each-primitive
  (lambda ()
    (if (iterator? (car lists_reg))
        (begin
          (set! generator_reg (car lists_reg))
          (set! pc iterate))
        (let ((arg-list 'undefined))
          (set! arg-list (listify lists_reg))
          (if (null? (car arg-list))
              (begin
                (set! value2_reg fail_reg)
                (set! value1_reg void-value)
                (set! pc apply-cont2))
              (if (dlr-exp? proc_reg)
                  (begin
                    (dlr-apply proc_reg (map car arg-list))
                    (set! lists_reg (map cdr arg-list))
                    (set! pc for-each-primitive))
                  (begin
                    (set! k2_reg
                      (make-cont2 '<cont2-88> arg-list proc_reg env_reg
                        handler_reg k_reg))
                    (set! info_reg 'none)
                    (set! env2_reg env_reg)
                    (set! args_reg (map car arg-list))
                    (set! pc apply-proc))))))))

(define make-initial-env-extended
  (lambda (env) (return* env)))

(define make-toplevel-env
  (lambda ()
    (let ((primitives 'undefined))
      (set! primitives
        (list (list '* times-prim) (list '+ plus-prim)
         (list '- minus-prim) (list '/ divide-prim) (list '< lt-prim)
         (list '<= lt-or-eq-prim) (list '= equal-sign-prim)
         (list '=? equal-sign-prim) (list '> gt-prim)
         (list '>= gt-or-eq-prim) (list 'abort abort-prim)
         (list 'abs abs-prim) (list 'append append-prim)
         (list 'apply apply-prim) (list 'assv assv-prim)
         (list 'boolean? boolean?-prim) (list 'caddr caddr-prim)
         (list 'cadr cadr-prim)
         (list 'call-with-current-continuation call/cc-prim)
         (list 'call/cc call/cc-prim) (list 'car car-prim)
         (list 'cdr cdr-prim) (list 'char? char?-prim)
         (list 'char=? char=?-prim)
         (list 'char-whitespace? char-whitespace?-prim)
         (list 'char-alphabetic? char-alphabetic?-prim)
         (list 'char-numeric? char-numeric?-prim)
         (list 'cons cons-prim)
         (list 'current-time current-time-prim) (list 'cut cut-prim)
         (list 'dir dir-prim) (list 'display display-prim)
         (list 'env env-prim) (list 'eq? eq?-prim)
         (list 'equal? equal?-prim) (list 'error error-prim)
         (list 'eval eval-prim) (list 'exit exit-prim)
         (list 'for-each for-each-prim) (list 'get get-prim)
         (list 'import import-prim) (list 'length length-prim)
         (list 'list list-prim)
         (list 'list->vector list-to-vector-prim)
         (list 'list->string list->string-prim)
         (list 'list-ref list-ref-prim) (list 'load load-prim)
         (list 'make-vector make-vector-prim) (list 'map map-prim)
         (list 'member member-prim) (list 'memq memq-prim)
         (list 'memv memv-prim) (list 'newline newline-prim)
         (list 'not not-prim) (list 'null? null?-prim)
         (list 'number->string number->string-prim)
         (list 'number? number?-prim) (list 'pair? pair?-prim)
         (list 'parse parse-prim)
         (list 'parse-string parse-string-prim)
         (list 'print print-prim) (list 'printf printf-primitive)
         (list 'range range-prim)
         (list 'read-string read-string-prim)
         (list 'require require-prim) (list 'reverse reverse-prim)
         (list 'set-car! set-car!-prim)
         (list 'set-cdr! set-cdr!-prim) (list 'sqrt sqrt-prim)
         (list 'string string-prim)
         (list 'string-length string-length-prim)
         (list 'string-ref string-ref-prim)
         (list 'string? string?-prim)
         (list 'string->number string->number-prim)
         (list 'string=? string=?-prim)
         (list 'substring substring-prim)
         (list 'symbol? symbol?-prim) (list 'unparse unparse-prim)
         (list 'using using-primitive) (list 'vector vector-prim)
         (list 'vector-ref vector-ref-prim)
         (list 'vector-set! vector-set!-prim)
         (list 'void void-prim)))
      (return*
        (make-initial-env-extended
          (make-initial-environment
            (map car primitives)
            (map cadr primitives)))))))

(define make-external-proc
  (lambda (external-function-object)
    (return* (make-proc '<proc-87> external-function-object))))

(define pattern?
  (lambda (x)
    (return*
      (or (null? x)
          (number? x)
          (boolean? x)
          (symbol? x)
          (and (pair? x) (pattern? (car x)) (pattern? (cdr x)))))))

(define pattern-variable?
  (lambda (x)
    (return*
      (and (symbol? x)
           (equal? "?" (substring (symbol->string x) 0 1))))))

(define constant?
  (lambda (x)
    (return*
      (and (not (pattern-variable? x)) (not (pair? x))))))

(define*
  occurs?
  (lambda ()
    (if (constant? pattern_reg)
        (begin (set! value_reg #f) (set! pc apply-cont))
        (if (pattern-variable? pattern_reg)
            (begin
              (set! value_reg (equal? var_reg pattern_reg))
              (set! pc apply-cont))
            (begin
              (set! k_reg
                (make-cont '<cont-44> pattern_reg var_reg k_reg))
              (set! pattern_reg (car pattern_reg))
              (set! pc occurs?))))))

(define*
  unify-patterns^
  (lambda ()
    (if (pattern-variable? p1_reg)
        (if (pattern-variable? p2_reg)
            (begin
              (set! value_reg (make-sub 'unit p1_reg p2_reg ap2_reg))
              (set! pc apply-cont))
            (begin
              (set! k_reg
                (make-cont '<cont-45> ap2_reg p1_reg p2_reg k_reg))
              (set! pattern_reg p2_reg)
              (set! var_reg p1_reg)
              (set! pc occurs?)))
        (if (pattern-variable? p2_reg)
            (begin
              (set! temp_1 p2_reg)
              (set! temp_2 p1_reg)
              (set! temp_3 ap2_reg)
              (set! temp_4 ap1_reg)
              (set! p1_reg temp_1)
              (set! p2_reg temp_2)
              (set! ap1_reg temp_3)
              (set! ap2_reg temp_4)
              (set! pc unify-patterns^))
            (if (and (constant? p1_reg)
                     (constant? p2_reg)
                     (equal? p1_reg p2_reg))
                (begin
                  (set! value_reg (make-sub 'empty))
                  (set! pc apply-cont))
                (if (and (pair? p1_reg) (pair? p2_reg))
                    (begin
                      (set! apair2_reg ap2_reg)
                      (set! apair1_reg ap1_reg)
                      (set! pair2_reg p2_reg)
                      (set! pair1_reg p1_reg)
                      (set! pc unify-pairs^))
                    (begin (set! value_reg #f) (set! pc apply-cont))))))))

(define*
  unify-pairs^
  (lambda ()
    (set! k_reg
      (make-cont '<cont-47> apair1_reg apair2_reg pair1_reg
        pair2_reg k_reg))
    (set! ap2_reg (car^ apair2_reg))
    (set! ap1_reg (car^ apair1_reg))
    (set! p2_reg (car pair2_reg))
    (set! p1_reg (car pair1_reg))
    (set! pc unify-patterns^)))

(define*
  instantiate^
  (lambda ()
    (if (constant? pattern_reg)
        (begin
          (set! value2_reg ap_reg)
          (set! value1_reg pattern_reg)
          (set! k_reg k2_reg)
          (set! pc apply-cont2))
        (if (pattern-variable? pattern_reg)
            (begin
              (set! avar_reg ap_reg)
              (set! var_reg pattern_reg)
              (set! pc apply-sub^))
            (if (pair? pattern_reg)
                (begin
                  (set! k2_reg
                    (make-cont2 '<cont2-92> ap_reg pattern_reg s_reg k2_reg))
                  (set! ap_reg (car^ ap_reg))
                  (set! pattern_reg (car pattern_reg))
                  (set! pc instantiate^))
                (error 'instantiate^ "bad pattern: ~a" pattern_reg))))))

(define make-sub
  (lambda args (return* (cons 'substitution args))))

(define*
  apply-sub^
  (lambda ()
    (let ((temp_1 'undefined))
      (set! temp_1 (cdr s_reg))
      (if (eq? (car temp_1) 'empty)
          (begin
            (set! value2_reg avar_reg)
            (set! value1_reg var_reg)
            (set! k_reg k2_reg)
            (set! pc apply-cont2))
          (if (eq? (car temp_1) 'unit)
              (let ((new-var 'undefined)
                    (new-pattern 'undefined)
                    (new-apattern 'undefined))
                (set! new-apattern (list-ref temp_1 3))
                (set! new-pattern (list-ref temp_1 2))
                (set! new-var (list-ref temp_1 1))
                (if (equal? var_reg new-var)
                    (begin
                      (set! value2_reg new-apattern)
                      (set! value1_reg new-pattern)
                      (set! k_reg k2_reg)
                      (set! pc apply-cont2))
                    (begin
                      (set! value2_reg avar_reg)
                      (set! value1_reg var_reg)
                      (set! k_reg k2_reg)
                      (set! pc apply-cont2))))
              (if (eq? (car temp_1) 'composite)
                  (let ((s1 'undefined) (s2 'undefined))
                    (set! s2 (list-ref temp_1 2))
                    (set! s1 (list-ref temp_1 1))
                    (set! k2_reg (make-cont2 '<cont2-93> s2 k2_reg))
                    (set! s_reg s1)
                    (set! pc apply-sub^))
                  (error 'apply-sub^ "bad substitution: ~a" s_reg)))))))

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

(define trace-lambda?^ (tagged-list^ 'trace-lambda >= 4))

(define raise?^ (tagged-list^ 'raise = 2))

(define dict?^ (tagged-list^ 'dict >= 1))

(define help?^ (tagged-list^ 'help = 2))

(define choose?^ (tagged-list^ 'choose >= 1))

(define try?^ (tagged-list^ 'try >= 2))

(define catch?^ (tagged-list^ 'catch >= 3))

(define finally?^ (tagged-list^ 'finally >= 2))

(define dlr-apply apply)

(define printf-prim printf)

(define REP-k (make-cont2 '<cont2-46>))

(define REP-handler (make-handler2 '<handler2-2>))

(define REP-fail (make-fail '<fail-1>))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define try-parse-handler (make-handler2 '<handler2-3>))

(define *tracing-on?* #t)

(define _closure-depth 0)

(define void-prim (make-proc '<proc-5>))

(define void-value '<void>)

(define exit-prim (make-proc '<proc-6>))

(define end-of-session (list 'exiting 'the 'interpreter))

(define eval-prim (make-proc '<proc-7>))

(define parse-prim (make-proc '<proc-8>))

(define string-length-prim (make-proc '<proc-9>))

(define string-ref-prim (make-proc '<proc-10>))

(define unparse-prim (make-proc '<proc-11>))

(define parse-string-prim (make-proc '<proc-12>))

(define read-string-prim (make-proc '<proc-13>))

(define apply-prim (make-proc '<proc-14>))

(define sqrt-prim (make-proc '<proc-15>))

(define print-prim (make-proc '<proc-16>))

(define string-prim (make-proc '<proc-17>))

(define substring-prim (make-proc '<proc-18>))

(define number->string-prim (make-proc '<proc-19>))

(define assv-prim (make-proc '<proc-20>))

(define memv-prim (make-proc '<proc-21>))

(define display-prim (make-proc '<proc-22>))

(define newline-prim (make-proc '<proc-23>))

(define *need-newline* #f)

(define load-prim (make-proc '<proc-24>))

(define load-stack '())

(define length-prim (make-proc '<proc-25>))

(define symbol?-prim (make-proc '<proc-26>))

(define number?-prim (make-proc '<proc-27>))

(define boolean?-prim (make-proc '<proc-28>))

(define string?-prim (make-proc '<proc-29>))

(define char?-prim (make-proc '<proc-30>))

(define char=?-prim (make-proc '<proc-31>))

(define char-whitespace?-prim (make-proc '<proc-32>))

(define char-alphabetic?-prim (make-proc '<proc-33>))

(define char-numeric?-prim (make-proc '<proc-34>))

(define null?-prim (make-proc '<proc-35>))

(define pair?-prim (make-proc '<proc-36>))

(define cons-prim (make-proc '<proc-37>))

(define car-prim (make-proc '<proc-38>))

(define cdr-prim (make-proc '<proc-39>))

(define cadr-prim (make-proc '<proc-40>))

(define caddr-prim (make-proc '<proc-41>))

(define list-prim (make-proc '<proc-42>))

(define plus-prim (make-proc '<proc-43>))

(define minus-prim (make-proc '<proc-44>))

(define times-prim (make-proc '<proc-45>))

(define divide-prim (make-proc '<proc-46>))

(define lt-prim (make-proc '<proc-47>))

(define gt-prim (make-proc '<proc-48>))

(define lt-or-eq-prim (make-proc '<proc-49>))

(define gt-or-eq-prim (make-proc '<proc-50>))

(define equal-sign-prim (make-proc '<proc-51>))

(define abs-prim (make-proc '<proc-52>))

(define equal?-prim (make-proc '<proc-53>))

(define eq?-prim (make-proc '<proc-54>))

(define memq-prim (make-proc '<proc-55>))

(define member-prim (make-proc '<proc-56>))

(define range-prim (make-proc '<proc-57>))

(define set-car!-prim (make-proc '<proc-58>))

(define set-cdr!-prim (make-proc '<proc-59>))

(define import-prim (make-proc '<proc-60>))

(define get-prim (make-proc '<proc-61>))

(define call/cc-prim (make-proc '<proc-63>))

(define abort-prim (make-proc '<proc-64>))

(define require-prim (make-proc '<proc-65>))

(define cut-prim (make-proc '<proc-66>))

(define reverse-prim (make-proc '<proc-67>))

(define append-prim (make-proc '<proc-68>))

(define string->number-prim (make-proc '<proc-69>))

(define string=?-prim (make-proc '<proc-70>))

(define list-to-vector-prim (make-proc '<proc-71>))

(define list->string-prim (make-proc '<proc-72>))

(define dir-prim (make-proc '<proc-73>))

(define current-time-prim (make-proc '<proc-74>))

(define map-prim (make-proc '<proc-75>))

(define for-each-prim (make-proc '<proc-76>))

(define env-prim (make-proc '<proc-77>))

(define using-primitive (make-proc '<proc-78>))

(define not-prim (make-proc '<proc-79>))

(define printf-primitive (make-proc '<proc-80>))

(define vector-prim (make-proc '<proc-81>))

(define vector-set!-prim (make-proc '<proc-82>))

(define vector-ref-prim (make-proc '<proc-83>))

(define make-vector-prim (make-proc '<proc-84>))

(define error-prim (make-proc '<proc-85>))

(define list-ref-prim (make-proc '<proc-86>))

(define toplevel-env (make-toplevel-env))

;; the trampoline
(define trampoline
  (lambda () (if pc (begin (pc) (trampoline)) final_reg)))

(define run
  (lambda (setup . args) (apply setup args) (trampoline)))


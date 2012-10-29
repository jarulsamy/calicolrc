(load "transformer-macros.ss")

;; Environments represented as data structures

;; bindings

(define make-binding
  (lambda (variable value)
    (list variable "" value)))

(define binding-variable
  (lambda (binding)
    (car binding)))

(define binding-docstring
  (lambda (binding)
    (cadr binding)))

(define binding-value
  (lambda (binding)
    (caddr binding)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-car! (cdr binding) docstring)))

(define set-binding-value!
  (lambda (binding value)
    (set-car! (cddr binding) value)))

;; frames

(define make-frame
  (lambda (variables values)
    (map make-binding variables values)))

(define first-binding
  (lambda (frame)
    (car frame)))

(define rest-of-bindings
  (lambda (frame)
    (cdr frame)))

(define empty-frame?
  (lambda (frame)
    (null? frame)))

(define search-frame
  (lambda (frame variable)
    (cond
      ((empty-frame? frame) #f)
      ((eq? (binding-variable (first-binding frame)) variable)
       (first-binding frame))
      (else (search-frame (rest-of-bindings frame) variable)))))

;; environments

;; <environment> = (environment . (<frame> ...))

(define environment?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda ()
    (cons 'environment '(()))))

(define make-initial-environment
  (lambda (vars vals)
    (cons 'environment (list (make-frame vars vals)))))

(define first-frame
  (lambda (env)
    (cadr env)))

(define frames
  (lambda (env)
    (cdr env)))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values)
    (cons 'environment (cons (make-frame variables values) (cdr env)))))

;; variable lookup

(define search-env
  (lambda (env variable)
    (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
      #f
      (let ((binding (search-frame (car frames) variable)))
        (if binding
          binding
          (search-frames (cdr frames) variable))))))

(define* lookup-value
  (lambda (variable env var-info handler fail k)
    (lookup-binding variable env var-info handler fail
      (lambda-cont2 (binding fail)
	(k (binding-value binding) fail)))))

;; new version does the dlr-env-contains check first for greater efficiency
(define* lookup-binding
  (lambda (variable env var-info handler fail k)
    (if (dlr-env-contains variable)
      (k (dlr-env-lookup variable) fail)
      (let ((binding (search-env env variable)))
	(if binding
	  (k binding fail)
	  (split-variable variable fail
	    (lambda-cont2 (components fail)
	      (if components
		(lookup-variable-components components "" env handler fail k)
		(runtime-error (format "unbound variable ~a" variable) var-info handler fail)))))))))

;;;; previous version:
;;(define* lookup-binding
;;  (lambda (variable env var-info handler fail k)
;;    (let ((binding (search-env env variable)))
;;      (if binding
;;	(k binding fail)
;;	(split-variable variable fail
;;	  (lambda-cont2 (components fail)
;;	    (if (dlr-env-contains variable)
;;	      (k (dlr-env-lookup variable) fail)
;;	      (if components
;;		(lookup-variable-components components "" env handler fail k)
;;		(runtime-error (format "unbound variable ~a" variable) var-info handler fail)))))))))
;;                    (handler (format "unbound variable ~a" variable) fail)))))))))

;; adds a new binding for var to the first frame if one doesn't exist
(define* lookup-binding-in-first-frame
  (lambda (var env handler fail k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding fail)
          (let ((new-binding (make-binding var 'undefined)))
            (let ((new-frame (cons new-binding frame)))
              (set-first-frame! env new-frame)
	      (k new-binding fail))))))))

(define* lookup-variable-components
  ;; math.x.y.z where math is a module or a DLR module/item
  ;; components: '(test x y z) "" ...
  ;; components: '(x y z) "test" ...
  ;; components: '(y z) "test.x" ...
  ;; components: '(z) "test.x.z" ...
  (lambda (components path env handler fail k)
    ;;(printf "components: ~s path: ~s\n" components path)
    (let ((var (car components)))
      (lookup-module-binding var env path handler fail
	(lambda-cont2 (binding fail)
	  (if (null? (cdr components))
	    (k binding fail)
	    (let ((result (binding-value binding))
		  (new-path (if (string=? path "")
			      (format "~a" var)
			      (format "~a.~a" path var))))
	      (if (not (environment? result))
                  (if (dlr-object? result)
                      (k (dlr-lookup-components result (cdr components)) fail)
                      (handler (format "~a is not a module" new-path) fail))
                  (lookup-variable-components
		    (cdr components) new-path result handler fail k)))))))))

(define* lookup-module-binding
  (lambda (var env path handler fail k)
    (let ((binding (search-env env var)))
      (cond
	(binding (k binding fail))
        ((dlr-env-contains var) (k (dlr-env-lookup var) fail))
	((string=? path "") (handler (format "unbound module '~a'" var) fail))
	(else (handler (format "unbound variable '~a' in module '~a'" var path) fail))))))

(define* split-variable
  (lambda (variable fail k)
    (let ((strings (group (string->list (symbol->string variable)) #\.)))
      (if (or (member "" strings) (= (length strings) 1))
	(k #f fail)
	(k (map string->symbol strings) fail)))))

(define group
  (lambda (chars delimiter)
    (letrec
      ((position
	(lambda (chars)
	  (if (char=? (car chars) delimiter)
	      0
	      (+ 1 (position (cdr chars))))))
       (group
	 (lambda (chars)
	   (cond
	     ((null? chars) '())
	     ((not (member delimiter chars)) (list (apply string chars)))
	     (else (let ((n (position chars)))
		     (cons (apply string (list-head chars n))
			   (group (cdr (list-tail chars n))))))))))
      (group chars))))

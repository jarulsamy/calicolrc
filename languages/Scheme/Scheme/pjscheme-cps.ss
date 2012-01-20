;; Calico Scheme scanner and s-expression reader
;;
;; includes support for vectors, rationals, exponents, and backquote
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")

;;------------------------------------------------------------------------
;; scanner - character stream represented as a position number

(define chars-to-scan 'undefined)
(define read-line-count 'undefined)
(define read-char-count 'undefined)

(define 1st
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

;; scan-input takes a string and returns a list of tokens created
;; from all of the characters in the string

(define* scan-input
  (lambda (input handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (set! read-char-count 0)
    (set! read-line-count 1)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 handler fail k)))

(define* scan-input-loop
  (lambda (chars handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (apply-action '(goto start-state) '() chars handler fail
      (lambda-cont3 (token chars-left fail)
	(if (token-type? token 'end-marker)
	  (k (list token) fail)
	  (scan-input-loop chars-left handler fail
	    (lambda-cont2 (tokens fail)
	      (k (cons token tokens) fail))))))))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop-newline <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

(define* apply-action
  (lambda (action buffer chars handler fail k)  ;; k receives 3 args: token, chars-left, fail
;;    (display "action: ")
;;    (display action)
;;    (display ", buffer: ")
;;    (write buffer)
;;    (newline)
    (record-case action
      (shift (next)
	(begin
	  (set! read-char-count (+ read-char-count 1))
	  (apply-action next (cons (1st chars) buffer) (remaining chars) handler fail k)))
      (replace (new-char next)
	(apply-action next (cons new-char buffer) (remaining chars) handler fail k))
      (drop-newline (next)
	(begin
	  (set! read-line-count (+ read-line-count 1))
	  (set! read-char-count 0)
	  (apply-action next buffer (remaining chars) handler fail k)))
      (drop (next)
	(begin
	  (set! read-char-count (+ read-char-count 1))
	  (apply-action next buffer (remaining chars) handler fail k)))
      (goto (state)
	(let ((action (apply-state state (1st chars))))
	  (if (eq? action 'error)
	    (scan-error chars handler fail)
	    (apply-action action buffer chars handler fail k))))
      (emit (token-type)
	(convert-buffer-to-token token-type buffer handler fail
	  (lambda-cont2 (v fail)
	    (k (append v (list read-line-count read-char-count)) chars fail))))
      (else (error 'apply-action "invalid action: ~a" action)))))
      
(define* scan-error
  (lambda (chars handler fail)
    (let ((c (1st chars)))
      (if (char=? c #\nul)
	(handler (format "scan error: unexpected end of input at line ~a, char ~a" 
			 read-line-count read-char-count)
		 fail)
	(handler (format "scan error: unexpected character ~a encountered at line ~a, char ~a" 
			 c read-line-count read-char-count)
		 fail)))))

(define* convert-buffer-to-token
  (lambda (token-type buffer handler fail k)
    (let ((buffer (reverse buffer)))
      (case token-type
	(integer
	  (k (list 'integer (list->string buffer)) fail))
	(decimal
	  (k (list 'decimal (list->string buffer)) fail))
	(rational
	  (k (list 'rational (list->string buffer)) fail))
	(identifier
	  (k (list 'identifier (string->symbol (list->string buffer))) fail))
	(boolean
	  (k (list 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T))) fail))
	(character
	  (k (list 'character (car buffer)) fail))
	(named-character
	  (let ((name (list->string buffer)))
	    (cond
	      ((string=? name "nul") (k (list 'character #\nul) fail))
	      ((string=? name "space") (k (list 'character #\space) fail))
	      ((string=? name "tab") (k (list 'character #\tab) fail))
	      ((string=? name "newline") (k (list 'character #\newline) fail))
	      ((string=? name "linefeed") (k (list 'character #\newline) fail))
	      ((string=? name "backspace") (k (list 'character #\backspace) fail))
	      ((string=? name "return") (k (list 'character #\return) fail))
	      ((string=? name "page") (k (list 'character #\page) fail))
	      (else (handler (format "invalid character name '~a' at line ~a, char ~a"
				     name read-line-count read-char-count)
			     fail)))))
	(string
	  (k (list 'string (list->string buffer)) fail))
	(else
	  (k (list token-type) fail))))))

(define token-type?
  (lambda (token class)
    (eq? (car token) class)))

(define get-line-count
  (lambda (token)
    (rac (rdc token))))

(define get-char-count
  (lambda (token)
    (rac token)))

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

;;------------------------------------------------------------------------
;; character categories

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
  (lambda (c)
    (or (char=? c #\+)
	(char=? c #\-))))

(define char-boolean?
  (lambda (c)
    (or (char=? c #\t)
	(char=? c #\T)
	(char=? c #\f)
	(char=? c #\F))))

;;------------------------------------------------------------------------
;; finite-state automaton

;; this is just a table lookup
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
	  ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
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
	  ;;((char=? c #\r) '(replace #\newline (goto string-state)))
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
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
	  ((char-delimiter? c) '(emit integer))
	  ((char-subsequent? c) '(shift (goto identifier-state)))
	  (else 'error)))
      (fractional-number-state
	(cond
	  ((char-numeric? c) '(shift (goto fractional-number-state)))
	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
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
      (else
	(error 'apply-state "invalid state: ~a" state)))))

;;------------------------------------------------------------------------
;; recursive descent parser
;;
;; <sexp> ::= <number> | <boolean> | <character> | <string> | <identifier>
;;          | ' <sexp>
;;          | ( <sexp>* )
;;          | ( <sexp>+ . <sexp> )
;;          | [ <sexp>* ]
;;          | [ <sexp>+ . <sexp> ]
;;          | ` <sexp>
;;          | , <sexp>
;;          | ,@ <sexp>

;; token stream represented as a list
(define first (lambda (x) (car x)))
(define rest-of (lambda (x) (cdr x)))

(define string->integer
  (lambda (str)
    (string->number str)))

(define string->decimal
  (lambda (str)
    (string->number str)))

(define string->rational
  (lambda (str)
    (string->number str)))

(define true?
  (lambda (v) 
    (if v #t #f)))

(define* read-sexp
  (lambda (tokens handler fail k)   ;; k receives 3 args: sexp, tokens-left, fail
    (record-case (first tokens)
      (integer (str)
	(k (string->integer str) (rest-of tokens) fail))
      (decimal (str)
	(k (string->decimal str) (rest-of tokens) fail))
      (rational (str)
	(let ((num (string->rational str)))
	  (if (true? num)
	    (k num (rest-of tokens) fail)
	    (handler (format "cannot represent ~a at line ~a, char ~a" 
			     str 
			     (get-line-count (first tokens)) 
			     (get-char-count (first tokens)))
		     fail))))
      (boolean (bool) (k bool (rest-of tokens) fail))
      (character (char) (k char (rest-of tokens) fail))
      (string (str) (k str (rest-of tokens) fail))
      (identifier (id) (k id (rest-of tokens) fail))
      (apostrophe () (read-abbreviation tokens 'quote handler fail k))
      (backquote () (read-abbreviation tokens 'quasiquote handler fail k))
      (comma () (read-abbreviation tokens 'unquote handler fail k))
      (comma-at () (read-abbreviation tokens 'unquote-splicing handler fail k))
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
	(read-vector (rest-of tokens) handler fail
	  (lambda-cont3 (sexps tokens-left fail)
	    (k (list->vector sexps) tokens-left fail))))
      (else (read-error tokens handler fail)))))

(define* read-abbreviation
  (lambda (tokens keyword handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (read-sexp (rest-of tokens) handler fail
      (lambda-cont3 (sexp tokens-left fail)
	(k (list keyword sexp) tokens-left fail)))))

(define* read-sexp-sequence
  (lambda (tokens expected-terminator handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator handler fail k))
      (dot ()
	(read-sexp (rest-of tokens) handler fail
	  (lambda-cont3 (sexp tokens-left fail)
	    (close-sexp-sequence sexp tokens-left expected-terminator handler fail k))))
      (else
	(read-sexp tokens handler fail
	  (lambda-cont3 (sexp1 tokens-left fail)
	    (read-sexp-sequence tokens-left expected-terminator handler fail
	      (lambda-cont3 (sexp2 tokens-left fail)
		(k (cons sexp1 sexp2) tokens-left fail)))))))))

(define* close-sexp-sequence
  (lambda (sexp tokens expected-terminator handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
	 ((token-type? (first tokens) expected-terminator)
	  (k sexp (rest-of tokens) fail))
	 ((eq? expected-terminator 'rparen)
	  (handler (format "parenthesized list terminated by bracket at line ~a, char ~a"
			   (get-line-count (first tokens)) (get-char-count (first tokens)))
		   fail))
	 ((eq? expected-terminator 'rbracket)
	  (handler (format "bracketed list terminated by parenthesis at line ~a, char ~a"
			   (get-line-count (first tokens)) (get-char-count (first tokens)))
		   fail))))
      (else (read-error tokens handler fail)))))

(define* read-vector
  (lambda (tokens handler fail k)
    (record-case (first tokens)
      (rparen ()
	(k '() (rest-of tokens) fail))
      (else
	(read-sexp tokens handler fail
	  (lambda-cont3 (sexp1 tokens-left fail)
	    (read-vector tokens-left handler fail
	      (lambda-cont3 (sexps tokens-left fail)
		(k (cons sexp1 sexps) tokens-left fail)))))))))

(define* read-error
  (lambda (tokens handler fail)
    (let ((token (first tokens))
	  (where (if (null? load-stack) "" (format " in ~a" (car load-stack)))))
      (if (token-type? token 'end-marker)
	(handler (format "read error: unexpected end of input at line ~a, char ~a~a" 
			 (get-line-count token) 
			 (get-char-count token)
			 where)
		 fail)
	(handler (format "read error: unexpected token ~a encountered at line ~a, char ~a~a"
			 (car token) 
			 (get-line-count token)
			 (get-char-count token)
			 where)
		 fail)))))

;; returns the entire file contents as a single string
(define read-content
  (lambda (filename)
    (apply string
      (call-with-input-file filename
	(lambda (port)
	  (let loop ((char (read-char port)))
	    (if (eof-object? char)
	      '()
	      (cons char (loop (read-char port))))))))))

;; takes a list of tokens and reads the next full sexp from the
;; tokens. It returns the result and the remaining tokens as a pair,
;; or an exception object of the form (exception "description").
;; Used only in scheme-to-csharp.ss
(define read-next-sexp
  (lambda (tokens)
    (read-sexp tokens init-handler2 init-fail
      (lambda-cont3 (sexp tokens-left fail)
	(halt* (cons sexp tokens-left))))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

;;(define init-cont (lambda-cont (v) (halt* v)))
(define init-cont2 (lambda-cont2 (v1 v2) (halt* v1)))
(define init-cont3 (lambda-cont3 (v1 v2 v3) (halt* v1)))
;;(define init-handler (lambda-handler (e) (halt* (list 'exception e))))
(define init-handler2 (lambda-handler2 (e fail) (halt* (list 'exception e))))
(define init-fail (lambda-fail () (halt* "no more choices")))

(define scan-string
  (lambda (input)
    (scan-input input init-handler2 init-fail init-cont2)))

(define scan-file
  (lambda (filename)
    (scan-input (read-content filename) init-handler2 init-fail init-cont2)))

(define read-string
  (lambda (input)
    (read-datum input init-handler2 init-fail init-cont3)))

(define* read-datum
  (lambda (input handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (scan-input input handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens handler fail
	  (lambda-cont3 (sexp tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k sexp tokens-left fail)
	      (handler (format "tokens left over at line ~a, char ~a" 
			       (get-line-count (first tokens-left))
			       (get-char-count (first tokens-left)))
		       fail))))))))

(define read-file
  (lambda (filename)
    (scan-input (read-content filename) init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(print-unparsed-sexps tokens init-handler2 init-fail init-cont2)))))

(define* print-unparsed-sexps
  (lambda (tokens handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k 'done fail)
      (read-sexp tokens handler fail
	(lambda-cont3 (sexp tokens-left fail)
	  (pretty-print sexp)
	  (print-unparsed-sexps tokens-left handler fail k))))))

;; for testing in c#
;; (define Main
;;   (lambda (args)
;;     (let ((str (array-ref args 0)))
;;       (printf "scanning string: \\\"{0}\\\"\n" str)
;;       (scan-string str)
;;       (display (trampoline))
;;       (newline)
;;       (printf "done!\n"))))

;; Handle command-line args too
;;(define* load-files
;;  (lambda (filenames handler fail k)
;;    (if (null? filenames)
;;	(k 'ok fail)
;;	(read-datum (format "(import \\\"~a\\\")" (car filenames)) handler fail
;; 	    (lambda-cont3 (datum tokens-left fail)
;;		(printf "   (import \\\"~a\\\")...\n" (car filenames))
;; 		(parse datum handler fail
;; 		    (lambda-cont2 (exp fail)
;; 			(m exp toplevel-env handler fail
;;			   (lambda-cont2 (result fail)
;;			      (load-files (cdr filenames) handler fail k))))))))))

;;------------------------------------------------------------------------

;;(define token-start 0)

;;(define* scan-input2
;;  (lambda (input handler fail k)   ;; k receives 2 args: a list of tokens, fail
;;    (set! read-char-count 0)
;;    (set! read-line-count 1)
;;    (set! token-start 0)
;;    (set! chars-to-scan (string-append input (string #\nul)))
;;    (scan-input-loop2 0 handler fail k)))

;;(define* scan-input-loop2
;;  (lambda (chars handler fail k)   ;; k receives 2 args: a list of tokens, fail
;;    (apply-action2 '(goto start-state) '() chars handler fail
;;      (lambda-cont3 (token chars-left fail)
;;	(if (token-type? token 'end-marker)
;;	  (k (list token) fail)
;;	  (scan-input-loop2 chars-left handler fail
;;	    (lambda-cont2 (tokens fail)
;;	      (k (cons token tokens) fail))))))))

;;(define* apply-action2
;;  (lambda (action buffer chars handler fail k)  ;; k receives 3 args: token, chars-left, fail
;;;;    (display "action: ")
;;;;    (display action)
;;;;    (display ", buffer: ")
;;;;    (write buffer)
;;;;    (newline)
;;    (record-case action
;;      (shift (next)
;;	(begin
;;	  (set! read-char-count (+ read-char-count 1))
;;	  (apply-action2 next (cons (1st chars) buffer) (remaining chars) handler fail k)))
;;      (replace (new-char next)
;;	(apply-action2 next (cons new-char buffer) (remaining chars) handler fail k))
;;      (drop-newline (next)
;;	(begin
;;	  (set! read-line-count (+ read-line-count 1))
;;	  (set! read-char-count 0)
;;	  (apply-action2 next buffer (remaining chars) handler fail k)))
;;      (drop (next)
;;	(begin
;;	  (set! read-char-count (+ read-char-count 1))
;;	  (apply-action2 next buffer (remaining chars) handler fail k)))
;;      (goto (state)
;;	(let ((action (apply-state2 state (1st chars))))
;;	  (if (eq? action 'error)
;;	    (scan-error chars handler fail)
;;	    (apply-action2 action buffer chars handler fail k))))
;;      (emit (token-type)
;;	(convert-buffer-to-token token-type buffer handler fail
;;	  (lambda-cont2 (v fail)
;;	    (k (append v (list read-line-count read-char-count)) chars fail))))
;;      (else (error 'apply-action2 "invalid action: ~a" action)))))

;;(define apply-state2
;;  (lambda (state c)
;;    (case state
;;      (start-state
;;	(cond
;;	  ((char=? c #\newline) '(drop-newline (goto start-state)))
;;	  ((char-whitespace? c) '(drop (goto start-state)))
;;	  ((char=? c #\;) '(drop (goto comment-state)))
;;	  ((char=? c #\() '(drop (emit lparen)))
;;	  ((char=? c #\[) '(drop (emit lbracket)))
;;	  ((char=? c #\)) '(drop (emit rparen)))
;;	  ((char=? c #\]) '(drop (emit rbracket)))
;;	  ((char=? c #\') '(drop (emit apostrophe)))
;;	  ((char=? c #\`) '(drop (emit backquote)))
;;	  ((char=? c #\,) '(drop (goto comma-state)))
;;	  ((char=? c #\#) '(drop (goto hash-prefix-state)))
;;	  ((char=? c #\") '(drop (goto string-state)))
;;	  ((char-initial? c) '(shift (goto identifier-state)))
;;	  ((char-sign? c) '(shift (goto signed-state)))
;;	  ((char=? c #\.) '(shift (goto decimal-point-state)))
;;	  ((char-numeric? c) '(shift (goto whole-number-state)))
;;	  ((char=? c #\nul) '(drop (emit end-marker)))
;;	  (else 'error)))
;;      (comment-state
;;	(cond
;;	  ((char=? c #\newline) '(drop-newline (goto start-state)))
;;	  ((char=? c #\nul) '(goto start-state))
;;	  (else '(drop (goto comment-state)))))
;;      (comma-state
;;	(cond
;;	  ((char=? c #\@) '(drop (emit comma-at)))
;;	  (else '(emit comma))))
;;      (hash-prefix-state
;;	(cond
;;	  ((char-boolean? c) '(shift (emit boolean)))
;;	  ((char=? c #\\) '(drop (goto character-state)))
;;	  ((char=? c #\() '(drop (emit lvector)))
;;	  (else 'error)))
;;      (character-state
;;	(cond
;;	  ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
;;	  ((not (char=? c #\nul)) '(shift (emit character)))
;;	  (else 'error)))
;;      (alphabetic-character-state
;;	(cond
;;	  ((char-alphabetic? c) '(shift (goto named-character-state)))
;;	  (else '(emit character))))
;;      (named-character-state
;;	(cond
;;	  ((char-delimiter? c) '(emit named-character))
;;	  (else '(shift (goto named-character-state)))))
;;      (string-state
;;	(cond
;;	  ((char=? c #\") '(drop (emit string)))
;;	  ((char=? c #\\) '(drop (goto string-escape-state)))
;;	  ((char=? c #\nul) 'error)
;;	  (else '(shift (goto string-state)))))
;;      (string-escape-state
;;	(cond
;;	  ((char=? c #\") '(shift (goto string-state)))
;;	  ((char=? c #\\) '(shift (goto string-state)))
;;	  ((char=? c #\b) '(replace #\backspace (goto string-state)))
;;	  ((char=? c #\f) '(replace #\page (goto string-state)))
;;	  ((char=? c #\n) '(replace #\newline (goto string-state)))
;;	  ;;((char=? c #\r) '(replace #\newline (goto string-state)))
;;	  ((char=? c #\t) '(replace #\tab (goto string-state)))
;;	  ((char=? c #\r) '(replace #\return (goto string-state)))
;;	  (else 'error)))
;;      (identifier-state
;;	(cond
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  (else 'error)))
;;      (signed-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto whole-number-state)))
;;	  ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (decimal-point-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto fractional-number-state)))
;;	  ((char-delimiter? c) '(emit dot))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (signed-decimal-point-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto fractional-number-state)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (whole-number-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto whole-number-state)))
;;	  ((char=? c #\.) '(shift (goto fractional-number-state)))
;;	  ((char=? c #\/) '(shift (goto rational-number-state)))
;;	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
;;	  ((char-delimiter? c) '(emit integer))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (fractional-number-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto fractional-number-state)))
;;	  ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
;;	  ((char-delimiter? c) '(emit decimal))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (rational-number-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto rational-number-state*)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (rational-number-state*
;;	(cond
;;	  ((char-numeric? c) '(shift (goto rational-number-state*)))
;;	  ((char-delimiter? c) '(emit rational))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (suffix-state
;;	(cond
;;	  ((char-sign? c) '(shift (goto signed-exponent-state)))
;;	  ((char-numeric? c) '(shift (goto exponent-state)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (signed-exponent-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto exponent-state)))
;;	  ((char-delimiter? c) '(emit identifier))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (exponent-state
;;	(cond
;;	  ((char-numeric? c) '(shift (goto exponent-state)))
;;	  ((char-delimiter? c) '(emit decimal))
;;	  ((char-subsequent? c) '(shift (goto identifier-state)))
;;	  (else 'error)))
;;      (else
;;	(error 'apply-state2 "invalid state: ~a" state)))))

;;(define scan-string2
;;  (lambda (input)
;;    (scan-input2 input init-handler2 init-fail init-cont2)))

;;(define scan-file2
;;  (lambda (filename)
;;    (scan-input2 (read-content filename) init-handler2 init-fail init-cont2)))

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
  (lambda (variable env handler fail k)
    (lookup-binding variable env handler fail
      (lambda-cont2 (binding fail)
	(k (binding-value binding) fail)))))

(define* lookup-binding
  (lambda (variable env handler fail k)
    (let ((binding (search-env env variable)))
      (if binding
	(k binding fail)
	(split-variable variable fail
	  (lambda-cont2 (components fail)
            (if (dlr-env-contains variable)
                (k (dlr-env-lookup variable) fail)
                (if components
		    (lookup-variable-components components "" env handler fail k)
                    (handler (format "unbound variable ~a" variable) fail)))))))))

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
;; Calico Scheme interpreter
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")

;;----------------------------------------------------------------------------
;; Interpreter with support for choose

(load "environments-cps.ss")
(load "parser-cps.ss")

(define void-value '<void>)

(define *need-newline* #f)

(define pretty-print-prim
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print (if (procedure-object? arg) '<procedure> arg))))

(define procedure-object?
  (lambda (x)
    (or (procedure? x) (and (pair? x) (eq? (car x) 'procedure)))))

(define newline-prim
  (lambda ()
    (set! *need-newline* #f)
    (newline)))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

(define display-prim
  (lambda (x)
    (let ((s (format "~a" x)))  ;; must use ~a, not ~s, to handle embedded newlines properly
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s))))

;; redefined as REP-k when no-csharp-support.ss is loaded
(define scheme-REP-k
  (lambda-cont2 (v fail)
    (if (not (eq? v void-value))
	(pretty-print-prim v))
    (if *need-newline* (newline))
    (read-eval-print fail)))

;; redefined as REP-handler when no-csharp-support.ss is loaded
(define scheme-REP-handler
  (lambda-handler2 (e fail)
    (REP-k `(uncaught exception: ,e) fail)))

;; redefined as REP-fail when no-csharp-support.ss is loaded
(define scheme-REP-fail
  (lambda-fail ()
    (REP-k "no more choices" REP-fail)))

(define start
  (lambda ()
    ;; start with fresh environments
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (read-eval-print REP-fail)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print REP-fail)))

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

(define* read-eval-print
  (lambda (fail)
    (set! load-stack '())  ;; in case a previous load encountered an error
    (let ((input (read-line "==> ")))  ;; or raw-read-line
      (scan-input input REP-handler fail
	(lambda-cont2 (tokens fail)
	  (read-and-eval-sexps tokens toplevel-env REP-handler fail REP-k))))))

(define* read-and-eval-sexps
  (lambda (tokens env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-sexp tokens handler fail
	(lambda-cont3 (datum tokens-left fail)
	  (parse datum handler fail
	    (lambda-cont2 (exp fail)
	      (m exp env handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-sexps tokens-left env handler fail k)))))))))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
    (cases expression exp
      (lit-exp (datum) (k datum fail))
      (var-exp (id) (lookup-value id env handler fail k))
      (func-exp (exp) (m exp env handler fail
                        (lambda-cont2 (proc fail)
                          (k (dlr-func proc) fail))))
      (if-exp (test-exp then-exp else-exp)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (assign-exp (var rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding var env handler fail
	      (lambda-cont2 (binding fail)
		(let ((old-value (binding-value binding)))
		  (set-binding-value! binding rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-binding-value! binding old-value) (fail))))
		    (k void-value new-fail))))))))
      (define-exp (var docstring rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding-in-first-frame var env handler fail
	      (lambda-cont2 (binding fail)
		(set-binding-value! binding rhs-value)
		(set-binding-docstring! binding docstring)
		;; definitions should occur only at top level, so no need to undo
		(k void-value fail))))))
      (define!-exp (var docstring rhs-exp)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (set-global-value! var rhs-value)
	    (set-global-docstring! var docstring)
	    (k void-value fail))))
      (define-syntax-exp (keyword clauses)
	(lookup-binding-in-first-frame keyword macro-env handler fail
	  (lambda-cont2 (binding fail)
	    (set-binding-value! binding (make-pattern-macro clauses))
	    (k void-value fail))))
      (begin-exp (exps) (eval-sequence exps env handler fail k))
      (lambda-exp (formals body)
	(k (closure formals body env) fail))
      (mu-lambda-exp (formals runt body)
	(k (mu-closure formals runt body env) fail))
      (try-catch-exp (body cvar cexps)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler fail k)))
      (try-finally-exp (body fexps)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail)))))))
      (try-catch-finally-exp (body cvar cexps fexps)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler fail
	     (lambda-cont2 (v fail)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler fail
		 (lambda-cont2 (v2 fail) (k v fail)))))))
      (raise-exp (exp)
	(m exp env handler fail
	  ;; TODO: pass in more info to handler (k, env) to support resume, etc.
	  (lambda-cont2 (e fail) (handler e fail))))
      (dict-exp (pairs)
	(k (list 'dict pairs) fail))
      (help-exp (var)
	(if (reserved-keyword? var)
	  (k (format "~a is a keyword" var) fail)
	  (lookup-binding var env handler fail
	    (lambda-cont2 (binding fail)
	      (k (binding-docstring binding) fail)))))
      (choose-exp (exps)
	(eval-choices exps env handler fail k))
      (app-exp (operator operands)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(if (dlr-exp? proc)
		  (k (dlr-apply proc args) fail)
		  (proc args env handler fail k)))))))
      (else (error 'm "bad abstract syntax: ~a" exp)))))

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
  (lambda (formals body env)
    (lambda-proc (args env2 handler fail k2)
      (if (= (length args) (length formals))
	(m body (extend env formals args) handler fail k2)
	(handler "incorrect number of arguments" fail)))))

(define mu-closure
  (lambda (formals runt body env)
    (lambda-proc (args env2 handler fail k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (m body new-env handler fail k2))
	(handler "not enough arguments given" fail)))))

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

(define make-initial-env-extended
  (lambda (env)
    ;; this is here as a hook for extending environments in C# etc.
    env))

(define length-prim
  (lambda-proc (args env2 handler fail k2)
    (if (= (length args) 1)
      (length-loop (car args) 0 (car args) handler fail k2)
      (handler "incorrect number of arguments to procedure length" fail))))

(define* length-loop
  (lambda (x sum ls handler fail k2)
    (cond
      ((null? x) (k2 sum fail))
      ((not (pair? x)) (handler (format "~a is not a proper list" ls) fail))
      (else (length-loop (cdr x) (+ sum 1) ls handler fail k2)))))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
     (make-initial-environment
      (list 'void 'exit 'eval 'parse 'parse-string 'apply 'sqrt 'print 'display 'newline 'load 'length
	    'null? 'cons 'car 'cdr 'cadr 'caddr 'list '+ '- '* '/ '< '> '= 'abs 'equal? 'eq? 'memq 'member 'range
	    'set-car! 'set-cdr! 'import 'get 'call-with-current-continuation 'call/cc 'abort 'require 'cut
	    'reverse 'append 'list->vector 'dir 'current-time 'map 'for-each 'env
	    'using 'not 'printf 'vector 'vector-set! 'vector-ref 'make-vector)
      (list
	;; void
	(lambda-proc (args env2 handler fail k2) (k2 void-value fail))
	;; exit
        (lambda-proc (args env2 handler fail k2)
	  (halt* '(exiting the interpreter)))
	;; eval
	(lambda-proc (args env2 handler fail k2)
	  (parse (car args) handler fail
	    (lambda-cont2 (exp fail)
	      (m exp toplevel-env handler fail k2))))   ;; use toplevel-env here?
	;; parse
	(lambda-proc (args env2 handler fail k2)
	  (parse (car args) handler fail k2))
	;; parse-string
	(lambda-proc (args env2 handler fail k2)
	  (scan-input (car args) handler fail
	    (lambda-cont2 (tokens fail)
	      (read-sexp tokens handler fail
		(lambda-cont3 (datum tokens-left fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (parse datum handler fail k2)
		    (handler (format "tokens left over at line ~a, char ~a" 
				     (get-line-count (first tokens-left))
				     (get-char-count (first tokens-left)))
			     fail)))))))
	;; apply
	(lambda-proc (args env2 handler fail k2)
	  (let ((proc (car args))
		(proc-args (cadr args)))
	    (proc proc-args env2 handler fail k2)))
;; FIX: need to check each fixed-arity primitive for correct number of args before calling
	;; sqrt
	(lambda-proc (args env2 handler fail k2) (k2 (apply sqrt args) fail))
	;; print
	(lambda-proc (args env2 handler fail k2) (for-each pretty-print-prim args) (k2 void-value fail))
	;; display
	(lambda-proc (args env2 handler fail k2) (apply display-prim args) (k2 void-value fail))
	;; newline
	(lambda-proc (args env2 handler fail k2) (newline-prim) (k2 void-value fail))
	;; load
	(lambda-proc (args env2 handler fail k2)
	   (load-file (car args) toplevel-env handler fail k2))
	;; length
	length-prim
	;; null?
	(lambda-proc (args env2 handler fail k2) (k2 (apply null? args) fail))
	;; cons
	(lambda-proc (args env2 handler fail k2) (k2 (apply cons args) fail))
	;; car
	(lambda-proc (args env2 handler fail k2) (k2 (apply car args) fail))
	;; cdr
	(lambda-proc (args env2 handler fail k2) (k2 (apply cdr args) fail))
	;; cadr
	(lambda-proc (args env2 handler fail k2) (k2 (apply cadr args) fail))
	;; caddr
	(lambda-proc (args env2 handler fail k2) (k2 (apply caddr args) fail))
	;; list
	(lambda-proc (args env2 handler fail k2) (k2 args fail))
	;; +
	(lambda-proc (args env2 handler fail k2) (k2 (apply + args) fail))
	;; - 
	(lambda-proc (args env2 handler fail k2) (k2 (apply - args) fail))
	;; *
	(lambda-proc (args env2 handler fail k2) (k2 (apply * args) fail))
	;; /
	(lambda-proc (args env2 handler fail k2)
          (cond
            ((= (length args) 1)
             (if (= (car args) 0)
                 (handler "division by zero" fail)
                 (k2 (apply / args) fail)))
            ((>= (length args) 2)
             (if (= (cadr args) 0)
                 (handler "division by zero" fail)
                 (k2 (apply / args) fail)))
            (else (handler "not enough args to /" fail))))
	;; <
	(lambda-proc (args env2 handler fail k2) (k2 (apply < args) fail))
	;; >
	(lambda-proc (args env2 handler fail k2) (k2 (apply > args) fail))
	;; =
	(lambda-proc (args env2 handler fail k2) (k2 (apply = args) fail))
	;; abs
	(lambda-proc (args env2 handler fail k2) (k2 (apply abs args) fail))
	;; equal?
	(lambda-proc (args env2 handler fail k2)
	  (if (= (length args) 2)
	    (equal-objects? (car args) (cadr args)
	      (lambda-cont (bool) (k2 bool fail)))
	    (handler "incorrect number of arguments to procedure equal?" fail)))
	;; eq?
	(lambda-proc (args env2 handler fail k2) (k2 (apply eq? args) fail))
	;; memq
	(lambda-proc (args env2 handler fail k2) (k2 (apply memq args) fail))
	;; member
	(lambda-proc (args env2 handler fail k2)
	  (if (= (length args) 2)
	    (member-prim (car args) (cadr args) (cadr args) handler fail k2)
	    (handler "incorrect number of arguments to procedure member" fail)))
	;; range
	(lambda-proc (args env2 handler fail k2) (k2 (apply range args) fail))
	;; set-car!
	(lambda-proc (args env2 handler fail k2) (k2 (apply set-car! args) fail))
	;; set-cdr
	(lambda-proc (args env2 handler fail k2) (k2 (apply set-cdr! args) fail))
	;; import
	(lambda-proc (args env2 handler fail k2) (import-primitive args env2 handler fail k2))
	;; get
	(lambda-proc (args env2 handler fail k2) (get-primitive args env2 handler fail k2))
	;; call/cc
	(lambda-proc (args env2 handler fail k2) (call/cc-primitive (car args) env2 handler fail k2))
	;; call/cc
	(lambda-proc (args env2 handler fail k2) (call/cc-primitive (car args) env2 handler fail k2))
	;; abort
	(lambda-proc (args env2 handler fail k2)
	  (if (null? args)
	    (REP-k void-value fail)
	    (REP-k (car args) fail)))
	;; require
	(lambda-proc (args env2 handler fail k2)
	  (if (true? (car args))
	    (k2 'ok fail)
	    (fail)))
	;; cut
	(lambda-proc (args env2 handler fail k2) (k2 'ok REP-fail))
	;; reverse
	(lambda-proc (args env2 handler fail k2) (k2 (apply reverse args) fail))
	;; append
	(lambda-proc (args env2 handler fail k2) (k2 (apply append args) fail))
	;; list->vector
	(lambda-proc (args env2 handler fail k2) (k2 (apply make-vector args) fail))
	;; dir
	(lambda-proc (args env2 handler fail k2) (k2 (dir args env2) fail))
	;; current-time
	(lambda-proc (args env2 handler fail k2) (k2 (get-current-time) fail))
	;; map
	(lambda-proc (args env2 handler fail k2)
	  (map-prim (car args) (cdr args) env2 handler fail k2))
	;; for-each
	(lambda-proc (args env2 handler fail k2)
	  (for-each-prim (car args) (cdr args) env2 handler fail k2))
	;; env
	(lambda-proc (args env2 handler fail k2) (k2 env2 fail))
	;; using (not defined in scheme-scheme)
	(lambda-proc (args env2 handler fail k2) (k2 (using-prim args env2) fail))
	;; not
	(lambda-proc (args env2 handler fail k2) (k2 (not (car args)) fail))
	;; printf
	(lambda-proc (args env2 handler fail k2) (apply printf-prim args) (k2 void-value fail))
        ;; vector
	(lambda-proc (args env2 handler fail k2) (k2 (make-vector args) fail))
        ;; vector-set!
	(lambda-proc (args env2 handler fail k2) (k2 (vector-set! (car args) (cadr args) (caddr args)) fail))
        ;; vector-ref
	(lambda-proc (args env2 handler fail k2) (k2 (apply vector-ref args) fail))
        ;; make-vector
	(lambda-proc (args env2 handler fail k2) (k2 (make-vector-size (car args)) fail))
	)))))

(define toplevel-env (make-toplevel-env))

(define* equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
	   (and (boolean? x) (boolean? y) (eq? x y))
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

(define* member-prim
  (lambda (x ls orig-ls handler fail k)
    (cond
      ((null? ls) (k #f fail))
      ((not (pair? ls)) (handler (format "improper list ~a" orig-ls) fail))
      (else (equal-objects? x (car ls)
	      (lambda-cont (bool)
		(if bool
		  (k ls fail)
		  (member-prim x (cdr ls) orig-ls handler fail k))))))))

;; supports procedures of any number of arguments
(define* map-prim
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args))
              (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler fail k))
            (else (mapN proc list-args env handler fail k)))))))

(define* iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define* iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env handler fail
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
          (proc (list item) env handler fail
            (lambda-cont2 (v1 fail)
              (iterate-collect-continue proc iterator env handler fail
                (lambda-cont2 (v2 fail)
                  (k (cons v1 v2) fail)))))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list))
      (cons (car arg-list) (listify (cdr arg-list))))
     ((vector? (car arg-list))
      (cons (my-vector->list (car arg-list)) (listify (cdr arg-list))))
     ((string? (car arg-list))
      (cons (string->list (car arg-list)) (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map" 
		  (get_type (car arg-list))))))) ;; get_type is defined in C#

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
	(proc (list (car list1)) env handler fail
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
	(proc (list (car list1) (car list2)) env handler fail
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
	(proc (map car lists) env handler fail
	  (lambda-cont2 (v1 fail)
	    (mapN proc (map cdr lists) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

(define* for-each-prim
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
      (iterate proc (car lists) env handler fail k)
      (let ((arg-list (listify lists)))
	(if (null? (car arg-list))
	  (k void-value fail)
	  (if (dlr-exp? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-prim proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-prim proc (map cdr arg-list) env handler fail k)))))))))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now)
		     1000000000))))))

(define* get-primitive
  (lambda (args env handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env handler fail
	(lambda-cont2 (v fail)
	  (cond
	    ((null? (cdr args)) (k v fail))
	    ((not (environment? v)) (handler (format "~a is not a module" sym) fail))
	    (else (get-primitive (cdr args) v handler fail k))))))))

;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define* import-primitive
  (lambda (args env handler fail k)
    (let ((filename (car args)))
	(if (null? (cdr args))
	  (load-file filename env handler fail k)
	  (let ((module-name (cadr args)))
	    (lookup-binding-in-first-frame module-name env handler fail
	      (lambda-cont2 (binding fail)
		(let ((module (extend env '() '())))
		  (set-binding-value! binding module)
		  (load-file filename module handler fail k)))))))))

(define* call/cc-primitive
  (lambda (proc env handler fail k)
    (let ((fake-k (lambda-proc (args env2 handler fail k2) (k (car args) fail))))
      (if (dlr-exp? proc)
	  (k (dlr-apply proc (list fake-k)) fail)
	  (proc (list fake-k) env handler fail k)))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

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

(define load-stack '())

(define* load-file
  (lambda (filename env handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (handler (format "filename is not a string: ~a" filename) fail))
      ((not (file-exists? filename))
       (handler (format "file does not exist: ~a" filename) fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-sexps tokens env handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* load-files
  (lambda (filenames env handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (load-file (car filenames) env handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env handler fail k))))))

;; help is now a special form
;;(define* help-prim
;;  (lambda (var env handler fail k)
;;    (lookup-binding var env handler fail
;;      (lambda-cont2 (binding fail)
;;	(k (binding-docstring binding) fail)))))

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
	
(define make-vector list->vector) ;; ignored in C#

;; not used
(define Main
  (lambda filenames
    (printf "Calico Scheme (0.2)\n")
    (printf "(c) 2009-2011, IPRE\n")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (set! load-stack '())
    ;; in the register machine, this call just sets up the registers
    (load-files filenames toplevel-env REP-handler REP-fail REP-k)
    ;; starts the computation after registers are set up
    (trampoline)))

;;------------------------------------------------------------------------
;; C# support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 handler fail k2)
      (k2 (apply* external-function-object args) fail))))

(define REP-k
  (lambda-cont2 (v fail)
    (set! last-fail fail)
    (halt* v)))

(define REP-fail
  (lambda-fail ()
    (halt* "no more choices")))

(define REP-handler
  (lambda-handler2 (e fail)
    (set! last-fail fail)
    (halt* (list 'exception e))))

(define last-fail REP-fail)

;; not used yet
(define reinitialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env))
    (set! load-stack '())
    (set! last-fail REP-fail)))

(define execute
  (lambda (input)
    (set! load-stack '())
    (scan-input input REP-handler last-fail
      (lambda-cont2 (tokens fail)
	(read-and-eval-sexps tokens toplevel-env REP-handler fail REP-k)))
    (trampoline)))

(define execute-file
  (lambda (filename)
    (set! load-stack '())
    (load-file filename toplevel-env REP-handler last-fail REP-k)
    (trampoline)))

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input try-parse-handler REP-fail
      (lambda-cont2 (tokens fail)
	(parse-sexps tokens try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(load "transformer-macros.ss")

;; Unification pattern-matcher

(define pattern?
  (lambda (x)
    (or (null? x)
	(number? x)
	(boolean? x)
	(symbol? x)
	(and (pair? x)
	     (pattern? (car x))
	     (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
	 (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x))
	 (not (pair? x)))))

(define* occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda-cont (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

(define* unify-patterns
  (lambda (p1 p2 k)
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2)))))))
      ((pattern-variable? p2) (unify-patterns p2 p1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
      (else (k #f)))))

(define* unify-pairs
  (lambda (pair1 pair2 k)
    (unify-patterns (car pair1) (car pair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate (cdr pair1) s-car
	    (lambda-cont (new-cdr1)
	      (instantiate (cdr pair2) s-car
		(lambda-cont (new-cdr2)
		  (unify-patterns new-cdr1 new-cdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate
  (lambda (pattern s k)
    (cond
      ((constant? pattern) (k pattern))
      ((pattern-variable? pattern) (apply-sub s pattern k))
      ((pair? pattern)
       (instantiate (car pattern) s
	 (lambda-cont (a)
	   (instantiate (cdr pattern) s
	     (lambda-cont (b)
	       (k (cons a b)))))))
      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

;;(define extend-sub
;;  (lambda (old-s new-var new-pattern)
;;    (list 'extended new-var new-pattern old-s)))

(define* apply-sub
  (lambda (s var k)
    (record-case (cdr s)
      (empty () (k var))
;;      (extended (new-var new-pattern old-s)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (apply-sub old-s var k)))
      (unit (new-var new-pattern)
	(if (equal? var new-var)
	  (k new-pattern)
	  (k var)))
      (composite (s1 s2)
	(apply-sub s1 var
	  (lambda-cont (pattern)
	    (instantiate pattern s2 k))))
      (else (error 'apply-sub "bad substitution: ~a" s)))))

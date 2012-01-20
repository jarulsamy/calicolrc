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
	  ;;((char=? c #\r) '(replace #\newline (goto string-state)))
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


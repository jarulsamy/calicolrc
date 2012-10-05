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

(define next-avail
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

(define scan-line 'undefined)
(define scan-char 'undefined)
(define scan-position 'undefined)
(define last-scan-line 'undefined)
(define last-scan-char 'undefined)
(define last-scan-position 'undefined)

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
      (else
        (set! scan-char (+ 1 scan-char))))
    (set! scan-position (+ 1 scan-position))))

(define token-start-line 'undefined)
(define token-start-char 'undefined)
(define token-start-position 'undefined)

(define mark-token-start
  (lambda ()
    (set! token-start-line scan-line)
    (set! token-start-char scan-char)
    (set! token-start-position scan-position)))

;; scan-input takes a string and returns a list of tokens created
;; from all of the characters in the string

(define* scan-input
  (lambda (input src handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (initialize-scan-counters)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 src handler fail k)))

(define* scan-input-loop
  (lambda (chars src handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (apply-action '(goto start-state) '() chars src handler fail
      (lambda-cont3 (token chars-left fail)
        (if (token-type? token 'end-marker)
          (k (list token) fail)
          (scan-input-loop chars-left src handler fail
            (lambda-cont2 (tokens fail)
              (k (cons token tokens) fail))))))))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

;; might be better if actions were:
;;(shift next-state)
;;(shift/emit token-type)
;;(replace new-char next-state)
;;(drop next-state)
;;(drop/emit token-type)
;;(emit token-type)

(define* apply-action
  (lambda (action buffer chars src handler fail k)  ;; k receives 3 args: token, chars-left, fail
;;    (display "action: ")
;;    (display action)
;;    (display ", buffer: ")
;;    (write buffer)
;;    (newline)
    (record-case action
      (shift (next)
        (increment-scan-counters chars)
        (apply-action next (cons (next-avail chars) buffer) (remaining chars) src handler fail k))
      (replace (new-char next)
        (increment-scan-counters chars)
        (apply-action next (cons new-char buffer) (remaining chars) src handler fail k))
      (drop (next)
        (increment-scan-counters chars)
        (apply-action next buffer (remaining chars) src handler fail k))
      (goto (state)
        (if (eq? state 'token-start-state)
          (mark-token-start))
        (let ((action (apply-state state (next-avail chars))))
          (if (eq? action 'error)
            (unexpected-char-error chars src handler fail)
            (apply-action action buffer chars src handler fail k))))
      (emit (token-type)
        (convert-buffer-to-token token-type buffer src handler fail
          (lambda-cont (token)
            (k token chars fail))))
      (else (error 'apply-action "invalid action: ~a" action)))))

(define* scan-error
  (lambda (msg line char src handler fail)
    (handler (format "scan error: ~a ~a" msg (where-at line char src)) fail)))

(define* unexpected-char-error
  (lambda (chars src handler fail)
    (let ((c (next-avail chars)))
      (if (char=? c #\nul)
	(scan-error "unexpected end of input" scan-line scan-char src handler fail)
	(scan-error (format "unexpected character ~a encountered" c) scan-line scan-char src handler fail)))))

(define* convert-buffer-to-token
  (lambda (token-type buffer src handler fail k)  ;; k receives 1 argument: token
    (let ((buffer (reverse buffer)))
      (case token-type
        (end-marker
          (k (make-token 'end-marker)))
        (integer
          (k (make-info-token 'integer (list->string buffer))))
        (decimal
          (k (make-info-token 'decimal (list->string buffer))))
        (rational
          (k (make-info-token 'rational (list->string buffer))))
        (identifier
          (k (make-info-token 'identifier (string->symbol (list->string buffer)))))
        (boolean
          (k (make-info-token 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character
          (k (make-info-token 'character (car buffer))))
        (named-character
          (let ((name (list->string buffer)))
            (cond
              ((string=? name "nul") (k (make-info-token 'character #\nul)))
              ((string=? name "space") (k (make-info-token 'character #\space)))
              ((string=? name "tab") (k (make-info-token 'character #\tab)))
              ((string=? name "newline") (k (make-info-token 'character #\newline)))
              ((string=? name "linefeed") (k (make-info-token 'character #\newline)))
              ((string=? name "backspace") (k (make-info-token 'character #\backspace)))
              ((string=? name "return") (k (make-info-token 'character #\return)))
              ((string=? name "page") (k (make-info-token 'character #\page)))
	      (else (scan-error (format "invalid character name #\\~a" name)
				token-start-line token-start-char src handler fail)))))
        (string
          (k (make-info-token 'string (list->string buffer))))
        (else
          (k (make-token token-type)))))))

;;------------------------------------------------------------------------
;; tokens
;;
;; <token> ::= (<token-type1> <token-start> <token-end>)
;;           | (<token-type2> <token-info> <token-start> <token-end>)
;;
;; <token-type1> ::= end-marker | lparen | rparen | lbracket | rbracket | lvector
;;                 | apostrophe | backquote | comma | comma-at | dot
;;
;; <token-type2> ::= integer | decimal | rational | boolean | character | string | identifier
;;
;; <token-start/end> ::= (<line> <char> <pos>)

(define make-token
  (lambda (token-type)
    (let ((start (list token-start-line token-start-char token-start-position))
	  (end (list last-scan-line last-scan-char last-scan-position)))
      (if (eq? token-type 'end-marker)
	(list token-type end end)
	(list token-type start end)))))

(define make-info-token
  (lambda (token-type token-info)
    (list token-type token-info
          (list token-start-line token-start-char token-start-position)
          (list last-scan-line last-scan-char last-scan-position))))

(define token-type?
  (lambda (token class)
    (eq? (car token) class)))

(define get-token-start
  (lambda (token)
    (rac (rdc token))))

(define get-token-end
  (lambda (token)
    (rac token)))

(define get-token-start-line
  (lambda (token)
    (car (get-token-start token))))

(define get-token-start-char
  (lambda (token)
    (cadr (get-token-start token))))

(define get-token-start-pos
  (lambda (token)
    (caddr (get-token-start token))))

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
  (lambda (tokens src handler fail k)   ;; k receives 3 args: sexp, tokens-left, fail
    (record-case (first tokens)
      (integer (str)
        (k (string->integer str) (rest-of tokens) fail))
      (decimal (str)
        (k (string->decimal str) (rest-of tokens) fail))
      (rational (str)
        (let ((num (string->rational str)))
          (if (true? num)
            (k num (rest-of tokens) fail)
	    (read-error (format "cannot represent ~a" str) tokens src handler fail))))
      (boolean (bool) (k bool (rest-of tokens) fail))
      (character (char) (k char (rest-of tokens) fail))
      (string (str) (k str (rest-of tokens) fail))
      (identifier (id) (k id (rest-of tokens) fail))
      (apostrophe () (read-abbreviation tokens 'quote src handler fail k))
      (backquote () (read-abbreviation tokens 'quasiquote src handler fail k))
      (comma () (read-abbreviation tokens 'unquote src handler fail k))
      (comma-at () (read-abbreviation tokens 'unquote-splicing src handler fail k))
      (lparen ()
        (let ((tokens (rest-of tokens)))
	  (read-sexp-sequence tokens 'rparen src handler fail
	    (lambda-cont3 (sexps tokens-left fail)
	      (k sexps tokens-left fail)))))
      (lbracket ()
        (let ((tokens (rest-of tokens)))
	  (read-sexp-sequence tokens 'rbracket src handler fail
	    (lambda-cont3 (sexps tokens-left fail)
	      (k sexps tokens-left fail)))))
      (lvector ()
        (read-vector-sequence (rest-of tokens) src handler fail
          (lambda-cont3 (sexps tokens-left fail)
            (k (list->vector sexps) tokens-left fail))))
      (else (unexpected-token-error tokens src handler fail)))))

(define* read-abbreviation
  (lambda (tokens keyword src handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (read-sexp (rest-of tokens) src handler fail
      (lambda-cont3 (sexp tokens-left fail)
        (k (list keyword sexp) tokens-left fail)))))

(define* read-sexp-sequence
  (lambda (tokens expected-terminator src handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont3 (sexp1 tokens-left fail)
	    (if (token-type? (first tokens-left) 'dot)
	      (read-sexp (rest-of tokens-left) src handler fail
		(lambda-cont3 (sexp2 tokens-left fail)
		  (close-sexp-sequence (cons sexp1 sexp2) tokens-left expected-terminator src handler fail k)))
	      (read-sexp-sequence tokens-left expected-terminator src handler fail
		(lambda-cont3 (sexps tokens-left fail)
		  (k (cons sexp1 sexps) tokens-left fail))))))))))

(define* close-sexp-sequence
  (lambda (sexp tokens expected-terminator src handler fail k)
    (record-case (first tokens)
      ((rparen rbracket) ()
       (cond
         ((token-type? (first tokens) expected-terminator)
          (k sexp (rest-of tokens) fail))
         ((eq? expected-terminator 'rparen)
          (read-error "parenthesized list terminated by bracket" tokens src handler fail))
         ((eq? expected-terminator 'rbracket)
	  (read-error "bracketed list terminated by parenthesis" tokens src handler fail))))
      (else (unexpected-token-error tokens src handler fail)))))

(define* read-vector-sequence
  (lambda (tokens src handler fail k)  ;; k gets 3 args: sexps, tokens-left, fail
    (record-case (first tokens)
      (rparen ()
	(close-sexp-sequence '() tokens 'rparen src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont3 (sexp1 tokens-left fail)
	    (read-vector-sequence tokens-left src handler fail
	      (lambda-cont3 (sexps tokens-left fail)
		(k (cons sexp1 sexps) tokens-left fail)))))))))

(define* unexpected-token-error
  (lambda (tokens src handler fail)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
	(read-error "unexpected end of input" tokens src handler fail)
	(read-error (format "unexpected ~a encountered" (car token)) tokens src handler fail)))))

(define* read-error
  (lambda (msg tokens src handler fail)
    (let ((token (first tokens)))
      (handler (format "read error: ~a ~a" msg
		       (where-at (get-token-start-line token) (get-token-start-char token) src))
	       fail))))

(define where-at
  (lambda (line char src)
    (if (eq? src 'stdin)
      (format "at line ~a, char ~a" line char)
      (format "at line ~a, char ~a of ~a" line char src))))

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

;;------------------------------------------------------------------------
;; annotated s-expressions
;;
;; <asexp> ::= (asexp <sexp> <info>)
;;
;; <sexp> ::= <number>
;;          | <boolean>
;;          | <character>
;;          | <string>
;;          | <vector>
;;          | <symbol>
;;          | ( <asexp>* )
;;          | ( <asexp>+ . <asexp> )
;;
;; <info> ::= (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos>)
;;          | (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos> <macro-name>+)

(define asexp-tag (box 'tag))

(define make-asexp
  (lambda (src start end sexp)
    (list asexp-tag sexp (cons src (append start end)))))

(define retag
  (lambda (sexp info)
    (list asexp-tag sexp info)))

(define asexp?
  (lambda (x)
    (and (pair? x) (eq? (car x) asexp-tag))))

(define get-sexp
  (lambda (asexp)
    (cadr asexp)))

(define get-source-info
  (lambda (asexp)
    (caddr asexp)))

(define get-srcfile
  (lambda (info)
    (car info)))

(define get-start-line
  (lambda (info)
    (cadr info)))

(define get-start-char
  (lambda (info)
    (caddr info)))

(define get-start-pos
  (lambda (info)
    (cadddr info)))

(define get-end-line
  (lambda (info)
    (car (cddddr info))))

(define get-end-char
  (lambda (info)
    (cadr (cddddr info))))

(define get-end-pos
  (lambda (info)
    (caddr (cddddr info))))

(define has-source-info?
  (lambda (asexp)
    (not (eq? (get-source-info asexp) 'none))))

;; true if no macro name at end of info list
(define original-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
	 (= (length (get-source-info asexp)) 7))))

(define source-info?
  (lambda (x)
    (or (eq? x 'none) (list? x))))

(define replace-info
  (lambda (asexp info)
    (retag (get-sexp asexp) info)))

(define car^ (lambda (asexp) (car (get-sexp asexp))))
(define cdr^ (lambda (asexp) (cdr (get-sexp asexp))))
(define cddr^ (lambda (asexp) (cddr (get-sexp asexp))))
(define cdddr^ (lambda (asexp) (cdddr (get-sexp asexp))))
(define cadr^ (lambda (asexp) (cadr (get-sexp asexp))))
(define caddr^ (lambda (asexp) (caddr (get-sexp asexp))))
(define cadddr^ (lambda (asexp) (cadddr (get-sexp asexp))))
(define map^ (lambda (f asexp) (map f (get-sexp asexp))))
(define symbol?^ (lambda (asexp) (symbol? (get-sexp asexp))))
(define string?^ (lambda (asexp) (string? (get-sexp asexp))))
(define length^ (lambda (asexp) (length (get-sexp asexp))))
(define eq?^ (lambda (asexp x) (eq? (get-sexp asexp) x)))
(define vector?^ (lambda (asexp) (vector? (get-sexp asexp))))
(define vector->list^ (lambda (asexp) (vector->list (get-sexp asexp))))

(define cons^
  (lambda (a b info)
    (cond
      ((null?^ b) (retag (list a) info))
      ((pair?^ b) (retag (cons a (get-sexp b)) info))
      (else (retag (cons a b) info)))))

(define ^cdr^
  (lambda (asexp)
    (if (asexp? (cdr^ asexp)) ;; asexp could represent an annotated dotted pair
      (cdr^ asexp)
      (retag (cdr^ asexp) 'none))))

;; unnecessary:
(define improper-list?^ (lambda (asexp) (improper-list-of-asexp? (get-sexp asexp))))

(define null?^
  (lambda (x)
    (and (asexp? x) (null? (get-sexp x)))))

(define pair?^
  (lambda (x)
    (and (asexp? x) (pair? (get-sexp x)))))

(define list?^
  (lambda (x)
    (and (asexp? x) (list-of-asexp? (get-sexp x)))))

(define list-of-asexp?
  (lambda (x)
    (or (null? x)
	(and (pair? x)
	     (asexp? (car x))
	     ;; example: (aread-string "(a . (b))")
	     (or (list-of-asexp? (cdr x))
		 (list?^ (cdr x)))))))

;; unnecessary:
(define improper-list-of-asexp?
  (lambda (x)
    (and (pair? x)
	 (asexp? (car x))
	 (or (asexp? (cdr x))
	     (improper-list-of-asexp? (cdr x))))))

;; for manual testing only
(define unannotate
  (lambda (x)
    (cond
      ((asexp? x) (unannotate (get-sexp x)))
      ((pair? x) (cons (unannotate (car x)) (unannotate (cdr x))))
      ((vector? x) (list->vector (unannotate (vector->list x))))
      (else x))))

;; for manual testing only
(define reannotate
  (lambda (x)
    (cond
      ((asexp? x) x)
      ((pair? x) (retag (reannotate-seq x) 'none))
      ((vector? x) (retag (list->vector (reannotate-seq (vector->list x))) 'none))
      (else (retag x 'none)))))

;; for manual testing only
(define reannotate-seq
  (lambda (x)
    (cond
      ((null? x) '())
      ((asexp? x) x)
      ((not (pair? x)) (reannotate x))
      ((or (null?^ (cdr x)) (pair?^ (cdr x)))  ;; necessary for annotated structures like (a . (b))
       (reannotate-seq (cons (car x) (get-sexp (cdr x)))))
      (else (cons (reannotate (car x)) (reannotate-seq (cdr x)))))))

(define* unannotate-cps
  (lambda (x k)   ;; k takes 1 arg
    (cond
      ((asexp? x)
       (unannotate-cps (get-sexp x) k))
      ((pair? x)
       (unannotate-cps (car x)
	 (lambda-cont (v1)
	   (unannotate-cps (cdr x)
	     (lambda-cont (v2)
	       (k (cons v1 v2)))))))
      ((vector? x)
       (unannotate-cps (vector->list x)
	 (lambda-cont (ls)
	   (k (list->vector ls)))))
      (else (k x)))))

(define* reannotate-cps
  (lambda (x k)    ;; k takes 1 arg
    (cond
      ((asexp? x) (k x))
      ((pair? x)
       (reannotate-seq-cps x (lambda-cont (v) (k (retag v 'none)))))
      ((vector? x)
       (reannotate-seq-cps (vector->list x)
	 (lambda-cont (v)
	   (k (retag (list->vector v) 'none)))))
      (else (k (retag x 'none))))))

(define* reannotate-seq-cps
  (lambda (x k)   ;; k takes 1 arg
    (cond
      ((null? x) (k '()))
      ((asexp? x) (k x))
      ((not (pair? x)) (reannotate-cps x k))
      ((or (null?^ (cdr x)) (pair?^ (cdr x)))  ;; necessary for annotated structures like (a . (b))
       (reannotate-seq-cps (cons (car x) (get-sexp (cdr x))) k))
      (else (reannotate-cps (car x)
	      (lambda-cont (v1)
		(reannotate-seq-cps (cdr x)
		  (lambda-cont (v2)
		    (k (cons v1 v2))))))))))

(define* read-asexp
  (lambda (tokens src handler fail k)   ;; k receives 4 args: asexp, end, tokens-left, fail
    (let ((start (get-token-start (first tokens)))
	  (end (get-token-end (first tokens))))
      (record-case (first tokens)
	(integer (str)
	  (k (make-asexp src start end (string->integer str)) end (rest-of tokens) fail))
	(decimal (str)
	  (k (make-asexp src start end (string->decimal str)) end (rest-of tokens) fail))
	(rational (str)
	  (let ((num (string->rational str)))
	    (if (true? num)
	      (k (make-asexp src start end num) end (rest-of tokens) fail)
	      (read-error (format "cannot represent ~a" str) tokens src handler fail))))
	(boolean (bool) (k (make-asexp src start end bool) end (rest-of tokens) fail))
	(character (char) (k (make-asexp src start end char) end (rest-of tokens) fail))
	(string (str) (k (make-asexp src start end str) end (rest-of tokens) fail))
	(identifier (id) (k (make-asexp src start end id) end (rest-of tokens) fail))
	(apostrophe () (read-annotated-abbreviation tokens 'quote src handler fail k))
	(backquote () (read-annotated-abbreviation tokens 'quasiquote src handler fail k))
	(comma () (read-annotated-abbreviation tokens 'unquote src handler fail k))
	(comma-at () (read-annotated-abbreviation tokens 'unquote-splicing src handler fail k))
	(lparen ()
	  (let ((tokens (rest-of tokens)))
	    (read-asexp-sequence tokens 'rparen src handler fail
	      (lambda-cont4 (asexps end tokens-left fail)
		(k (make-asexp src start end asexps) end tokens-left fail)))))
	(lbracket ()
	  (let ((tokens (rest-of tokens)))
	    (read-asexp-sequence tokens 'rbracket src handler fail
	      (lambda-cont4 (asexps end tokens-left fail)
		(k (make-asexp src start end asexps) end tokens-left fail)))))
	(lvector ()
	  (read-avector-sequence (rest-of tokens) src handler fail
	    (lambda-cont4 (asexps end tokens-left fail)
	      (k (make-asexp src start end (list->vector asexps)) end tokens-left fail))))
	(else (unexpected-token-error tokens src handler fail))))))

(define* read-annotated-abbreviation
  (lambda (tokens keyword src handler fail k)
    (let ((start (get-token-start (first tokens)))
	  (keyword-end (get-token-end (first tokens))))
      (read-asexp (rest-of tokens) src handler fail
	(lambda-cont4 (asexp end tokens-left fail)
	  (k (make-asexp src start end (list (make-asexp src start keyword-end keyword) asexp))
	     end tokens-left fail))))))

(define* read-avector-sequence
  (lambda (tokens src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (record-case (first tokens)
      (rparen ()
	(close-asexp-sequence '() tokens 'rparen src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-asexp tokens src handler fail
          (lambda-cont4 (asexp1 end tokens-left fail)
	    (read-avector-sequence tokens-left src handler fail
	      (lambda-cont4 (asexps end tokens-left fail)
		(k (cons asexp1 asexps) end tokens-left fail)))))))))

(define* read-asexp-sequence
  (lambda (tokens expected-terminator src handler fail k)  ;; k gets 4 args: asexps, end, tokens-left, fail
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-asexp-sequence '() tokens expected-terminator src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-asexp tokens src handler fail
          (lambda-cont4 (asexp1 end tokens-left fail)
	    (if (token-type? (first tokens-left) 'dot)
	      (read-asexp (rest-of tokens-left) src handler fail
		(lambda-cont4 (asexp2 end tokens-left fail)
		  (if (or (null?^ asexp2) (pair?^ asexp2))
		    (close-asexp-sequence
		      (cons asexp1 (get-sexp asexp2)) tokens-left expected-terminator src handler fail k)
		    (close-asexp-sequence
		      (cons asexp1 asexp2) tokens-left expected-terminator src handler fail k))))
	      (read-asexp-sequence tokens-left expected-terminator src handler fail
		(lambda-cont4 (asexps end tokens-left fail)
		  (k (cons asexp1 asexps) end tokens-left fail))))))))))

(define* close-asexp-sequence
  (lambda (asexps tokens expected-terminator src handler fail k)  ;; k gets 4 args: asexps, end, tokens-left, fail
    (let ((end (get-token-end (first tokens))))
      (record-case (first tokens)
	((rparen rbracket) ()
	 (cond
	   ((token-type? (first tokens) expected-terminator)
	    (k asexps end (rest-of tokens) fail))
	   ((eq? expected-terminator 'rparen)
	    (read-error "parenthesized list terminated by bracket" tokens src handler fail))
	   ((eq? expected-terminator 'rbracket)
	    (read-error "bracketed list terminated by parenthesis" tokens src handler fail))))
	(else (unexpected-token-error tokens src handler fail))))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(define init-cont (lambda-cont (v) (halt* v)))
(define init-cont2 (lambda-cont2 (v1 v2) (halt* v1)))
(define init-cont3 (lambda-cont3 (v1 v2 v3) (halt* v1)))
(define init-handler (lambda-handler (e) (halt* (list 'exception e))))
(define init-handler2 (lambda-handler2 (e fail) (halt* (list 'exception e))))
(define init-fail (lambda-fail () (halt* "no more choices")))

(define scan-string
  (lambda (input)
    (scan-input input 'stdin init-handler2 init-fail init-cont2)))

(define scan-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail init-cont2)))

(define read-string
  (lambda (input)
    (read-datum input 'stdin init-handler2 init-fail init-cont3)))

(define* read-datum
  (lambda (input src handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (scan-input input src handler fail
      (lambda-cont2 (tokens fail)
        (read-sexp tokens src handler fail
          (lambda-cont3 (sexp tokens-left fail)
            (if (token-type? (first tokens-left) 'end-marker)
              (k sexp tokens-left fail)
	      (read-error "tokens left over" tokens-left src handler fail))))))))

(define read-file
  (lambda (filename) 
   (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
        (read-file-loop tokens filename init-handler2 init-fail init-cont2)))))

(define* read-file-loop
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
        (lambda-cont3 (sexp tokens-left fail)
	  (read-file-loop tokens-left src handler fail
	    (lambda-cont2 (sexps fail)
	      (k (cons sexp sexps) fail))))))))

(define print-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
        (print-file-loop tokens filename init-handler2 init-fail init-cont2)))))

(define* print-file-loop
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k 'done fail)
      (read-sexp tokens src handler fail
        (lambda-cont3 (sexp tokens-left fail)
          (pretty-print sexp)
          (print-file-loop tokens-left src handler fail k))))))

(define aread-string
  (lambda (input)
    (aread-datum input 'stdin init-handler2 init-fail init-cont3)))

(define* aread-datum
  (lambda (input src handler fail k)  ;; k receives 3 args: asexp, tokens-left, fail
    (scan-input input src handler fail
      (lambda-cont2 (tokens fail)
        (read-asexp tokens src handler fail
          (lambda-cont4 (asexp end tokens-left fail)
            (if (token-type? (first tokens-left) 'end-marker)
              (k asexp tokens-left fail)
	      (read-error "tokens left over" tokens-left src handler fail))))))))

(define aread-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
        (aread-file-loop tokens filename init-handler2 init-fail init-cont2)))))

(define* aread-file-loop
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-asexp tokens src handler fail
        (lambda-cont4 (asexp end tokens-left fail)
	  (aread-file-loop tokens-left src handler fail
	    (lambda-cont2 (asexps fail)
	      (k (cons asexp asexps) fail))))))))
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

(define* lookup-binding
  (lambda (variable env var-info handler fail k)
    (let ((binding (search-env env variable)))
      (if binding
	(k binding fail)
	(split-variable variable fail
	  (lambda-cont2 (components fail)
	    (if (dlr-env-contains variable)
	      (k (dlr-env-lookup variable) fail)
	      (if components
		(lookup-variable-components components "" env handler fail k)
		(runtime-error (format "unbound variable ~a" variable) var-info handler fail)))))))))
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
	 (memq x (get-reserved-keywords)))))

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
;; Calico Scheme interpreter with support for choose
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")
(load "environments-cps.ss")
(load "parser-cps.ss")

;;----------------------------------------------------------------------------
;; enables interpreter to be run directly in Petite Chez Scheme, independently of C#

;; dummy versions of functions used in C# code
(define dlr-exp? (lambda (x) #f))
(define dlr-apply apply)
(define dlr-func (lambda (x) x))
(define dlr-env-contains (lambda (x) #f))
(define dlr-env-lookup (lambda (x) #f))
(define dlr-object? (lambda (x) #f))
(define dlr-lookup-components (lambda (x y) #f))
(define set-global-value! (lambda (var x) #f))
(define set-global-docstring! (lambda (var x) #f))
(define printf-prim printf)
(define using-prim (lambda ignore #f))
(define iterator? (lambda ignore #f))
(define get_type (lambda (x) 'unknown))

;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------
;; read-eval-print loop

(define start
  (lambda ()
    ;; start with fresh environments
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (read-eval-print-loop)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop)))

(define* read-eval-print-loop
  (lambda ()
    (let ((input (raw-read-line "==> ")))  ;; read-line or raw-read-line
      ;; execute gets redefined as execute-rm in the scheme register machine
      (let ((result (execute input 'stdin)))
	(if (not (void? result))
	    (safe-print result))
	(if *need-newline*
	  (newline))
	(if (end-of-session? result)
	  (halt* 'goodbye)
	  (read-eval-print-loop))))))

(define REP-k
  (lambda-cont2 (v fail)
    (set! *last-fail* fail)
    (halt* v)))

(define REP-handler
  (lambda-handler2 (e fail)
    (set! *last-fail* fail)
    (halt* (list 'exception e))))

(define REP-fail
  (lambda-fail ()
    (halt* "no more choices")))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define exception?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'exception))))

(define execute-string
  (lambda (input)
    (execute input 'stdin)))

(define execute-file
  (lambda (filename)
    (execute (read-content filename) filename)))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (let ((result (scan-input input src REP-handler *last-fail* REP-k)))
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
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;; not used
(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (set! *last-fail* REP-fail)))

;;----------------------------------------------------------------------------
;; for register machine only

(define execute-string-rm
  (lambda (input)
    (execute-rm input 'stdin)))

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

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input 'stdin try-parse-handler *last-fail*
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens 'stdin try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

;;----------------------------------------------------------------------------
;; old read-eval-print loop

;; redefined as REP-k when no-csharp-support.ss is loaded
;;(define scheme-REP-k
;;  (lambda-cont2 (v fail)
;;    (if (not (void? v))
;;	(safe-print v))
;;    (if *need-newline*
;;      (newline))
;;    (read-eval-print fail)))

;; redefined as REP-handler when no-csharp-support.ss is loaded
;;(define scheme-REP-handler
;;  (lambda-handler2 (e fail)
;;    (REP-k `(uncaught exception: ,e) fail)))

;; redefined as REP-fail when no-csharp-support.ss is loaded
;;(define scheme-REP-fail
;;  (lambda-fail ()
;;    (REP-k "no more choices" REP-fail)))

;;(define start
;;  (lambda ()
;;    ;; start with fresh environments
;;    (set! toplevel-env (make-toplevel-env))
;;    (set! macro-env (make-macro-env^))
;;    (read-eval-print REP-fail)))

;; avoids reinitializing environments on startup (useful for crash recovery)
;;(define restart
;;  (lambda ()
;;    (printf "Restarting...\n")
;;    (read-eval-print REP-fail)))

;;(define* read-eval-print
;;  (lambda (fail)
;;    (set! load-stack '())  ;; in case a previous load encountered an error
;;    (let ((input (raw-read-line "==> ")))  ;; read-line or raw-read-line
;;      (scan-input input 'stdin REP-handler fail
;;	(lambda-cont2 (tokens fail)
;;	  (read-and-eval-asexps tokens 'stdin toplevel-env REP-handler fail REP-k))))))

;;----------------------------------------------------------------------------

(define* read-and-eval-asexps
  (lambda (tokens src env handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-asexp tokens src handler fail
	(lambda-cont4 (datum end tokens-left fail)
	  (aparse datum handler fail
	    (lambda-cont2 (exp fail)
	      (m exp env handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-asexps tokens-left src env handler fail k)))))))))))

(define handle-debug-info
  (lambda (exp result)
    (let ((info (rac exp)))
      (if (eq? info 'none)
	  (printf "~s evaluates to ~a~%" (aunparse exp) result)
	  (printf "~s at line ~a char ~a of ~a evaluates to ~a~%"
		  (aunparse exp)
		  (get-start-line info)
		  (get-start-char info)
		  (get-srcfile info)
		  result)))))

(define *tracing-on?* #t)

(define make-debugging-k
  (lambda (exp k)
    (if (not *tracing-on?*)
	k
	(lambda-cont2 (v fail)
	  (handle-debug-info exp v)
	  (k v fail)))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
   (let ((k (make-debugging-k exp k)))   ;; need to reindent
    (cases aexpression exp
      (lit-aexp (datum info) (k datum fail))
      (var-aexp (id info) (lookup-value id env info handler fail k))
      (func-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (dlr-func proc) fail))))
      (if-aexp (test-exp then-exp else-exp info)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (assign-aexp (var rhs-exp var-info info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding var env var-info handler fail
	      (lambda-cont2 (binding fail)
		(let ((old-value (binding-value binding)))
		  (set-binding-value! binding rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-binding-value! binding old-value) (fail))))
		    (k void-value new-fail))))))))
      (define-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding-in-first-frame var env handler fail
	      (lambda-cont2 (binding fail)
		(set-binding-value! binding rhs-value)
		(set-binding-docstring! binding docstring)
		;; definitions should occur only at top level, so no need to undo
		(k void-value fail))))))
      (define!-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (set-global-value! var rhs-value)
	    (set-global-docstring! var docstring)
	    (k void-value fail))))
      (define-syntax-aexp (name clauses aclauses info)
	(lookup-binding-in-first-frame name macro-env handler fail
	  (lambda-cont2 (binding fail)
	    (set-binding-value! binding (make-pattern-macro^ clauses aclauses))
	    (k void-value fail))))
      (begin-aexp (exps info)
	(eval-sequence exps env handler fail k))
      (lambda-aexp (formals bodies info)
	(k (closure formals bodies env) fail))
      (mu-lambda-aexp (formals runt bodies info)
	(k (mu-closure formals runt bodies env) fail))
      (trace-lambda-aexp (name formals bodies info)
	  (k (trace-closure name formals bodies env) fail))
      (mu-trace-lambda-aexp (name formals runt bodies info)
	(k (mu-closure formals runt bodies env) fail))
      (try-catch-aexp (body cvar cexps info)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler fail k)))
      (try-finally-aexp (body fexps info)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail)))))))
      (try-catch-finally-aexp (body cvar cexps fexps info)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler fail
	     (lambda-cont2 (v fail)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler fail
		 (lambda-cont2 (v2 fail) (k v fail)))))))
      (raise-aexp (exp info)
	(m exp env handler fail
	  ;; TODO: pass in more info to handler (k, env) to support resume, etc.
	  (lambda-cont2 (e fail) (handler e fail))))
      (dict-aexp (pairs info)
	(k (list 'dict pairs) fail))
      (help-aexp (var var-info info)
	(if (reserved-keyword? var)
	  (k (format "~a is a keyword" var) fail)
	  (lookup-binding var env var-info handler fail
	    (lambda-cont2 (binding fail)
	      (k (binding-docstring binding) fail)))))
      (choose-aexp (exps info)
	(eval-choices exps env handler fail k))
      (app-aexp (operator operands info)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(cond
		  ((dlr-exp? proc) (k (dlr-apply proc args) fail))
		  ((procedure-object? proc) (proc args env info handler fail k))
		  (else (runtime-error (format "attempt to apply non-procedure ~a" proc)
				       info handler fail))))))))
      (else (error 'm "bad abstract syntax: ~s" exp)))))
)

(define* runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (format "runtime error: ~a" msg) fail)
      (let ((src (get-srcfile info))
	    (line (get-start-line info))
	    (char (get-start-char info)))
	(handler (format "runtime error: ~a ~a" msg (where-at line char src)) fail)))))

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

(define closure-depth 0)

;; FIXME: cps or just loop
(define repeat
  (lambda (item times)
    (if (= times 0)
	'()
	(cons item (repeat item (- times 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (= (length args) (length formals))
	  (begin
	    (printf "~scall: ~s~%" (apply string-append (repeat " |" closure-depth)) (cons name args))
	    (set! closure-depth (+ closure-depth 1))
	    (eval-sequence bodies (extend env formals args) handler fail
		 (lambda-cont2 (v fail)
	            (set! closure-depth (- closure-depth 1))
	            (printf "~sreturn: ~s~%" (apply string-append (repeat " |" closure-depth)) v)
		    (k2 v fail))))
	  (runtime-error "incorrect number of arguments in application" info handler fail)))))

(define closure
  (lambda (formals bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (= (length args) (length formals))
	(eval-sequence bodies (extend env formals args) handler fail k2)
	(runtime-error "incorrect number of arguments in application" info handler fail)))))

(define mu-closure
  (lambda (formals runt bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (if (>= (length args) (length formals))
	(let ((new-env
		(extend env
		  (cons runt formals)
		  (cons (list-tail args (length formals))
			(list-head args (length formals))))))
	  (eval-sequence bodies new-env handler fail k2))
	(runtime-error "not enough arguments in application" info handler fail)))))

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

;;----------------------------------------------------------------------------
;; Primitives

(define length-one?
  (lambda (ls)
    (and (not (null? ls)) (null? (cdr ls)))))

(define length-two?
  (lambda (ls)
    (and (not (null? ls)) (not (null? (cdr ls))) (null? (cddr ls)))))

(define length-at-least?
  (lambda (n ls)
    (cond
      ((< n 1) #t)
      ((or (null? ls) (not (pair? ls))) #f)
      (else (length-at-least? (- n 1) (cdr ls))))))

(define all-numeric?
  (lambda (ls)
    (or (null? ls)
	(and (number? (car ls))
	     (all-numeric? (cdr ls))))))

;; void
(define void-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 void-value fail)))

(define void-value '<void>)

(define void?
  (lambda (x) (eq? x void-value)))

;; exit
(define exit-prim
  (lambda-proc (args env2 info handler fail k2)
    (halt* end-of-session)))

(define end-of-session?
  (lambda (x) (eq? x end-of-session)))

(define end-of-session '(exiting the interpreter))

;; eval
(define eval-prim
  (lambda-proc (args env2 info handler fail k2)
    (reannotate-cps (car args)
      (lambda-cont (adatum)
	(aparse adatum handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env handler fail k2)))))))

;; parse
(define parse-prim
  (lambda-proc (args env2 info handler fail k2)
    (reannotate-cps (car args)
      (lambda-cont (adatum)
	(aparse adatum handler fail k2)))))

;; unparse
(define unparse-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car args)) fail)))

;; parse-string
(define parse-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-asexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (aparse adatum handler fail k2)
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; read-string
(define read-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-asexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k2 adatum fail)
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; apply
(define apply-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((proc (car args))
	  (proc-args (cadr args)))
      (proc proc-args env2 info handler fail k2))))

;; sqrt
(define sqrt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to sqrt" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "sqrt called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply sqrt args) fail)))))

;; print
(define print-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each safe-print args)
    (k2 void-value fail)))

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
    (or (procedure? x) (and (pair? x) (eq? (car x) 'procedure)))))

(define environment-object?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

;; display
;; fix: why is this so complicated?
(define display-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((s (format "~a" (car args))))  ;; must use ~a, not ~s, to handle embedded newlines properly
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s)
      (k2 void-value fail))))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (equal? (substring s (- len 1) len) "\n"))))

;; newline
(define newline-prim
  (lambda-proc (args env2 info handler fail k2)
    (set! *need-newline* #f)
    (newline)
    (k2 void-value fail)))

(define *need-newline* #f)

;; load
(define load-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-one? args))
       (runtime-error "incorrect number of arguments to load" info handler fail)
       (load-file (car args) toplevel-env info handler fail k2))))

(define load-stack '())

(define* load-file
  (lambda (filename env info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (runtime-error (format "filename ~a is not a string" filename) info handler fail))
      ((not (file-exists? filename))
       (runtime-error (format "attempted to load nonexistent file ~a" filename) info handler fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-asexps tokens filename env handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* load-files
  (lambda (filenames env info handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (load-file (car filenames) env info handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env info handler fail k))))))

;; length
(define length-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-one? args))
      (runtime-error "incorrect number of arguments to length" info handler fail)
      (length-loop (car args) 0 (car args) info handler fail k2))))

(define* length-loop
  (lambda (x sum ls info handler fail k2)
    (cond
      ((null? x) (k2 sum fail))
      ((not (pair? x))
       (runtime-error (format "length called on improper list ~s" ls) info handler fail))
      (else (length-loop (cdr x) (+ sum 1) ls info handler fail k2)))))

;; symbol?
(define symbol?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error
         (format "incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument" args)
         info handler fail))
      (else (k2 (apply symbol? args) fail)))))

;; number?
(define number?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to number?" info handler fail))
      (else (k2 (apply number? args) fail)))))

;; boolean?
(define boolean?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to boolean?" info handler fail))
      (else (k2 (apply boolean? args) fail)))))

;; string?
(define string?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string?" info handler fail))
      (else (k2 (apply string? args) fail)))))

;; null?
(define null?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to null?" info handler fail))
      (else (k2 (apply null? args) fail)))))

;; pair?
(define pair?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to pair?" info handler fail))
      (else (k2 (apply pair? args) fail)))))

;; cons
(define cons-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to cons" info handler fail))
      (else (k2 (apply cons args) fail)))))

;; car
(define car-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to car" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "car called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply car args) fail)))))

;; cdr
(define cdr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdr args) fail)))))

;; cadr
(define cadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadr" info handler fail))
      ((not (length-at-least? 2 (car args)))
       (runtime-error (format "cadr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply cadr args) fail)))))

;; caddr
(define caddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caddr" info handler fail))
      ((not (length-at-least? 3 (car args)))
       (runtime-error (format "caddr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply caddr args) fail)))))

;; list
(define list-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 args fail)))

;; +
(define plus-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (all-numeric? args))
      (runtime-error "+ called on non-numeric argument(s)" info handler fail)
      (k2 (apply + args) fail))))

;; -
(define minus-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args)
       (runtime-error "incorrect number of arguments to -" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "- called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply - args) fail)))))

;; *
(define times-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (all-numeric? args))
      (runtime-error "* called on non-numeric argument(s)" info handler fail)
      (k2 (apply * args) fail))))

;; /
(define divide-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args)
       (runtime-error "incorrect number of arguments to /" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "/ called on non-numeric argument(s)" info handler fail))
      ((member 0 (cdr args))
       (runtime-error "division by zero" info handler fail))
      (else (k2 (apply / args) fail)))))

;; <
(define lt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "< called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply < args) fail)))))

;; >
(define gt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "> called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply > args) fail)))))

;; <=
(define lt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <=" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "<= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply <= args) fail)))))

;; >=
(define gt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >=" info handler fail))
      ((not (all-numeric? args))
       (runtime-error ">= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply >= args) fail)))))

;; =
(define equal-sign-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to =" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "= called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply = args) fail)))))

;; abs
(define abs-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to abs" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "abs called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply abs args) fail)))))

;; equal?
(define equal?-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to equal?" info handler fail)
      (equal-objects? (car args) (cadr args)
	(lambda-cont (bool) (k2 bool fail))))))

(define* equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
	   ;; (eq? x y) would be easier, but Eq doesn't work correctly for bools in Scheme.cs:
	   (and (boolean? x) (boolean? y) (or (and x y) (and (not x) (not y))))
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

;; eq?
(define eq?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to eq?" info handler fail))
      (else (k2 (apply eq? args) fail)))))

;; memq
(define memq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to memq" info handler fail))
      (else (k2 (apply memq args) fail)))))

;; member
(define member-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to member" info handler fail)
      (member-loop (car args) (cadr args) (cadr args) info handler fail k2))))

(define* member-loop
  (lambda (x y ls info handler fail k)
    (cond
      ((null? y) (k #f fail))
      ((not (pair? y))
       (runtime-error (format "member called on improper list ~s" ls) info handler fail))
      (else (equal-objects? x (car y)
	      (lambda-cont (bool)
		(if bool
		  (k y fail)
		  (member-loop x (cdr y) ls info handler fail k))))))))

;; range
(define range-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((or (null? args) (length-at-least? 4 args))
       (runtime-error "incorrect number of arguments to range" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "range called on non-numeric argument(s)" info handler fail))
      (else (k2 (apply range args) fail)))))

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
	
;; set-car!
(define set-car!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-car!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-car! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-car! args) fail)))))

;; set-cdr!
(define set-cdr!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-cdr!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-cdr! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-cdr! args) fail)))))

;; import
;; bug fix needed:
;; (import "my-fact.ss" 'm)
;; (m.m.m.m.m.fib 10) =>  89

(define import-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((filename (car args)))
      (if (null? (cdr args))
	(load-file filename env2 'none handler fail k2)
	(let ((module-name (cadr args)))
	  (lookup-binding-in-first-frame module-name env2 handler fail
	    (lambda-cont2 (binding fail)
	      (let ((module (extend env2 '() '())))
		(set-binding-value! binding module)
		(load-file filename module 'none handler fail k2)))))))))

;; get
(define get-prim
  (lambda-proc (args env2 info handler fail k2)
    (get-primitive args env2 info handler fail k2)))

(define* get-primitive
  (lambda (args env info handler fail k)
    (let ((sym (car args)))
      (lookup-value sym env 'none handler fail
	(lambda-cont2 (v fail)
	  (cond
	    ((null? (cdr args)) (k v fail))
	    ((not (environment? v))
	     (runtime-error (format "invalid module ~a" sym) info handler fail))
	    (else (get-primitive (cdr args) v info handler fail k))))))))

;; call/cc
(define call/cc-prim
  (lambda-proc (args env info handler fail k)
    (if (not (length-one? args))
      (runtime-error "incorrect number of arguments to call/cc" info handler fail)
      (let ((proc (car args)))
	(if (not (procedure-object? proc))
	  (runtime-error "call/cc called with non-procedure" info handler fail)
	  (let ((fake-k (lambda-proc (args env2 info handler fail k2) (k (car args) fail))))
	    (if (dlr-exp? proc)
	      (k (dlr-apply proc (list fake-k)) fail)
	      (proc (list fake-k) env info handler fail k))))))))

;; abort
(define abort-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (null? args)
      (REP-k void-value fail)
      (REP-k (car args) fail))))

;; require
(define require-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to require" info handler fail))
      ((true? (car args)) (k2 'ok fail))
      (else (fail)))))

;; cut
(define cut-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (null? args))
      (runtime-error "incorrect number of arguments to cut" info handler fail)
      (k2 'ok REP-fail))))

;; reverse
(define reverse-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to reverse" info handler fail))
      ((not (list? args))
       (runtime-error (format "reverse called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply reverse args) fail)))))

;; append
(define append-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to append" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "append called on incorrect list structure ~s" (car args)) info handler fail))
      (else (append-all args (lambda-cont (v) (k2 v fail)))))))
;;      (else (k2 (apply append args) fail)))))

(define* append2
  (lambda (ls1 ls2 k)
    (if (null? ls1)
      (k ls2)
      (append2 (cdr ls1) ls2
	(lambda-cont (v)
	  (k (cons (car ls1) v)))))))

(define* append-all
  (lambda (lists k)
    (cond
      ((null? lists) (k '()))
      ((null? (cdr lists)) (k (car lists)))
      (else (append-all (cdr lists)
	      (lambda-cont (ls)
		(append2 (car lists) ls k)))))))

;; list->vector
(define list-to-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->vector" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->vector called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply list->vector args) fail)))))

;; dir
(define dir-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (dir args env2) fail)))

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

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

;; current-time
(define current-time-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (get-current-time) fail)))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now) 1000000000))))))

;; map
(define map-prim
  (lambda-proc (args env2 info handler fail k2)
    (map-primitive (car args) (cdr args) env2 handler fail k2)))

;; supports procedures of any number of arguments
(define* map-primitive
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args))
              (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler fail k))
            (else (mapN proc list-args env handler fail k)))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list))
      (cons (car arg-list) (listify (cdr arg-list))))
     ((vector? (car arg-list))
      (cons (vector->list (car arg-list)) (listify (cdr arg-list))))
     ((string? (car arg-list))
      (cons (string->list (car arg-list)) (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map" 
		  (get_type (car arg-list))))))) ;; get_type is defined in C#

(define* iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define* iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env 'none handler fail
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
          (proc (list item) env 'none handler fail
            (lambda-cont2 (v1 fail)
              (iterate-collect-continue proc iterator env handler fail
                (lambda-cont2 (v2 fail)
                  (k (cons v1 v2) fail)))))))))

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
	(proc (list (car list1)) env 'none handler fail
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
	(proc (list (car list1) (car list2)) env 'none handler fail
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
	(proc (map car lists) env 'none handler fail
	  (lambda-cont2 (v1 fail)
	    (mapN proc (map cdr lists) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

;; for-each
(define for-each-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each-primitive (car args) (cdr args) env2 handler fail k2)))

(define* for-each-primitive
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
      (iterate proc (car lists) env handler fail k)
      (let ((arg-list (listify lists)))
	(if (null? (car arg-list))
	  (k void-value fail)
	  (if (dlr-exp? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-primitive proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env 'none handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-primitive proc (map cdr arg-list) env handler fail k)))))))))

;; end
(define env-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 env2 fail)))

;; using (not defined in scheme)
(define using-primitive
  (lambda-proc (args env2 info handler fail k2)
    (k2 (using-prim args env2) fail)))

;; not
(define not-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to not" info handler fail))
      (else (k2 (not (car args)) fail)))))

;; printf (not defined in scheme)
(define printf-primitive
  (lambda-proc (args env2 info handler fail k2)
    (apply printf-prim args)
    (k2 void-value fail)))

;; vector
(define vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (list->vector args) fail)))

;; vector-set!
(define vector-set!-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (vector-set! (car args) (cadr args) (caddr args)) fail)))

;; vector-ref
(define vector-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply vector-ref args) fail)))

;; make-vector
(define make-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply make-vector args) fail)))

;; error
(define error-prim
  (lambda-proc (args env2 info handler fail k2)
    (let* ((location (format "Error in ~a: " (car args)))
	   (message (string-append location (apply format (cdr args)))))
      (runtime-error message info handler fail))))

;; list-ref
(define list-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to list-ref" info handler fail))
      (else (k2 (apply list-ref args) fail)))))

;; Add new procedures above here!
;; Then, add NAME to env
;; Then, add NAME_proc to Scheme.cs

;; this is here as a hook for extending environments in C# etc.
(define make-initial-env-extended
  (lambda (env) env))

(define make-toplevel-env
  (lambda ()
    (make-initial-env-extended
     (make-initial-environment
      (list 'void 'exit 'eval 'parse 'parse-string 'read-string 'apply 'sqrt 'print 'display 'newline
	    'load 'length 'symbol? 'number? 'boolean? 'string? 'null? 'pair? 'cons 'car 'cdr 'cadr 'caddr
	    'list '+ '- '* '/ '< '> '= '=? 'abs 'equal? 'eq? 'memq 'member
	    'range 'set-car! 'set-cdr! 'import 'get 'call-with-current-continuation 'call/cc 'abort 'require
	    'cut 'reverse 'append 'list->vector 'dir 'current-time 'map 'for-each 'env 'using 'not 'printf
	    'vector 'vector-set! 'vector-ref 'make-vector '<= '>= 'error 'list-ref 'unparse)
      (list void-prim exit-prim eval-prim parse-prim parse-string-prim read-string-prim apply-prim sqrt-prim
	    print-prim display-prim newline-prim load-prim length-prim symbol?-prim number?-prim boolean?-prim
	    string?-prim null?-prim pair?-prim cons-prim car-prim cdr-prim cadr-prim
	    caddr-prim list-prim plus-prim minus-prim times-prim divide-prim lt-prim gt-prim
	    equal-sign-prim equal-sign-prim abs-prim equal?-prim eq?-prim memq-prim member-prim range-prim
	    set-car!-prim set-cdr!-prim
	    import-prim get-prim call/cc-prim call/cc-prim abort-prim require-prim cut-prim reverse-prim
	    append-prim list-to-vector-prim dir-prim current-time-prim map-prim for-each-prim env-prim
	    using-primitive not-prim printf-primitive vector-prim vector-set!-prim vector-ref-prim
	    make-vector-prim lt-or-eq-prim gt-or-eq-prim error-prim list-ref-prim unparse-prim)))))

(define toplevel-env (make-toplevel-env))

;;------------------------------------------------------------------------
;; C# support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 info handler fail k2)
      (k2 (apply* external-function-object args) fail))))

;; not used
(define Main
  (lambda filenames
    (printf "Calico Scheme (0.2)\n")
    (printf "(c) 2009-2011, IPRE\n")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    ;; in the register machine, this call just sets up the registers
    (load-files filenames toplevel-env 'none REP-handler REP-fail REP-k)
    ;; starts the computation after registers are set up
    (trampoline)))

;; temporary - remove before transforming to C#
;;(load "no-csharp-support.ss")
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

;;-----------------------------------------------------------------------------------------------

;; for testing only
(define up
  (lambda (p1 p2)
    (unify-patterns p1 p2 init-cont)))

;; for testing only
(define aup
  (lambda (s1 s2)
    (let* ((ap1 (aread-string s1))
	   (ap2 (aread-string s2))
	   (p1 (unannotate ap1))
	   (p2 (unannotate ap2)))
      (unify-patterns^ p1 p2 ap1 ap2
	(lambda-cont (s)
	  (if (not s)
	    s
	    (print-sub s)))))))

;;-----------------------------------------------------------------------------------------------
;; annotated version

(define* unify-patterns^
  (lambda (p1 p2 ap1 ap2 k)    ;; k receives: subst/#f
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2 ap2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2 ap2)))))))
      ((pattern-variable? p2) (unify-patterns^ p2 p1 ap2 ap1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs^ p1 p2 ap1 ap2 k))
      (else (k #f)))))

(define* unify-pairs^
  (lambda (pair1 pair2 apair1 apair2 k)
    (unify-patterns^ (car pair1) (car pair2) (car^ apair1) (car^ apair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate^ (cdr pair1) s-car (^cdr^ apair1)
	    (lambda-cont2 (new-cdr1 new-acdr1)
	      (instantiate^ (cdr pair2) s-car (^cdr^ apair2)
		(lambda-cont2 (new-cdr2 new-acdr2)
		  (unify-patterns^ new-cdr1 new-cdr2 new-acdr1 new-acdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate^
  (lambda (pattern s ap k2)   ;; k2 receives: sexp, asexp
    (cond
      ((constant? pattern) (k2 pattern ap))
      ((pattern-variable? pattern) (apply-sub^ s pattern ap k2))
      ((pair? pattern)
       (instantiate^ (car pattern) s (car^ ap)
	 (lambda-cont2 (a aa)
	   (instantiate^ (cdr pattern) s (^cdr^ ap)
	     (lambda-cont2 (b ab)
	       (k2 (cons a b) (cons^ aa ab (get-source-info ap))))))))
      (else (error 'instantiate^ "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

(define* apply-sub^
  (lambda (s var avar k2)        ;; k2 receives: sexp, asexp
    (record-case (cdr s)
      (empty () (k2 var avar))
      (unit (new-var new-pattern new-apattern)
	(if (equal? var new-var)
	  (k2 new-pattern new-apattern)
	  (k2 var avar)))
      (composite (s1 s2)
	(apply-sub^ s1 var avar
	  (lambda-cont2 (pattern apattern)
	    (instantiate^ pattern s2 apattern k2))))
      (else (error 'apply-sub^ "bad substitution: ~a" s)))))

(define print-sub
  (lambda (s)
    (record-case (cdr s)
      (empty () (void))
      (unit (new-var new-pattern new-apattern)
	(display new-var)
	(display " = ")
	(newline)
	(pretty-print new-pattern)
	(pretty-print new-apattern)
	(newline))
      (composite (s1 s2)
	(print-sub s1)
	(print-sub s2))
      (else (error 'print-sub "bad substitution: ~s" s)))))

;;-----------------------------------------------------------------------------------------------
;; unannotated version

;;(define* unify-patterns
;;  (lambda (p1 p2 k)
;;    (cond
;;      ((pattern-variable? p1)
;;       (if (pattern-variable? p2)
;;	 (k (make-sub 'unit p1 p2))
;;	 (occurs? p1 p2
;;	   (lambda-cont (bool)
;;	     (if bool
;;	       (k #f)
;;	       (k (make-sub 'unit p1 p2)))))))
;;      ((pattern-variable? p2) (unify-patterns p2 p1 k))
;;      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
;;      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
;;      (else (k #f)))))

;;(define* unify-pairs
;;  (lambda (pair1 pair2 k)
;;    (unify-patterns (car pair1) (car pair2)
;;      (lambda-cont (s-car)
;;	(if (not s-car)
;;	  (k #f)
;;	  (instantiate (cdr pair1) s-car
;;	    (lambda-cont (new-cdr1)
;;	      (instantiate (cdr pair2) s-car
;;		(lambda-cont (new-cdr2)
;;		  (unify-patterns new-cdr1 new-cdr2
;;		    (lambda-cont (s-cdr)
;;		      (if (not s-cdr)
;;			(k #f)
;;			(k (make-sub 'composite s-car s-cdr))))))))))))))

;;(define* instantiate
;;  (lambda (pattern s k)
;;    (cond
;;      ((constant? pattern) (k pattern))
;;      ((pattern-variable? pattern) (apply-sub s pattern k))
;;      ((pair? pattern)
;;       (instantiate (car pattern) s
;;	 (lambda-cont (a)
;;	   (instantiate (cdr pattern) s
;;	     (lambda-cont (b)
;;	       (k (cons a b)))))))
;;      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;;;(define extend-sub
;;;;  (lambda (old-s new-var new-pattern)
;;;;    (list 'extended new-var new-pattern old-s)))

;;(define* apply-sub
;;  (lambda (s var k)
;;    (record-case (cdr s)
;;      (empty () (k var))
;;;;      (extended (new-var new-pattern old-s)
;;;;	(if (equal? var new-var)
;;;;	  (k new-pattern)
;;;;	  (apply-sub old-s var k)))
;;      (unit (new-var new-pattern)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (k var)))
;;      (composite (s1 s2)
;;	(apply-sub s1 var
;;	  (lambda-cont (pattern)
;;	    (instantiate pattern s2 k))))
;;      (else (error 'apply-sub "bad substitution: ~a" s)))))

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

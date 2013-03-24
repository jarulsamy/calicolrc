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
    (handler (make-exception "ScanError" msg src line char) fail)))

(define* unexpected-char-error
  (lambda (chars src handler fail)
    (let ((c (next-avail chars)))
      (if (char=? c #\nul)
	(scan-error "unexpected end of input" scan-line scan-char src handler fail)
	(scan-error (format "unexpected character '~a' encountered" c) scan-line scan-char src handler fail)))))

(define* convert-buffer-to-token
  (lambda (token-type buffer src handler fail k)  ;; k receives 1 argument: token
    (let ((buffer (reverse buffer)))
      (case token-type
        (end-marker
          (k (make-token1 'end-marker)))
        (integer
          (k (make-token2 'integer (list->string buffer))))
        (decimal
          (k (make-token2 'decimal (list->string buffer))))
        (rational
          (k (make-token2 'rational (list->string buffer))))
        (identifier
          (k (make-token2 'identifier (string->symbol (list->string buffer)))))
        (boolean
          (k (make-token2 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character
          (k (make-token2 'character (car buffer))))
        (named-character
          (let ((name (list->string buffer)))
            (cond
              ((string=? name "nul") (k (make-token2 'character #\nul)))
              ((string=? name "space") (k (make-token2 'character #\space)))
              ((string=? name "tab") (k (make-token2 'character #\tab)))
              ((string=? name "newline") (k (make-token2 'character #\newline)))
              ((string=? name "linefeed") (k (make-token2 'character #\newline)))
              ((string=? name "backspace") (k (make-token2 'character #\backspace)))
              ((string=? name "return") (k (make-token2 'character #\return)))
              ((string=? name "page") (k (make-token2 'character #\page)))
	      (else (scan-error (format "invalid character name #\\~a" name)
				token-start-line token-start-char src handler fail)))))
        (string
          (k (make-token2 'string (list->string buffer))))
        (else
          (k (make-token1 token-type)))))))

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

(define make-token1
  (lambda (token-type)
    (let ((start (list token-start-line token-start-char token-start-position))
	  (end (list last-scan-line last-scan-char last-scan-position)))
      (if (eq? token-type 'end-marker)
	(list token-type end end)
	(list token-type start end)))))

(define make-token2
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

(define rac
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (rac (cdr ls))))))

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
;; annotated s-expressions
;;
;; <aexp> ::= (#&atom <number> <info>)
;;          | (#&atom <boolean> <info>)
;;          | (#&atom <character> <info>)
;;          | (#&atom <string> <info>)
;;          | (#&atom <vector> <info>)
;;          | (#&atom <symbol> <info>)
;;          | (#&atom () <info>)
;;          | (#&pair <aexp> <aexp> <info>)
;;
;; <info> ::= (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos>)
;;          | (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos> <macro-name>+)

(define atom-tag (box 'atom))
(define pair-tag (box 'pair))

(define aatom?
  (lambda (x)
    (and (pair? x) (eq? (car x) atom-tag))))

(define apair?
  (lambda (x)
    (and (pair? x) (eq? (car x) pair-tag))))

(define annotated?
  (lambda (x)
    (and (pair? x) (or (eq? (car x) atom-tag) (eq? (car x) pair-tag)))))

(define untag-atom^
  (lambda (aatom)
    (cadr aatom)))

(define atom?^
  (lambda (asexp)
    (eq? (car asexp) atom-tag)))

(define pair?^
  (lambda (asexp)
    (eq? (car asexp) pair-tag)))

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
(define cadddr^ (lambda (asexp) (car^ (cdr^ (cdr^ (cdr^ asexp))))))
(define eq?^ (lambda (asexp sym) (eq? (cadr asexp) sym)))
(define vector->list^ (lambda (asexp) (vector->list (cadr asexp))))
(define symbol->string^ (lambda (asexp) (symbol->string (cadr asexp))))

(define list?^
  (lambda (asexp)
    (or (null?^ asexp)
	(and (pair?^ asexp) (list?^ (caddr asexp))))))

;; must wrap annotated lists with this before using ,@
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

(define cons^
  (lambda (a b info)
    (list pair-tag a b info)))

(define-native map^
  (lambda (f^ asexp)
    (cond
      ((null?^ asexp) (list atom-tag '() 'none))
      (else (cons^ (f^ (car^ asexp)) (map^ f^ (cdr^ asexp)) 'none)))))
      
(define *reader-generates-annotated-sexps?* #t)

;; for manual testing only
(define annotate
  (lambda (x info)
    (cond
      ((not *reader-generates-annotated-sexps?*) x)
      ((annotated? x) x)
      ((pair? x) (list pair-tag (annotate (car x) 'none) (annotate (cdr x) 'none) info))
      (else (list atom-tag x info)))))

(define* annotate-cps
  (lambda (x info k)   ;; k receives 1 arg: an annotated sexp
    (cond
      ((not *reader-generates-annotated-sexps?*) (k x))
      ((annotated? x) (k x))
      ((pair? x)
       (annotate-cps (car x) 'none
	 (lambda-cont (v1)
	   (annotate-cps (cdr x) 'none
	     (lambda-cont (v2)
	       (k (list pair-tag v1 v2 info)))))))
      (else (k (list atom-tag x info))))))

;; for manual testing only
(define unannotate
  (lambda (x)
    (cond
      ((aatom? x) (unannotate (cadr x)))
      ((apair? x) (cons (unannotate (cadr x)) (unannotate (caddr x))))
      ((pair? x) (cons (unannotate (car x)) (unannotate (cdr x))))
      ((vector? x) (list->vector (unannotate (vector->list x))))
      (else x))))

(define* unannotate-cps
  (lambda (x k)   ;; k receives 1 arg: an unannotated sexp
    (cond
      ((aatom? x)
       (unannotate-cps (cadr x) k))
      ((apair? x)
       (unannotate-cps (cadr x)
	 (lambda-cont (v1)
	   (unannotate-cps (caddr x)
	     (lambda-cont (v2)
	       (k (cons v1 v2)))))))
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

(define make-info
  (lambda (src start end)
    (cons src (append start end))))

(define replace-info
  (lambda (asexp new-info)
    (if (atom?^ asexp)
      (list atom-tag (cadr asexp) new-info)
      (list pair-tag (cadr asexp) (caddr asexp) new-info))))

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

(define get-source-info
  (lambda (asexp)
    (rac asexp)))

(define source-info?
  (lambda (x)
    (or (eq? x 'none) (list? x))))

(define has-source-info?
  (lambda (asexp)
    (not (eq? (get-source-info asexp) 'none))))

;; true if no macro name at end of info list
(define original-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
	 (= (length (get-source-info asexp)) 7))))

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

(define* unexpected-token-error
  (lambda (tokens src handler fail)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
	(read-error "unexpected end of input" tokens src handler fail)
	(read-error (format "unexpected '~a' encountered" (car token)) tokens src handler fail)))))

(define* read-error
  (lambda (msg tokens src handler fail)
    (let ((token (first tokens)))
      (handler (make-exception "ReadError" msg src 
		     (get-token-start-line token) 
		     (get-token-start-char token))
	       fail))))

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

(define* read-sexp
  (lambda (tokens src handler fail k)   ;; k receives 4 args: sexp, end, tokens-left, fail
    (let ((start (get-token-start (first tokens)))
	  (end (get-token-end (first tokens))))
      (record-case (first tokens)
	(integer (str)
	  (annotate-cps (string->integer str) (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(decimal (str)
	  (annotate-cps (string->decimal str) (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(rational (str)
	  (let ((num (string->rational str)))
	    (if (true? num)
	      (annotate-cps num (make-info src start end)
		(lambda-cont (sexp)
		  (k sexp end (rest-of tokens) fail)))
	      (read-error (format "cannot represent ~a" str) tokens src handler fail))))
	(boolean (bool)
	  (annotate-cps bool (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(character (char)
	  (annotate-cps char (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(string (str)
	  (annotate-cps str (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(identifier (id)
	  (annotate-cps id (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(apostrophe () (read-abbreviation tokens 'quote src handler fail k))
	(backquote () (read-abbreviation tokens 'quasiquote src handler fail k))
	(comma () (read-abbreviation tokens 'unquote src handler fail k))
	(comma-at () (read-abbreviation tokens 'unquote-splicing src handler fail k))
	(lparen ()
	  (let ((tokens (rest-of tokens)))
	    (read-sexp-sequence tokens 'rparen src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(annotate-cps sexps (make-info src start end)
		  (lambda-cont (sexp)
		    (k sexp end tokens-left fail)))))))
	(lbracket ()
	  (let ((tokens (rest-of tokens)))
	    (read-sexp-sequence tokens 'rbracket src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(annotate-cps sexps (make-info src start end)
		  (lambda-cont (sexp)
		    (k sexp end tokens-left fail)))))))
	(lvector ()
	  (read-vector-sequence (rest-of tokens) src handler fail
	    (lambda-cont4 (sexps end tokens-left fail)
	      (annotate-cps (list->vector sexps) (make-info src start end)
		(lambda-cont (sexp)
		  (k sexp end tokens-left fail))))))
	(else (unexpected-token-error tokens src handler fail))))))

(define* read-abbreviation
  (lambda (tokens keyword src handler fail k)
    (let ((start (get-token-start (first tokens)))
	  (keyword-end (get-token-end (first tokens))))
      (annotate-cps keyword (make-info src start keyword-end)
	(lambda-cont (v)
	  (read-sexp (rest-of tokens) src handler fail
	    (lambda-cont4 (sexp end tokens-left fail)
	      (annotate-cps (list v sexp) (make-info src start end)
		(lambda-cont (v2)
		  (k v2 end tokens-left fail))))))))))

(define* read-vector-sequence
  (lambda (tokens src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (record-case (first tokens)
      (rparen ()
	(close-sexp-sequence '() tokens 'rparen src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp1 end tokens-left fail)
	    (read-vector-sequence tokens-left src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(k (cons sexp1 sexps) end tokens-left fail)))))))))

(define* read-sexp-sequence
  (lambda (tokens expected-terminator src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp1 end tokens-left fail)
	    (if (token-type? (first tokens-left) 'dot)
	      (read-sexp (rest-of tokens-left) src handler fail
		(lambda-cont4 (sexp2 end tokens-left fail)
		  (close-sexp-sequence
		    (cons sexp1 sexp2) tokens-left expected-terminator src handler fail k)))
	      (read-sexp-sequence tokens-left expected-terminator src handler fail
		(lambda-cont4 (sexps end tokens-left fail)
		  (k (cons sexp1 sexps) end tokens-left fail))))))))))

(define* close-sexp-sequence
  (lambda (sexps tokens expected-terminator src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (let ((end (get-token-end (first tokens))))
      (record-case (first tokens)
	((rparen rbracket) ()
	 (cond
	   ((token-type? (first tokens) expected-terminator)
	    (k sexps end (rest-of tokens) fail))
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
(define init-cont4 (lambda-cont4 (v1 v2 v3 v4) (halt* v1)))
(define init-handler (lambda-handler (e) (halt* (list 'exception e))))
(define init-handler2 (lambda-handler2 (e fail) (halt* (list 'exception e))))
(define init-fail (lambda-fail () (halt* "no more choices")))

(define scan-string
  (lambda (input)
    (scan-input input 'stdin init-handler2 init-fail init-cont2)))

(define scan-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail init-cont2)))

(define aread-string
  (lambda (input)
    (aread-datum input 'stdin init-handler2 init-fail init-cont3)))

(define* aread-datum
  (lambda (input src handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (scan-input input src handler fail
      (lambda-cont2 (tokens fail)
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp end tokens-left fail)
            (if (token-type? (first tokens-left) 'end-marker)
              (k sexp tokens-left fail)
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
      (read-sexp tokens src handler fail
        (lambda-cont4 (sexp end tokens-left fail)
	  (aread-file-loop tokens-left src handler fail
	    (lambda-cont2 (sexps fail)
	      (k (cons sexp sexps) fail))))))))

(load "transformer-macros.ss")

;; Environments represented as data structures

;; <env> ::= (environment <frame> ...)
;; <frame> ::= (<vector-of-bindings> <list-of-vars>)
;; <binding> ::= (<value> . <docstring>)

;; bindings

(define make-binding
  (lambda (value)
    (cons value "")))

(define binding-value
  (lambda (binding)
    (car binding)))

(define binding-docstring
  (lambda (binding)
    (cdr binding)))

(define set-binding-value!
  (lambda (binding value)
    (set-car! binding value)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-cdr! binding docstring)))

;; frames

(define make-frame
  (lambda (variables values)
    (list (list->vector (map make-binding values)) variables)))

(define empty-frame?
  (lambda (frame)
    (null? (cadr frame))))

(define frame-bindings
  (lambda (frame)
    (car frame)))

;; environments

;; <environment> ::= (environment <frame> <frame>*)

(define environment?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda ()
    (list 'environment (make-frame '() '()))))

(define make-initial-environment
  (lambda (vars vals)
    (list 'environment (make-frame vars vals))))

(define first-frame
  (lambda (env)
    (cadr env)))

(define first-frame-vars
  (lambda (env)
    (cadr (first-frame env))))

(define initial-contours
  (lambda (env)
    (cdr (first-frame env))))

(define frames
  (lambda (env)
    (cdr env)))

(define add-binding
  (lambda (new-var new-binding frame)
    (let ((bindings (vector->list (car frame)))
	  (vars (cadr frame)))
      ;; must add new binding to the end of the frame, not the front, to
      ;; preserve correct lexical addresses of other variables in frame
      (list (list->vector (append bindings (list new-binding)))
	    (append vars (list new-var))))))

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

(define-native search-frame
  (lambda (frame var)
    (search-for-binding var (car frame) (cadr frame) 0)))

(define-native search-for-binding
  (lambda (var bindings variables i)
    (cond
     ((null? variables) #f)
     ((eq? (car variables) var) (vector-ref bindings i))
     (else (search-for-binding var bindings (cdr variables) (+ i 1))))))

;; for macro environments
(define in-first-frame?
  (lambda (var env)
    (true? (memq var (first-frame-vars env)))))

;; for macro environments
(define get-first-frame-value
  (lambda (var env)
    (binding-value (search-frame (first-frame env) var))))

(define* lookup-value-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (k (binding-value (vector-ref bindings offset)) fail))))

(define* lookup-binding-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (k (vector-ref bindings offset) fail))))

(define* lookup-value
  (lambda (var env var-info handler fail k)
    (lookup-variable var env var-info handler fail
      (lambda-cont2 (var fail)
	(k (dlr-env-lookup var) fail))
      (lambda-cont3 (dlr-obj components fail)
	(k (get-external-member dlr-obj components) fail))
      (lambda-cont2 (binding fail)
	(k (binding-value binding) fail)))))

(define* lookup-variable
  (lambda (var env var-info handler fail gk dk sk) ;; gk: global dlr, dk: dlr obj, sk: scheme module
    ;;(printf "in lookup-variable var: ~s env: ~s\n" var env)
    (let ((binding (search-env env var)))  ;; look in scheme env for variable as a whole
      (if binding
	(sk binding fail)  ;; found it in the scheme env
	(let ((components (split-variable var)))  ;; split into components, if any
	  (cond
	    ((and (null? (cdr components)) (dlr-env-contains (car components)))
	     (gk (car components) fail))  ;; (set! Myro 42)
	    ((and (not (null? (cdr components)))  ;; (set! Myro.robot 42)
		  (dlr-env-contains (car components))
		  (dlr-object-contains (dlr-env-lookup (car components)) components))
	     (dk (dlr-env-lookup (car components)) components fail))
	    ((null? (cdr components))  ;; (set! a.b..c val)
	     (runtime-error (format "unbound variable '~a'" var) var-info handler fail))
	    (else (lookup-variable-components components "" env var-info handler fail dk sk))))))))

;; math.x.y.z where math is a module or a DLR module/item
;; components: '(math x y z) "" ...
;; components: '(x y z) "math" ...
;; components: '(y z) "math.x" ...
;; components: '(z) "math.x.y" ...
(define* lookup-variable-components
  (lambda (components path module var-info handler fail dk sk)
    ;;(printf "in lookup-variable-components. components: ~s path: ~s\n" components path)
    (let* ((var (car components))
	   (binding (search-env module var)))
      (cond
	(binding  ;; (set! c val)
	  (if (null? (cdr components))
	    (sk binding fail)
	    (let ((value (binding-value binding))
		  (new-path (if (string=? path "") (format "~a" var) (format "~a.~a" path var))))
	      (cond
	        ((environment? value)
		 (lookup-variable-components (cdr components) new-path value var-info handler fail dk sk))
		((dlr-object-contains value components)
		 (dk value components fail))
		(else (runtime-error (format "'~a' is not a module" new-path) var-info handler fail))))))
	((string=? path "") (runtime-error (format "unbound module '~a'" var) var-info handler fail))
	(else (runtime-error (format "unbound variable '~a' in module '~a'" var path) var-info handler fail))))))

;; adds a new binding for var to the first frame if one doesn't exist
(define* lookup-binding-in-first-frame
  (lambda (var env handler fail k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding fail)
	  (let ((new-binding (make-binding 'undefined)))
	    (let ((new-frame (add-binding var new-binding frame)))
	      (set-first-frame! env new-frame)
	      (k new-binding fail))))))))

;; (split-variable 'a) => (a)
;; (split-variable 'a.b.c.d) => (a b c d)
;; (split-variable 'a.b..c) => ()
(define split-variable
  (lambda (var)
    (let ((strings (string-split (symbol->string var) #\.)))
      (if (member "" strings)
	'()
	(map string->symbol strings)))))

(define string-split
  (lambda (s delimiter-char)
    (letrec
      ((position
	(lambda (chars)
	  (if (char=? (car chars) delimiter-char)
	      0
	      (+ 1 (position (cdr chars))))))
       (split
	 (lambda (chars)
	   (cond
	     ((null? chars) '())
	     ((not (member delimiter-char chars)) (list (apply string chars)))
	     (else (let ((n (position chars)))
		     (cons (apply string (list-head chars n))
			   (split (cdr (list-tail chars n))))))))))
      (split (string->list s)))))

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

(define *use-lexical-address* #t)

;; for the macro environment
(load "environments-cps.ss")

;;--------------------------------------------------------------------------
;; these definitions enable parser-cps.ss to be run directly in Petite
;; Chez Scheme, because parser-cps.ss uses environments-cps.ss, which
;; relies on these functions being defined.

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

;;--------------------------------------------------------------------------
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
;;         | (choose <exp> ...)

;;--------------------------------------------------------------------------

(define-datatype aexpression aexpression?
  (lit-aexp
    (datum anything?)
    (info source-info?))
  (var-aexp
    (id symbol?)
    (info source-info?))
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
  (choose-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  )

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

(define anything?
  (lambda (datum) #t))

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
    '(quote func define! quasiquote lambda if set! define begin cond and or
	    let let* letrec case record-case try catch finally raise
	    define-syntax choose define-datatype cases trace-lambda)))

(define mit-style-define?^
  (lambda (asexp)
    (not (symbol?^ (cadr^ asexp)))))

;; used in aunparse
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
;;	 (true? (search-env macro-env (untag-atom^ (car^ asexp)))))))

(define-native tagged-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (car^ asexp) keyword)))))

(define quote?^ (tagged-list^ 'quote = 2))
(define quasiquote?^ (tagged-list^ 'quasiquote = 2))
(define unquote?^ (tagged-list^ 'unquote >= 2))  ;; >= for alan bawden's qq-expand algorithm
(define unquote-splicing?^ (tagged-list^ 'unquote-splicing >= 2))
(define if-then?^ (tagged-list^ 'if = 3))
(define if-else?^ (tagged-list^ 'if = 4))
(define assignment?^ (tagged-list^ 'set! = 3))
(define func?^ (tagged-list^ 'func = 2))
(define define?^ (tagged-list^ 'define >= 3))
(define define!?^ (tagged-list^ 'define! >= 3))
(define define-syntax?^ (tagged-list^ 'define-syntax >= 3))
(define define-var^ (lambda (x) (untag-atom^ (cadr^ x))))
(define define-docstring^ (lambda (x) (untag-atom^ (caddr^ x))))
(define begin?^ (tagged-list^ 'begin >= 2))
(define lambda?^ (tagged-list^ 'lambda >= 3))
(define trace-lambda?^ (tagged-list^ 'trace-lambda >= 4))
(define raise?^ (tagged-list^ 'raise = 2))
(define choose?^ (tagged-list^ 'choose >= 1))
(define try?^ (tagged-list^ 'try >= 2))
(define try-body^ (lambda (x) (cadr^ x)))
(define catch?^ (tagged-list^ 'catch >= 3))
(define catch-var^ (lambda (x) (untag-atom^ (cadr^ (caddr^ x)))))
(define catch-exps^ (lambda (x) (cddr^ (caddr^ x))))
(define finally?^ (tagged-list^ 'finally >= 2))
(define try-finally-exps^ (lambda (x) (cdr^ (caddr^ x))))
(define try-catch-finally-exps^ (lambda (x) (cdr^ (cadddr^ x))))

(define* aparse
  (lambda (adatum senv handler fail k)   ;; k receives 2 args: aexp, fail
    (let ((info (get-source-info adatum)))
      (cond
	((literal?^ adatum) (k (lit-aexp (untag-atom^ adatum) info) fail))
	((symbol?^ adatum)
	 (if *use-lexical-address*
	     (get-lexical-address (untag-atom^ adatum) senv 0 info fail k)
	     (k (var-aexp (untag-atom^ adatum) info) fail)))
	((vector?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp v info) fail))))
	((quote?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp (cadr v) info) fail))))
	((quasiquote?^ adatum)
	 (qq-expand-cps (cadr^ adatum) 0
	   (lambda-cont (v)
	     (annotate-cps v 'none
	       (lambda-cont (expansion)
		 (if (original-source-info? adatum)
		   (aparse (replace-info expansion (snoc 'quasiquote info)) senv handler fail k)
		   (aparse (replace-info expansion info) senv handler fail k)))))))
	((unquote?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((unquote-splicing?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((syntactic-sugar?^ adatum)
	 (expand-once^ adatum handler fail
	   (lambda-cont2 (expansion fail)
	     (aparse expansion senv handler fail k))))
	((if-then?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (if-aexp v1 v2 (lit-aexp #f 'none) info) fail))))))
	((if-else?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (aparse (cadddr^ adatum) senv handler fail
		   (lambda-cont2 (v3 fail)
		     (k (if-aexp v1 v2 v3 info) fail))))))))
	((assignment?^ adatum)
	 (aparse (caddr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (assign-aexp (untag-atom^ (cadr^ adatum)) v var-info info) fail)))))
	((func?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (e fail)
	     (k (func-aexp e info) fail))))
	((define?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define!?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define! <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define! <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define-syntax?^ adatum)
	 (let ((name (define-var^ adatum))
	       (aclauses (cddr^ adatum)))
	   (unannotate-cps aclauses
	     (lambda-cont (clauses)
	       (k (define-syntax-aexp name clauses aclauses info) fail)))))
	((begin?^ adatum)
	 (cond
	   ((null?^ (cdr^ adatum)) (aparse-error "bad concrete syntax:" adatum handler fail))
	   ((null?^ (cddr^ adatum)) (aparse (cadr^ adatum) senv handler fail k))
	   (else (aparse-all (cdr^ adatum) senv handler fail
		   (lambda-cont2 (exps fail)
		     (k (begin-aexp exps info) fail))))))
	((lambda?^ adatum)
	 (unannotate-cps (cadr^ adatum)
	    (lambda-cont (formals)
	        (let ((formals-list
			(if (list? formals)
			    formals
			    (cons (last formals) (head formals)))))
		  (aparse-all (cddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (list? formals)
			     (k (lambda-aexp formals bodies info) fail)
			     (k (mu-lambda-aexp (head formals) (last formals) bodies info) fail))))))))
	((trace-lambda?^ adatum)
	 (unannotate-cps (caddr^ adatum)
	      (lambda-cont (formals)
		(let ((formals-list
			(if (list? formals)
			    formals
			    (cons (last formals) (head formals))))
		      (name (untag-atom^ (cadr^ adatum))))
		  (aparse-all (cdddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (list? formals)
			     (k (trace-lambda-aexp name formals bodies info) fail)
			     (k (mu-trace-lambda-aexp name (head formals) (last formals) bodies info) fail))))))))
	((try?^ adatum)
	 (cond
	  ;; (try <body>)
	   ((= (length^ adatum) 2)
	    (aparse (try-body^ adatum) senv handler fail k))
	   ;; (try <body> (catch <var> <exp> ...))
	   ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (catch-exps^ adatum) senv handler fail
		  (lambda-cont2 (cexps fail)
		    (let ((cvar (catch-var^ adatum)))
		      (k (try-catch-aexp body cvar cexps info) fail)))))))
	   ;; (try <body> (finally <exp> ...))
	   ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (try-finally-exps^ adatum) senv handler fail
		  (lambda-cont2 (fexps fail)
		    (k (try-finally-aexp body fexps info) fail))))))
	   ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	   ((and (= (length^ adatum) 4) (catch?^ (caddr^ adatum)) (finally?^ (cadddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (catch-exps^ adatum) senv handler fail
		  (lambda-cont2 (cexps fail)
		    (aparse-all (try-catch-finally-exps^ adatum) senv handler fail
		      (lambda-cont2 (fexps fail)
			(let ((cvar (catch-var^ adatum)))
			  (k (try-catch-finally-aexp body cvar cexps fexps info) fail)))))))))
	   (else (aparse-error "bad try syntax:" adatum handler fail))))
	((raise?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (k (raise-aexp v info) fail))))
	((choose?^ adatum)
	 (aparse-all (cdr^ adatum) senv handler fail
	   (lambda-cont2 (exps fail)
	     (k (choose-aexp exps info) fail))))
	((application?^ adatum)
	 (aparse (car^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse-all (cdr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (app-aexp v1 v2 info) fail))))))
	(else (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define* aparse-all
  (lambda (adatum-list senv handler fail k)
    (if (null?^ adatum-list)
      (k '() fail)
      (aparse (car^ adatum-list) senv handler fail
	(lambda-cont2 (a fail)
	  (aparse-all (cdr^ adatum-list) senv handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps adatum
	(lambda-cont (datum)
	  (handler (make-exception "ParseError" (format "~s ~a" msg datum)
			 (get-srcfile info)
			 (get-start-line info) 
			 (get-start-char info))
		   fail))))))

;; used once in interpreter-cps.ss
(define* aparse-sexps
  (lambda (tokens src senv handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (adatum end tokens-left fail)
	  (aparse adatum senv handler fail
	    (lambda-cont2 (v1 fail)
	      (aparse-sexps tokens-left src senv handler fail
		(lambda-cont2 (v2 fail)
		  (k (cons v1 v2) fail))))))))))

(define* get-lexical-address
  (lambda (id senv depth info fail k)
    (cond
      ((null? senv) (k (var-aexp id info) fail))   ;; free!
      ((memq id (car senv))
       (get-lexical-address-offset id (car senv) depth 0 info fail k))
      (else (get-lexical-address id (cdr senv) (+ depth 1) info fail k)))))

(define* get-lexical-address-offset
  (lambda (id contours depth offset info fail k)
    (if (eq? (car contours) id)
      (k (lexical-address-aexp depth offset id info) fail)
      (get-lexical-address-offset id (cdr contours) depth (+ offset 1) info fail k))))

;;(define get-lexical-address
;;  ;; given an environment and variable id, return the depth of the
;;  ;; frame and offset OR return it as a var-exp signifying it as an
;;  ;; unbound variable
;;  (lambda (senv id info)
;;    (get-lexical-address-frames (frames senv) id 0 0 info)))

;;(define get-lexical-address-frames
;;  ;; given a list of frames, get the lexical address of variable
;;  (lambda (frames variable depth offset info)
;;    (cond 
;;     ((null? frames) (var-aexp variable info)) ;; free!
;;     (else (let ((result (get-lexical-address-frame (car frames) variable depth offset info)))
;;	     (if (not (car result))
;;		 (get-lexical-address-frames (cdr frames) variable (+ 1 depth) 0 info)
;;		 (cadr result)))))))

;;(define get-lexical-address-frame
;;  ;; returns (#t pos) or (#f) signifying bound or free, respectively
;;  (lambda (frame variable depth offset info)
;;    (cond
;;     ((empty-frame? frame) (list #f)) ;; not in this frame
;;     ((>= offset (vector-length frame)) (list #f)) ;; not here
;;     ((eq? (binding-variable (vector-ref frame offset)) variable)
;;      (list #t (lexical-address-aexp depth offset variable info)))
;;     (else (get-lexical-address-frame frame variable depth (+ 1 offset) info)))))

;;--------------------------------------------------------------------------
;; Macro support

;; transformer macros:

(define let-transformer^
  (lambda-macro (adatum handler fail k)
    (if (symbol?^ (cadr^ adatum))
      ;; named let
      (let* ((name (cadr^ adatum))
	     (bindings (caddr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cdddr^ adatum)))
	(k `(letrec ((,name (lambda ,vars ,@(at^ bodies)))) (,name ,@(at^ exps)))))
      ;; ordinary let
      (let* ((bindings (cadr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cddr^ adatum)))
	(k `((lambda ,vars ,@(at^ bodies)) ,@(at^ exps)))))))

(define letrec-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((decls (cadr^ adatum))
	   (vars (map^ car^ decls))
	   (procs (map^ cadr^ decls))
	   (bodies (cddr^ adatum)))
      (create-letrec-assignments^ vars procs
	(lambda-cont2 (bindings assigns)  ;; bindings and assigns are unannotated
	  (k `(let ,bindings ,@assigns ,@(at^ bodies))))))))

(define* create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null?^ vars)
      (k2 '() '())
      (create-letrec-assignments^ (cdr^ vars) (cdr^ procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car^ vars) 'undefined) bindings)
	      (cons `(set! ,(car^ vars) ,(car^ procs)) assigns)))))))

(define mit-define-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((name (caadr^ adatum))
	  (formals (cdadr^ adatum))
	  (bodies (cddr^ adatum)))
      (k `(define ,name (lambda ,formals ,@(at^ bodies)))))))

(define and-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#t))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(if ,(car^ exps) (and ,@(at^ (cdr^ exps))) #f)))))))

;; avoids variable capture
(define or-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#f))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(let ((bool ,(car^ exps))
			(else-code (lambda () (or ,@(at^ (cdr^ exps))))))
		    (if bool bool (else-code)))))))))

(define* amacro-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (handler (make-exception "MacroError" msg (get-start-line info)
		     (get-srcfile info)
		     (get-start-char info))
	       fail))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((clauses (cdr^ adatum)))
      (if (null?^ clauses)
	(amacro-error "empty (cond) expression" adatum handler fail)
	(let ((first-clause (car^ clauses))
	      (other-clauses (cdr^ clauses)))
	  (if (or (null?^ first-clause) (not (list?^ first-clause)))
	    (amacro-error "improper cond clause" first-clause handler fail)
	    (let ((test-exp (car^ first-clause))
		  (then-exps (cdr^ first-clause)))
	      (cond
		((eq?^ test-exp 'else)
		 (cond
		   ((null?^ then-exps)
		    (amacro-error "improper else clause" first-clause handler fail))
		   ((null?^ (cdr^ then-exps)) (k (car^ then-exps)))
		   (else (k `(begin ,@(at^ then-exps))))))
		((null?^ then-exps)
		 (if (null?^ other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@(at^ other-clauses)))))
			 (if bool bool (else-code))))))
		((eq?^ (car^ then-exps) '=>)
		 (cond
		   ((null?^ (cdr^ then-exps))
		    (amacro-error "improper => clause" first-clause handler fail))
		   ((null?^ other-clauses)
		    (k `(let ((bool ,test-exp)
			      (th (lambda () ,(cadr^ then-exps))))
			  (if bool ((th) bool)))))
		   (else (k `(let ((bool ,test-exp)
				   (th (lambda () ,(cadr^ then-exps)))
				   (else-code (lambda () (cond ,@(at^ other-clauses)))))
			       (if bool ((th) bool) (else-code)))))))
		((null?^ other-clauses)
		 (if (null?^ (cdr^ then-exps))
		   (k `(if ,test-exp ,(car^ then-exps)))
		   (k `(if ,test-exp (begin ,@(at^ then-exps))))))
		((null?^ (cdr^ then-exps))
		 (k `(if ,test-exp ,(car^ then-exps) (cond ,@(at^ other-clauses)))))
		(else (k `(if ,test-exp (begin ,@(at^ then-exps)) (cond ,@(at^ other-clauses)))))))))))))

(define let*-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((bindings (cadr^ adatum))
	  (bodies (cddr^ adatum)))
      (nest-let*-bindings^ bindings bodies k))))

(define* nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null?^ bindings)
	    (null?^ (cdr^ bindings)))
	(k `(let ,bindings ,@(at^ bodies)))
	(nest-let*-bindings^ (cdr^ bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car^ bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (case-clauses->cond-clauses^ 'r clauses
	  (lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null?^ clauses)
      (k '())
      (case-clauses->simple-cond-clauses^ var (cdr^ clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car^ clauses)))
	    (cond
	      ((eq?^ (car^ clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol?^ (car^ clause))
	       (k (cons `((eq? ,var ',(car^ clause)) ,@(at^ (cdr^ clause))) new-clauses)))
	      (else (k (cons `((memq ,var ',(car^ clause)) ,@(at^ (cdr^ clause)))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((eq? ,var ',(car^ clause)) (,name)) new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((memq ,var ',(car^ clause)) (,name)) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((eq? (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((memq (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

;;----------------------------------------------------------------------------

(define define-datatype-transformer^
  (lambda-macro (datatype-def handler fail k)
    (let* ((datatype-name (cadr^ datatype-def))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ datatype-name) "?"))))
      (if (not (eq?^ (caddr^ datatype-def) type-tester-name))
	(amacro-error
	  (format "datatype tester predicate not named ~a" type-tester-name)
	  (caddr^ datatype-def) handler fail)
	(let ((variants (cdddr^ datatype-def)))
	  (make-dd-variant-constructors^ variants
	    (lambda-cont2 (variant-names constructor-defs)
	      (let ((tester-def
		      `(define ,type-tester-name
			 (lambda (x)
			   (and (pair? x) (not (not (memq (car x) ',variant-names))))))))
		(k `(begin ,tester-def ,@constructor-defs))))))))))

(define* make-dd-variant-constructors^
  (lambda (variants k2)
    (if (null?^ variants)
      (k2 '() '())
      (make-dd-variant-constructor^ (car^ variants)
	(lambda-cont2 (name def)
	  (make-dd-variant-constructors^ (cdr^ variants)
	    (lambda-cont2 (names defs)
	      (k2 (cons name names) (cons def defs)))))))))

(define* make-dd-variant-constructor^
  (lambda (variant k2)
    (let ((name (car^ variant))
	  (fields (cdr^ variant)))
      (verify-dd-constructor-fields^ name fields 'args
	(lambda-cont (verify-code)
	  (let ((constructor-def
		  `(define ,name
		     (lambda args
		       (if (= (length args) ,(length^ fields))
			   ,verify-code
			   (error ',name "wrong number of arguments"))))))
	    (k2 name constructor-def)))))))

(define* verify-dd-constructor-fields^
  (lambda (name fields cdrs k)
    (if (null?^ fields)
	(k `(cons ',name args))
	(verify-dd-constructor-fields^ name (cdr^ fields) `(cdr ,cdrs)
	  (lambda-cont (verify-code)
	    (k `(if (,(cadar^ fields) (car ,cdrs))
		    ,verify-code
		    (error ',name "~a is not of type ~a" (car ,cdrs) ',(cadar^ fields)))))))))

(define cases-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((type-name (cadr^ adatum))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ type-name) "?")))
	   (exp (caddr^ adatum))
	   (clauses (cdddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings)
		(if (not (,type-tester-name r))
		  (error 'cases "~a is not a valid ~a" r ',type-name)
		  (cond ,@new-clauses)))))))))

;;----------------------------------------------------------------------------
;; temporary

(define-native dd1
  "(define-datatype thing thing?
     (thing0)
     (thing1
       (f1 thing1-field1?))
     (thing2
       (f1 thing2-field1?)
       (f2 thing2-field2?))
     (thing3
       (f1 thing3-field1?)
       (f2 (list-of thing3-field2?))
       (f3 thing3-field3?)))")

(define-native cases1
  "(cases thing (cons x y)
     (thing0 () b1)
     (thing1 (f1) b1 b2 b3)
     (thing2 (f1 f2 . f3) b1 b2 b3)
     (thing3 args b1 b2 b3)
     (else d1 d2 d3))")

(define-native dd2
  "(define-datatype expression expression?
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
       (operands (list-of expression?))))")

(define-native cases2
  "(cases expression exp
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
      (else (error 'm \"bad abstract syntax: ~s\" exp)))")

;;----------------------------------------------------------------------------

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'define-datatype 'cases)
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

;; pattern macros:

;; used in interpreter-cps.ss
(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define pattern-macro?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'pattern-macro))))

(define macro-clauses
  (lambda (macro)
    (cadr macro)))

(define macro-aclauses
  (lambda (macro)
    (caddr macro)))

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

(define* expand-once^
  (lambda (adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (let ((macro-keyword (untag-atom^ (car^ adatum))))
      (let ((macro (get-first-frame-value macro-keyword macro-env)))
	(if (pattern-macro? macro)
	    (process-macro-clauses^
	      (macro-clauses macro) (macro-aclauses macro) adatum handler fail k)
	    ;; macro transformer functions take 1-arg continuations:
	    (macro adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (if (has-source-info? expansion)
		      (k expansion fail)
		      (let ((info (get-source-info adatum)))
			(if (original-source-info? adatum)
			  (k (replace-info expansion (snoc macro-keyword info)) fail)
			  (k (replace-info expansion info) fail)))))))))))))

(define* process-macro-clauses^
  (lambda (clauses aclauses adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (if (null? clauses)
      (aparse-error "no matching clause found for" adatum handler fail)
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses))
	    (left-apattern (caar^ aclauses))
	    (right-apattern (cadar^ aclauses)))
	(unannotate-cps adatum
	  (lambda-cont (datum)
	    (unify-patterns^ left-pattern datum left-apattern adatum
	      (lambda-cont (subst)
		(if subst
		  (instantiate^ right-pattern subst right-apattern (lambda-cont2 (v av) (k av fail)))
		  (process-macro-clauses^ (cdr clauses) (cdr^ aclauses) adatum handler fail k))))))))))

;;--------------------------------------------------------------------------------------------
;; quasiquote expansion
;;
;; based on Appendix B of Alan Bawden's paper "Quasiquotation in Lisp", with some optimizations
;;
;; this version matches the functionality of Petite's quasiquote expander

;; for testing only
(define qqtest
  (lambda (s)
    (let ((adatum (aread-string s)))
      (if (not (and (list?^ adatum) (= (length^ adatum) 2) (eq?^ (car^ adatum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset))
	(qq-expand-cps (cadr^ adatum) 0
	  (lambda-cont (v)
	    (annotate-cps v 'none
	      (lambda-cont (expansion)
		(replace-info expansion (snoc 'quasiquote (get-source-info adatum)))))))))))

;; expands annotated code
(define* qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car^ ax) ,v)))))
	 ((and (unquote?^ ax) (not (null?^ (cdr^ ax))) (null?^ (cddr^ ax))) (k (cadr^ ax)))
	 (else (k `(quote ,ax))))) ;; illegal
      ((vector?^ ax)
       (annotate-cps (vector->list^ ax) 'none
	 (lambda-cont (v)
	   (qq-expand-cps v depth
	     (lambda-cont (v2)
	       (k `(list->vector ,v2)))))))
      ((not (pair?^ ax)) (k `',ax))
      ((null?^ (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands annotated code
(define* qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car^ ax) ,v))))))
	 ((unquote?^ ax) (k `(list . ,(cdr^ ax))))
	 ((null?^ (cddr^ ax)) (k (cadr^ ax)))
	 (else (k `(append . ,(cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair?^ ax)) (k `'(,ax)))
      ((null?^ (cdr^ ax))
       (qq-expand-list-cps (car^ ax) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
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
;; for manual testing only in scheme

(define aunparse
  (lambda (aexp)
    (cases aexpression aexp
      (lit-aexp (datum info)
	(cond
	  ((literal? datum) datum)
	  ((vector? datum) datum)
	  (else `(quote ,datum))))
      (var-aexp (id info) id)
      (lexical-address-aexp (depth offset id info) id)
      (if-aexp (test-aexp then-aexp else-aexp info)
	`(if ,(aunparse test-aexp) ,(aunparse then-aexp) ,(aunparse else-aexp)))
      (assign-aexp (var rhs-exp var-info info)
	`(set! ,var ,(aunparse rhs-exp)))
      (func-aexp (exp info)
	`(func ,(aunparse exp)))
      (define-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define ,id ,(aunparse rhs-exp))
	  `(define ,id ,docstring ,(aunparse rhs-exp))))
      (define!-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define! ,id ,(aunparse rhs-exp))
	  `(define! ,id ,docstring ,(aunparse rhs-exp))))
      (define-syntax-aexp (name clauses aclauses info)
	`(define-syntax ,name ,@clauses))
      (begin-aexp (exps info)
	`(begin ,@(map aunparse exps)))
      (lambda-aexp (formals bodies info)
	`(lambda ,formals ,@(map aunparse bodies)))
      (mu-lambda-aexp (formals runt bodies info)
	`(lambda (,@formals . ,runt) ,@(map aunparse bodies)))
      (app-aexp (operator operands info)
	`(,(aunparse operator) ,@(map aunparse operands)))
      (try-catch-aexp (body catch-var catch-exps info)
	`(try ,(aunparse body) (catch ,catch-var ,@(map aunparse catch-exps))))
      (try-finally-aexp (body finally-exps info)
	`(try ,(aunparse body) (finally ,@(map aunparse finally-exps))))
      (try-catch-finally-aexp (body catch-var catch-exps finally-exps info)
	`(try ,(aunparse body)
	      (catch ,catch-var ,@(map aunparse catch-exps))
	      (finally ,@(map aunparse finally-exps))))
      (raise-aexp (exp info)
	`(raise ,(aunparse exp)))
      (choose-aexp (exps info)
	`(choose ,@(map aunparse exps)))
      (else (error 'aunparse "bad abstract syntax: ~s" aexp)))))

(define expand-macro
  (lambda (transformer sexp)
    (transformer sexp init-handler2 init-fail init-cont)))

;; will be overridden by toplevel-env definition in interpreter-cps.ss
;;(define toplevel-env (make-empty-environment))

(define aparse-string
  (lambda (string)
    (aread-datum string 'stdin init-handler2 init-fail
      (lambda-cont3 (adatum tokens-left fail)
	(aparse adatum (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

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
	(aparse-sexps tokens filename (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

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
;; these definitions enable the interpreter code to run in Petite Chez
;; Scheme, independently of C#

;; dummy versions of functions defined in C# code
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
      ;; execute gets redefined as execute-rm when no-csharp-support.ss is loaded
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
    (read-sexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum (initial-contours toplevel-env) REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;; not used
(define initialize-globals
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! load-stack '())
    (initialize-execute)
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
	(aparse-sexps tokens 'stdin (initial-contours toplevel-env) try-parse-handler fail
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

(define *tracing-on?* #f)

(define make-debugging-k
  (lambda (exp k)
    (if (not *tracing-on?*)
	k
	(lambda-cont2 (v fail)
	  (handle-debug-info exp v)
	  (k v fail)))))

(define-native highlight-expression
  (lambda (exp)
    ;; call: (function 1 2 3) 
    ;;          ["filename.ss" at line 13 column 4]
    (printf "call: ~s~%" (aunparse exp))
    (let ((info (rac exp)))
      (if (not (eq? info 'none))
	  (printf "['~a' at line ~a column ~a]~%"
		  (get-srcfile info)
		  (get-start-line info)
		  (get-start-char info))))))

(define-native handle-debug-info
  (lambda (exp result)
    (printf "~s evaluates to ~a~%" (aunparse exp) result)))

(define *stack-trace* '(()))

(define *stack-trace-length* 0)

(define *use-stack-trace* #t)

(define get-use-stack-trace
  (lambda ()
    *use-stack-trace*))

(define set-use-stack-trace
  (lambda (value)
    (set! *use-stack-trace* value)))

(define initialize-stack-trace
  (lambda ()
    (set-car! *stack-trace* '())
    (set! *stack-trace-length* 0)))

(define-native initialize-execute
  (lambda () 'Ok))

(define push-stack-trace
  (lambda (exp)
    ;; Maximum stack strace size
    ;; FIXME: ideally it would chop out middle
    ;; should use a C# data structure
    (if (< *stack-trace-length* 500)
	(begin
	  (set-car! *stack-trace* (cons exp (car *stack-trace*)))
	  (set! *stack-trace-length* (+ *stack-trace-length* 1)))
	(begin
	  (set! *stack-trace* '(()))
	  (set! *stack-trace-length* 0)))))

(define pop-stack-trace
  (lambda (exp)
    ;;(printf "~a: ~a\n" 'pop exp)
    (if (not (null? (car *stack-trace*)))
	(begin
	  (set! *stack-trace-length* (- *stack-trace-length* 1))
	  (set-car! *stack-trace* (cdr (car *stack-trace*)))))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
   (if *tracing-on?* (highlight-expression exp))
   (let ((k (if *tracing-on?* (make-debugging-k exp k) k)))
    (cases aexpression exp
      (lit-aexp (datum info) (k datum fail))
      (var-aexp (id info)
	(lookup-value id env info handler fail k))
      (lexical-address-aexp (depth offset id info)
	(lookup-value-by-lexical-address depth offset (frames env) fail k))
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
	    (lookup-variable var env var-info handler fail
	      (lambda-cont2 (var fail)
		(let ((old-value (dlr-env-lookup var)))
		  ;; need to undo the assignment if we back up
		  (set-global-value! var rhs-value)
		  (let ((new-fail (lambda-fail () (set-global-value! var old-value) (fail))))
		    (k void-value new-fail))))
	      (lambda-cont3 (dlr-obj components fail) ;; dlr-obj is Myro, components is (Myro robot)
		(let ((old-value (get-external-member dlr-obj components)))
		  (set-external-member! dlr-obj components rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-external-member! dlr-obj components old-value) (fail))))
		    (k void-value new-fail))))
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
	(k (mu-trace-closure name formals runt bodies env) fail))
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
      (choose-aexp (exps info)
	(eval-choices exps env handler fail k))
      (app-aexp (operator operands info)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(if *use-stack-trace* (push-stack-trace exp))
		(cond
		  ((dlr-proc? proc) 
		   (let ((result (dlr-apply proc args)))
		     (if *use-stack-trace* (pop-stack-trace exp))
		     (k result fail)))
		  ((procedure-object? proc) 
		   (if *use-stack-trace*
		       (proc args env info handler fail 
			  (lambda-cont2 (v2 fail)
			     (pop-stack-trace exp)
			     (k v2 fail)))
		       (proc args env info handler fail k)))
		  (else (runtime-error (format "attempt to apply non-procedure '~a'" proc)
				       info handler fail))))))))
      (else (error 'm "bad abstract syntax: '~s'" exp))))))

(define make-exception
  (lambda (exception message source line column)
    (list exception message source line column (make-stack-trace))))

(define make-stack-trace
  (lambda ()
    (let ((trace (car *stack-trace*)))
      (reverse (map format-stack-trace trace)))))

(define get-procedure-name
  (lambda (exp)
    (cases aexpression exp
      (lexical-address-aexp (depth offset id info) id)
      (var-aexp (id info) id)
      (app-aexp (operator operands info)
	  (get-procedure-name operator))
      (else 'unknown))))

(define format-stack-trace
  (lambda (exp)
    (let ((info (rac exp)))
      (list (get-srcfile info)
	    (get-start-line info)
	    (get-start-char info)
	    (get-procedure-name exp)))))

(define* runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (make-exception "RunTimeError" msg 'none 'none 'none) fail)
      (let ((src (get-srcfile info))
	    (line (get-start-line info))
	    (char (get-start-char info)))
	(handler (make-exception "RunTimeError" msg src line char) fail)))))

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
    (if (null? (cdr exps))
      (m (car exps) env handler fail k)
      (m (car exps) env handler fail
	(lambda-cont2 (result fail)
	  (eval-sequence (cdr exps) env handler fail k))))))

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

(define make-trace-depth-string
  (lambda (level)
    (if (= level 0)
      ""
      (string-append " |" (make-trace-depth-string (- level 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
	(if (= (length args) (length formals))
	  (begin
	    (printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name args))
	    ;;(printf "k: ~a\n" (make-safe-continuation k2))
	    (set! trace-depth (+ trace-depth 1))
	    (eval-sequence bodies (extend env formals args) handler fail 
	      (lambda-cont2 (v fail)
		(set! trace-depth (- trace-depth 1))
		(printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		(k2 v fail))))
	  (runtime-error "incorrect number of arguments in application" info handler fail))))))

;; experimental
(define-native make-safe-continuation
  (lambda (k)
    (cond
      ((not (pair? k)) '<???>)
      ((eq? (car k) 'fail-continuation) '<fail>)
      ((memq (car k) '(handler handler2)) '<handler>)
      ((memq (car k) '(continuation continuation2 continuation3 continuation4))
       (cons (cadr k) (map make-safe-continuation (filter continuation-object? (cddr k)))))
      (else '<???>))))

;; experimental
(define continuation-object?
  (lambda (x)
    (and (pair? x) (memq (car x) '(continuation continuation2 continuation3 continuation4)))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
	(if (>= (length args) (length formals))
	  (let ((new-env
		  (extend env
		    (cons runt formals)
		    (cons (list-tail args (length formals))
			  (list-head args (length formals))))))
	    (printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name args))
	    (set! trace-depth (+ trace-depth 1))
	    (eval-sequence bodies new-env handler fail
	      (lambda-cont2 (v fail)
		(set! trace-depth (- trace-depth 1))
		(printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		(k2 v fail))))
	  (runtime-error "not enough arguments in application" info handler fail))))))

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

(define all-char?
  (lambda (ls)
    (or (null? ls)
	(and (char? (car ls))
	     (all-char? (cdr ls))))))

;; void
(define void-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 void-value fail)))

(define void-value '<void>)

(define void?
  (lambda (x) (eq? x void-value)))

;; zero?
(define zero?-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (= (car args) 0) fail)))

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
    (cond
      ((length-one? args)  ;; petite uses toplevel env
       (annotate-cps (car args) 'none
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours toplevel-env) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp toplevel-env handler fail k2))))))
      ((length-two? args)
       (annotate-cps (car args) 'none
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours (cadr args)) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp (cadr args) handler fail k2))))))
      (else (runtime-error "incorrect number of arguments to eval" info handler fail)))))
	       
;; eval-ast
(define eval-ast-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to eval-ast" info handler fail))
      ((not (list? (car args)))  ;; is there a better test for exp?  aexpression?
       (runtime-error "eval-ast called on non-abstract syntax tree argument" info handler fail))
      (else (m (car args) toplevel-env handler fail k2)))))  ;; petite uses toplevel env

;; parse
(define parse-prim
  (lambda-proc (args env2 info handler fail k2)
    (annotate-cps (car args) 'none
      (lambda-cont (adatum)
        (aparse adatum (initial-contours toplevel-env) handler fail k2)))))  ;; was env2

;; string-length
(define string-length-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string-length" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-length called on non-string argument" info handler fail))
      (else (k2 (apply string-length args) fail)))))

;; string-ref
(define string-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to string-ref" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-ref called with non-string first argument" info handler fail))
      ((not (number? (cadr args)))
       (runtime-error "string-ref called with non-numberic second argument" info handler fail))
      (else (k2 (apply string-ref args) fail)))))

;; unparse
(define unparse-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car args)) fail)))   ;; aunparse should be in CPS

;; unparse-procedure
(define unparse-procedure-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car (caddr (car args)))) fail)))  ;; aunparse should be in CPS

;; parse-string
(define parse-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens 'stdin handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (aparse adatum (initial-contours toplevel-env) handler fail k2)  ;; was env2
	      (read-error "tokens left over" tokens-left 'stdin handler fail))))))))

;; read-string
(define read-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) 'stdin handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens 'stdin handler fail
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

;; odd?
(define odd?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to odd?" info handler fail))
      (else (k2 (odd? (car args)) fail)))))

;; even?
(define even?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to even?" info handler fail))
      (else (k2 (even? (car args)) fail)))))

;; quotient
(define quotient-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to quotient" info handler fail))
      (else (k2 (apply quotient args) fail)))))

;; remainder
(define remainder-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to remainder" info handler fail))
      (else (k2 (apply remainder args) fail)))))

;; print
(define print-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each safe-print args)
    (k2 void-value fail)))

;; string
(define string-prim 
  ;; turns a list of char into a string
  (lambda-proc (args env2 info handler fail k2)
     ;;(k2 (apply string-append (map (lambda (c) (format "~s" c)) args)) fail)))
     (k2 (apply char->string args) fail)))

;; substring
(define substring-prim 
  ;; (substring "string" start stop)
  (lambda-proc (args env2 info handler fail k2)
     (k2 (substring (car args) (cadr args) (caddr args)) fail))) 

;; number->string
(define number->string-prim 
  ;; given a number, returns those digits as a string
  (lambda-proc (args env2 info handler fail k2)
     (k2 (number->string (car args)) fail)))

;; assv
(define assv-prim 
  ;; given 'a '((b 1) (a 2)) returns (a 2)
  (lambda-proc (args env2 info handler fail k2)
     (k2 (assv (car args) (cadr args)) fail)))
;; memv
(define memv-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (memv (car args) (cadr args)) fail)))

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
       (load-file (car args) toplevel-env info handler fail k2))))  ;; petite uses toplevel env

(define load-stack '())

(define* load-file
  (lambda (filename env2 info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (runtime-error (format "filename '~a' is not a string" filename) info handler fail))
      ((not (file-exists? filename))
       (runtime-error (format "attempted to load nonexistent file '~a'" filename) info handler fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-asexps tokens filename env2 handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* read-and-eval-asexps
  (lambda (tokens src env2 handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (datum end tokens-left fail)
	  (aparse datum (initial-contours env2) handler fail  ;; was env2
	    (lambda-cont2 (exp fail)
	      (m exp env2 handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-asexps tokens-left src env2 handler fail k)))))))))))

(define* load-files
  (lambda (filenames env2 info handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (load-file (car filenames) env2 info handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env2 info handler fail k))))))

;; length
(define length-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (length-one? args)
      (length-loop (car args) 0 (car args) info handler fail k2)
      (runtime-error "incorrect number of arguments to length" info handler fail))))

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

;; char?
(define char?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char?" info handler fail))
      (else (k2 (apply char? args) fail)))))

;; char=?
(define char=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to char=?" info handler fail))
      ((or (not (char? (car args))) (not (char? (cadr args))))
       (runtime-error "char=? requires arguments of type char" info handler fail))
      (else (k2 (apply char=? args) fail)))))

;; char-whitespace?
(define char-whitespace?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-whitespace?" info handler fail))
      (else (k2 (apply char-whitespace? args) fail)))))

;; char-alphabetic?
(define char-alphabetic?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-alphabetic?" info handler fail))
      (else (k2 (apply char-alphabetic? args) fail)))))

;; char-numeric?
(define char-numeric?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-numeric?" info handler fail))
      (else (k2 (apply char-numeric? args) fail)))))

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

;; make-set
(define make-set-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to set" info handler fail))
      (else (make-set (car args) env2 info handler fail k2)))))
    
(define* make-set
  (lambda (lst env2 info handler fail k2)
    (if (null? lst)
      (k2 lst fail)
      (make-set (cdr lst) env2 info handler fail
	(lambda-cont2 (v fail)
	  (if (member (car lst) v)
	    (k2 v fail)
	    (k2 (cons (car lst) v) fail)))))))

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

;; modulo
(define modulo-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to %" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "% called on non-numeric argument(s)" info handler fail))
      ((= (cadr args) 0)
       (runtime-error "modulo by zero" info handler fail))
      (else (k2 (apply modulo args) fail)))))

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

(define-native range
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
(define import-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((filename (car args)))
      (if (null? (cdr args))
	(load-file filename env2 'none handler fail k2)
	(let ((module-name (cadr args)))
	  (lookup-binding-in-first-frame module-name env2 handler fail
	    (lambda-cont2 (binding fail)
	      (let ((module (make-toplevel-env)))
		(set-binding-value! binding module)
		(load-file filename module 'none handler fail k2)))))))))

;; get-stack-trace-prim
(define get-stack-trace-prim
  (lambda-proc (args env2 info handler fail k)
    (k (car *stack-trace*) fail)))

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
	     (runtime-error (format "invalid module '~a'" sym) info handler fail))
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
	    (if (dlr-proc? proc)
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
    (append-all args info handler fail k2)))
;;    (cond
;;      ((not (length-two? args))
;;       (runtime-error "incorrect number of arguments to append" info handler fail))
;;      ((not (list? (car args)))
;;       (runtime-error (format "append called on incorrect list structure ~s" (car args)) info handler fail))
;;      (else (append-all args (lambda-cont (v) (k2 v fail)))))))
;;      (else (k2 (apply append args) fail)))))

(define* append2
  (lambda (ls1 ls2 fail k2)
    (if (null? ls1)
      (k2 ls2 fail)
      (append2 (cdr ls1) ls2 fail
	(lambda-cont2 (v fail)
	  (k2 (cons (car ls1) v) fail))))))

(define* append-all
  (lambda (lists info handler fail k2)
    (cond
      ((null? lists) (k2 '() fail))
      ((null? (cdr lists)) (k2 (car lists) fail))
      ((not (list? (car lists)))
       (runtime-error (format "append called on incorrect list structure ~s" (car lists)) info handler fail))
      (else (append-all (cdr lists) info handler fail
	      (lambda-cont2 (ls fail)
		(append2 (car lists) ls fail k2)))))))

;; string->number
(define string->number-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->number" info handler fail))
      (else (k2 (apply string->number args) fail)))))

;; string=?
(define string=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to string=?" info handler fail))
     (else (k2 (apply string=? args) fail)))))

;; list->vector
(define list-to-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->vector" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->vector called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply list->vector args) fail)))))

;; list->string
(define list->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->string" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->string called on incorrect list structure ~s" (car args)) info handler fail))
      ((not (all-char? (car args)))
       (runtime-error (format "list->string called on non-char list ~s" (car args)) info handler fail))
      (else (k2 (apply list->string args) fail)))))

;; dir
(define dir-prim
  (lambda-proc (args env2 info handler fail k2)
    (make-set (dir args env2) env2 info handler fail k2)))

(define dir
  (lambda (args env)
    (if (or (null? args) (environment? (car args)))
	(sort symbol<? (if (null? args)
			   (append (get-variables-from-frames (frames macro-env))
				   (get-variables-from-frames (frames env)))
			   (get-variables-from-frames (frames (car args)))))
	(get-external-members (car args)))))

(define get-variables-from-frame
  (lambda (frame)
    (cadr frame)))

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
      (if (dlr-proc? proc)
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
      (if (dlr-proc? proc)
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
      (if (dlr-proc? proc)
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
	  (if (dlr-proc? proc) 
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-primitive proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env 'none handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-primitive proc (map cdr arg-list) env handler fail k)))))))))

;; env
(define current-environment-prim
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
    (k2 (apply vector_native args) fail)))

(define-native vector_native
  (lambda args
    (apply vector args)))

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
    (cond 
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to 'error' (should be 2)" info handler fail))
      (else
       (let* ((location (format "Error in '~a': " (car args)))
	      (message (string-append location (apply format (cdr args)))))
	 (runtime-error message info handler fail))))))

;; list-ref
(define list-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to list-ref" info handler fail))
      (else (k2 (apply list-ref args) fail)))))

;; current-directory
(define current-directory-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args) (k2 (current-directory) fail))
      ((length-one? args)
       (if (string? (car args))
	   (k2 (current-directory (car args)) fail)
	   (runtime-error "directory must be a string" info handler fail)))
      (else (runtime-error "incorrect number of arguments to current-directory" info handler fail)))))

;; Add new procedures above here!
;; Then, add NAME to env
;; Then, add NAME_proc to Scheme.cs (if you use map or apply on it internally)

;; this is here as a hook for extending environments in C# etc.
(define make-initial-env-extended
  (lambda (env) env))

(define make-toplevel-env
  (lambda ()
    (let ((primitives 
	   (list
	    (list '* times-prim)
	    (list '+ plus-prim)
	    (list '- minus-prim)
	    (list '/ divide-prim)
	    (list '% modulo-prim)
	    (list '< lt-prim)
	    (list '<= lt-or-eq-prim)
	    (list '= equal-sign-prim)
	    (list '> gt-prim)
	    (list '>= gt-or-eq-prim)
	    (list 'abort abort-prim)
	    (list 'abs abs-prim)
	    (list 'append append-prim)
	    (list 'apply apply-prim)
	    (list 'assv assv-prim)
	    (list 'boolean? boolean?-prim)
	    (list 'caddr caddr-prim)
	    (list 'cadr cadr-prim)
	    (list 'call-with-current-continuation call/cc-prim)
	    (list 'call/cc call/cc-prim)
	    (list 'car car-prim)
	    (list 'cdr cdr-prim)
	    (list 'char? char?-prim)
	    (list 'char=? char=?-prim)
	    (list 'char-whitespace? char-whitespace?-prim)
	    (list 'char-alphabetic? char-alphabetic?-prim)
	    (list 'char-numeric? char-numeric?-prim)
	    (list 'cons cons-prim)
	    (list 'current-time current-time-prim)
	    (list 'cut cut-prim)
	    (list 'dir dir-prim)
	    (list 'display display-prim)
	    (list 'current-environment current-environment-prim)
	    (list 'eq? eq?-prim)
	    (list 'equal? equal?-prim)
	    (list 'error error-prim)
	    (list 'eval eval-prim)
	    (list 'eval-ast eval-ast-prim)
	    (list 'exit exit-prim)
	    (list 'for-each for-each-prim)
	    (list 'get get-prim)
	    (list 'get-stack-trace get-stack-trace-prim)
	    (list 'import import-prim)
	    (list 'length length-prim)
	    (list 'list list-prim)
	    (list 'list->vector list-to-vector-prim)
	    (list 'list->string list->string-prim)
	    (list 'list-ref list-ref-prim)
	    (list 'load load-prim)
	    (list 'make-set make-set-prim)
	    (list 'make-vector make-vector-prim)
	    (list 'map map-prim)
	    (list 'member member-prim)
	    (list 'memq memq-prim)
	    (list 'memv memv-prim)
	    (list 'newline newline-prim)
	    (list 'not not-prim)
	    (list 'null? null?-prim)
	    (list 'number->string number->string-prim)
	    (list 'number? number?-prim)
	    (list 'pair? pair?-prim)
	    (list 'parse parse-prim)
	    (list 'parse-string parse-string-prim)
	    (list 'print print-prim)
	    (list 'printf printf-primitive)
	    (list 'range range-prim)
	    (list 'read-string read-string-prim)
	    (list 'require require-prim)
	    (list 'reverse reverse-prim)
	    (list 'set-car! set-car!-prim)
	    (list 'set-cdr! set-cdr!-prim)
	    (list 'sqrt sqrt-prim)
	    (list 'odd? odd?-prim)
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
	    (list 'unparse unparse-prim)    ;; unparse should be in CPS
	    (list 'unparse-procedure unparse-procedure-prim)  ;; unparse should be in CPS
	    (list 'using using-primitive)
	    (list 'vector vector-prim)
	    (list 'vector-ref vector-ref-prim)
	    (list 'vector-set! vector-set!-prim)
	    (list 'void void-prim)
	    (list 'zero? zero?-prim)
	    (list 'current-directory current-directory-prim)
	    (list 'cd current-directory-prim)
	    )))
      (make-initial-env-extended
        (make-initial-environment (map car primitives) (map cadr primitives))))))
	
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
	  (instantiate^ (cdr pair1) s-car (cdr^ apair1)
	    (lambda-cont2 (new-cdr1 new-acdr1)
	      (instantiate^ (cdr pair2) s-car (cdr^ apair2)
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
	   (instantiate^ (cdr pattern) s (cdr^ ap)
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

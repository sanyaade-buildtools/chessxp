;;;; lexer.lisp
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :lexer
  (:use :cl :parsergen)
  (:nicknames :lex)
  (:export
   #:compile-re
   #:match-re
   #:find-re
   #:split-re
   #:deflexer
   #:parse))

(in-package :lexer)

(defclass re ()
  ((pattern    :initarg :pattern    :accessor re-pattern)
   (expression :initarg :expression :accessor re-expression))
  (:documentation "Regular expression."))

(defclass lex-state ()
  ((source   :initarg :source   :accessor lex-source)
   (captures :initarg :captures :accessor lex-captures))
  (:documentation "Token pattern match."))

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (princ (re-pattern re) s)))

(defmethod print-object ((lex lex-state) s)
  "Output the result of a match."
  (print-unreadable-object (lex s :type t :identity t)))

(defvar *case-fold* nil "Case-insentitive comparison.")
(defvar *multi-line* nil "Dot and EOL also match newlines.")

(defconstant +lowercase-letter+ "abcdefghijklmnopqrstuvwxyz")
(defconstant +uppercase-letter+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +digit+ "0123456789")
(defconstant +hex-digit+ "0123456789abcdefABCDEF")
(defconstant +punctuation+ "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"")
(defconstant +letter+ #.(concatenate 'string +lowercase-letter+ +uppercase-letter+))
(defconstant +alpha-numeric+ #.(concatenate 'string +letter+ +digit+))
(defconstant +spaces+ #.(format nil "~c~c" #\space #\tab))
(defconstant +newlines+ #.(format nil "~c~c~c" #\newline #\return #\linefeed))

(defparser re-parser
  ((start exprs) $1)

  ;; multiple expressions bound together
  ((exprs compound exprs) (bind $1 $2))
  ((exprs compound) $1)

  ;; either expression a or b (a|b)
  ((compound expr :or expr) (either $1 $3))

  ;; optional and repition (?, *, +)
  ((compound expr :maybe) (maybe $1))
  ((compound expr :many) (many $1))
  ((compound expr :many1) (many1 $1))

  ;; single expression
  ((compound expr) $1)

  ;; capture expression (x)
  ((expr :capture compound :end-capture) $2)

  ;; single character
  ((expr :char) (ch $1 :case-fold *case-fold*))

  ;; any character and end of line/input
  ((expr :any) (any-char :match-newline-p *multi-line*))
  ((expr :eol) (eol :match-newline-p *multi-line*))

  ;; escaped characters and named sets
  ((expr :escape :char)
   (case $2
    (#\s (one-of +spaces+))
    (#\S (none-of +spaces+))
    (#\n (one-of +newlines+))
    (#\N (none-of +newlines+))
    (#\a (one-of +letter+))
    (#\A (none-of +letter+))
    (#\l (one-of +lowercase-letter+))
    (#\L (none-of +lowercase-letter+))
    (#\u (one-of +uppercase-letter+))
    (#\U (none-of +uppercase-letter+))
    (#\p (one-of +punctuation+))
    (#\P (none-of +punctuation+))
    (#\w (one-of +alpha-numeric+))
    (#\W (none-of +alpha-numeric+))
    (#\d (one-of +digit+))
    (#\D (none-of +digit+))
    (#\x (one-of +hex-digit+))
    (#\X (none-of +hex-digit+))
    (#\z (ch #\null))
    (otherwise
     (ch $2 :case-fold *case-fold*))))

  ;; sets of characters ([..], [^..])
  ((expr :set chars :end-set) (one-of $2 :case-fold *case-fold*))
  ((expr :set :none chars :end-set) (none-of $2 :case-fold *case-fold*))

  ;; list of character (for sets)
  ((chars :char chars) (cons $1 $2))
  ((chars :char) (list $1)))

(defun compile-re (pattern &key case-fold multi-line)
  "Create a regular expression pattern match."
  (let ((*case-fold* case-fold)
        (*multi-line* multi-line))
    (with-input-from-string (s pattern)
      (flet ((next-token ()
               (let ((c (read-char s nil nil)))
                 (when c
                   (case c
                     (#\% :escape)
                     (#\$ :eol)
                     (#\. :any)
                     (#\^ :none)
                     (#\( :capture)
                     (#\) :end-capture)
                     (#\[ :set)
                     (#\] :end-set)
                     (#\? :maybe)
                     (#\* :many)
                     (#\+ :many1)
                     (#\| :or)
                     (otherwise
                      (values :char c)))))))
        (let ((expr (re-parser #'next-token)))
          (when expr
            (make-instance 're :pattern pattern :expression expr)))))))

(defun match-re (re s &key (start 0) (end (length s)) exact)
  "Check to see if a regexp pattern matches a string."
  (with-input-from-string (source s :start start :end end)
    (let ((st (make-instance 'lex-state :source source)))
      (when (funcall (re-expression re) st)
        (let ((pos (file-position source)))
          (if exact
              (when (= pos end)
                (values s pos))
          (values (subseq s start pos) pos)))))))

(defun find-re (re s &key (start 0) (end (length s)) all)
  "Find a regexp pattern match somewhere in a string."
  (do ((st (make-instance 'lex-state :source s :pos start :end end)
           (make-instance 'lex-state :source s :pos start :end end)))
      ((funcall (re-expression re) st)
       (let ((m (cons start (lex-pos st))))
         (if (not all)
             m
           (cons m (find-re re s :start (lex-pos st) :end end :all t)))))
    (unless (< (incf start) (length s))
      (return nil))))

(defun split-re (re s &key (start 0) (end (length s)) all coalesce-seps)
  "Split a string into one or more strings by regexp pattern match."
  (let ((m (find-re re s :start start :end end :all all)))
    (if (null m)
        (subseq s start end)
      (if (null all)
          (destructuring-bind (left right)
              m
            (values (subseq s (car left) (cdr left))
                    (subseq s (car right) (cdr right))))
        (mapcar #'(lambda (m) (subseq s (car m) (cdr m))) m)))))

(defun next (st pred)
  "Read the next character, update the pos, test against predicate."
  (let ((c (read-char (lex-source st) nil nil)))
    (if (funcall pred c)
        t
      (unread-char c (lex-source st)))))

(defun bind (&rest ps)
  "Bind parse combinators together to compose a new combinator."
  #'(lambda (st)
      (dolist (p ps t)
        (unless (funcall p st)
          (return nil)))))

(defun fail (reason)
  "Fail a parse combinator."
  #'(lambda (st)
      (error "Parse error on line ~a: ~a" (lex-line st) reason)))

(defun either (p1 p2)
  "Try one parse combinator, if it fails, try another."
  #'(lambda (st)
      (with-slots (source)
          st
        (let ((pos (file-position source)))
          (or (funcall p1 st)
              (progn
                (file-position source pos)
                (funcall p2 st)))))))

(defun any-char (&key match-newline-p)
  "Expect a non-eoi character."
  #'(lambda (st)
      (next st #'(lambda (x)
                   (and x (if match-newline-p t (char/= x #\newline)))))))

(defun eol (&key (match-newline-p t))
  "End of file or line."
  #'(lambda (st)
      (let ((x (next st #'identity)))
        (if (and match-newline-p (char= x #\newline))
            t
          (null x)))))

(defun ch (c &key case-fold)
  "Match an exact character."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (funcall test x c)))))))

(defun one-of (cs &key case-fold)
  "Match any character from a set."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (find x cs :test test)))))))

(defun none-of (cs &key case-fold)
  "Match any character not in a set."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (not (find x cs :test test))))))))

(defun maybe (p)
  "Optionally match a parse combinator."
  #'(lambda (st)
      (or (funcall p st) t)))

(defun many (p)
  "Match a parse combinator zero or more times."
  #'(lambda (st)
      (do () ((null (funcall p st)) st))))

(defun many1 (p)
  "Match a parse combinator one or more times."
  (bind p (many p)))

(defun many-til (p term)
  "Match a parse combinator many times until a terminal."
  #'(lambda (st)
      (do ()
          ((funcall term st) t)
        (unless (funcall p st)
          (return nil)))))

(defun between (b1 b2 &key match-newline-p)
  "Match everything between two characters."
  (bind b1 (many-til (any-char match-newline-p) b2)))

(provide "LEXER")

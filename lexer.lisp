;;;; lexer.lisp
;;;;

(defpackage :lexer
  (:use :cl)
  (:nicknames :lex)
  (:export
   #:deflexer
   #:parse))

(in-package :lexer)

(defclass re ()
  ((pattern    :initarg :pattern    :accessor re-pattern)
   (expression :initarg :expression :accessor re-expression))
  (:documentation "Regular expression."))

(defclass lex-state ()
  ((source             :initarg :source :accessor lex-source)
   (pos    :initform 0 :initarg :pos    :accessor lex-pos)
   (line   :initform 1 :initarg :line   :accessor lex-line)
   (token              :initarg :token  :accessor lex-token))
  (:documentation "Token pattern match."))

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (princ (re-pattern re) s)))

(defmethod print-object ((lex lex-state) s)
  "Output the result of a match."
  (print-unreadable-object (lex s :type t :identity t)))

(defun parse (source p)
  "Parse a string with a parse combinator. Return the token parsed or nil."
  (with-output-to-string (s)
    (let ((st (make-instance 'lex-state :source source :token s)))
      (unless (funcall p st)
        (return-from parse nil)))))

(defun next (st pred)
  "Read the next character, update the pos, test against predicate."
  (with-slots (source pos line token)
      st
    (let ((c (char source pos)))
      (when (funcall pred c)
        (prog1
            c
          (princ c token)
          (incf pos)
          (when (char= c #\newline)
            (incf line)))))))

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
      (with-slots (pos token)
          st
        (let ((old-pos pos)
              (token-pos (file-position token)))
          (or (funcall p1 st)
              (progn
                (setf pos old-pos)
                (file-position token token-pos)
                (funcall p2 st)))))))

(defun any-char (&optional match-newline-p)
  "Expect a non-eoi character."
  (flet ((pred (x)
           (and x (if match-newline-p t (char/= x #\newline)))))
    #'(lambda (st)
        (next st #'pred))))

(defun ch (c &optional case-fold)
  "Match an exact character."
  (flet ((pred (x)
           (let ((test (if case-fold #'char-equal #'char=)))
             (and x (funcall test x c)))))
    #'(lambda (st)
        (next st #'pred))))

(defun one-of (cs &optional case-fold)
  "Match any character from a set."
  (flet ((pred (x)
           (let ((test (if case-fold #'char-equal #'char=)))
             (and x (find x cs :test test)))))
    #'(lambda (st)
        (next st #'pred))))

(defun none-of (cs &optional case-fold)
  "Match any character not in a set."
  (flet ((pred (x)
           (let ((test (if case-fold #'char-equal #'char=)))
             (and x (not (find x cs :test test))))))
    #'(lambda (st)
        (next st #'pred))))

(defun many (p)
  "Match a parse combinator zero or more times."
  #'(lambda (st)
      (do () ((null (funcall p st)) st))))

(defun many1 (p)
  "Match a parse combinator one or more times."
  (bind p (many p)))

(defun many-til (p term)
  "Match a parse combinator many times until a terminal."
  (either term (bind p (many-til p term))))

(defun between (b1 b2 &optional match-newline-p)
  "Match everything between two characters."
  (bind b1 (many-til (any-char match-newline-p) b2)))


(provide "LEXER")

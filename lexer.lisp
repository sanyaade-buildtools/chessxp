;;;; lexer.lisp
;;;;

(defpackage :lexer
  (:use :cl)
  (:nicknames :lex)
  (:export
   #:compile-re
   #:match-re
   #:split-re
   #:replace-re
   #:deflexer))

(in-package :lexer)

(defclass re ()
  ((pattern    :initarg :pattern    :accessor re-pattern)
   (expression :initarg :expression :accessor re-expression))
  (:documentation "Regular expression."))

(defclass lex-state ()
  ((source :initarg :source :accessor lex-source)
   (pos    :initarg :pos    :accessor lex-pos)
   (line   :initarg :line   :accessor lex-line)
   (token  :initarg :token  :accessor lex-token))
  (:documentation "Token pattern match."))

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (princ (re-pattern re) s)))

(defmethod print-object ((lex lex-state) s)
  "Output the result of a match."
  (print-unreadable-object (lex s :type t :identity t)))

(defun next (st pred)
  "Read the next character, update the pos, test against predicate."
  (with-slots (source pos line)
    (let ((c (char source pos)))
      (when (funcall pred c)
        (prog1
            c
          (incf pos)
          (when (char= c #\newline)
            (incf line)))))))

(defun bind (m p)
  "Bind parse combinators together to compose a new combinator."
  #'(lambda (st)
      (let ((nst (funcall m st)))
        (when nst
          (funcall p nst)))))

(defun fail (reason)
  "Fail a parse combinator."
  #'(lambda (st)
      (error "Parse error on line ~a: ~a" (lex-line st) reason)))

(defun either (p1 p2)
  "Try one parse combinator, if it fails, try another."
  #'(lambda (st)
      (let ((pos (lex-pos st)))
        (or (funcall p1 st)
            (progn
              (setf (lex-pos st) pos)
              (funcall p2 st))))))

(defun any-char (&optional match-newline-p)
  "Expect a non-eoi character."
  (flet ((pred (x)
           (and x (if match-newline-p t (char<> x #\newline)))))
    #'(lambda (st)
        (next st #'pred))))

(defun match (c &optional case-fold)
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
  (bind p (many p))

(defun many-til (p term)
  "Match a parse combinator many times until a terminal."
  (either term (bind p (many-til p term))))

(defun between (b1 b2 &optional match-newline-p)
  "Match everything between two characters."
  (bind b1 (many-til (any-char match-newline-p) b2)))

(defun compile-re (pattern &key case-fold)
  "Create a regular expression from a pattern string."
  (let ((re (make-instance 're :pattern pattern :expression nil)))
    (with-input-from-string (s pattern)
      (do ((c (read-char s nil nil)
              (read-char s nil nil)))
          ((null c) re)
        (case c
         (#\%
         (#\[
         (#\(
         (#\

(provide "LEXER")

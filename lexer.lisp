;;;; lexer.lisp
;;;;

(defpackage :lexer
  (:use :cl)
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

(defun match-re (re s &key (start 0) (end (length s)) exact)
  "Check to see if a regexp pattern matches a string."
  (with-input-from-string (source s :start start :end end)
    (let ((st (make-instance 'lex-state :source source)))
      (when (funcall (re-expression re) st)
        (if exact
            (when (= (lex-pos st) (length s))
              (values s (lex-pos st)))
          (values (subseq s start (lex-pos st)) (lex-pos st))))))

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
  (with-slots (source pos end)
      st
    (when (and (< pos end) (funcall pred (char source pos)))
      (incf pos))))

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
      (let ((pos (lex-pos st)))
        (or (funcall p1 st)
            (progn
              (setf (lex-pos st) pos)
              (funcall p2 st))))))

(defun any-char (&optional match-newline-p)
  "Expect a non-eoi character."
  #'(lambda (st)
      (next st #'(lambda (x)
                   (and x (if match-newline-p t (char/= x #\newline)))))))

(defun ch (c &optional case-fold)
  "Match an exact character."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (funcall test x c)))))))

(defun one-of (cs &optional case-fold)
  "Match any character from a set."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (find x cs :test test)))))))

(defun none-of (cs &optional case-fold)
  "Match any character not in a set."
  (let ((test (if case-fold #'char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (not (find x cs :test test))))))))

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

(defun between (b1 b2 &optional match-newline-p)
  "Match everything between two characters."
  (bind b1 (many-til (any-char match-newline-p) b2)))


(provide "LEXER")

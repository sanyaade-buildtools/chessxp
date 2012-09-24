;;;; pgn.lisp
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :pgn
  (:use :cl :chess :capi :parsergen)
  (:export
   #:load-pgn
   #:create-pgn-layout
   ))

(in-package :pgn)

(defstruct pgn tags moves comments result)

(defstruct move piece rank file capture dest castle promote check)

(defconstant +ws-chars+ #.(format nil "~c~c~c~c" #\space #\tab #\linefeed #\return)
  "Whitespace characters that are skipped during parsing.")

(defparser tag-parser
  ((start tag) $1)
  ((tag :open key ws value :close)
   (cons $2 $4))
  ((chars :char chars)
   `(,$1 ,@$2))
  ((chars))
  ((key chars)
   (coerce $1 'string))
  ((value :quote chars :quote)
   (coerce $2 'string))
  ((ws :space ws))
  ((ws :space)))

(defparser move-parser
  ((start move) $1)
  ((move castle check)
   `(make-move :castle ,$1 :check ,$2))
  ((move :pawn pawn) $2)
  ((move pawn) $1)
  ((move :piece tile check)
   `(make-move :piece ,$1 :dest ,$2 :check ,$3))
  ((move :piece :file tile check)
   `(make-move :piece ,$1 :file ,$2 :dest ,$3 :check ,$4))
  ((move :piece :file :capture tile check)
   `(make-move :piece ,$1 :file ,$2 :capture t :dest ,$4 :check ,$5))
  ((move :piece :rank tile check)
   `(make-move :piece ,$1 :rank ,$2 :dest ,$3 :check ,$4))
  ((move :piece :rank :capture tile check)
   `(make-move :piece ,$1 :rank ,$2 :capture t :dest ,$4 :check ,$5))
  ((move :piece :file :rank tile check)
   `(make-move :piece ,$1 :file ,$2 :rank ,$3 :dest ,$4 :check ,$5))
  ((move :piece :file :rank :capture tile check)
   `(make-move :piece ,$1 :file ,$2 :rank ,$3 :capture t :dest ,$5 :check ,$5))
  ((move :piece :capture tile check)
   `(make-move :piece ,$1 :capture t :dest ,$3 :check ,$4))
  ((pawn tile promote check)
   `(make-move :piece :p :dest ,$1 :promote ,$2 :check ,$3))
  ((pawn :file :capture tile promote check)
   `(make-move :piece :p :file ,$1 :capture t :dest ,$3 :promote ,$4 :check ,$5))
  ((pawn :file :rank :capture tile promote check)
   `(make-move :piece :p :file ,$1 :rank ,$2 :capture t :dest ,$4 :promote ,$5 :check ,$6))
  ((pawn tile check)
   `(make-move :piece :p :dest ,$1 :check ,$2))
  ((pawn :file :capture tile check)
   `(make-move :piece :p :file ,$1 :capture t :dest ,$3 :check ,$4))
  ((pawn :file :rank :capture tile check)
   `(make-move :piece :p :file ,$1 :rank ,$2 :capture t :dest ,$4 :check ,$5))
  ((castle :castle :to :castle :to :castle) :queenside)
  ((castle :castle :to :castle) :kingside)
  ((tile :file :rank) (chess:tile $1 $2))
  ((promote :promote :piece) $2)
  ((check :check) :check)
  ((check :mate) :mate)
  ((check)))

(defun slurp (pathname)
  "Read an entire file in one shot."
  (with-open-file (stream pathname :if-does-not-exist nil)
    (when stream
      (let ((source (make-string (file-length stream))))
        (prog1
            source
          (read-sequence source stream))))))

(defun load-pgn (pathname)
  "Load a PGN, return a list of PGN games."
  (let ((source (slurp pathname)))
    (when source
      (print source))))

(defun create-pgn-layout ()
  "Create the interface layout."
  (let ((editor (make-instance 'editor-pane :text nil))
        (board (create-chess-pane)))
    (let ((board-col (make-instance 'column-layout :description (list board editor))))
      (let ((layout (make-instance 'row-layout :description (list board-col))))
        (contain layout)))))

(defun read-header-tags (stream)
  "Read all the header tags, create a assoc list from it."
  (loop :for line := (read-line stream nil nil)
        :while line
        :collect (with-input-from-string (s line)
                   (read-tag s))))

(defun skip-ws (stream)
  "Read all whitespace characters from a stream."
  (loop :for c := (peek-char nil stream nil)
        :while (find c +ws-chars+)
        :do (read-char stream)))

(defun read-tag (stream)
  "Tokenize the stream and parse a tag."
  (flet ((next-token ()
           (let ((c (read-char stream nil nil)))
             (when c
               (case c
                 (#\" :quote)
                 (#\[ :open)
                 (#\] :close)
                 (#\space :space)
                 (#\tab :space)
                 (otherwise 
                  (values :char c)))))))
    (tag-parser #'next-token)))

(defun read-move (stream)
  "Tokenize the stream and parse a move."
  (flet ((next-token ()
           (case (read-char stream nil nil)
             (#\a (values :file 0))
             (#\b (values :file 1))
             (#\c (values :file 2))
             (#\d (values :file 3))
             (#\e (values :file 4))
             (#\f (values :file 5))
             (#\g (values :file 6))
             (#\h (values :file 7))
             (#\1 (values :rank 0))
             (#\2 (values :rank 1))
             (#\3 (values :rank 2))
             (#\4 (values :rank 3))
             (#\5 (values :rank 4))
             (#\6 (values :rank 5))
             (#\7 (values :rank 6))
             (#\8 (values :rank 7))
             (#\N (values :piece :n))
             (#\B (values :piece :b))
             (#\R (values :piece :r))
             (#\Q (values :piece :q))
             (#\K (values :piece :k))
             (#\P :pawn)
             (#\= :promote)
             (#\O :castle)
             (#\- :to)
             (#\x :capture)
             (#\+ :check)
             (#\# :mate))))
    (move-parser #'next-token)))

(provide "PGN")
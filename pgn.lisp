;;;; pgn.lisp
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :pgn
  (:use :cl :chess :capi :lexer :parsergen)
  (:export
   #:load-pgn
   #:create-pgn-layout
   ))

(in-package :pgn)

(defstruct pgn tags moves result)
(defstruct move player piece rank file capture tile castle promote check comments)

(deflexer pgn-lexer (:multi-line t)
  ("%s+")
  ("%n+")
  ("\;(%N+)"                  (values :comment $1))
  ("{([^}]+)}"                (values :comment $1))
  ("%[(%a+)%s+\"([^\"]*)\"%]" (values :tag (cons $1 $2)))
  ("N"                        (values :piece :n))
  ("B"                        (values :piece :b))
  ("R"                        (values :piece :r))
  ("Q"                        (values :piece :q))
  ("K"                        (values :piece :k))
  ("O%-O%-O"                  (values :castle :queenside))
  ("O%-O"                     (values :castle :kingside))
  ("%*"                       (values :result :in-progress))
  ("1\/2-1\/2"                (values :result :draw))
  ("1%-0"                     (values :result :white-wins))
  ("0%-1"                     (values :result :black-wins))
  ("[a-h]"                    (values :file (- (char-code (char $$ 0)) #.(char-code #\a))))
  ("%d+"                      (values :int (parse-integer $$)))
  ("P"                        :pawn)
  ("%.%.%."                   :dots)
  ("%.+"                      :dot)
  ("x"                        :capture)
  ("="                        :promote)
  ("#"                        :mate)
  ("%+"                       :check))

(defparser pgn-parser
  ((pgn game) $1)

  ((games game games) `(,$1 ,@$2))
  ((games game) `(,$1))

  ((game tags move-list :result)
   (make-pgn :tags $1 :moves $2 :result $3))

  ((tags :tag tags) `(,$1 ,@$2))
  ((tags :tag) `(,$1))

  ((move-list white-move-list) $1)
  ((move-list black-move-list) $1)

  ((white-move-list :int :dot white-move comments black-move-list)
   `(,(apply #'make-move `(,@$3 :comments ,$4)) ,@$5))
  ((white-move-list :int :dot white-move black-move comments white-move-list)
   `(,(apply #'make-move $3) ,(apply #'make-move `(,@$4 :comments ,$5)) ,@$6))
  ((white-move-list :int :dot white-move black-move white-move-list)
   `(,(apply #'make-move $3) ,(apply #'make-move $4) ,@$5))
  ((white-move-list :int :dot white-move)
   `(,(apply #'make-move $3)))
  ((white-move-list))

  ((black-move-list :int :dots black-move comments white-move-list)
   `(,(apply #'make-move `(,@$3 :comments ,$4)) ,@$5))
  ((black-move-list :int :dots black-move white-move-list)
   `(,(apply #'make-move $3) ,@$4))

  ((comments :comment comments) `(,$1 ,@$2))
  ((comments :comment) `(,$1))

  ((white-move move) `(:player :white ,@$1))
  ((black-move move) `(:player :black ,@$1))

  ((move action check) `(,@$1 ,@$2))
  ((move action) $1)

  ((tile :file :int) (tile $2 $1))

  ((check :check) '(:check :check))
  ((check :mate) '(:check :mate))

  ((action :pawn pawn) `(:piece :p ,@$1))
  ((action pawn) `(:piece :p ,@$1))
  ((action piece) $1)
  ((action castle) `(:castle ,$1))

  ((castle :castle) $1)
  ((promote :promote :piece) $2)

  ((pawn :file :capture tile promote)
   `(:piece :p :file ,$1 :capture t :tile ,$3 :promote ,$4))
  ((pawn :file :capture tile)
   `(:piece :p :file ,$1 :capture t :tile ,$3))
  ((pawn tile promote)
   `(:piece :p :tile ,$1 :promote ,$2))
  ((pawn tile)
   `(:piece :p :tile ,$1))

  ((piece :piece :file :int :capture tile)
   `(:piece ,$1 :file ,$2 :rank ,$3 :capture t :tile ,$5))
  ((piece :piece :file :int tile)
   `(:piece ,$1 :file ,$2 :rank ,$3 :tile ,$4))
  ((piece :piece :file :capture tile)
   `(:piece ,$1 :file ,$2 :capture t :tile ,$4))
  ((piece :piece :file tile)
   `(:piece ,$1 :file ,$2 :tile ,$3))
  ((piece :piece :int :capture tile)
   `(:piece ,$1 :rank ,$2 :capture t :tile ,$4))
  ((piece :piece :int tile)
   `(:piece ,$1 :rank ,$2 :tile ,$3))
  ((piece :piece :capture tile)
   `(:piece ,$1 :capture t :tile ,$3))
  ((piece :piece tile)
   `(:piece ,$1 :tile ,$2)))

(defun slurp (pathname)
  "Read an entire file in one shot."
  (with-open-file (stream pathname :if-does-not-exist nil)
    (when stream
      (let ((source (make-string (file-length stream))))
        (subseq source 0 (read-sequence source stream))))))

(defun load-pgn (pathname)
  "Load a PGN, return a list of PGN games."
  (let ((source (slurp pathname)))
    (when source
      (pgn-parser (pgn-lexer source)))))

(defun create-pgn-layout ()
  "Create the interface layout."
  (let ((editor (make-instance 'editor-pane :text nil))
        (board (create-chess-pane)))
    (let ((board-col (make-instance 'column-layout :description (list board editor))))
      (let ((layout (make-instance 'row-layout :description (list board-col))))
        (contain layout)))))

(provide "PGN")
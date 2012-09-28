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

(defstruct pgn tags moves comments result)
(defstruct move piece rank file capture dest castle promote check)

(deflexer pgn-lexer ()
  (#/%s+/)
  (#/%n+/)
  (#/\;(%N+)/                  `(:comment ,$1))
  (#/{([^}]+)}/                `(:comment ,$1))
  (#/%[(%a+)%s+\"([^\"]*)\"%]/ `(:tag (cons ,$1 ,$2)))
  (#/N/                        `(:piece :n))
  (#/B/                        `(:piece :b))
  (#/R/                        `(:piece :r))
  (#/Q/                        `(:piece :q))
  (#/K/                        `(:piece :k))
  (#/O-O-O/                    `(:castle :queenside))
  (#/O-O/                      `(:castle :kingside))
  (#/%*/                       `(:result :in-progress))
  (#/1\/2-1\/2/                `(:result :draw))
  (#/1-0/                      `(:result :white))
  (#/0-1/                      `(:result :black))
  (#/[abcdefgh]/               `(:file ,$$))
  (#/%d+/                      `(:int ,(parse-integer $$)))
  (#/%.%.+/                    `(:dots))
  (#/%./                       `(:dot))
  (#/x/                        `(:capture))
  (#/=/                        `(:promote))
  (#/#/                        `(:mate))
  (#/%+/                       `(:check)))

(defparser pgn-parser
  ((pgn game) $1)

  ((games game games) (cons $1 $2))
  ((games game) (list $1))

  ((game tags moves) $1)

  ((comments :comment comments) (concatenate 'string $1 $2))
  ((comments :comment) $1)

  ((tags :tag tags) (cons $1 $2))
  ((tags :tag) (list $1))

  ((moves move moves) (cons $1 $2))
  ((moves move) (list $1))

  ((move white black))

  ((white :int :dot half-move comments))
  ((white :int :dot half-move))

  ((black :int :dots half-move comments))
  ((black :int :dots half-move))
  ((black half-move))
  
  ((half-move piece) $1)
  ((half-move pawn) $1)
  ((half-move castle) $1)

  ((piece origin :capture tile))
  ((piece origin tile))

  ((pawn :file :capture tile))
  ((pawn tile))

  ((castle :castle) $1)

  ((origin :piece :file :int))
  ((origin :piece :file))
  ((origin :piece :int))
  ((origin :piece))

  ((tile :file :int)))

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
      (let ((tokens (pgn-lexer source)))
        (pgn-parser #'(lambda () 
                        (values-list (pop tokens))))))))

(defun create-pgn-layout ()
  "Create the interface layout."
  (let ((editor (make-instance 'editor-pane :text nil))
        (board (create-chess-pane)))
    (let ((board-col (make-instance 'column-layout :description (list board editor))))
      (let ((layout (make-instance 'row-layout :description (list board-col))))
        (contain layout)))))

(provide "PGN")
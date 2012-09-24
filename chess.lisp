;;;; chess.lisp
;;;;

(defpackage :chess
  (:use :cl :lw :capi :gp)
  (:export
   #:load-chess
   #:new-game

   ;; board functions
   #:empty-board
   #:setup-board
   #:print-board
   #:offboard-p
   #:tile
   #:rank
   #:file
   #:piece
   #:place
   #:opponent

   ;; game functions
   #:new-game
   #:legal-moves
   #:perform-move

   ;; interface functions
   #:create-chess-pane
   #:chess-game))

(in-package :chess)

(defsystem chess (:package "CHESS")
  :members ("board.lisp"
            "eval.lisp"
            "game.lisp"
            "interface.lisp"
            "fen.lisp"
            "pgn.lisp")
  :rules ((:in-order-to :compile :all (:requires (:load :previous)))))

(defun load-chess ()
  "Compile and load the chess system."
  (compile-system 'chess :force t :load t))

(provide "CHESS")
;;;; fen.lisp
;;;;

(defpackage :fen
  (:use :cl :lw :chess)
  (:export
   #:+fen-start+

   ;; fen functions
   #:parse-fen))

(in-package :fen)

(defconstant +fen-start+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  "The initial game setup FEN string.")

(defun parse-fen (fen)
  "Separate a FEN string and parse the individual sections. Return a game structure."
  (destructuring-bind (setup turn castles en-passant half-move move)
      (split-sequence '(#\space) fen :coalesce-separators t)
    (chess::make-game
     :board (parse-setup setup)
     :turn (parse-turn turn)
     :castles (parse-castles castles)
     :en-passant (parse-en-passant en-passant)
     :half-move (parse-integer half-move)
     :move (parse-integer move))))

(defun parse-setup (setup)
  "Return an 0x88 board representation from a FEN setup string."
  (let ((board (empty-board)))
    (prog1
        board
      (loop 
       :for rank :from 0
       :for seq :in (split-sequence '(#\/) setup)
       :do (loop 
            :for file :from 0
            :for p :across seq
            :do (if (digit-char-p p)
                    (let ((n (- (char-code p) (char-code #\1))))
                      (assert (< 0 n 9))
                      (incf file (1- n)))
                  (ecase p
                    (#\p (place board (tile rank file) :black :p))
                    (#\P (place board (tile rank file) :white :p))
                    (#\n (place board (tile rank file) :black :n))
                    (#\N (place board (tile rank file) :white :n))
                    (#\b (place board (tile rank file) :black :b))
                    (#\B (place board (tile rank file) :white :b))
                    (#\r (place board (tile rank file) :black :r))
                    (#\R (place board (tile rank file) :white :r))
                    (#\q (place board (tile rank file) :black :q))
                    (#\Q (place board (tile rank file) :white :q))
                    (#\k (place board (tile rank file) :black :k))
                    (#\K (place board (tile rank file) :white :k)))))))))

(defun parse-turn (turn)
  "Return the current player."
  (cond
   ((string-equal turn "w") :white)
   ((string-equal turn "b") :black)))

(defun parse-castles (castles)
  "Return what castle moves are available to each player."
  (if (string= castles "-")
      '((:kingside) (:queenside))
    (let (kingside queenside)
      (loop :for c :across castles
            :do (ecase c
                  (#\K (push :white kingside))
                  (#\k (push :black kingside))
                  (#\Q (push :white queenside))
                  (#\q (push :black queenside))))
      (list (push :kingside kingside)
            (push :queenside queenside)))))

(defun parse-en-passant (en-passant)
  "Parse the available en passant capture tile."
  (if (string= en-passant "-")
      nil
    (with-input-from-string (s en-passant)
      (chess::read-tile s))))

(provide "FEN")
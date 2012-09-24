;;;; game.lisp
;;;;

(defstruct game board turn kings en-passant castles last-move half-move move)
(defstruct move tile dest castle capture pawn push en-passant promote)

(defun new-game ()
  "Create a new game with the default board setup."
  (let ((white-king (cons :white (tile (back-rank :white) 4)))
        (black-king (cons :black (tile (back-rank :black) 4))))
    (make-game 
     :turn :white
     :board (setup-board (empty-board))
     :castles '((:kingside :white :black) (:queenside :white :black))
     :kings (list white-king black-king)
     :half-move 0
     :move 1)))

(defun legal-moves (game)
  "Return a list of all legal moves available to the current player."
  (with-slots (board turn)
      game
    (let ((moves (pseudo-legal-moves board turn)))
      (remove-if-not #'(lambda (move) (legal-move-p game move)) moves))))

(defun legal-move-p (game move)
  "T if the move is 100% legal to make (available and not in check)."
  (with-slots (tile dest castle pawn en-passant)
      move
    (if castle
        (when (castle-available-p game castle)
          (let ((tiles (list tile dest (1+ (min tile dest)))))
            (notany #'(lambda (x) (in-check-p (game-board game) x)) tiles)))
      (with-slots (board kings turn)
          game
        (if (and pawn en-passant)
            (eq dest (game-en-passant game))
          (multiple-value-bind (piece capture)
              (move board tile dest)
            (unwind-protect
                (let ((king (cdr (assoc turn kings))))
                  (not (in-check-p board king)))
              (setf (aref board tile) piece
                    (aref board dest) capture))))))))

(defun castle-available-p (game side)
  "T if the current player is allowed to castle."
  (with-slots (turn castles)
      game
    (member turn (rest (assoc side castles)))))

(defun disable-castle (game &rest sides)
  "Remove the sides available to castle on for the current player."
  (with-slots (turn castles)
      game
    (flet ((remove-side (side)
             (if (null (member side sides))
                 side
               (remove turn side))))
      (setf castles (mapcar #'remove-side castles)))))

(defun perform-move (game move)
  "Update the game board and state."
  (with-slots (tile dest pawn castle promote en-passant push)
      move
    (if castle
        (perform-castle-move game move)
      (let ((piece (move (game-board game) tile dest)))
        (when promote
          (place (game-board game) dest (game-turn game) promote))
        (when en-passant
          (capture (game-board game) en-passant))
        (when push
          (setf (game-en-passant game) dest))
        (when (eq (second piece) :k)
          (rplacd (assoc (game-turn game) (game-kings game)) dest))
        (cond
          ((eq tile (tile (back-rank (game-turn game)) 7)) (disable-castle game :kingside))
          ((eq tile (tile (back-rank (game-turn game)) 0)) (disable-castle game :queenside)))
        (if pawn
            (setf (game-half-move game) 0)
          (incf (game-half-move game)))
        (when (eq (setf (game-turn game) (opponent (game-turn game))) :white)
          (incf (game-move game)))))
    (setf (game-last-move game) move)))

(defun perform-castle-move (game move)
  "Transpose the king and rook pieces. Disable castle afterwards."
  (with-slots (board turn)
      game
    (let ((rank (if (eq turn :white) 0 7)))
      (case (move-castle move)
        (:kingside
         (move board (tile rank 4) (tile rank 6))
         (move board (tile rank 7) (tile rank 5)))
        (:queenside
         (move board (tile rank 4) (tile rank 2))
         (move board (tile rank 0) (tile rank 3)))))
    (disable-castle game :kingside :queenside)))

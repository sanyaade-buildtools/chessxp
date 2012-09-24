;;;; eval.lisp
;;;;

;;; sliding directions
(defconstant +right+ '(1 2 3 4 5 6 7))
(defconstant +left+ '(-1 -2 -3 -4 -5 -6 -7))
(defconstant +up+ '(16 32 48 64 80 96 112))
(defconstant +down+ '(-16 -32 -48 -64 -80 -96 -112))

;;; l-shaped attacks (knights)
(defconstant +l-moves+ '(-31 -33 -14 -18 31 33 14 18))

;;; 8-direction attacks (king)
(defconstant +k-moves+ '(-1 -15 -16 -17 1 15 16 17))

;;; joined sliding directions
(defconstant +up-right+ (mapcar #'+ +up+ +right+))
(defconstant +up-left+ (mapcar #'+ +up+ +left+))
(defconstant +down-right+ (mapcar #'+ +down+ +right+))
(defconstant +down-left+ (mapcar #'+ +down+ +left+))

;;; attack directions by piece
(defconstant +attack-table+
  `((:b ,@(list +up-right+ +up-left+ +down-right+ +down-left+))
    (:r ,@(list +right+ +left+ +up+ +down+))
    (:q ,@(list +up-right+ +up-left+ +down-right+ +down-left+ +right+ +left+ +up+ +down+))
    (:n ,@(mapcar #'list +l-moves+))
    (:k ,@(mapcar #'list +k-moves+))))

(defun in-check-p (board tile)
  "T if the tile in question can be attacked by the opponent."
  (let* ((color (piece board tile))
         (opp (opponent color)))
    (flet ((check-move (move)
             (with-slots (capture dest)
                 move
               (multiple-value-bind (color piece)
                   (piece board dest)
                 (when (and capture (eq color opp) (eq piece :p))
                   (return-from in-check-p t))))))
      (pseudo-pawn-moves #'check-move board color tile))
    (dolist (kind '(:b :n :r :q :k))
      (flet ((check-move (move)
               (with-slots (capture dest)
                   move
                 (multiple-value-bind (color piece)
                     (piece board dest)
                   (when (and capture (eq color opp) (eq piece kind))
                     (return-from in-check-p t))))))
        (pseudo-piece-moves #'check-move board color tile kind)))))

(defun pseudo-legal-moves (board turn)
  "Collect all the pseudo-legal moves available to the current player."
  (let ((moves))
    (flet ((push-move (move)
             (push move moves)))
      (piece-moves #'push-move board turn)
      (castle-moves #'push-move board turn))
    moves))

(defun piece-moves (chan-move board turn)
  "Collect all pseudo-legal piece moves for the current player."
  (flet ((collect-moves (tile piece)
           (if (eq piece :p)
               (pseudo-pawn-moves chan-move board turn tile)
             (pseudo-piece-moves chan-move board turn tile piece))))
    (map-tiles #'collect-moves board turn)))

(defun pseudo-pawn-moves (chan-move board turn tile)
  "Return a list of available pawn moves from a given tile."
  (let ((delta (pawn-advance turn)))
    (flet ((pawn-move (&key (offset 0) capture en-passant)
             (let ((dest (+ tile delta offset)))
               (flet ((gen-move (&optional promote)
                        (make-move :tile tile
                                   :dest dest
                                   :capture capture
                                   :pawn t
                                   :push (oddp delta)
                                   :en-passant en-passant
                                   :promote promote)))
                 (if (= (rank dest) (back-rank (opponent turn)))
                     (dolist (piece '(:r :b :n :q))
                       (funcall chan-move (gen-move piece)))
                   (funcall chan-move (gen-move)))))))
      (unless (piece board (+ tile delta))
        (pawn-move)
        (when (and (= (rank tile) (pawn-rank turn)) 
                   (null (piece board (+ tile delta delta))))
          (pawn-move :offset delta)))
      (dolist (i '(-1 1))
        (let ((x (piece board (+ tile delta i))))
          (if (eq x (opponent turn))
              (pawn-move :offset i :capture t)
            (when (and (null x) (= (rank (+ tile delta i)) (en-passant-rank turn)))
              (pawn-move :offset i :en-passant (+ tile i)))))))))

(defun pseudo-piece-moves (chan-move board turn tile piece)
  "Return a list of valid moves for the given piece on a tile."
  (dolist (attacks (rest (assoc piece +attack-table+)))
    (block attack
      (dolist (delta attacks)
        (let ((dest (+ tile delta)))
          (if (offboard-p dest)
              (return-from attack)
            (let ((color (piece board dest)))
              (unless (eq color turn)
                (funcall chan-move (make-move :tile tile :dest (+ tile delta) :capture color)))
              (when color
                (return-from attack)))))))))

(defun castle-moves (chan-move board turn)
  "Create a couple castling moves."
  (let ((king (tile (back-rank turn) 4)))
    (flet ((blocked-p (&rest tiles)
             (some #'(lambda (x) (piece board (+ king x))) tiles)))
      (unless (blocked-p 1 2)
        (funcall chan-move (make-move :tile king :dest (+ king 2) :castle :kingside)))
      (unless (blocked-p -1 -2)
        (funcall chan-move (make-move :tile king :dest (- king 2) :castle :queenside))))))
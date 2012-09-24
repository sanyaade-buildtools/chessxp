;;;; board.lisp
;;;;

(defconstant +back-rank+ #(:r :n :b :q :k :b :n :r)
  "The back rank pieces for a new setup.")
(defconstant +rank-axis+ "~&  +---+---+---+---+---+---+---+---+"
  "Used to separate ranks when printing a board.")
(defconstant +file-axis+ "~&    a   b   c   d   e   f   g   h~%"
  "Final bottom axis when printing a board.")
(defconstant +pieces+ '((#\P :p) (#\N :n) (#\B :b) (#\R :r) (#\Q :q) (#\K :k))
  "Mapping of piece characters to symbol.")

(defun empty-board ()
  "Create an empty 0x88 board."
  (make-array 128 :initial-element nil))

(defun setup-board (board)
  "Set a board for a new game."
  (prog1
      board
    (dotimes (i 8)
      (let ((piece (aref +back-rank+ i)))
        (place board (tile 1 i) :white :p)
        (place board (tile 6 i) :black :p)
        (place board (tile 0 i) :white piece)
        (place board (tile 7 i) :black piece)))))

(defun piece (board tile)
  "Return the piece at a given tile."
  (unless (offboard-p tile)
    (values-list (aref board tile))))

(defun place (board tile color piece)
  "Set a piece on the board. Replaces whatever was there."
  (unless (offboard-p tile)
    (setf (aref board tile) (list color piece))))

(defun capture (board tile)
  "Remove a piece from the board."
  (setf (aref board tile) nil))

(defun move (board tile dest)
  "Move a piece from one position of the board to the other."
  (let ((piece (aref board tile))
        (capture (aref board dest)))
    (setf (aref board tile) nil
          (aref board dest) piece)
    (values piece capture)))

(defun tile (rank file)
  "Return the tile index for the rank and file of a board space."
  (declare (fixnum rank file))
  (the fixnum (+ (ash rank 4) file)))

(defun rank (tile)
  "Return the rank for a given tile."
  (declare (fixnum tile))
  (the fixnum (ash tile -4)))

(defun file (tile)
  "Return the file for a given tile."
  (declare (fixnum tile))
  (the fixnum (logand tile 7)))

(defun offboard-p (tile)
  "True if the tile is not on the board."
  (declare (fixnum tile))
  (or (< tile 0) (> tile 127) (plusp (logand tile #x88))))

(defun opponent (color)
  "Return the keyword symbol for the opponent."
  (case color
    (:white :black)
    (:black :white)))

(defun back-rank (color)
  "Return the back rank for a player."
  (case color
    (:white 0)
    (:black 7)))

(defun pawn-rank (color)
  "Returns the pawn rank for a player."
  (case color
    (:white 1)
    (:black 6)))

(defun en-passant-rank (color)
  "Returns the rank that a player can capture via en passant on."
  (case color
    (:white 5)
    (:black 2)))

(defun pawn-advance (color)
  "The board tile delta for a single pawn advance."
  (case color
    (:white 16)
    (:black -16)))

(defun map-tiles (function board player)
  "Call a function for every tile that matches a given player."
  (loop :for tile :from 0 :below (length board)
        :collect (multiple-value-bind (color piece)
                     (piece board tile)
                   (when (eq color player)
                     (funcall function tile piece)))))

(defun write-tile (tile &optional (stream t))
  "Write a tile string to a stream."
  (format stream "~c~a" (code-char (+ (file tile) #.(char-code #\a))) (1+ (rank tile))))

(defun read-file (stream &optional error-p)
  "Try and parse a file character from a stream."
  (let ((file (peek-char nil stream error-p)))
    (if (find file "abcdefgh")
        (- (char-code (read-char stream)) #.(char-code #\a))
      (when error-p
        (error "Invalid file character ~c" file)))))

(defun read-rank (stream &optional error-p)
  "Try and parse a rank character from a stream."
  (let ((rank (peek-char nil stream error-p)))
    (if (find rank "12345678")
        (- (char-code (read-char stream)) #.(char-code #\1))
      (when error-p
        (error "Invalid rank character ~c" rank)))))

(defun read-tile (stream)
  "Try and parse a tile from a stream."
  (let ((file (read-file stream t))
        (rank (read-rank stream t)))
    (tile rank file)))

(defun read-piece (stream)
  "Try and parse a piece character from a stream. Default to pawn."
  (let* ((ch (read-char stream))
         (piece (assoc ch +pieces+)))
    (if piece
        (second piece)
      (prog1
          :p
        (unread-char ch stream)))))

(defun piece-char (board tile)
  "Return the character for displaying a tile."
  (multiple-value-bind (color piece)
      (piece board tile)
    (if (null color)
        #\space
      (let ((c (char (symbol-name piece) 0)))
        (funcall (if (eq color :black) #'char-downcase #'char-upcase) c)))))

(defun print-board (board &optional (s t))
  "Output a board to a stream."
  (format s +rank-axis+)
  (dotimes (rank 8)
    (format s "~&~a |" (- 8 rank))
    (dotimes (file 8)
      (let ((tile (tile (- 7 rank) file)))
        (format s " ~c |" (piece-char board tile))))
    (format s +rank-axis+))
  (format s +file-axis+))

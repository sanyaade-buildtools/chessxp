;;;; interface.lisp
;;;;

(defclass chess-pane (output-pane)
  ((game   :initarg :game :accessor chess-game)
   (pieces                :accessor chess-pieces)
   (board                 :accessor chess-board))
  (:documentation "Canvas pane used to render a chess board."))

(defvar *pane*)

(defconstant +root+ #p"/Users/jeff/Projects/lchess/")

(defconstant +chess-pane-width+ 345
  "Width of the interface pane.")
(defconstant +chess-pane-height+ 345
  "Height of the interface pane.")

(defconstant +board-inset+ 12
  "Offset from the top-left in pixels.")
(defconstant +board-tile-size+ 40
  "Square size of each tile.")

(defun load-chess-image (pane filename)
  "Load an image off disc and convert it to the pane format."
  (let ((i (read-external-image (merge-pathnames filename +root+))))
    (convert-external-image pane i)))

(defun create-chess-pane (&optional game)
  ""
  (make-instance 'chess-pane
                 :create-callback 'initialize-chess-pane
                 :display-callback 'render-chess-pane
                 :visible-min-width +chess-pane-width+
                 :visible-max-width +chess-pane-width+
                 :visible-min-height +chess-pane-height+
                 :visible-max-height +chess-pane-height+
                 :game (if game game (new-game))))

(defun initialize-chess-pane (pane)
  "Load all images, and prepare the pane for rendering."
  (with-slots (board pieces)
      pane
    (setf board (load-chess-image pane #p"i/board.png"))

    ;; load all the pieces
    (setf pieces `(((:white :p) . ,(load-chess-image pane #p"i/white_pawn.png"))
                   ((:white :n) . ,(load-chess-image pane #p"i/white_knight.png"))
                   ((:white :b) . ,(load-chess-image pane #p"i/white_bishop.png"))
                   ((:white :r) . ,(load-chess-image pane #p"i/white_rook.png"))
                   ((:white :q) . ,(load-chess-image pane #p"i/white_queen.png"))
                   ((:white :k) . ,(load-chess-image pane #p"i/white_king.png"))
                   ((:black :p) . ,(load-chess-image pane #p"i/black_pawn.png"))
                   ((:black :n) . ,(load-chess-image pane #p"i/black_knight.png"))
                   ((:black :b) . ,(load-chess-image pane #p"i/black_bishop.png"))
                   ((:black :r) . ,(load-chess-image pane #p"i/black_rook.png"))
                   ((:black :q) . ,(load-chess-image pane #p"i/black_queen.png"))
                   ((:black :k) . ,(load-chess-image pane #p"i/black_king.png"))))))

(defun render-chess-pane (pane x y width height)
  "Draw the board, pieces, captures, and moves onto the pane."
  (declare (ignorable x y width height))
  (with-slots (game)
      pane
    (render-board pane)
    (render-last-move pane (game-last-move game))
    (render-pieces pane (game-board game))))

(defun render-board (pane)
  "Display the board and all the pieces on it."
  (draw-image pane (chess-board pane) 0 0))

(defun render-last-move (pane move)
  "Highlight the tiles where the last move was made from."
  (flet ((highlight (tile)
           (let ((x (+ (* (file tile) 40) +board-inset+ 1))
                 (y (+ (* (- 7 (rank tile)) 40) +board-inset+ 1)))
             (draw-rectangle pane x y 39 39 :foreground (color:make-rgb 1 1 0 0.8) :filled t))))
    (when move
      (highlight (move-tile move))
      (highlight (move-dest move)))))

(defun render-pieces (pane board)
  "Draw a piece image in the proper location on the board."
  (flet ((render (piece rank file)
           (let ((img (cdr (assoc piece (chess-pieces pane) :test #'equalp)))
                 (x (+ (* file 40) +board-inset+))
                 (y (+ (* (- 7 rank) 40) +board-inset+)))
             (draw-image pane img x y))))
    (dotimes (rank 8)
      (dotimes (file 8)
        (multiple-value-bind (color piece)
            (piece board (tile rank file))
          (when (and color piece)
            (render (list color piece) rank file)))))))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname proj3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; CS 151 University of Chicago
;; Autumn 2013
;; Project 3 seed code
;; (c) John Reppy, Adam Shaw; Nov 2013

;; acknowledgements to Joe Ellis and Tristan Rasmussen for
;; helping develop prototype implementation

(require racket/match)
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;; GENERAL UTILITY CODE ;;;;;;;;;;;;;;;;;;;;

;; nat-fold : (nat X -> X) X nat -> X
;; computes f(n-1, f(n-2, ... f(0, init) ... ))
(define (nat-fold f n init)
  (local
    {(define (fold i acc) (if (< i n) (fold (add1 i) (f i acc)) acc))}
    (fold 0 init)))

(check-expect (nat-fold cons 4 '()) '(3 2 1 0))

;; list-flatten : (listof (listof X)) -> (listof X)
;; flatten a list of lists into a list
(define (list-flatten ll)
  (foldr append '() ll))

;;;;;;;;;;;;;;;;;;;; PLAYER, MOVE, AND GAME DATA DEFINITIONS ;;;;;;;;;;;;;;;;;;;;

;; a (pair X Y) is a (make-pair x y), where 
;; x is an X and y is a Y.
(define-struct pair (fst snd))

;; a size is either 1, 2, or 3,
;; representing small, medium, large in that order

;; a player is either 'blue or 'orange

;; a square is a natural number between 0 and 8 inclusive

;; a piece is a (make-piece s p) where
;; - s is a size, and
;; - p is a player
(define-struct piece (size player))

;; an intro is a (make-intro p s) where
;; - p is a piece, and
;; - s is a square
(define-struct intro (piece square))

;; a shft is a (make-shft src dst) where
;; - src and dst are both squares
(define-struct shft (src dst))

;; a move is either
;; - an intro, or
;; - a shft

;; an inventory is a (listof piece) ordered by increasing size

;; a board is a (listof (listof piece)) representing a rectangular
;; grid of squares.  The playing board is a 3x3 grid, while the
;; inventory boards are 3x2 grids. Each square is represented
;; by a list of pieces, where the pieces are ordered from outermost
;; (i.e., biggest) to innermost (smallest).  An square with no pieces
;; is represented by the empty list.  The order of the squares in
;; this list is row-major order; for example, the playing board
;; squares are ordered as follows:
;;
;;    0 1 2
;;    3 4 5
;;    6 7 8

;; a game is a (make-game next inv board) where
;; - next is a player, 
;; - inv is a pair (make-pair oinv binv), where
;;   - oinv ("orange inventory") is an inventory,
;;   - binv ("blue inventory") is an inventory, and
;; - board is a board (per definition above)
(define-struct game (next inv board))

;;;;;;;;;;;;;;;;;;;; PLAYER, MOVE, AND GAME CODE FOR PROJECT 2 ;;;;;;;;;;;;;;;;;;;;

;; the pieces (useful for testing)
;;
(define orange1 (make-piece 1 'orange))
(define orange2 (make-piece 2 'orange))
(define orange3 (make-piece 3 'orange))
(define blue1   (make-piece 1 'blue))
(define blue2   (make-piece 2 'blue))
(define blue3   (make-piece 3 'blue))

;; piece=? : piece piece -> bool
;; equality test on pieces
;;
(define (piece=? p q)
  (match* (p q)
    [((piece ps pp) (piece qs qp))
     (and (= ps qs) (symbol=? pp qp))]))

(check-expect (piece=? orange1 orange2) #f)
(check-expect (piece=? orange1 blue1) #f)
(check-expect (piece=? blue3 blue3) #t)

;; move->string : move -> string 
;; return a string representation of a move (convenience)
(define (move->string m)
 (local 
   {(define $ string-append)
    (define n number->string)}
 (match m
   [(shft s d) ($ "S/" (n s) "=>" (n d))]
   [(intro (piece s c) i) 
    ($ "I/" (if (symbol=? 'blue c) "B" "O") (n s) "=>" (n i))])))

;; other-player : player -> player
;; given a player, return the other player
(define (other-player player) 
  (match player
    ['orange 'blue]
    ['blue 'orange]))

;; select-by-player : player (pair X X) -> X
;; select the orange/blue item depending on the value of player
(define (select-by-player player items)
  (match* (player items)
    [('orange (pair item _)) item]
    [('blue   (pair _ item)) item]))

;; map-by-player : (X -> X) player (pair X X) -> (pair X X)
;; apply the pair of items to a new pair by applying the function f to the
;; items corresponding to player and leaving the other item unchanged.
(define (map-by-player f player items)
  (match* (player items)
    [('orange (pair orange blue)) (make-pair (f orange) blue)]
    [('blue   (pair orange blue)) (make-pair orange (f blue))]))

;; new-inv : player -> inventory
;; create a new inventory for player p
(define (new-inv p)
  (build-list 6 (λ (i) (make-piece (add1 (floor (/ i 2))) p))))

;; new-game : player -> game
;; create a new game where player p goes first
(define (new-game p)
  (make-game p 
             (make-pair (new-inv 'orange) (new-inv 'blue)) 
             (make-list 9 empty)))

;; pieces-at : board int -> (listof piece)
;; return the list of pieces at the given game-board square
(define (pieces-at b i) (list-ref b i))

;; update-at : board int (listof piece) -> board
;; functional update of board b: replace the value of square i by ps
(define (update-at b i ps)
  (local
    {(define (update j pss)
       (if (= j i)
           (cons ps (rest pss))
           (cons (first pss) (update (add1 j) (rest pss)))))}
    (update 0 b)))

;; b-3b1 : board (for tests)
(define b-3b1
  (list '() '() '() (list blue1) '() '() '() '() '()))

(check-expect (update-at (make-list 9 empty) 3 (list blue1))
              b-3b1)

;; square-available? : piece square board -> bool
;; tests if legal to place piece p at square i on board b
(define (square-available? p i b)
  (local {(define ps (pieces-at b i))}
    (or (empty? ps) 
        (> (piece-size p) (piece-size (first ps))))))

(check-expect (square-available? orange2 3 b-3b1) true)

;; lift-top-piece : board square -> board
;; remove first piece from square i on board b, returning new board  
;; PRE: given square occupied by at least one piece
(define (lift-top-piece b i) 
  (update-at b i (rest (pieces-at b i))))

(check-expect (lift-top-piece b-3b1 3)
              (make-list 9 empty))

;; a list of the winning board positions
(define winners
  '((0 1 2) (3 4 5) (6 7 8)  ;; three across
    (0 3 6) (1 4 7) (2 5 8)  ;; three down
    (0 4 8) (2 4 6)))        ;; diagonal

;; board-victory? : player board -> bool
;; test whether the given player is victorious in the given board state
(define (board-victory? player brd)
  (local
    {(define (has-sq i)
       (local {(define ps (pieces-at brd i))}
         (and (cons? ps) (symbol=? player (piece-player (first ps))))))}
    (ormap (lambda (sqs) (andmap has-sq sqs)) winners)))

(check-expect
 (board-victory? 'blue
                 (list '() '() '()
                       (list blue1) (list blue2) (list blue3)
                       '() '() '()))
 #t)

(check-expect
 (board-victory? 'blue
                 (list '() '() '()
                       (list blue1) (list orange1) (list blue3)
                       '() '() '()))
 #f)
                     
;; victory? : player game -> bool
;; test whether the given player is victorious in the given game state.
(define (victory? player g) 
  (board-victory? player (game-board g)))

;; apply-intro-lift : piece game -> game
;; remove the piece from the owning player's inventory  
;; PRE: the piece belongs to the current player
(define (apply-intro-lift p g)
  (local
    {(define (remove ps)
       (cond
         [(empty? ps) (error 'apply-intro.remove "piece not in inventory")]
         [(piece=? (first ps) p) (rest ps)]
         [else (cons (first ps) (remove (rest ps)))]))
     (define player (piece-player p))}
    (make-game
     (game-next g)
     (map-by-player remove player (game-inv g))
     (game-board g))))

;; apply-shft-lift : square game -> (pair piece game)
;; lift piece off given square, return pair of that piece and game without it
(define (apply-shft-lift src g)
  (local
    {(define brd (game-board g))
     (define ps (pieces-at brd src))}
    (make-pair
     (first ps)
     (make-game (game-next g) (game-inv g) (update-at brd src (rest ps))))))

;; apply-drop : piece square game -> game
;; place given piece on given square
;; PRE: square is available to that piece
(define (apply-drop p dst g)
  (make-game
   (game-next g)
   (game-inv g)
   (update-at (game-board g) dst (cons p (pieces-at (game-board g) dst)))))

;; switch-players : game -> game
;; toggle next player in game
(define (switch-players g)
  (match g [(game p i b)
    (make-game (other-player p) i b)]))

;;;;;;;;;;;;;;;;;;;; RENDERING CODE FOR PROJECT 2 ;;;;;;;;;;;;;;;;;;;;

;; the size of a square in pixels
;;
(define square-sz 100)
(define square-radius (/ square-sz 2))

;; the radii of the pieces
;;
(define piece-radii '(20 30 40))

;; the size of the board in squares
;;
(define board-num-rows 3)
(define board-num-cols 3)

;; the size of an inventory in squares
;;
(define inv-num-rows 3)
(define inv-num-cols 2)

;; the size of the board in pixels
;;
(define board-wid (* board-num-cols square-sz))
(define board-ht (* board-num-rows square-sz))

;; the size of an inventory in pixels
;;
(define inv-wid (* inv-num-cols square-sz))
(define inv-ht (* inv-num-rows square-sz))

;; padding between grids
;;
(define padding 5)

;; the size of the window in pixels
;;
(define win-wid (+ inv-wid padding board-wid padding inv-wid))
(define win-ht board-ht)

;; A tile-grid is a (make-tile-grid ulc n m fg-color bg-color) that represents an
;; n x m grid of square tiles, where
;;  - ulc is a posn (the universal coordinates of the upper-left corner of the grid)
;;  - n is a nat (the number of rows),
;;  - m is a nat (the number of squares per row),
;;  - fg-color is a string (the foreground color), and
;;  - bg-color is a string (the background color). 
;;
(define-struct tile-grid (ulc nrows ncols foreground background))

;; the grids for the orange player, the board, and the blue player
;; are laid out left to right.
;;
(define orange-inv-grid (make-tile-grid (make-posn 0 0) 3 2 "black" "Cornsilk"))
(define board-grid (make-tile-grid (make-posn inv-wid 0) 3 3 "black" "white"))
(define blue-inv-grid (make-tile-grid (make-posn (+ inv-wid board-wid) 0) 3 2 "black" "LightCyan"))

;; tile-grid-width : tile-grid -> nat
;; return the width of a tile-grid in pixels
;;
(define (tile-grid-width grid) (* square-sz (tile-grid-ncols grid)))

;; tile-grid-height : tile-grid -> nat
;; return the width of a tile-grid in pixels
;;
(define (tile-grid-height grid) (* square-sz (tile-grid-nrows grid)))

;; draw-grid-background : tile-grid color color -> image
;; render an image for the grid's background with the given colors for the
;; grid lines and square backgrounds.
;;
(define (draw-grid-background grid)
  (local
    {(define wid (tile-grid-width grid))
     (define ht (tile-grid-height grid))
     (define hline (rectangle (- wid 4) 4 'solid (tile-grid-foreground grid)))
     (define vline (rectangle 4 (- ht 4) 'solid (tile-grid-foreground grid)))
     (define (place-hline i img)
       (place-image/align hline 2 (+ square-sz (* i square-sz)) "left" "top" img))
     (define (place-vline i img)
       (place-image/align vline (+ square-sz (* i square-sz)) 2 "left" "top" img))}
    (nat-fold
     place-hline
     (sub1 (tile-grid-nrows grid))
     (nat-fold
      place-vline
      (sub1 (tile-grid-ncols grid))
      (empty-scene wid ht (tile-grid-background grid))))))

;; point-in-grid : nat nat grid -> int
;; test to see if the point (x, y) is in the area covered by the grid.
;; If it is, then return the index of the grid square; otherwise return -1.
;;
(define (point-in-grid x y grid)
  (local
    {(define col (floor (/ (- x (posn-x (tile-grid-ulc grid))) square-sz)))
     (define row (floor (/ (- y (posn-y (tile-grid-ulc grid))) square-sz)))}
    (if (and (<= 0 col (- (tile-grid-ncols grid) 1))
             (<= 0 row (- (tile-grid-nrows grid) 1)))
        (+ (* (tile-grid-ncols grid) row) col)
        -1)))

(check-expect (point-in-grid 350 50 board-grid) 1)
(check-expect (point-in-grid 390 150 board-grid) 4)
(check-expect (point-in-grid 250 150 board-grid) 3)
(check-expect (point-in-grid 150 150 board-grid) -1)

;; render-piece-with-border : num piece -> image
;; render piece as a circle with black border of given width
(define (render-piece-with-border w p)
  (local
    {(define p-color (if (symbol=? (piece-player p) 'orange) "orange" "blue"))
     (define radius (list-ref piece-radii (sub1 (piece-size p))))}
    (overlay (circle (- radius w) "solid" p-color)
             (circle radius "solid" "black"))))

;; render-piece : piece -> image
;; render piece as a plain circle
(define (render-piece p)
  (circle
   (list-ref piece-radii (sub1 (piece-size p)))
   "solid"
   (if (symbol=? (piece-player p) 'orange) "orange" "blue")))

;; draw-pieces : tile-grid boolean board -> image
;; draw board, in xray mode or not
(define (draw-pieces grid xray? squares)
  (local
    {(define ncols (tile-grid-ncols grid))
     (define render  ;; pick a render function for pieces based on the xray mode
       (if xray?
           (λ (ps) (foldl
                    (λ (p img) (overlay (render-piece-with-border 2 p) img))
                    empty-image ps))
           (λ (ps) (render-piece (first ps)))))
     ;; draw : board num num image -> image
     (define (draw pss col row img)
       (match pss
         ['() img]
         [(cons '() rest) (next rest col row img)]
         [(cons ps rest)
          (next rest col row
                (place-image (render ps)
                             (+ (* col square-sz) square-radius)
                             (+ (* row square-sz) square-radius)
                             img))]))
     ;; next : board num num image -> image
     (define (next pss col row img)
       (if (< (add1 col) ncols) (draw pss (add1 col) row img)
           (draw pss 0 (add1 row) img)))}
    (draw squares 0 0 (draw-grid-background grid))))

;; layout-window : image image image -> image
;;
(define (layout-window o-grid brd b-grid msg)
  (local {(define msg-img (text msg 24 "black"))}
   (above/align "left"
                (beside o-grid brd b-grid)
                (overlay
                 msg-img
                 (rectangle (+ (image-width msg-img) 10)
                            (+ (image-height msg-img) 10)
                            "solid" "white")))))

;;;;;;;;;;;;;;;;;;;; WORLD STATE CODE FOR PROJECT 2 ;;;;;;;;;;;;;;;;;;;;

;; a drag-piece is a (make-drag-piece p x y mk-move), where
;;  - p is a piece
;;  - x and y are integer coordinates
;;  - mk-move : square -> move is a function that will make the move object
;;    represented by completion of this drag-piece operation
(define-struct drag (piece x y mk-move))

;; a world-state is either a drag-piece or one of the following symbols:
;;   'waiting-for-human
;;   'waiting-for-computer
;;   'computer-thinking
;;   'victory
;;   'quit

;; a world is a (make-world g xray? inv log st checkpt), where
;;  - g is a game
;;  - xray? is a boolean, which is true when xray mode is on
;;  - inv is a pair (make-pair oinv binv), where
;;    - oinv is a 3x2 board for the orange inventory
;;    - binv is a 3x2 board for the blue inventory
;;  - log is a list of played moves in most-recent-first order
;;  - st is a world-state
;;  - checkpt is a world or false; it is a world (to restore) 
;;      when st is a drag-piece (see above), false otherwise
(define-struct world (game xray? inv log state checkpt))

;; world-update-state : world world-state -> world
;; functional update of a world's state component
(define (world-update-state wrld new-state)
  (match wrld
    [(world g xray? inv log _ cp) 
     (make-world g xray? inv log new-state cp)]))

;; world-update-game : world game -> world
;; functional update of a world's game component
(define (world-update-game wrld new-g)
  (match wrld
    [(world _ xray? inv log st cp) 
     (make-world new-g xray? inv log st cp)]))

;; new-world : player -> world
;; create an initial world where player p is the first player
(define (new-world p)
  (local
    {(define g (new-game p))}
    (make-world g
                false
                (make-pair
                 (map list (pair-fst (game-inv g)))
                 (map list (pair-snd (game-inv g))))
                '()
                'waiting-for-human
                #f)))

;; world-check-victory : player world -> world
;; check to see if the player has a victory in the current world;
;; if so, update the game state to 'victory 
(define (world-check-victory player wrld)
  (if (victory? player (world-game wrld))
      (world-update-state wrld 'victory)
      wrld))

;; world-victory? : world -> boolean
;; return true if the world is in a victory state
(define (world-victory? wrld)
  (and (symbol? (world-state wrld)) (symbol=? 'victory (world-state wrld))))

;; finish-move : world -> world
;; check to see if the current player wins; if not, switch players
(define (finish-move wrld)
  (if (world-victory? wrld)
      wrld
      (world-update-game wrld (switch-players (world-game wrld)))))

;; world-msg : world -> string
;; return a message that represents the current world state
(define (world-msg wrld)
  (match (world-state wrld)
    ['waiting-for-human
      (if (symbol=? (game-next (world-game wrld)) 'blue)
       "It is your turn; please play a blue piece."
       "It is your turn; please play an orange piece.")]
    [(drag _ _ _ _) "Place your piece on the board."]
    ['waiting-for-computer "I'm thinking ..."]
    ['victory (if (victory? 'blue (world-game wrld))
                  "Blue wins! Play again? (y/n)"
                  "Orange wins! Play again? (y/n)")]
    ['quit "Thanks for playing!"]
    [s (error 'world-msg 
              (string-append "unknown state: '" (symbol->string s)))]))

;; lift-piece? : player integer integer tile-grid board -> (or square -1)
;; calculate which piece the player has clicked on (if any)
(define (lift-piece? player x y grid brd)
  (local
    {(define sq (point-in-grid x y grid))
     (define ps (if (< sq 0) '() (pieces-at brd sq)))}
    (if (and (cons? ps) (symbol=? player (piece-player (first ps))))
        sq
        -1)))

(check-expect
 (lift-piece? 'blue 250 150 board-grid (build-list 9 (λ (i) (list blue1))))
 3)

;; remove-from-inv-board : piece -> board -> board
;; remove the piece from the board that represents an inventory
(define (remove-from-inv-board p)
  (local
    {(define (remove pss)
       (cond
         [(empty? pss) (error 'remove-from-inv-board "piece not found")]
         [(empty? (first pss)) (cons '() (remove (rest pss)))]
         [(piece=? (caar pss) p) (cons '() (rest pss))]
         [else (cons (first pss) (remove (rest pss)))]))}
    remove))
    
(check-expect ((remove-from-inv-board blue1) 
               (list (list blue2) '() (list orange1) '() '() (list blue1)))
              (list (list blue2) '() (list orange1) '() '() '()))

(check-expect ((remove-from-inv-board blue2) 
               (list (list blue2) '() (list orange1) '() '() (list blue1)))
              (list '() '() (list orange1) '() '() (list blue1)))

(check-expect ((remove-from-inv-board orange1)
               (list (list blue2) '() (list orange1) '() '() (list blue1)))
              (list (list blue2) '() '() '() '() (list blue1)))

;;;;;;;;;;;;;;;;;;;; VALIDATION CODE FOR PROJECT 2 ;;;;;;;;;;;;;;;;;;;;

;; all-pieces? : (listof piece) -> bool
;; Test that all of the game pieces (and no more) are in the given list.
(define (all-pieces? ps)
  (local
    {(define (count-piece p ps)
       (foldl (λ (q n) (if (piece=? p q) (add1 n) n)) 0 ps))}
    (and (= (count-piece orange1 ps) 2)
         (= (count-piece orange2 ps) 2)
         (= (count-piece orange3 ps) 2)
         (= (count-piece blue1 ps) 2)
         (= (count-piece blue2 ps) 2)
         (= (count-piece blue3 ps) 2))))

;; decreasing-size? : (listof piece) -> bool
;; test pieces in the list are in decreasing order of size
(define (decreasing-size? ps)
  (local
    {(define (chk prev-sz ps)
       (or (empty? ps)
           (and (> prev-sz (piece-size (first ps)))
                (chk (piece-size (first ps)) (rest ps)))))}
    (chk 4 ps)))

(check-expect (decreasing-size? (list blue3 blue2 blue2)) #f)
(check-expect (decreasing-size? (list blue3 blue2)) #t)
(check-expect (decreasing-size? '()) #t)
(check-expect (decreasing-size? (list blue1)) #t)

;; squares-valid? : board -> bool
;; are all the squares on a board valid?
;;
(define (squares-valid? pss) 
  (andmap decreasing-size? pss))

;; world-valid? : world -> bool
;; check to see if a world is in a valid state.  Returns false if not.
(define (world-valid? wrld)
  (match wrld
    [(world _ _ _ _ 'victory _) true]
    [(world _ _ _ _ 'quit _) true]
    [(world (game _ (pair goinv gbinv) brd) _ (pair oinv binv) _ st _)
     (local
       {(define brd-ps (list-flatten brd))
        (define drag-ps (if (drag? st) (list (drag-piece st)) '()))}
       (and (squares-valid? brd)
            (all-pieces? (append drag-ps brd-ps goinv gbinv))
            (all-pieces? (append drag-ps 
                                 brd-ps 
                                 (list-flatten oinv) 
                                 (list-flatten binv)))))]))         

(check-expect (world-valid? (new-world 'blue)) #t)

;;;;;;;;;;;;;;;;;;;; GUI CODE FOR PROJECT 2 ;;;;;;;;;;;;;;;;;;;;

;; draw-world : world -> image
;;
(define (draw-world wrld)
  (match wrld
    [(world (game _ _ brd) xray? (pair oinv binv) _ state _)
     (local
       {(define win (layout-window
                     (draw-pieces orange-inv-grid false oinv)
                     (draw-pieces board-grid xray? brd)
                     (draw-pieces blue-inv-grid false binv)
                     (world-msg wrld)))}
       (if (drag? state)
           (place-image (render-piece (drag-piece state)) 
                        (drag-x state) 
                        (drag-y state) 
                        win)
           win))]))

;; mouse-fn : world integer integer MouseEvent -> world
;;
(define (mouse-fn wrld x y evt)
  (match* (wrld evt)
    [((world _ _ inv _ (drag p _ _ mk-mv) _) "drag")
     (world-update-state wrld (make-drag p x y mk-mv))]
    ;; we can't handle moves drags outside the window, so revert to the checkpoint
    [((world _ _ _ _ (drag _ _ _ _) chkpt) "leave") chkpt]
    [((world g xray? inv log 'waiting-for-human _) "button-down")
     (local
       {(define player (game-next g))
        (define inv-sq 
          (lift-piece? player x y
                      (select-by-player player (make-pair orange-inv-grid 
                                                          blue-inv-grid))
                      (select-by-player player inv)))}
       (if (<= 0 inv-sq)
           ;; an intro move; we remove the piece from both copies 
           ;; of the inventory and switch to the 'drag state.
           (local
             {(define p 
                (first (pieces-at (select-by-player player inv) inv-sq)))
              (define new-inv 
                (map-by-player (λ (pinv) (lift-top-piece pinv inv-sq))
                               player 
                               inv))}
             (make-world (apply-intro-lift p g) 
                         xray? 
                         new-inv 
                         log
                         (make-drag p x y (λ (sq) (make-intro p sq)))
                         wrld))
           ;; check for a shift
           (local
             {(define brd-sq (lift-piece? player x y board-grid (game-board g)))}
             (if (<= 0 brd-sq)
                 ;; a shift move
                 (local
                   {(define p (first (pieces-at (game-board g) brd-sq)))}
                   (world-check-victory
                    (other-player player)
                    (make-world
                    (pair-snd (apply-shft-lift brd-sq g))
                     xray?
                     inv
                     log
                     (make-drag p x y (λ (dst) (make-shft brd-sq dst)))
                     wrld)))
                 ;; no valid square was selected, so no state change
                 wrld))))]
    [((world g xray? inv log (drag p x y mk-move) chkpt) "button-up")
     (local
       {(define place-sq (point-in-grid x y board-grid))}
       (if (and (<= 0 place-sq) (square-available? p place-sq (game-board g)))
           ;; place the piece, check for victory, and then switch players
           (local
             {(define new-wrld
                (world-check-victory
                 (piece-player p)
                 (make-world
                  (apply-drop p place-sq g) xray? inv
                  (cons (mk-move place-sq) log)
                  'waiting-for-computer #f)))}
             (finish-move new-wrld))
           ;; illegal placement, so revert to the checkpoint
           chkpt))]
    [(_ _) wrld]))

;; tick-fn : (game -> move) ->  (world -> world)
;; given a move-choosing function for the computer to choose its moves, 
;;   tick-fn returns a (world->world) function that checks on every tick interval
;;   whether it's the computer's turn; if so, it uses the move-choosing function
;;   to choose and play a move, and if not, it returns the world as is
(define (tick-fn pick-move)
  (λ (wrld)
    (match wrld
      [(world g xray? inv log 'waiting-for-computer _)
       (match (pick-move g)
         [(intro p sq)
          ;; intro move: we place the piece, check for vicory, and then switch players
          (finish-move
           (world-check-victory
            (piece-player p)
            (make-world
             (apply-drop p sq (apply-intro-lift p g))
             xray?
             (map-by-player (remove-from-inv-board p) (piece-player p) inv)
             (cons (make-intro p sq) log)
             'waiting-for-human
             #f)))]
         [(shft src dst)
          (match (apply-shft-lift src g)
            [(pair p tmp-g)
             (local
               {(define other (other-player (piece-player p)))
                (define tmp-wrld
                  (world-check-victory
                   other
                   (make-world tmp-g xray? inv log 'waiting-for-computer #f)))}
               (if (world-victory? tmp-wrld)
                tmp-wrld ;; human wins!
                ;; complete the shift
                (finish-move
                 (world-check-victory
                  (piece-player p)
                  (make-world
                   (apply-drop p dst tmp-g) xray? inv
                   (cons (make-shft src dst) log)
                   'waiting-for-human #f)))))])])]
      [_ wrld])))

;;;;;;;;;;;;;;;;;;;; PROJECT 2 CODE ;;;;;;;;;;;;;;;;;;;;

;; world-toggle-xray : world -> world
;; negate the current xray? value
(define (world-toggle-xray w)
  (match w
    [(world g x i l s c)
     (make-world g (not x) i l s c)]))

;; safe-symbol=? : ANY symbol -> bool
;; convenience: check if something's a symbol, then compare it
(define (safe-symbol=? x s)
  (and (symbol? x) (symbol=? x s)))

;; stop? : world -> boolean
;; check to see if world is in the 'quit state
(define (stop? w)
  (safe-symbol=? (world-state w) 'quit))

;; available-intros : game -> (listof move)
;; compute the list of available intro moves for a given game
;;
(define (available-intros g)
  (match g
    [(game player inv brd)
     (local
       {(define pinv (select-by-player player inv))
        (define (fold-squares p)
          (λ (sq mvs) 
            (if (square-available? p sq brd) (cons (make-intro p sq) mvs) mvs)))
        (define (fold-pieces sz mvs)
          (local
            {(define p (make-piece (add1 sz) player))}
            (if (member? p pinv) (nat-fold (fold-squares p) 9 mvs) mvs)))}
       (nat-fold fold-pieces 3 '()))]))

;; available-shfts : game -> (listof move)
;; compute the list of available shft moves for a given game
;;
(define (available-shfts g)
  (match g
    [(game player inv brd)
     (local
       {(define (fold-squares src p)
          (λ (sq mvs)
            (if (and (not (equal? sq src)) (square-available? p sq brd))
                (cons (make-shft src sq) mvs)
                mvs)))
        (define (fold-board i pss mvs)
                            (match pss
                              ['() mvs]
                              [(cons '() r) (fold-board (add1 i) r mvs)]
                              [(cons (cons (piece sz player2) _) r)
                               (if (symbol=? player player2)
                                   (fold-board
                                    (add1 i)
                                    r
                                    (nat-fold (fold-squares i (make-piece sz player)) 9 mvs))
                                   (fold-board (add1 i) r mvs))]))}
       (fold-board 0 brd '()))]))

;; possible-moves : game -> (listof move)
;; enumerate possible moves from current game state
(define (possible-moves g)
  (append (available-intros g) (available-shfts g)))

;; random-move : game -> (or move false)
;; pick a legal move at random, if one exists
;;
(define (random-move game)
  (local
    {(define mvs (possible-moves game))}
    (if (not (empty? mvs))
        (list-ref mvs (random (length mvs)))
        false)))

;; key-fn : player -> world key-event -> world
;; respond to user's keystrokes
(define (key-fn first-player)
  (λ (wrld a-key)
    (local {(define state (world-state wrld))}
      (cond
        [(drag? state) wrld]  ;; ignore keys while dragging
        [else
         (match a-key
           ["q" (world-update-state wrld 'quit)]
           ["r" (new-world first-player)]
           ["x" (world-toggle-xray wrld)]
           ["y" (if (symbol=? state 'victory)
                    (new-world first-player)
                    wrld)]
           ["n" (if (symbol=? state 'victory)
                    (world-update-state wrld 'quit)
                    wrld)]
           [_ wrld])]))))
  
;; run-game : player (game -> move) -> world
;; given a first player and a move-choosing function,
;;   call big-bang and run the application
(define (run-game first-player pick-move)
  (big-bang (new-world first-player)
            [name "DrGobblers"]
            [check-with world-valid?]
            [on-tick (tick-fn pick-move) 0.75]
            [to-draw draw-world]
            [on-mouse mouse-fn]
            [on-key (key-fn first-player)]
            [stop-when stop? draw-world]))

;; run : player -> (listof string)
;; convenience: return log of most recently played game as list of strings
(define (run p)
  (map move->string (reverse (world-log (run-game p random-move)))))

;;;;;;;;;;;;;;;;;;;; PROJECT 3 CODE ;;;;;;;;;;;;;;;;;;;;

;; === minimax

;; exgame and ixgame are sample games used for check-expects
(define exgame
  (make-game 'orange
             (make-pair (list (make-piece 1 'orange)
                              (make-piece 2 'orange) 
                              (make-piece 3 'orange))
                        (list (make-piece 1 'blue) 
                              (make-piece 2 'blue) 
                              (make-piece 3 'blue) (make-piece 3 'blue)))
             (list (list (make-piece 1 'orange)) empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                   empty empty)))

;; apply-intro-lift : piece game -> game
;; remove the piece from the owning player's inventory  
;; PRE: the piece belongs to the current player

;; apply-shft-lift : square game -> (pair piece game)
;; lift piece off given square, return pair of that piece and game without it

;; apply-drop : piece square game -> game
;; place given piece on given square
;; PRE: square is available to that piece

;; score : game -> integer
;; assign numeric score to game
(define (score g)
  (cond
    [(victory? 'orange g) 1]
    [(victory? 'blue g) -1]
    [else 0]))

;; nextgame : move -> game 
;; consumes a move and yields the game that results
(define (nextgame m g)
  (cond 
    [(intro? m) (switch-players (apply-drop (intro-piece m) 
                            (intro-square m) 
                            (apply-intro-lift (intro-piece m) g)))]
    [(shft? m) (switch-players 
                (apply-drop (pair-fst 
                             (apply-shft-lift (shft-src m) g))
                            (shft-dst m)
                            (pair-snd 
                             (apply-shft-lift (shft-src m) g))))]))

;; minimax : nat game -> integer
;; given max ply and game, return score
(define (minimax ply g)
  (cond
    [(victory? (game-next g) g) (score g)]
    [(symbol=? 'orange (game-next g))
     (cond
       [(<= ply 1) 
        (apply max (map (λ (x) (score x)) 
                        (map (λ (x) (nextgame x g)) 
                             (possible-moves g))))]
       [else (apply max (map (λ (x) (minimax (sub1 ply) x))
                             (map (λ (x) (nextgame x g)) 
                                  (possible-moves g))))])]
    [(symbol=? 'blue (game-next g))
     (cond
       [(<= ply 1) 
        (apply min (map (λ (x) (score x)) 
                        (map (λ (x) (nextgame x g)) 
                             (possible-moves g))))]
       [else (apply min (map (λ (x) (minimax (sub1 ply) x))
                    (map (λ (x) (nextgame x g)) 
                         (possible-moves g))))])]))
    
;; minimax-choose-move : nat game -> move
;; given max ply and game, choose best available move
(define (minimax-choose-move ply g)
  (local
    {(define pm (possible-moves g))
     
     ;; findbest : move (listof move) -> move
     ;; finds the best move for the corresponding player
     (define (findbest mmm rg)
       (local
         {(define mms (minimax (sub1 ply) (nextgame mmm g)))}
         (cond
           [(symbol=? (game-next g) 'orange)
            (cond
              [(empty? rg) mmm]
              [(< mms (minimax (sub1 ply) (nextgame (first rg) g))) 
               (findbest (first rg) (rest rg))]
              [else (findbest mmm (rest rg))])]
           [(symbol=? (game-next g) 'blue)
            (cond
              [(empty? rg) mmm]
              [(> mms (minimax (sub1 ply) (nextgame (first rg) g))) 
               (findbest (first rg) (rest rg))]
              [else (findbest mmm (rest rg))])])))
     
     ;; choosebest : move -> (or move error)
     ;; chooses the best move or yields an error
     (define (choosebest m)
       (cond
         [(victory? (game-next g) g) (error "the game has already been won")]
         [else m]))}
    (choosebest (findbest (first pm) (rest pm)))))
           
  
     
;; === minimax with transposition table

;; a tt-entry is a (make-tt-entry hash score depth)
;; where hash, score, and depth are numbers
(define-struct tt-entry (hash score depth))

(define TT-size 100003) ;; a prime number
(define TT (make-vector TT-size empty))

;; hashg : game -> num
;; gives the hash number corresponding to the hash
(define (hashg g)
  (local
    {(define boardq (game-board g))
     
     ;; sqhash : (listof piece) num -> num
     ;; gives the 6-bit hash for a square on the board
     (define (sqhash sqq n)
       (cond
         [(empty? sqq) n]
         [(equal? (first sqq) (make-piece 3 'orange))
          (sqhash (rest sqq) (+ 32 n))]
         [(equal? (first sqq) (make-piece 3 'blue))
          (sqhash (rest sqq) (+ 16 n))]
         [(equal? (first sqq) (make-piece 2 'orange))
          (sqhash (rest sqq) (+ 8 n))]
         [(equal? (first sqq) (make-piece 2 'blue))
          (sqhash (rest sqq) (+ 4 n))]
         [(equal? (first sqq) (make-piece 1 'orange))
          (sqhash (rest sqq) (+ 2 n))]
         [(equal? (first sqq) (make-piece 1 'blue))
          (sqhash (rest sqq) (+ 1 n))]))
     
     (define boardhash (map (λ (x) (sqhash x 0)) boardq))}
    (+ (* (list-ref boardhash 0) (expt 2 49))
       (* (list-ref boardhash 1) (expt 2 43))
       (* (list-ref boardhash 2) (expt 2 37)) 
       (* (list-ref boardhash 3) (expt 2 31))
       (* (list-ref boardhash 4) (expt 2 25))
       (* (list-ref boardhash 5) (expt 2 19))
       (* (list-ref boardhash 6) (expt 2 13))
       (* (list-ref boardhash 7) (expt 2 7))
       (* (list-ref boardhash 8) (expt 2 1))
       (cond
         [(symbol=? (game-next g) 'orange) 0]
         [(symbol=? (game-next g) 'blue) 1]))))

;; minimax-tt : nat game -> integer
;; given max ply and game, return score using minimax search 
;; with the transposition table
(define (minimax-tt ply g)
  (local
    {;; checkbucket : num game -> num
     ;; finds or adds/alters the corresponding entry in a bucket of the table
     (define (checkbucket iply g)
       (local
         {(define hg (hashg g))
          (define bl (vector-ref TT (modulo hg 100003)))
          
          ;; icb : (listof tt-entry) num game -> num
          ;; hidden function performing the same as checkbucket
          (define (icb blq iply g)
            (cond
              [(empty? blq)
               (begin 
                 (vector-set! TT (modulo hg 100003)
                              (cons (make-tt-entry hg (minimax iply g) iply) blq))
                 (minimax iply g))]
              [else (cond
                      [(= hg (tt-entry-hash (first blq)))
                       (cond
                         [(<= iply (tt-entry-depth (first blq)))
                          (tt-entry-score (first blq))]
                         [else 
                          (begin 
                            (vector-set! 
                             TT (modulo hg 100003) 
                             (cons (make-tt-entry hg (minimax iply g) iply)
                                   (remove (first blq) bl)))
                            (minimax iply g))])]
                      [else (icb (rest blq) iply g)])]))}
         (icb bl iply g)))}
    
    (cond
      [(victory? (game-next g) g) (score g)]
      [(symbol=? 'orange (game-next g))
       (checkbucket ply g)]
      [(symbol=? 'blue (game-next g))
       (checkbucket ply g)])))     


;; minimax-tt-choose-move : nat game -> move
;; given max ply and game, return best move, and use transposition table
(define (minimax-tt-choose-move ply g)
  (local
    {(define pm (possible-moves g))
     
     ;; findbest : move (listof move) -> move
     ;; finds the best move for the corresponding player
     (define (findbest mmm rg)
       (local
         {(define mms (minimax-tt (sub1 ply) (nextgame mmm g)))}
         (cond
           [(symbol=? (game-next g) 'orange)
            (cond
              [(empty? rg) mmm]
              [(< mms (minimax-tt (sub1 ply) (nextgame (first rg) g))) 
               (findbest (first rg) (rest rg))]
              [else (findbest mmm (rest rg))])]
           [(symbol=? (game-next g) 'blue)
            (cond
              [(empty? rg) mmm]
              [(> mms (minimax-tt (sub1 ply) (nextgame (first rg) g))) 
               (findbest (first rg) (rest rg))]
              [else (findbest mmm (rest rg))])])))
     
     ;; choosebest : move -> (or move error)
     ;; chooses the best move or yields an error
     (define (choosebest m)
       (cond
         [(victory? (game-next g) g) (error "the game has already been won")]
         [else m]))}
    (choosebest (findbest (first pm) (rest pm)))))

;; === main functions

;; run-mm : nat player -> bool
;; runs game and returns true
(define (run-mm ply human-player)
  ((λ (x) true)
   (big-bang (new-world human-player)
             [name "DrGobblers"]
             [check-with world-valid?]
             [on-tick (tick-fn (λ (x) (minimax-choose-move ply x))) 0.75]
             [to-draw draw-world]
             [on-mouse mouse-fn]
             [on-key (key-fn human-player)]
             [stop-when stop? draw-world])))

;; run-mm-tt : nat player -> bool
;; runs game and returns true
(define (run-mm-tt ply human-player)
  ((λ (x) true)
   (big-bang (new-world human-player)
             [name "DrGobblers"]
             [check-with world-valid?]
             [on-tick (tick-fn (λ (x) (minimax-tt-choose-move ply x))) 0.75]
             [to-draw draw-world]
             [on-mouse mouse-fn]
             [on-key (key-fn human-player)]
             [stop-when stop? draw-world])))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname proj1-edit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Yunchen Liu

(require 2htdp/image)
(require racket/match)



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

;; a shift is a (make-shift src dst) where
;; - src and dst are both squares
(define-struct shift (src dst))

;; a move is either
;; - an intro, or
;; - a shift

;; an inventory is a (listof piece)

;; a board is a (listof (listof piece)) of length exactly 9
;; representing the squares of the board.  Each square is represented
;; by a list of pieces, where the pieces are ordered from outermost
;; (i.e., biggest) to innermost (smallest).  An square with no pieces
;; is represented by the empty list.

;; The order of the 9 items in the list corresponds to the 
;; squares on the board as follows:
;; 0 1 2
;; 3 4 5
;; 6 7 8

;; a game is a (make-game next oinv binv board) where
;; - next is a player, 
;; - oinv ("orange inventory") is an inventory,
;; - binv ("blue inventory") is an inventory, and
;; - board is a board (per definition above)
(define-struct game (next oinv binv board))

;; exgame and ixgame are sample games used for check-expects
(define exgame
  (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange) 
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue) 
                   (make-piece 2 'blue) 
                   (make-piece 3 'blue) (make-piece 3 'blue))
             (list (list (make-piece 1 'orange)) empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                   empty empty)))

(define ixgame
  (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange) 
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue) 
                   (make-piece 2 'blue) 
                   (make-piece 3 'blue) (make-piece 3 'blue))
             (list (list (make-piece 1 'orange)) empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                  empty empty)))
                   
;; new-game: player -> game
;; generates initial game state
(define (new-game color)
  (make-game color 
             (list (make-piece 1 'orange) (make-piece 1 'orange)
                   (make-piece 2 'orange) (make-piece 2 'orange)
                   (make-piece 3 'orange) (make-piece 3 'orange))
             (list (make-piece 1 'blue) (make-piece 1 'blue)
                   (make-piece 2 'blue) (make-piece 2 'blue)
                   (make-piece 3 'blue) (make-piece 3 'blue))
             (list empty empty empty empty empty
                   empty empty empty empty)))

(check-expect (new-game 'blue)
              (make-game 'blue 
                         (list (make-piece 1 'orange) (make-piece 1 'orange)
                               (make-piece 2 'orange) (make-piece 2 'orange)
                               (make-piece 3 'orange) (make-piece 3 'orange))
                         (list (make-piece 1 'blue) (make-piece 1 'blue)
                               (make-piece 2 'blue) (make-piece 2 'blue)
                               (make-piece 3 'blue) (make-piece 3 'blue))
                         (list empty empty empty empty empty
                               empty empty empty empty)))

;; pieces-at : board int -> (listof piece)
;; return the list of (from zero to three) pieces at the given square
(define (pieces-at boardlist n)
  (list-ref boardlist n))

(check-expect (pieces-at (game-board exgame) 0) 
              (list (make-piece 1 'orange)))

;; pieces-valid? : game-> bool
;; test that the collection of pieces in the game (both board and inv)
;; includes exactly the correct pieces and that each inv contains only
;; pieces of the right color
(define (pieces-valid? gamestate)
  (local{(define boardlist (game-board gamestate))
         (define (checkinv invq)
           (cond
             [(empty? invq) 0]
             [(symbol=? 'orange (piece-player (first invq))) 
              (cond
                [(= 1 (piece-size (first invq))) 
                 (+ 1 (checkinv (rest invq)))]
                [(= 2 (piece-size (first invq))) 
                 (+ 10 (checkinv (rest invq)))]
                [(= 3 (piece-size (first invq))) 
                 (+ 100 (checkinv (rest invq)))])]
             [(symbol=? 'blue (piece-player (first invq)))
              (cond
                [(= 1 (piece-size (first invq))) 
                 (+ 1000 (checkinv (rest invq)))]
                [(= 2 (piece-size (first invq))) 
                 (+ 10000 (checkinv (rest invq)))]
                [(= 3 (piece-size (first invq))) 
                 (+ 100000 (checkinv (rest invq)))])]))
         
         (define (checkboard boardq)
           (cond
             [(empty? boardq) 0]
             [else (+ (checkinv (first boardq)) 
                      (checkboard (rest boardq)))]))}
    
    (= (+ (checkinv (game-oinv gamestate)) 
          (checkinv (game-binv gamestate)) 
          (checkboard (game-board gamestate))) 222222)))

(check-expect (pieces-valid? ixgame) true)
(check-expect (pieces-valid? (make-game 'blue 
                         (list (make-piece 2 'orange) (make-piece 2 'orange)
                               (make-piece 3 'orange) (make-piece 3 'orange))
                         (list (make-piece 1 'blue) (make-piece 1 'blue)
                               (make-piece 2 'blue) (make-piece 2 'blue)
                               (make-piece 3 'blue) (make-piece 3 'blue))
                         (list empty empty empty empty empty
                               empty empty empty empty)))
              false)


;; squares-valid? : board -> bool
;; test whether all squares on the board are in a valid state
(define (squares-valid? boardq)
  (local {(define (checksquaregob squareq)
            (cond
              [(empty? squareq) true]
              [(= 1 (length squareq)) true]
              [(= 2 (length squareq)) (> (piece-size (first squareq)) 
                                         (piece-size (list-ref squareq 1)))]
              [(= 3 (length squareq)) (> (piece-size (first squareq))
                                         (piece-size (list-ref squareq 1))
                                         (piece-size (list-ref squareq 2)))]
              [else false]))
          
          (define (checkboardnum boardq)
            (foldr (λ (x y) (and (checksquaregob x) y)) true boardq))}
    (checkboardnum boardq))) 

(check-expect (squares-valid? (game-board ixgame)) true)
(check-expect (squares-valid? 
               (list (list (make-piece 3 'blue) (make-piece 2 'blue)
                           (make-piece 1 'blue)) 
                     empty empty empty empty
                     (list (make-piece 1 'blue) (make-piece 1 'blue)
                           (make-piece 2 'blue) (make-piece 2 'blue)
                           (make-piece 3 'blue) (make-piece 3 'blue)) 
                     empty empty empty))
              false)

;; square-available? : piece square board -> bool
;; checks if square is empty, or non-empty but can be gobbled by stated piece
(define (square-available? pieceq squarenum boardq)
  (cond
    [(empty? (list-ref boardq squarenum)) true]
    [else (> (piece-size pieceq) (piece-size (first
                                              (list-ref boardq squarenum))))]))

(check-expect (square-available? 
               (make-piece 2 'orange) 5 
               (list empty empty empty empty empty empty empty empty empty))
              true)
(check-expect (square-available?
               (make-piece 2 'blue) 0
               (list (list (make-piece 3 'orange))
                     empty empty empty empty empty empty empty empty))
              false)

;; move-legal? : move game -> bool
;; test whether a given move is legal in a given game state :
;; that the player to move possesses the piece in question (in case of "intro")
;; that the destination square is available per (square-available?)
(define (move-legal? move gamestate)
  (cond
    [(intro? move) 
     (and (square-available? (intro-piece move) 
                             (intro-square move) 
                             (game-board gamestate))
          (cond
            [(symbol=? 'blue (game-next gamestate))
             (member (intro-piece move) (game-binv gamestate))]
            [(symbol=? 'orange (game-next gamestate))
             (member (intro-piece move) (game-oinv gamestate))]))]
    [(and (shift? move)
     (not (empty? (list-ref (game-board gamestate) (shift-src move)))))
     (and (symbol=? (piece-player (first (list-ref (game-board gamestate) 
                                                   (shift-src move))))
               (game-next gamestate))
     (square-available? (first (list-ref (game-board gamestate) 
                                         (shift-src move)))
                        (shift-dst move)
                        (game-board gamestate)))]))

(check-expect (move-legal? (make-intro (make-piece 2 'orange) 0) exgame)
              false)
(check-expect (move-legal? (make-shift 0 1) exgame) false)
(check-expect (move-legal? (make-shift 0 1) ixgame) true)

;; victory? : player game -> bool
;; test whether the given player is victorious in the given game state
(define (victory? color gamestate)
  (local
    {(define (emptyspace? squareq)
       (empty? (list-ref (game-board gamestate) squareq)))
     
     (define (topcolor squareq)
       (piece-player (first (list-ref (game-board gamestate) squareq))))
     
     (define (inarow? n1 n2 n3)
       (if (and (not(emptyspace? n1)) 
                (not (emptyspace? n2)) 
                (not (emptyspace? n3)))
           (and (symbol=? color (topcolor n1)) 
                (symbol=? color (topcolor n2)) 
                (symbol=? color (topcolor n3)))
           false))}  
    (or (inarow? 0 1 2) (inarow? 3 4 5) (inarow? 6 7 8)
        (inarow? 0 3 6) (inarow? 1 4 7) (inarow? 2 5 8)
        (inarow? 0 4 8) (inarow? 2 4 6))))

(check-expect (victory? 'orange exgame) true)

;; introdelete : move game -> inventory
;; deletes the piece being introduced from the inventory
(define (introdelete moveq gamestate)
  (cond 
    [(symbol=? (game-next gamestate) 'orange)
     (remove (intro-piece moveq) (game-oinv gamestate))]
    [(symbol=? (game-next gamestate) 'blue)
     (remove (intro-piece moveq) (game-binv gamestate))]))

(check-expect (introdelete (make-intro (make-piece 2 'blue) 8) exgame)
             (list (make-piece 1 'blue) 
                   (make-piece 3 'blue) (make-piece 3 'blue)))
(check-expect (introdelete (make-intro (make-piece 2 'orange) 2) ixgame)
              (list (make-piece 1 'orange) 
                    (make-piece 3 'orange)))


;; intromove : move board -> board
;; makes a new board after the specified move
(define (intromove moveq boardq)
  (cond
    [(= (- 9 (length boardq)) (intro-square moveq))
     (cons (cons (intro-piece moveq) (first boardq))
           (rest boardq))]
    [else (cons (first boardq) (intromove moveq (rest boardq)))]))

(check-expect (intromove (make-intro (make-piece 3 'blue) 6) 
                         (game-board exgame))
             (list (list (make-piece 1 'orange)) empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 3 'blue) (make-piece 2 'orange) 
                         (make-piece 1 'blue)) 
                  empty empty))

;; shiftdelete : move board -> board
;; deletes the outermost src piece
(define (shiftdelete moveq boardq)
       (cond
         [(= (- 9 (length boardq)) (shift-src moveq)) 
          (cons (rest (first boardq)) (rest boardq))]
         [else (cons (first boardq) (shiftdelete moveq (rest boardq)))]))

(check-expect (shiftdelete (make-shift 3 2) (game-board ixgame))
             (list (list (make-piece 1 'orange)) empty empty 
                   (list (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                  empty empty))

;; shiftmove : move board -> board
;; performs a shift on the game board
(define (shiftmove moveq boardq)
  (local
    {(define fixedboard boardq)
     (define (shiftm moveq boardq)
       (cond
         [(= (- 9 (length boardq)) (shift-dst moveq)) 
          (cons (cons (first (list-ref fixedboard (shift-src moveq))) 
                 (first boardq)) (rest boardq))]
         [else (cons (first boardq) (shiftm moveq (rest boardq)))]))}
    (shiftm moveq boardq)))

(check-expect (shiftmove (make-shift 3 2) (game-board ixgame))
              (list (list (make-piece 1 'orange)) empty 
                    (list (make-piece 3 'orange)) 
                    (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                    empty empty 
                    (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                    empty empty))

;; apply-move : move game -> game
;; apply the given move to the game and return game's consequent state
(define (apply-move moveq gamestate)
  (cond
    [(move-legal? moveq gamestate)
     (cond
       [(and (intro? moveq) (symbol=? (game-next gamestate) 'orange))
        (make-game 'blue
                   (introdelete moveq gamestate)
                   (game-binv gamestate)
                   (intromove moveq (game-board gamestate)))]
       [(and (intro? moveq) (symbol=? (game-next gamestate) 'blue))
        (make-game 'orange
                   (game-oinv gamestate)
                   (introdelete moveq gamestate)
                   (intromove moveq (game-board gamestate)))]
       [else
        (cond
          [(symbol=? (game-next gamestate) 'blue)
           (cond
             [(victory? 'orange
                        (make-game 
                         'orange
                         (game-oinv gamestate)
                         (game-binv gamestate)
                         (shiftdelete moveq (game-board gamestate))))
              (make-game 'orange 
                         (game-oinv gamestate)
                         (cons (first (list-ref (game-board gamestate) 
                                                (shift-src moveq)))
                               (game-binv gamestate))
                         (shiftdelete moveq (game-board gamestate)))]
             [else (make-game 
                    'orange
                    (game-oinv gamestate)
                    (game-binv gamestate)
                    (shiftdelete moveq
                                 (shiftmove moveq 
                                            (game-board gamestate))))])]
          [(symbol=? (game-next gamestate) 'orange)
           (cond
             [(victory? 'blue
                        (make-game 
                         'blue
                         (game-oinv gamestate)
                         (game-binv gamestate)
                         (shiftdelete moveq (game-board gamestate))))
              (make-game 'blue 
                         (cons (first (list-ref (game-board gamestate)
                                                (shift-src moveq)))
                               (game-oinv gamestate))
                         (game-binv gamestate)
                         (shiftdelete moveq (game-board gamestate)))]
             [else 
              (make-game 
               'blue
               (game-oinv gamestate)
               (game-binv gamestate)
               (shiftdelete moveq
                            (shiftmove moveq 
                                       (game-board gamestate))))])])])]
    [else (error 'apply-move "Invalid move")]))

(check-expect (apply-move (make-intro (make-piece 3 'orange) 0) ixgame)
  (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange))
             (list (make-piece 1 'blue) 
                   (make-piece 2 'blue) 
                   (make-piece 3 'blue) (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'orange)) 
                   empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                  empty empty)))

(check-error (apply-move (make-intro (make-piece 3 'blue) 0) ixgame))

(check-expect (apply-move (make-intro (make-piece 3 'blue) 0) exgame)
  (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue) 
                   (make-piece 2 'blue) 
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'blue) (make-piece 1 'orange)) 
                   empty empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                  empty empty)))

(check-expect (apply-move (make-shift 0 1) ixgame)
  (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue) 
                   (make-piece 2 'blue) 
                   (make-piece 3 'blue) (make-piece 3 'blue))
             (list empty (list (make-piece 1 'orange)) empty 
                   (list (make-piece 3 'orange) (make-piece 2 'blue)) 
                   empty empty 
                   (list (make-piece 2 'orange) (make-piece 1 'blue)) 
                   empty empty)))
(check-expect 
 (apply-move 
  (make-shift 1 2)
  (make-game 
   'blue
   (list (make-piece 1 'orange) (make-piece 2 'orange) (make-piece 3 'orange))
   (list (make-piece 1 'blue) (make-piece 2 'blue) (make-piece 3 'blue))
   (list empty (list (make-piece 3 'blue) (make-piece 1 'orange)) empty 
         empty (list (make-piece 3 'orange) (make-piece 2 'blue)) empty empty  
         (list (make-piece 2 'orange) (make-piece 1 'blue)) empty)))
  
 (make-game
  'orange
  (list (make-piece 1 'orange) (make-piece 2 'orange) (make-piece 3 'orange))
  (list (make-piece 3 'blue) (make-piece 1 'blue) (make-piece 2 'blue)
        (make-piece 3 'blue))
  (list empty (list (make-piece 1 'orange)) empty empty 
        (list (make-piece 3 'orange) (make-piece 2 'blue)) empty empty
        (list (make-piece 2 'orange) (make-piece 1 'blue)) empty)))

;; board-image : board -> image
;; draw a bird eye view of the board
(define (board-image boardq)
  (local
    {(define (corresimage squarelist)
       (cond
         [(empty? squarelist) (square 70 "outline" "black")]
         [else (overlay (circle 
                (* 10 (piece-size (first squarelist))) 
                "solid" 
                (symbol->string (piece-player (first squarelist)))) 
               (corresimage (rest squarelist)))]))
     
     (define (imagelist boardq)
       (map (λ (x) (corresimage x)) boardq))
     
     (define (l i) (list-ref (imagelist boardq) i))
     
     (define (createboard boardq)
       (above
        (beside (l 0) (l 1) (l 2))
        (beside (l 3) (l 4) (l 5))
        (beside (l 6) (l 7) (l 8))))}
    (createboard (imagelist boardq))))

;;eyeball test
(board-image (game-board ixgame))
    
;; xray-board-image : board -> image
;; creates a transparent bird's eye view of the board
(define (xray-board-image boardq)
  (local
    {(define (corresimage squarelist)
       (cond
         [(empty? squarelist) (square 70 "outline" "black")]
         [else (overlay (circle 
                (* 10 (piece-size (first squarelist))) 
                "outline" 
                (symbol->string (piece-player (first squarelist)))) 
               (corresimage (rest squarelist)))]))
     
     (define (imagelist boardq)
       (map (λ (x) (corresimage x)) boardq))
     
     (define (l i) (list-ref (imagelist boardq) i))
     
     (define (createboard boardq)
       (above
        (beside (l 0) (l 1) (l 2))
        (beside (l 3) (l 4) (l 5))
        (beside (l 6) (l 7) (l 8))))}
    (createboard (imagelist boardq))))

;;eyeball test
(xray-board-image (game-board ixgame))
          
;; game-image : bool game -> image
;; displays the entire game
(define (game-image visibility gamestate)
  (local
    {(define (correspimage invlist)
       (cond
         [(empty? invlist) empty-image]
         [else (beside (circle 
                        (* 10 (piece-size (first invlist))) 
                        "solid" 
                        (symbol->string (piece-player (first invlist))))
                       (correspimage (rest invlist)))]))}
     
    (above
     (cond
       [visibility (xray-board-image (game-board gamestate))]
       [else (board-image (game-board gamestate))])
     (correspimage (game-oinv gamestate))
     (correspimage (game-binv gamestate))
     (cond
       [(symbol=? 'blue (game-next gamestate))
        (text "It's blue player's turn" 20 "blue")]
       [(symbol=? 'orange (game-next gamestate))
        (text "It's orange player's turn" 20 "orange")]))))
        
;; eyeball test
(game-image true exgame)
(game-image false ixgame)
     
          
          
          
          
    
  

#lang racket/base

(define-struct posn (x y))

;; Constants:

(define E " ") ;See CellValue data definition below
(define D "o")   ;
(define W "M")  ;

(define INIT-BOARD ;See Board data definition below
  (vector (vector W W W W W W W W W W W W W)
          (vector W D D D D D D D D D D D W)
          (vector W D W D W W W W W D W D W)
          (vector W D W D W D D D W D W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W W D W W W D W W D W)
          (vector W D D D D D E D D D D D W)
          (vector W W W W W W W W W W W W W)))

(define CELL-SIZE 20)

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)




;; Data definitions:


;; Score is Natural
;; interp. dots eaten by pac-man since start of game

(define INIT-SCORE  0)

;; CellValue is one of:
;; - "empty"
;; - "dot"
;; - "wall"
;; interp. the content of a board cell

;; Direction is one of:
;; - "U"
;; - "D"
;; - "L"
;; - "R"
;; interp. direction that a sprite is facing

(define-struct sprite (pos dir) #:transparent)
;; Sprite is (make-sprite Posn Direction)
;; interp. the position in Board coordinates, and the direction of a sprite

(define INIT-PM (make-sprite (make-posn 6 6) "R"))

;; Board is (vectorof (vectorof CellValue))
;; interp. the game board

(define-struct gs (pm board score) #:transparent)
;; GameState is (make-gs Sprite Board Score)
;; interp. all parts of the pac-man game; pac-man, the current
;; board, and the current score

(define INIT-GS (make-gs INIT-PM INIT-BOARD INIT-SCORE))



;; Functions:


;; GameState -> GameState
;; runs the game
(define (main)
  (begin 
         (tick INIT-GS)
         (game-over? INIT-GS)
         #;(big-bang INIT-GS
                   (on-tick tick .3)
                   ;(on-key key-handler)
                   (on-tilt tilt-handler)
                   (stop-when game-over?)
                   (to-draw render))))

(define (run moves)
  (letrec ([loop (lambda (board moves)
                   (cond
                     [(null? moves)
                      board]
                     [else
                      (loop 
                       ;; simulating the rapid arrival of movement events
                       (tick (repeat 100 (lambda () (set-pm-dir board (car moves)))))                      
                       (cdr moves))]))])
    (loop INIT-GS moves)))


(define (repeat n thunk)
  (cond
    [(< n 0)
     (thunk)]
    [else
     (thunk)
     (repeat (- n 1) thunk)]))


(define (sample-run)
  (run '("L" "L" "L" "L" "L"
             "U" "U" "U" "U" "U" 
             "R" "R" "R" "R" "R" "R" "R" "R" "R" "R"
             "D" "D" "D" "D" "D" "D"
             "L" "L" "L"
             "U" "U" "L" "U" "L" "L")))


;; Board Natural Natural -> CellValue
;; looks up the value of a Board cell
(define (board-ref board x y)
  (vector-ref (vector-ref board y) x))

;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-tick handler:


;; GameState -> GameState
;; advances the game
(define (tick gs)
  (let* ([pm     (gs-pm gs)]
         [board  (gs-board gs)]
         [score  (gs-score gs)]
         [new-pm    (tick-sprite pm board)]
         [new-board (tick-board board new-pm)]
         [new-score (tick-score board new-board score)])
    (make-gs new-pm
             new-board
             new-score)))

;; Board Sprite -> Board
;; updates the board by removing a dot if pac-man is in a cell with a dot
(define (tick-board board pm)
  (let* ([pos (sprite-pos pm)]
        [x   (posn-x pos)]
        [y   (posn-y pos)])
    (if (string=? (board-ref board x y) D)
        (new-board x y board)
        board)))

;; Number Number Board -> Board
;; produces a new board with an empty cell at x, y
(define (new-board x y board)
  (build-vector (vector-length board)
                (lambda (j)
                  (build-vector (vector-length (vector-ref board j))
                                (lambda (i)
                                  (if (and (= y j) (= x i))
                                      E
                                      (board-ref board i j)))))))

;; Board Board Score -> Score
;; increases the score if the board has changed
(define (tick-score last-board new-board score)
  (if (equal? last-board new-board)
      score
      (add1 score)))

;; Sprite Board -> Sprite
;; updates sprite's position based on its direction
(define (tick-sprite sprite board)
  (let* ([pos     (sprite-pos sprite)]
         [dir     (sprite-dir sprite)]
         [new-pos (move-posn pos dir)]
         [new-x   (posn-x new-pos)]
         [new-y   (posn-y new-pos)])
         (make-sprite (cond [(string=? (board-ref board new-x new-y) E) new-pos]
                       [(string=? (board-ref board new-x new-y) W)      pos]
                       [(string=? (board-ref board new-x new-y) D)   new-pos])
                 dir)))

;; Posn Direction -> Posn
;; produces pac-man's new position based on its direction
(define (move-posn pos dir)
  (let ([x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(string=? dir "U")  (make-posn       x  (sub1 y))]
          [(string=? dir "D")  (make-posn       x  (add1 y))]
          [(string=? dir "L")  (make-posn (sub1 x)        y)]
          [(string=? dir "R")  (make-posn (add1 x)        y)])))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------

;; GameStateDirection -> GameState
;; changes pac-man's Direction to dir
(define (set-pm-dir gs dir)
  (make-gs (make-sprite (sprite-pos (gs-pm gs)) dir)
           (gs-board gs)
           (gs-score gs)))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; stop-when handler:


;; GameState -> Boolean
;; determines if pac-man has eaten all the dots
(define (game-over? gs)
  (empty-board? (gs-board gs)))

;; Board -> Boolean
;; determines if the board is empty
(define (empty-board? board)
  (letrec ([;; Natural -> Boolean
            ;; determines if all rows have no dots
            empty-rows? 
            (lambda (j)
              (cond [(= j (vector-length board)) #t]
                    [else
                     (and (empty-row? (vector-ref board j) 0)
                          (empty-rows? (add1 j)))]))]
          
           ;; (vectorof CellValue) Natural -> Boolean
           ;; determines if a single row has no dots
           [empty-row? (lambda (row i)
                         (cond [(= i (vector-length row)) #t]
                               [else
                                (and (not (string=? (vector-ref row i) D))
                                     (empty-row? row (add1 i)))]))])
    (empty-rows? 0)))


(define (display-board board)
  (for-each (lambda (row)
              (for-each display (vector->list row))
              (newline))
            (vector->list board)))


(let loop ([i 1000])
  (cond [(> i 0)
         (run '("L" "L" "L" "L" "L"
                    "U" "U" "U" "U" "U" 
                    "R" "R" "R" "R" "R" "R" "R" "R" "R" "R"
                    "D" "D" "D" "D" "D" "D"
                    "L" "L" "L"
                    "U" "U" "L" "U"))
         (loop (- i 1))]
        [else
         (let ([gs 
                (run '("L" "L" "L" "L" "L"
                           "U" "U" "U" "U" "U" 
                           "R" "R" "R" "R" "R" "R" "R" "R" "R" "R"
                           "D" "D" "D" "D" "D" "D"
                           "L" "L" "L"
                           "U" "U" "L" "U"))])
           (display (gs-score gs))
           (newline)
           (display-board (gs-board gs)))]))
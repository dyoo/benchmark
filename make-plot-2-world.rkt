#lang racket/base
(require 2htdp/universe
         2htdp/image
         racket/stream
         "make-plot-2.rkt")

(define (stream-lift/naturals f)
  (let loop ([i 0])
    (stream-cons (f i)
                 (loop (add1 i)))))

     
(define (get-picture w)
  (apply above 
         (text (format "Day ~a" (- w)) 30 "black")
         (make-plot/day w)))


(define pictures (stream-lift/naturals get-picture))

(define (draw w)
  (stream-ref pictures w))


(define (key w k)
  (cond
    [(key=? k "up")
     (max 0 (sub1 w))]
    [(key=? k "down")
     (add1 w)]
    [(key=? k "1")
     (+ w 1)]
    [(key=? k "2")
     (+ w 2)]
    [(key=? k "3")
     (+ w 3)]
    [(key=? k "4")
     (+ w 4)]
    [(key=? k "u")
     0]
    [else
     w]))

(big-bang 0
          (to-draw draw)
          (on-key key))


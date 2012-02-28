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
    [else
     w]))

(big-bang 0
          (to-draw draw)
          (on-key key))


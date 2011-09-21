#lang planet dyoo/whalesong/base

(define (f x)
  (* x x))

(display (f (f (f (f 1024)))))
(newline)

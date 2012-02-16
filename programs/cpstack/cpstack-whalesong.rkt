#lang planet dyoo/whalesong/base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         cpstak.sch
; Description:  continuation-passing version of TAK
; Author:       Will Clinger
; Created:      20-Aug-87
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (sub1 x)
             y
             z
             (lambda (v1)
               (tak (sub1 y)
                    z
                    x
                    (lambda (v2)
                      (tak (sub1 z)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  (tak x y z (lambda (a) a)))
 
;;; call: (cpstak 18 12 6)
 
(let loop ((n 20) (v 0))
  (if (zero? n)
      (begin (display v) (newline))
      (loop (sub1 n)
	    (cpstak 18 12 2))))

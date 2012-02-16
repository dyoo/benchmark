#lang planet dyoo/whalesong/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         tak.sch
; Description:  TAK benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 09:58:18 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; TAK -- A vanilla version of the TAKeuchi function

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (sub1 x) y z)
           (tak (sub1 y) z x)
           (tak (sub1 z) x y))))
 
;;; call: (tak 18 12 6)
(letrec ([loop (lambda (n v)
		 (if (zero? n)
		     (begin (display v) (newline))
		     (loop (sub1 n) (tak 18 12 6))))])
  (loop 1500 0))
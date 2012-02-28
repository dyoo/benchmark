#lang racket/base

(require plot
         racket/list
         racket/set
         "make-plots.rkt")

(provide make-plot/day)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (histogram-with-points data)
  (define-values (histogram-points label-points)
    (for/fold ([histogram-points '()]
               [label-points '()])
              ([data-point data]
               [i (in-naturals)])
       (define floored-val (inexact->exact (floor (second data-point))))
       (values (cons (discrete-histogram (list (vector (first data-point) (second data-point)))
                                         #:x-min i)
                     histogram-points)
               (cons (point-label (vector (+ i 0.5) (second data-point)) 
                                  (string-append (number->string floored-val) "X"))
                     label-points))))
  (append histogram-points label-points))


(define distinct-days
  (sort (set->list (list->set (apply append (for/list ([class+data analyzed-points])
                                        (define the-class (first class+data))
                                        (define the-data (second class+data))
                                        (map analyzed-point-day the-data)))))
        >))
    
(define (make-plot/day [day 0])
  (define latest-points 
    (apply append
           (for/list ([class+data analyzed-points])
             (define the-class (first class+data))
             (define the-data (second class+data))
             (define the-filtered-data
               (filter (lambda (a) (= (analyzed-point-day a) (list-ref distinct-days day)))
                       the-data))
             (for/list ([d the-filtered-data])
               (list the-class (analyzed-point-platform d) (analyzed-point-times-slower d))))))
  
  
  ;; whalesong-points: (listof (list [test-type string] [times-racket number]))
  (define whalesong-points (for/list ([p latest-points]
                                      #:when (and (regexp-match #px"whalesong" (second p))
                                                  (not (regexp-match #px"^test$" (first p)))))
                             (list (first p) (third p))))
  
  (define (halve l)
    (define len (length l))
    (values (take l (quotient len 2))
            (drop l (quotient len 2))))
  
  (define (make-plot whalesong-points)
    (plot (histogram-with-points whalesong-points)
          #:y-max (max 200 (+ (apply max (map second whalesong-points)) 10))
          #:title "Performance of the JS-based evaluator relative to Racket"
          #:x-label "Benchmark programs"
          #:y-label "X times slower than native Racket"
          #:width 1200))
  
  (let-values ([(first-half second-half) (halve whalesong-points)])
    (list (make-plot first-half)
          (make-plot second-half))))

;(make-plot/day 0)